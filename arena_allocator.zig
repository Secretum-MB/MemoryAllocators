//
// Linear Allocation, or Arena Allocator
//

const std = @import("std");
const assert = std.debug.assert;


/////////////////////////////
// NOTE(mathias): HELPER functions

// Bit of a bit trick. For a number to be a power of two it must have one
// 1 in it's binary representation. The rest must be zero. If x is the
// number in question, if it is a power of two, if we subtract 1 from it,
// it will decrement the 1, where-ever it is. When we & x and x-1, there
// cannot be any overlapping 1's so the result is 0. In the case where
// x is not a power of two, subtracting 1 from it cannot achieve what
// was achieved above, namely, it cannot decrement/remove ALL the 1's.
// The result of this x & x-1 may be many things, but it cannot be 0.
//
fn isPowerOfTwo(x: usize) bool
{
  return (x & (x-1)) == 0;
}

fn alignForward(ptr: usize, alignment: usize) usize
{
  assert(isPowerOfTwo(alignment));

  var next_ptr_addr = ptr;

  // Realize that any legitimate alignment is a power of two.
  // When you subtract 1 from a power of two, the 1 turns to 0 and the rest become
  // 1. If we & this value with ANY LARGER power of two, we get 0. And of course, a
  // power of two is a factor of any larger power of two - so there is no remainder.
  // (assumption: ptr has been previously established to be a power of two) (Same
  // as ptr % alignment (but this is faster, only use when values are pow2)
  const overlap = ptr & (alignment-1);

  if (overlap != 0) {
    // if there is overlap, the pointer address is not aligned
    // with the datatype that will be needed for next allocation.
    // Correct for this by incrementing the pointer address.
    next_ptr_addr += alignment - overlap;
  }

  return next_ptr_addr;
}


/////////////////////////////
// NOTE(mathias): Arena Allocator

const DEFAULT_ARENA_SIZE_FOR_TEST: u32 = 300;

const Arena = struct {
  base_memory: []u8,
  curr_offset: u32 = 0,
  prev_offset: u32 = 0,
};

// See below for discussion
//
const ArenaTemp = struct {
  arena: *Arena,
  curr_offset: u32,
  prev_offset: u32,
};


/////////////////////////////
// NOTE(mathias): Arena Allocator Functions

pub fn arena_init(backing_memory: []u8) Arena
{
  return Arena{ .base_memory = backing_memory };
}

pub fn arena_alloc(a: *Arena, comptime T: type, count: u32) ![]T
{
  const requested_memory = count * @sizeOf(T);
  const mem_end_int = @ptrToInt(a.base_memory.ptr + a.base_memory.len);
  const curr_int = @ptrToInt(a.base_memory.ptr + a.curr_offset);
  const curr_int_aligned = alignForward(curr_int, @alignOf(T));

  if (curr_int_aligned + requested_memory <= mem_end_int) {
    const padding = @truncate(u32, curr_int_aligned - curr_int);
    const curr_ptr_aligned = @intToPtr([*]T, curr_int_aligned);
    a.prev_offset = a.curr_offset + padding;
    a.curr_offset += requested_memory + padding;
    return curr_ptr_aligned[0..count];
  } else {
    return error.NotEnoughMemory;
  }
}

// if old_alloc is the most recent allocation we can shrink/expand it. Otherwise, we
// are forced to create a brand new allocation and copy old memory into new.
//
pub fn arena_resize(a: *Arena, old_alloc: anytype, new_count: u32) !@TypeOf(old_alloc)
{
  // make sure old_alloc belongs to this arena's memory space
  if (@ptrToInt(old_alloc.ptr) >= @ptrToInt(a.base_memory.ptr) and
      @ptrToInt(old_alloc.ptr) <= @ptrToInt(a.base_memory.ptr + a.base_memory.len)) {

    const requested_memory = new_count * @typeInfo(@TypeOf(old_alloc)).Pointer.alignment;

    // old_alloc is the most recent allocation from the allocator
    if (@ptrToInt(old_alloc.ptr) == @ptrToInt(a.base_memory.ptr + a.prev_offset)) {
      if (a.prev_offset + requested_memory <= a.base_memory.len) {
        a.curr_offset = a.prev_offset + requested_memory;
        return old_alloc.ptr[0..new_count];
      } else {
        return error.NotEnoughMemory;
      }
    // old_alloc is not the most recent allocation
    } else {
      const T = @typeInfo(@TypeOf(old_alloc)).Pointer.child;
      const new_memory = try arena_alloc(a, T, new_count);
      const copy_len = if (new_count < old_alloc.len) new_count else old_alloc.len;
      @memcpy(@ptrCast([*]u8, new_memory.ptr), @ptrCast([*]u8, old_alloc.ptr), copy_len);
      return new_memory;
    }
  } else {
    return error.MemoryOutOfBoundsOfArena;
  }
}

// this will invalidate any previous memory allocated by this arena
//
pub fn arena_reset(arena: *Arena) void
{
  arena.curr_offset = 0;
  arena.prev_offset = 0;
}


// The ability to create a save point (or maybe multiple?) can be very useful.
// Besides the memory which is being written to itself, the data structure
// really only is the offset fields. The ArenaTemp structure contains a pointer
// to the Arena as well as a copy of the two offset fields at the time the save
// is captured.
// The idea is then to use the Arena as you would and when you decide you want
// some space to play with - and to throw away later - you create a save point.
// This function will save the arena's offsets into the TempArena structure.
// You then use the original arena as you would - creating as many allocations,
// resizing, etc. as you please. When you are ready to throw away all of this you
// call the second function below which will restore the arena's original offset
// fields from before you started using it as scratch space. When you begin using
// the arena again, you will be overwriting the scratch memory.
//
pub fn arenaStartSavePoint(a: *Arena) ArenaTemp
{
  return ArenaTemp{
    .arena = a,
    .curr_offset = a.curr_offset,
    .prev_offset = a.prev_offset,
  };
}

pub fn arenaEndSavePoint(a_temp: ArenaTemp) void
{
  a_temp.arena.curr_offset = a_temp.curr_offset;
  a_temp.arena.prev_offset = a_temp.prev_offset;
}



/////////////////////////////
// NOTE(mathias): for Debug purposes only..
//
fn arena_print(arena: *Arena) void
{
  std.debug.print("arena: prev_offset: {d}, curr_offset: {d}\n", .{
    arena.prev_offset,
    arena.curr_offset,
  });

  // make sure the arena capacity is a multiple of 25
  var i: usize = 0;
  while (i < arena.base_memory.len): (i += 25) {
    std.debug.print("{any}\n", .{ arena.base_memory[i..i+25]});
  }
  std.debug.print("\n",.{});
}


/////////////////////////////
// NOTE(mathias): Test the allocator's various functionality

test "test basic usage of arena allocator" {
  const page_allocator = std.heap.page_allocator;
  const needed_memory  = try page_allocator.alloc(u8, DEFAULT_ARENA_SIZE_FOR_TEST);
  defer page_allocator.free(needed_memory);

  var my_arena = arena_init(needed_memory);

  const memory_1 = try arena_alloc(&my_arena, u8, 50);
  for (memory_1) |*ptr| {
    ptr.* = 255;
  }
  std.debug.print("\nAllocated and filled first memory region\n", .{});
  arena_print(&my_arena);

  // the 50 bytes above will not end on an alignment proper for the usizes below.
  // There is 2 bytes of overlap with a proper alignment, therefore, 6 bytes of
  // padding will be introduced by the below call to alloc.
  // There will be a total of 136 bytes allocated.
  const memory_2 = try arena_alloc(&my_arena, usize, 10);
  for (memory_2) |*ptr| {
      ptr.* = 127;
  }
  std.debug.print("Allocated and filled second memory region\n", .{});
  arena_print(&my_arena);

  // resize memory_2
  // this region is the last allocated one, so we can shrink/expand it instead
  // of making a new allocation. Here we grow to a total size of 15 usize.
  const memory_2_expanded = try arena_resize(&my_arena, memory_2, 15);
  for (memory_2_expanded[10..]) |*ptr| {
    ptr.* = 111;
  }
  std.debug.print("expanded and filled second memory region\n", .{});
  arena_print(&my_arena);

  // resize memory_1
  // Because we're expanding an old region, a brand new allocation will be made at end.
  // In this case, the contents of the old region will be copied to the new region.
  const memory_1_expanded = try arena_resize(&my_arena, memory_1, 71);
  for (memory_1_expanded[50..]) |*ptr| {
    ptr.* = 244;
  }
  std.debug.print("expanded and filled first memory region\n", .{});
  arena_print(&my_arena);

  // max out backing memory
  const memory_3 = try arena_alloc(&my_arena, u16, 26);
  for (memory_3) |*ptr| {
    ptr.* = 69;
  }
  std.debug.print("Allocated and filled third memory region\n", .{});
  arena_print(&my_arena);

  // reset arena
  arena_reset(&my_arena);
  std.debug.print("Reset the Arena\n", .{});
  arena_print(&my_arena);

  // reuse the arena
  const memory_reuse = try arena_alloc(&my_arena, i8, 300);
  for (memory_reuse) |*ptr| {
    ptr.* = -1;
  }
  std.debug.print("Reuse the Arena\n", .{});
  arena_print(&my_arena);
  return;
}

test "usage of temporary arena allocator storage" {
  var backing_memory: [250]u8 = undefined;
  var my_arena = arena_init(&backing_memory);
  var permenant_mem = try arena_alloc(&my_arena, u16, 75);
  for (permenant_mem) |*ptr| {
    ptr.* = 42;
  }
  arena_print(&my_arena);

  // we make a save point like this
  const arena_save = arenaStartSavePoint(&my_arena);

  // now we continue to use our original arena
  var memory_crap = try arena_alloc(&my_arena, u8, 80);
  for (memory_crap) |*ptr| {
    ptr.* = 69;
  }
  arena_print(&my_arena);

  // when we're finished with the scratch memory, we restore the arena like so
  arenaEndSavePoint(arena_save);

  // notice the offsets have been restored, any new allocations will now
  // overwrite the scratch memory we used above.
  arena_print(&my_arena);
  return;
}
