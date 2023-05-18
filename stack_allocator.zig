//
// Stack Allocator, or Fixed-Size Stack-like Allocator
//

 // This is the second allocator implemented here. It is similar to the Arena
 // Allocator but follows the LIFO methodology and allows the user to free the
 // allocations made - if they follow last-in-first-out order. To achieve this
 // we must save the padding required for the current memory allocation -
 // this must be kept for all allocations. This is achieved by placing a header
 // immediately before the memory allocation (after any needed padding for, now
 // the header and data). The Header is not saved within the structure, rather,
 // it is placed into the backing memory, just before the start of the allocation.
 // Freeing is done by simply decrementing the offsets to the start and end of
 // the previous allocation. For the start offset, this requires reading from the
 // header struct in memory. We also need to read the padding from the struct in
 // order to decrement both offsets. This allocator also allows us to resize the
 // most recent or a prior allocation. Note: the alignment is more complicated
 // for this as we now need the header and the data to be aligned, further,
 // the header wastes space that would otherwise be usable for our data. It was
 // therefore recommended that we use the Arena allocator with Save Points instead.


const std = @import("std");

const StackAllocatorHeader = struct {
  padding: u32,
  prev_offset: u32,
};

const StackAllocator = struct {
  base_memory: []u8,
  curr_offset: u32 = 0,
  prev_offset: u32 = 0,

  const HEADER_SIZE = @sizeOf(StackAllocatorHeader);
  const HEADER_ALIGNMENT = @alignOf(StackAllocatorHeader);

  pub fn init(backing_memory: []u8) StackAllocator
  {
    return StackAllocator{ .base_memory = backing_memory };
  }

  pub fn reset(self: *StackAllocator) void
  {
    self.curr_offset = 0;
    self.prev_offset = 0;
  }

  pub fn alloc(self: *StackAllocator, comptime T: type, count: u32) ![]T
  {
    const requested_memory = count * @sizeOf(T);
    const end_pos = @ptrToInt(self.base_memory.ptr + self.base_memory.len);
    const curr_offset = @ptrToInt(self.base_memory.ptr + self.curr_offset);
    const curr_offset_aligned = alignForward(curr_offset, @alignOf(T));
    if (curr_offset_aligned + HEADER_SIZE + requested_memory <= end_pos) {
      const padding = @truncate(u32, curr_offset_aligned - curr_offset);
      @intToPtr(*StackAllocatorHeader, curr_offset_aligned).* = .{
        .padding = padding,
        .prev_offset = self.prev_offset,
      };
      self.prev_offset = self.curr_offset + padding + HEADER_SIZE;
      self.curr_offset += padding + HEADER_SIZE + requested_memory;
      return @intToPtr([*]T, curr_offset_aligned + HEADER_SIZE)[0..count];
    } else {
      return error.NotEnoughMemory;
    }
  }

  pub fn resize(self: *StackAllocator, old_alloc: anytype, new_count: u32) !@TypeOf(old_alloc)
  {
    const beg_address = @ptrToInt(self.base_memory.ptr);
    const end_address = beg_address + self.base_memory.len;
    if (@ptrToInt(old_alloc.ptr) < beg_address or @ptrToInt(old_alloc.ptr) >= end_address) {
      return error.MemoryOutOfBoundsOfStack;
    }
    // are we resizing the previously allocated memory: if so, shrink expand it
    if (@ptrToInt(old_alloc.ptr) == beg_address + self.prev_offset) {
      const requested_memory = new_count * @typeInfo(@TypeOf(old_alloc)).Pointer.alignment;
      if (self.prev_offset + requested_memory <= self.base_memory.len) {
        self.curr_offset = self.prev_offset + requested_memory;
        return old_alloc.ptr[0..new_count];
      } else {
        return error.NotEnoughMemory;
      }
    } else if (@ptrToInt(old_alloc.ptr) > beg_address + self.prev_offset) {
      return error.AttemptToResizeMemoryAlreadyFreed;
    } else {
      // we're resizing memory older than the most recent allocation
      const T = @typeInfo(@TypeOf(old_alloc)).Pointer.child;
      var new_memory = try self.alloc(T, new_count);
      const copy_len = if (new_count < old_alloc.len) new_count else old_alloc.len;
      @memcpy(@ptrCast([*]u8, new_memory.ptr), @ptrCast([*]u8, old_alloc.ptr), copy_len);
      return new_memory;
    }
  }

  pub fn free(self: *StackAllocator, old_alloc: anytype) !void
  {
    const end_address = @ptrToInt(self.base_memory.ptr + self.base_memory.len);
    if (@ptrToInt(old_alloc.ptr) < @ptrToInt(self.base_memory.ptr) or
        @ptrToInt(old_alloc.ptr) >= end_address) {
      return error.MemoryOutOfBoundsOfStack;
    }
    const last_alloc = @ptrToInt(self.base_memory.ptr + self.prev_offset);
    if (@ptrToInt(old_alloc.ptr) == last_alloc) {
      const header = @intToPtr(*StackAllocatorHeader, last_alloc - HEADER_SIZE).*;
      self.curr_offset = self.prev_offset - HEADER_SIZE - header.padding;
      self.prev_offset = header.prev_offset;
      return;
    } else {
      return error.AttemptToFreeMemoryOtherThanLastAllocated;
    }
  }

  /////////////////////////////
  // NOTE(mathias): Helper functions
  fn isPowerOfTwo(x: usize) bool
  {
    return (x & (x-1)) == 0;
  }

  // This is not the most general solution to the 'alignment with header' problem,
  // but it is sufficient where the Header struct is not modifiable by the user.
  // What is returned here is the address where the caller should place the Header.
  // Many implementations return instead the address of where the data ought begin.
  //
  fn alignForward(ptr: usize, alignment: usize) usize
  {
    std.debug.assert(isPowerOfTwo(alignment));
    std.debug.assert(HEADER_ALIGNMENT == 4);
    std.debug.assert(HEADER_SIZE == 8);

    var aligned_ptr = ptr;
    const overlap_header = ptr & (HEADER_ALIGNMENT-1);
    if (overlap_header != 0) {
      aligned_ptr += HEADER_ALIGNMENT - overlap_header;
    }

    const overlap_data = aligned_ptr & (alignment-1);
    if (overlap_data != 0) {
      if ((overlap_data & (HEADER_ALIGNMENT-1)) == 0) {
        aligned_ptr += alignment - overlap_data;
      } else {
        unreachable; // Assumes Header will keep its 4/8 byte size/alignment
      }
    }
    return aligned_ptr;
  }

  /////////////////////////////
  // NOTE(mathias): For debug only
  fn print(self: *StackAllocator) void
  {
    std.debug.print("\nStackAllocator:  Prev: {d}; Curr: {d}\n",
                    .{self.prev_offset, self.curr_offset});
    var i: usize = 0;
    while (i < self.base_memory.len) {
      std.debug.print("{any}  ", .{self.base_memory[i..i+8]});
      i += 8;
      std.debug.print("{any}  ", .{self.base_memory[i..i+8]});
      i += 8;
      std.debug.print("{any}  ", .{self.base_memory[i..i+8]});
      i += 8;
      std.debug.print("{any}\n", .{self.base_memory[i..i+8]});
      i += 8;
    }
  }
};



test "basic stack allocator test" {
  var backing_mem: [256]u8 = undefined;
  var my_stack = StackAllocator.init(&backing_mem);

  var memory_1 = try my_stack.alloc(u16, 25);
  for (memory_1) |*ptr| {
    ptr.* = 16;
  }
  my_stack.print();

  var memory_2 = try my_stack.alloc(u32, 4);
  for (memory_2) |*ptr| {
    ptr.* = 32;
  }
  my_stack.print();

  var memory_3 = try my_stack.alloc(usize, 5);
  for (memory_3) |*ptr| {
    ptr.* = 64;
  }
  my_stack.print();

  var memory_4 = try my_stack.alloc(i8, 9);
  for (memory_4) |*ptr| {
    ptr.* = -1;
  }
  my_stack.print();

  var memory_5 = try my_stack.alloc(i16, 7);
  for (memory_5) |*ptr| {
    ptr.* = 5;
  }
  my_stack.print();

  var memory_6 = try my_stack.alloc(u256, 1);
  for (memory_6) |*ptr| {
    ptr.* = 128;
  }
  my_stack.print();

  var memory_7 = try my_stack.alloc(u8, 24);
  for (memory_7) |*ptr| {
    ptr.* = 1;
  }
  my_stack.print();

  try my_stack.free(memory_7);
  try my_stack.free(memory_6);
  try my_stack.free(memory_5);
  my_stack.print();

  var resized = try my_stack.resize(memory_4, 15);
  for (resized[0..]) |*ptr| {
    ptr.* = 66;
  }
  my_stack.print();

  var resized2 = try my_stack.resize(memory_3, 6);
  for (resized2[0..]) |*ptr| {
    ptr.* = 65;
  }
  my_stack.print();
}
