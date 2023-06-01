//
// Common funtions I like to use in all my projects to aid in debugging,
// memory allocation, etc.
//

const std = @import("std");
const print = std.debug.print;
const os = std.os;
const mem = std.mem;
const Allocator = mem.Allocator;

const file = @This();


/////////////////////////////
// NOTE(mathias): print functions for debugging

pub fn debugPrintD(text: []const u8, data: anytype) void {
  print("{s} {d}\n", .{ text, data });
}

pub fn debugPrintS(text: []const u8, data: anytype) void {
  print("{s} {s}\n", .{ text, data });
}

pub fn debugPrintX(text: []const u8, data: anytype) void {
  print("{s} {X}\n", .{ text, data });
}

pub fn debugPrintAny(text: []const u8, data: anytype) void {
  print("{s} {any}\n", .{ text, data });
}

pub fn debugPrintMem(memory: anytype, comptime format_char: u8) void {
  // print header row
  print("\n         ", .{});
  for (0..32) |i| print("{d:3} ", .{ i });
  print("\n", .{});
  for (0..136) |_| print("{s}", .{ "-" });
  print("\n", .{});

  const size_of_elem = @sizeOf(@typeInfo(@TypeOf(memory)).Pointer.child); // wrong for arrays?
  var ptr_to_mem = @ptrToInt(memory.ptr);
  var remaining_bytes = size_of_elem * memory.len;
  var row_num: u32 = 0;

  const format_string = switch(format_char) {
    'd' => "{d:3} ",
    'X' => "{X:3} ",
    else => unreachable,
  };

  while (true) {
    if (remaining_bytes < 32) {

      print("{d:6} | ", .{ row_num * 32 });

      for (0..remaining_bytes) |_| {
        print(format_string, .{ @intToPtr(*u8, ptr_to_mem).* });
        ptr_to_mem += 1;
      }
      print("\n\n", .{});
      return;
    }

    remaining_bytes -= 32;

    print("{d:6} | ", .{ row_num * 32 });
    row_num += 1;

    for (0..32) |_| {
      print(format_string, .{ @intToPtr(*u8, ptr_to_mem).* });
      ptr_to_mem += 1;
    }
    print("\n", .{});

    if (remaining_bytes == 0) {
      print("\n\n", .{});
      return;
    }
  }
}


/////////////////////////////
// NOTE(mathias): Memory allocation

 // BACKGROUND:
 // I'm calling the below an Arena allocator in keeping with the authors who I was
 // inspiration by: Allen Webster and Ryan Fleury. Really it is an hybrid allocator
 // that is between an arena (or scratch allocator) and a stack allocator. It
 // seems to take the best from both, namely that you can create temporary,
 // scratch, memory using save points, in addition to pushing and popping the
 // offset within the backing memory. This implementation, like theirs, does
 // not use headers at the start of allocations to keep track of the amount of
 // padding between the allocations. It follows the arena approach of a simple
 // linear array of accessible memory. When we pop then it is possible we leave
 // some number of padding bytes that could have been freed. This is inefficient
 // memory wise, but trivially so. Also, as this allocator may be used to provide
 // your application with all its dynamic memory needs, it is likely to have
 // a large capacity, so the occasional padding bytes left won't be an issue.

 // I am not keeping an field for the previous position, something common in
 // other arena and stack allocators. This field is only necessary when you wish
 // to resize allocations, something that does not make sense here as the stack
 // is being conceived of as one large block. Given that we can pop arbitrarily
 // far back, it would be difficult to know what to do with this field anyway.

 // USAGE NOTES:
 // The basic init() function reserves virtual memory to the tune of
 // DEFAULT_RESERVE_SIZE. On Linux, memory is committed as pages are written to. The
 // various functions that set the offset backwards may potentially make a syscall
 // to decommit memory. All virtual memory is released when release() is called.

 // I implemented the Zig std library Allocator interface for the Arena. I tested
 // that using it with std.ArrayList works well. It should work with the rest of
 // the standard library's data structures needing allocators too.

 // TODO(mathias): 
 // The desired behavior of having memory committed only as needed by the
 // allocator, and leaving it reserved otherwise, is currently satisfied,
 // by default, on Linux. Other POSIX systems would require that we
 // modify the methods that involve allocating memory (pushing forward the
 // offset) to achieve this. The init methods would need to be updated too.

pub fn KB(comptime x: usize) usize { return x << 10; }
pub fn MB(comptime x: usize) usize { return x << 20; }
pub fn GB(comptime x: usize) usize { return x << 30; }

pub fn Arena() type
{
  return struct {
    base_memory: []align(mem.page_size) u8,
    curr_pos: usize = 0,

    const Self = @This();
    const DEFAULT_RESERVE_SIZE = GB(1);

    /// Initialize Arena with the default amount of reserved memory
    pub fn init() !Self
    {
      /////////////////////////////
      // NOTE(mathias): when an identical call to mmap is made by the std library,
      // for instance, by heap.page_allocator, the total memory requested is both
      // reserved and committed. (Because in debug zig initialized data with 0xAA,
      // thereby touching all the pages that were reserved by mmap.)
      // The solution I've found online is to either 1) map the file /dev/zero with
      // PROT_NONE, or 2) use MAP_ANONYMOUS with PROT_NONE. This would reserve the
      // memory but because PROT_NONE prevents reads/writes, the memory is not
      // committed. To commit memory as needed you would use mprotect() to alter the
      // permissions of the memory you want to commit.
      // However, when I make this call myself without such restrictions it appears
      // to be reserved without a commit. And When I begin to write to it, the OS
      // appears to commit just the amounts I write.
      //
      // I found out that the desired behavior is the default on Linux. This is
      // not necessarily true for other POSIX systems however. The above strategy
      // will work across POSIX systems. For now I will leave this as is..
      //
      const mmap = try os.mmap(
        null,
        mem.alignForward(DEFAULT_RESERVE_SIZE, mem.page_size),
        os.PROT.READ | os.PROT.WRITE,
        os.MAP.PRIVATE | os.MAP.ANONYMOUS,
        -1,
        0,
      );
      return Self { .base_memory = mmap };
    }

    /// Calls mmap and therefore the minimum allocation will be 4096 bytes (a page).
    pub fn initCapacity(capacity: usize) !Self
    {
      const mmap = try os.mmap(
        null,
        mem.alignForward(capacity, mem.page_size),
        os.PROT.READ | os.PROT.WRITE,
        os.MAP.PRIVATE | os.MAP.ANONYMOUS,
        -1,
        0,
      );
      return Self { .base_memory = mmap };
    }

    /// Releases all memory pages reserved by the Arena.
    pub fn release(self: *Self) void
    {
      os.munmap(self.base_memory);
    }

    pub fn push(self: *Self, comptime T: type, num_bytes: usize) ![]T
    {
      const start_addr = @ptrToInt(self.base_memory.ptr) + self.curr_pos;
      const start_alig = mem.alignForward(start_addr, @alignOf(T));
      const end_mem = @ptrToInt(self.base_memory.ptr) + self.base_memory.len;

      if (start_alig + num_bytes <= end_mem) {
        const padding = start_alig - start_addr;
        self.curr_pos += padding + num_bytes;
        return @intToPtr([*]T, start_alig)[0 .. num_bytes/@sizeOf(T)];
      } else {
        return error.NotEnoughMemory;
      }
    }

    pub fn pushArray(self: *Self, comptime T: type, count: usize) ![]T
    {
      return self.push(T, count * @sizeOf(T));
    }

    fn popTo(self: *Self, pos: usize) !void
    {
      if (pos > self.curr_pos) return error.CurrPositionBelowRequestedPosition;

      const old_addr = @ptrToInt(self.base_memory.ptr) + self.curr_pos;
      const new_addr = @ptrToInt(self.base_memory.ptr) + pos;
      const new_addr_aligned = mem.alignForward(new_addr, mem.page_size);

      // doesn't happen as often as you might think, new_addr aliged forward by page size
      if (new_addr_aligned < old_addr) {
        const decommit_size = old_addr - new_addr_aligned;
        _ = try os.madvise(
          @intToPtr([*]align(mem.page_size) u8, new_addr_aligned),
          decommit_size,
          os.MADV.DONTNEED,
        );
      }

      self.curr_pos = pos;
    }

    pub fn popAmount(self: *Self, num_bytes: usize) !void
    {
      if (num_bytes > self.curr_pos) return error.AttemptToFreeBeyondTotalAllocation;

      return self.popTo(self.curr_pos - num_bytes);
    }

    /// An save-point for our Arena.
    const ArenaTemp = struct {
      arena: *file.Arena(),
      curr_pos: usize,
    };

    /// The return value is an save-point for our Arena, use it to release the scratch.
    pub fn getScratch(self: *Self) ArenaTemp
    {
      return ArenaTemp { .arena = self, .curr_pos = self.curr_pos };
    }

    pub fn releaseScratch(self: *Self, skratch: ArenaTemp) void
    {
      self.popTo(skratch.curr_pos) catch unreachable;
    }

    /// The below mehods implement Zig's Allocator interface for my Arena
    pub fn allocator(self: *Self) Allocator
    {
      return .{
        .ptr = self,
        .vtable = &.{
          .alloc = alloc,
          .resize = resize,
          .free = free,
        },
      };
    }

    fn alloc(ctx: *anyopaque, n: usize, log2_ptr_align: u8, ra: usize) ?[*]u8
    {
      _ = ra;
      const self = @ptrCast(*file.Arena(), @alignCast(@alignOf(file.Arena()), ctx));
      const ptr_align = @as(usize, 1) << @intCast(Allocator.Log2Align, log2_ptr_align);
      const adjust_off = mem.alignPointerOffset(self.base_memory.ptr + self.curr_pos,
                                                ptr_align) orelse return null;
      const adjusted_index = self.curr_pos + adjust_off;
      const new_end_index = adjusted_index + n;
      if (new_end_index > self.base_memory.len) return null;
      self.curr_pos = new_end_index;
      return self.base_memory.ptr + adjusted_index;
    }

    fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_size: usize, ra: usize) bool
    {
      _ = buf_align;
      _ = ra;
      const self = @ptrCast(*file.Arena(), @alignCast(@alignOf(file.Arena()), ctx));

      // for FBA this test is warned to not always work.. I'm not sure about my case
      if (!self.isLastAllocation(buf)) {
        if (new_size <= buf.len) return true;
        std.debug.assert(false); // MB: Can't grow memory that is buried in Arena
        return false;            // MB: preventing this as it re-initiallizes mem of list
      }

      if (new_size <= buf.len) {
        const sub = buf.len - new_size;
        self.popAmount(sub) catch unreachable;
        return true;
      }

      const add = new_size - buf.len;
      if (add + self.curr_pos > self.base_memory.len) return false;
      self.curr_pos += add;
      return true;
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ra: usize) void
    {
      _ = buf_align;
      _ = ra;
      const self = @ptrCast(*file.Arena(), @alignCast(@alignOf(file.Arena()), ctx));
      if (self.isLastAllocation(buf)) {
        self.popAmount(buf.len) catch unreachable;
      }
    }

    fn isLastAllocation(self: *Self, buf: []u8) bool
    {
      const end_self = @ptrToInt(self.base_memory.ptr) + self.curr_pos;
      const end_buf  = @ptrToInt(buf.ptr) + buf.len;
      if (end_self == end_buf) return true;
      return false;
    }
  };
}



/////////////////////////////
// TODO(mathias): see this discussion: https://cs.stackexchange.com/questions/120946/quadratic-probing-infinite-loop
// for dealing with the evenual need to grow the table.
// we're not going to test the performance of growing (alloc new) table. So this todo pertains
// to when we actually implement our own Hash Table


/////////////////////////////
// the point of segregating the data too is that there is no padding introduced so more of the actual,
// data you want fit into the cache line. Placing metadata inside the buckets is a disaster as it takes
// the padding of the larger of key/value, potentially 8 bytes will be needed to store 2 what is essentially,
// just 2 bits of information (empty, full, tombstone).

// NOTE(mathias): let's do 1) metadata and key/values together (so no dummie T); 
// 2) metadata and key/values segregated (no dummies needed)
//
// I expect the variants that do not segregate wil be faster for individual finds as no cash eviction needed
// for looking up a found items value, and remember probing strategy won't require us to look very far.
// but what if you're doing operations in a loop.. if you segregate you could do something fancy like return
// a list of all the inexes appropriate for the various insertions, then loop over the segregated values array
// and populate values all at once.. this will not work if the arrays need to grow, in which case earlier keys
// inserted will be placed into the new array at different indexes than what you saved in your return list.
// (if we can ensureCapacity - easy enough, then this method is dope. (you'd want to sort the indexes)).
// but realize that this too is just a win if we're not inserting but just doing retrievals!



/////////////////////////////
// TODO(mathias): if we implement with a power of two table size.. we can replace:
// value % capacity, with value & (capacity -1).

/////////////////////////////
// TODO(mathias): build an api for pushArray but where we can pick custom alignment