//
// Common funtions I like to use in all my projects to aid in debugging,
// memory allocation, etc.
//

const std = @import("std");
const print = std.debug.print;
const os = std.os;
const mem = std.mem;
const Allocator = mem.Allocator;
const Wyhash = std.hash.Wyhash;

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

pub fn debugPrintB(text: []const u8, data: anytype) void {
  print("{s} {b}\n", .{ text, data });
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


    pub fn getPosition(self: *Self) usize
    {
      return self.curr_pos;
    }

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

    pub fn pushArray(self: *Self, comptime T: type, count: usize) ![]align(@alignOf(T)) T
    {
      return self.push(T, @alignOf(T), count * @sizeOf(T));
    }

    pub fn pushArrayAligned(self: *Self, comptime T: type, comptime alignment: u32, count: usize)
      ![]align(alignment) T
    {
      return self.push(T, alignment, count * @sizeOf(T));
    }

    fn push(self: *Self, comptime T: type, comptime alignment: u32, num_bytes: usize)
      ![]align(alignment) T
    {
      const start_addr = @ptrToInt(self.base_memory.ptr) + self.curr_pos;
      const start_alig = mem.alignForward(start_addr, alignment);
      const end_mem = @ptrToInt(self.base_memory.ptr) + self.base_memory.len;

      if (start_alig + num_bytes <= end_mem) {
        const padding = start_alig - start_addr;
        self.curr_pos += padding + num_bytes;
        return @intToPtr([*]align(alignment) T, start_alig)[0 .. num_bytes/@sizeOf(T)];
      } else {
        return error.NotEnoughMemory;
      }
    }

    pub fn popAmount(self: *Self, num_bytes: usize) !void
    {
      if (num_bytes > self.curr_pos) return error.AttemptToFreeBeyondTotalAllocation;

      return self.popTo(self.curr_pos - num_bytes);
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



// comment this
//
fn hash(comptime T: type, key: T) u64
{
  return Wyhash.hash(0, mem.asBytes(&key));
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


/////////////////////////////
// TODO(mathias): if we implement with a power of two table size.. we can replace:
// value % capacity, with value & (capacity -1).

/////////////////////////////
// TODO(mathias): when we design our set api we're going to want to be careful..not sure
// how the void value will impact the state of some of the internals.
/////////////////////////////
// TODO(mathias): think about how to use this API for sets

// do a final comparison test between mine and stdlib where we're both using pow2 table sizes

// try changing from indexing into to ptr arithmetic (for final version only)

// make note about cost of carrying the allocator around like this
// if we want to go into an embedded environ it may be best for caller to provide everytime
// consider doing what std lib does and making an unmanaged that is leaner
//
pub fn HashMap(comptime K: type, comptime V: type) type
{
  return struct {
    metadata: ?[*]Metadata = null,
    count: u32 = 0,
    available: u32 = 0,
    allocator: Allocator,

    const Self = @This();
    const MAX_LOAD_PERCENTAGE: u32 = 80;

    const Header = struct {
      keys: [*]K,
      values: [*]V,
      capacity: u32,
    };

    const Metadata = packed struct {
      fingerprint: u7 = 0,
      used: u1 = 0,

      fn isFull(self: Metadata) bool
      {
        return self.used == 1;
      }

      fn fill(self: *Metadata, fingerprint: u7) void
      {
        self.used = 1;
        self.fingerprint = fingerprint;
      }

      fn takeFingerprint(hashed: u64) u7
      {
        return @truncate(u7, hashed >> (64 - 7));
      }

      fn isEmpty(self: Metadata) bool
      {
        return @bitCast(u8, self) == 0;
      }

      fn isTombstone(self: Metadata) bool
      {
        return (self.used == 0 and self.fingerprint == 1);
      }

      fn setTombstone(self: *Metadata) void
      {
        self.used = 0;
        self.fingerprint = 1;
      }
    };

    comptime {
      std.debug.assert(@sizeOf(Metadata) == 1);
      std.debug.assert(@alignOf(Metadata) == 1);
    }

    const ModifyOrInsertResult = struct {
      key_ptr:   *K,
      value_ptr: *V,
      found_existing: bool,
    };

    const Entry = struct {
      key:   K,
      value: V,
    };

    // for internal use only
    const GetIndexOrNextResult = struct {
      found_existing: bool,
      index: usize,
    };

    pub fn capacity(self: *Self) u32
    {
      return self.header().capacity;
    }

    pub fn init(allocator: Allocator) !Self
    {
      const default_size: u32 = 16;
      return try initCapacity(allocator, default_size);
    }

    pub fn initCapacity(allocator: Allocator, cap: u32) !Self
    {
      const next_capacity = try std.math.ceilPowerOfTwo(u32, cap);

      var result = Self {
        .metadata = null,  // alloc function will set this
        .count = 0,
        .available = next_capacity * MAX_LOAD_PERCENTAGE / 100,
        .allocator = allocator,
      };

      try result.alloc(allocator, next_capacity);

      return result;
    }

    pub fn deinit(self: *Self) void
    {
      self.dealloc();
      self.* = undefined;
    }

    /// Standard insert method, will check for duplicate, and if not, for capacity.
    /// Returns optional Entry struct which, when not null, carries old key/value.
    pub fn insert(self: *Self, key: K, value: V) !?Entry
    {
      const find = self.getIndexOrNext(key);
      if (find.found_existing) {
        const old_key = &self.header().keys[find.index];
        const old_val = &self.header().values[find.index];

        const result = Entry {
          .key   = old_key.*,
          .value = old_val.*,
        };

        old_key.* = key;
        old_val.* = value;

        return result;
      } else {
        const hashed = hash(K, key);
        const fingerprint = Metadata.takeFingerprint(hashed);

        if (self.available == 0) {
          try self.grow();
          const find2 = self.getIndexOrNext(key);
          self.metadata.?[find2.index].fill(fingerprint);
          self.header().keys[find2.index] = key;
          self.header().values[find2.index] = value;
        } else {
          self.metadata.?[find.index].fill(fingerprint);
          self.header().keys[find.index] = key;
          self.header().values[find.index] = value;
        }
      
        self.count += 1;
        self.available -= 1;
        return null;
      }
    }

    /// Will check for duplicate and if not, for capacity. Returns pointers to key/value
    pub fn modifyOrInsert(self: *Self, key: K) !ModifyOrInsertResult
    {
      var result: ModifyOrInsertResult = undefined;

      const find = self.getIndexOrNext(key);
      if (find.found_existing) {
        result.found_existing = true;
        result.key_ptr = &self.header().keys[find.index];
        result.value_ptr = &self.header().values[find.index];
        return result;
      } else {
        result.found_existing = false;

        const hashed = hash(K, key);
        const fingerprint = Metadata.takeFingerprint(hashed);
        var key_ptr: *K = undefined;
        var val_ptr: *V = undefined;

        if (self.available == 0) {
          try self.grow();
          const find2 = self.getIndexOrNext(key);
          self.metadata.?[find2.index].fill(fingerprint);
          key_ptr = &self.header().keys[find2.index];
          val_ptr = &self.header().values[find2.index];
        } else {
          self.metadata.?[find.index].fill(fingerprint);
          key_ptr = &self.header().keys[find.index];
          val_ptr = &self.header().values[find.index];
        }

        result.key_ptr = key_ptr;
        result.value_ptr = val_ptr;
        key_ptr.* = key;
        val_ptr.* = undefined;

        self.count += 1;
        self.available -= 1;
        return result;
      }
    }

    /// Assumes capacity and does not check if key is duplicate, just inserts.
    pub fn insertAssumeCapacityNoMember(self: *Self, key: K, value: V) void
    {
      const hashed = hash(K, key);
      const fingerprint = Metadata.takeFingerprint(hashed);

      const mask = self.capacity() - 1;
      var probe: u64 = 0;
      var idx = (hashed + probe) & mask;
      var i: u64 = 0;
      while (self.metadata.?[idx].isFull()) {
        i += 1;
        probe = (i * i + i) >> 1;
        idx = (hashed + probe) & mask;
      }

      self.metadata.?[idx].fill(fingerprint);
      self.header().keys[idx] = key;
      self.header().values[idx] = value;

      self.count += 1;
      self.available -= 1;
    }

    pub fn contains(self: *Self, key: K) bool
    {
      const result = self.getIndexOrNext(key);
      return result.found_existing;
    }

    pub fn get(self: *Self, key: K) ?V
    {
      const find = self.getIndexOrNext(key);
      if (find.found_existing) return self.header().values[find.index];
      return null;
    }

    pub fn remove(self: *Self, key: K) ?Entry
    {
      const find = self.getIndexOrNext(key);
      if (find.found_existing) {
        self.metadata.?[find.index].setTombstone();
        self.count -= 1;
        self.available += 1;
        return Entry {
          .key = self.header().keys[find.index],
          .value = self.header().values[find.index],
        };
      }
      return null;
    }

    inline fn getIndexOrNext(self: *Self, key: K) GetIndexOrNextResult
    {
      const hashed = hash(K, key);
      const fingerprint = Metadata.takeFingerprint(hashed);
      const mask = self.capacity() - 1;

      var probe: u64 = 0;
      var idx = (hashed + probe) & mask;
      var metadata = self.metadata.? + idx;

      var first_tombstone_idx: usize = self.capacity(); // invalid index represents not set
      var i: u64 = 0;
      while (!metadata[0].isEmpty()) {
        if (metadata[0].isFull() and fingerprint == metadata[0].fingerprint) {
          if (key == self.header().keys[idx]) {
            return GetIndexOrNextResult {
              .found_existing = true,
              .index = idx,
            };
          }
        } else if (first_tombstone_idx == self.capacity() and metadata[0].isTombstone()) {
          first_tombstone_idx = idx;
        }
        i += 1;
        probe = (i * i + i) >> 1;
        idx = (hashed + probe) & mask;
        metadata = self.metadata.? + idx;
      }

      // In cases where we don't find the key we can return the index of the first
      // tombstone we encountered along the probe path to use for insertions.
      // This reduces future probing paths.
      // But more importantly, this is required when we start inserting identical keys
      // that were previously removed, as without this they would be inserted into empty
      // slots, filling beyond the table load factor, eventually leading above loop to run
      // indefinitely. This is caused by us not checking for the need to grow until after
      // the search. And we shouldn't grow, we should insert insert in first tombstone.
      if (first_tombstone_idx < self.capacity()) idx = first_tombstone_idx;

      return GetIndexOrNextResult {
        .found_existing = false,
        .index = idx,
      };
    }

    /// For debugging purposes, prints the map's fields plus memory to stderr
    pub fn debugPrintMap(self: *Self, comptime format_char: u8) void
    {
      const cap = self.capacity();

      debugPrintD("Count:", self.count);
      debugPrintD("Load %:", self.count * 100 / cap);
      debugPrintD("Available:", self.available);
      debugPrintD("Capacity:", cap);

      // create slice of memory region pertaining to metadata, keys, values
      const hdr_align: u32 = @alignOf(Header);
      const key_align = if (@sizeOf(K) == 0) 1 else @alignOf(K);
      const val_align = if (@sizeOf(V) == 0) 1 else @alignOf(V);
      const max_align = comptime @max(hdr_align, @max(key_align, val_align));

      const meta_start = 0;
      const meta_end = meta_start + (@sizeOf(Metadata) * cap);
      const key_start = mem.alignForward(meta_end, key_align);
      const key_end = key_start + (@sizeOf(K) * cap);
      const val_start = mem.alignForward(key_end, val_align);
      const val_end = val_start + (@sizeOf(V) * cap);
      const buff_end = mem.alignForward(val_end, max_align);

      const slice_start = @ptrCast([*]u8, self.metadata.?);
      const slice = slice_start[0..buff_end];
      debugPrintMem(slice, format_char);
    }

    fn header(self: *Self) *Header
    {
      const aligned = @alignCast(@alignOf(Header), self.metadata.?);
      const ptr = @ptrCast([*]Header, aligned);
      return @ptrCast(*Header, ptr - 1);
    }

    fn grow(self: *Self) !void
    {
      const new_size = self.capacity() << 1;
      var new_map = Self {
        .metadata = null,  // alloc function will set this
        .count = 0,        // updated below in this function
        .available = new_size * MAX_LOAD_PERCENTAGE / 100,
        .allocator = self.allocator,
      };
      try new_map.alloc(self.allocator, new_size);
      defer new_map.deinit();

      // populate new map with old map's entries
      if (self.count != 0) {
        const metadata = self.metadata.?;
        const keys = self.header().keys;
        const values = self.header().values;
        var idx: u32 = 0;
        while (idx < self.capacity()) : (idx += 1) {
          if (metadata[idx].isFull()) {
            new_map.insertAssumeCapacityNoMember(keys[idx], values[idx]);
            if (new_map.count == self.count)
              break;
          }
        }
      }

      mem.swap(Self, &new_map, self);
    }

    fn alloc(self: *Self, allocator: Allocator, new_size: u32) !void
    {
      const hdr_align: u32 = @alignOf(Header);
      const key_align = if (@sizeOf(K) == 0) 1 else @alignOf(K);
      const val_align = if (@sizeOf(V) == 0) 1 else @alignOf(V);
      const max_align = comptime @max(hdr_align, @max(key_align, val_align));

      // find number of bytes the new HashMap will need
      const hdr_start: usize = 0;
      const meta_start = hdr_start + @sizeOf(Header);
      const meta_end = meta_start + (@sizeOf(Metadata) * new_size);
      const key_start = mem.alignForward(meta_end, key_align);
      const key_end = key_start + (@sizeOf(K) * new_size);
      const val_start = mem.alignForward(key_end, val_align);
      const val_end = val_start + (@sizeOf(V) * new_size);
      const buff_end = mem.alignForward(val_end, max_align);

      const num_bytes = buff_end - hdr_start;
      const memory = try allocator.alignedAlloc(u8, max_align, num_bytes);
      const base_ptr = @ptrToInt(memory.ptr);

      const hdr = @intToPtr(*Header, base_ptr);
      hdr.capacity = new_size;
      if (@sizeOf([*]K) != 0) {
        hdr.keys = @intToPtr([*]K, base_ptr + key_start);
      }
      if (@sizeOf([*]V) != 0) {
        hdr.values = @intToPtr([*]V, base_ptr + val_start);
      }

      self.metadata = @intToPtr([*]Metadata, base_ptr + meta_start);
      @memset(self.metadata.?[0..new_size], Metadata{});
    }

    fn dealloc(self: *Self) void
    {
      if (self.metadata == null) return;

      const cap = self.capacity();
      const hdr_align: u32 = @alignOf(Header);
      const key_align = if (@sizeOf(K) == 0) 1 else @alignOf(K);
      const val_align = if (@sizeOf(V) == 0) 1 else @alignOf(V);
      const max_align = comptime @max(hdr_align, @max(key_align, val_align));

      const hdr_start: usize = 0;
      const meta_start = hdr_start + @sizeOf(Header);
      const meta_end = meta_start + (@sizeOf(Metadata) * cap);
      const key_start = mem.alignForward(meta_end, key_align);
      const key_end = key_start + (@sizeOf(K) * cap);
      const val_start = mem.alignForward(key_end, val_align);
      const val_end = val_start + (@sizeOf(V) * cap);
      const buff_end = mem.alignForward(val_end, max_align);

      const slice_start = @intToPtr([*]align(max_align) u8, @ptrToInt(self.header()));
      self.allocator.free(slice_start[0..buff_end]);

      self.metadata = null;
      self.available = 0;
    }
  };
}




// run the below through lldb check out what happens with voids 

pub fn main() !void {
  var arena = try Arena().init();
  defer arena.release();
  const allocator = arena.allocator();

  var map = try HashMap(u32, u32).init(allocator);
  defer map.deinit();

  for (0..25) |key| {
    _ = try map.insert(@intCast(u32, key), @intCast(u32, key + 1));
  }

  map.debugPrintMap('d');

  for (0..25) |key| {
    _ = map.remove(@intCast(u32, key));
  }

  map.debugPrintMap('d');
}
