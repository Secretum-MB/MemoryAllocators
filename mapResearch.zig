//
// Play with quadratic probing sequences
//

const std = @import("std");
const math = std.math;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;

const base = @import("base.zig");
const Arena = base.Arena();


/////////////////////////////
// TODO(mathias): Implement the ability to use either. We're going to want to see which actually,
// performs better. I think cache eviction might be better for triangular, but clustering might be 
// worse. I wonder too if growing by a factor of 2 each time is not expensive in terms of memory..


/////////////////////////////
// TODO(mathias): when we do timing tests, include the std libraries hashMap too (make sure to pre-request
// the capacity we'll use)



// fameous for its terrible clustering effects.. but i wonder if table large enough and hash
// function good enough if this is simply not an issue.
//
fn linearProbe(i: i32) i32
{
  return i;
}

// This form of quadratic probing grows slowly from the origin, but obviously faster than linear
// probing. Promises to visit all positions in table, before any repeats, if requirement below 
// is met. The question for it is whether there is bad clustering.
// It requires that the table size be a power of 2. This means that it can grow quite large.
//
fn triangularProbe(i: i32) i32
{
  return (i * i + i) >> 1;
}

// This is the simplest form of alternating quadratic probing simply alternating +-i^2.
// Promises to visit all positions of the table, before any repeats, if the table size
// conforms to the requirement of beign a prime number and congruent to 3 % 4, or 
// simply prime_table_size % 4 == 3
// The questions are, can negative offsets cause additional cache misses? Is clustering
// noticably better?
//
fn alternatingProbe(i: i32) i32
{
  // if i is even we want offset to be Negative, else Positive
  const sign: i32 = if (i & 1 == 0) -1 else 1;
  return i * i * sign;
}


fn hashGivenPos(hash_value: usize, probe_offset: i32, table_size: usize) usize
{
  const offset = @intCast(i128, hash_value) + probe_offset;
  if (offset >= 0) return @intCast(usize, offset) % table_size;

  const abs_offset = math.absInt(offset) catch unreachable;
  const neg_offset = @intCast(usize, abs_offset) % table_size;
  return table_size - neg_offset;
}


fn numProbesToExploreEntireTable(allocator: Allocator, 
                                 table_size: usize,
                                 probe_strategy: *const fn(i32)i32
) !i32 {
  var prob_iter: i32 = 0;
  var needed_hits_left = @intCast(i32, table_size);
  var set_of_seen_pos = std.AutoHashMap(usize, void).init(allocator);

  while (true): (prob_iter += 1) {
    const probe_pos = hashGivenPos(table_size/2, probe_strategy(prob_iter), table_size);
    const is_dup = try set_of_seen_pos.fetchPut(probe_pos, {});

    if (is_dup == null) needed_hits_left -= 1;
    if (needed_hits_left == 0) return prob_iter + 1;

    if (is_dup != null) return error.HitSamePosMaybeWeAreCycling;
  }
}


test "test: is it true that these probing strategies are non-cyclic and can't fail?:" {
  var arena = try base.Arena().init();
  defer arena.release();
  const allocator = arena.allocator();
  const a_save_point = arena.getScratch();

  // TEST LINEAR PROBE STRATEGY:
  {
    const table_sizes: [9]usize = .{ 1<<4, 1<<5, 1<<6, 1<<7, 1<<8, 1<<9, 1<<10, 1<<11, 1<<12};

    // TEST 1: num probes needed to explore all table
    for (0..table_sizes.len) |i| {
      const result = try numProbesToExploreEntireTable(allocator, table_sizes[i], linearProbe);
      try std.testing.expect(result == @intCast(i32, table_sizes[i]));
    }
    arena.releaseScratch(a_save_point);
  }

  { // TEST TRIANGLE STRATEGY:
    const table_sizes: [9]usize = .{ 1<<4, 1<<5, 1<<6, 1<<7, 1<<8, 1<<9, 1<<10, 1<<11, 1<<12};
    
    // TEST 1: num probes needed to explore all table
    for (0..table_sizes.len) |i| {
      const result = try numProbesToExploreEntireTable(allocator, table_sizes[i], triangularProbe);
      try std.testing.expect(result == @intCast(i32, table_sizes[i]));
    }
    arena.releaseScratch(a_save_point);
  }

  { // TEST ALTERNATING PROBING
    const table_sizes: [9]usize = .{ 19, 31, 59, 107, 199, 491, 907, 2003, 4079 };    
    
    // TEST 1: num probes needed to explore all table
    for (0..table_sizes.len) |i| {
      const result = try numProbesToExploreEntireTable(allocator, table_sizes[i], alternatingProbe);
      try std.testing.expect(result == @intCast(i32, table_sizes[i]));
    }
  }
}


// I coppied and pasted from std library: see hash_map.zig
// this is their default hashMap hashing function for non-string keys
fn hash(comptime T: type, key: T) u64
{
  return Wyhash.hash(0, std.mem.asBytes(&key));
}

const Tombstone = enum(u8) {
  Empty, Filled, Removed,
};

// we don't actually need to insert anything, we just need to read and write to tombstone.
// the function returns the number of probing efforts needed to make the insertion happen
fn hashTableInsert(comptime T: type, key: T, table: []Tombstone, table_size: usize,
                   probe_strategy: *const fn(i32)i32
) usize {
  const hashed = hash(T, key);
  const origin_pos = hashGivenPos(hashed, 0, table_size);
  if (table[origin_pos] != Tombstone.Filled) {
    table[origin_pos] = Tombstone.Filled;
    return 0;
  }

  var prob_iter: i32 = 1;
  while (true): (prob_iter += 1) {
    const next_pos = hashGivenPos(hashed, probe_strategy(prob_iter), table_size);
    if (table[next_pos] != Tombstone.Filled) {
      table[next_pos] = Tombstone.Filled;
      return @intCast(usize, prob_iter);
    }
  }
}

fn insertionProbingPerformance(arena: *Arena, table_sizes: []const usize, load_factor: f32,
                               probe_strategy: *const fn(i32)i32, strategy_name: []const u8
) !void {
  for (table_sizes) |table_size| {
    const table_max_cap = @floatToInt(u32, load_factor * @intToFloat(f32, table_size));
    var table = try arena.pushArray(Tombstone, table_size);
    @memset(table, Tombstone.Empty);

    // we'll track how many insertions were poorly performed
    var greater_5:  u32 = 0;
    var greater_15: u32 = 0;

    // Keys: hash monotonically increasing u32
    // I experimented with random ints and floats too and the results were indistinguishable
    for (0..table_max_cap) |key| {
      const result = hashTableInsert(u32, @intCast(u32, key), table, table_size, probe_strategy);
      if (result > 5 and result <= 15) { greater_5  += 1; }
      else if (result > 15)            { greater_15 += 1; }
    }
    // report results
    const total_insertions = @intToFloat(f32, table_max_cap);
    base.debugPrintS("\n\n", strategy_name);
    base.debugPrintD("table size: ", table_size);
    base.debugPrintD("MAX Load Factor: ", load_factor);
    base.debugPrintD("total insertions: ", table_max_cap);
    std.debug.print("{d} Insertions required more than 5 probes. {d:.3}% of total\n",
      .{ greater_5, @intToFloat(f32, greater_5) / total_insertions * 100 });
    std.debug.print("{d} Insertions required more than 15 probes. {d:.3}% of total\n",
      .{ greater_15, @intToFloat(f32, greater_15) / total_insertions * 100 });
  }
}

// test "probing count performance on table filling"
// {
//   var arena = try Arena.init();
//   defer arena.release();
//   const scratch = arena.getScratch();

//   const load_factor = 0.66;

//   { // Linear probing: conclusion: significantly worse than the quadratic strategies
//     // Load factor: 50% - mostly over 99% of insertions required less than 5 probes.
//     // Load factor: 66% - We are in the 95-97% range even for small tables.
//     // Load factor: 80% - many table sizes are in the 80-90%; larger tables have non-negl > 15!
//     const table_sizes: [9]usize = .{ 1<<4, 1<<5, 1<<6, 1<<7, 1<<8, 1<<9, 1<<10, 1<<11, 1<<12};
//     try insertionProbingPerformance(&arena, &table_sizes, load_factor, linearProbe, "LINEAR PROBING");
//     arena.releaseScratch(scratch);
//   }

//   { // Triangular probing: Not sure I can tell from these that its worse than Alternating
//     // Load factor: 50% - over 99% of insertions required fewer than 5 probes.
//     // Load factor: 66% - over 98% for small-medium tables; ~98% for large tables.
//     // Load factor: 80% - 96-98% required the same, with lower performance at larger table sizes.
//     const table_sizes: [9]usize = .{ 1<<4, 1<<5, 1<<6, 1<<7, 1<<8, 1<<9, 1<<10, 1<<11, 1<<12};
//     try insertionProbingPerformance(&arena, &table_sizes, load_factor, triangularProbe, "TRIANGULAR PROBING");
//     arena.releaseScratch(scratch);
//   }

//   { // Alternating probing: seems as good if not a tiny amount better than triangular.
//     // Load factor: 50% - over 99% for larger tables; ~100% for rest required fewer than 5 probes.
//     // Load factor: 66% - over 99% for small - medium tables; ~98% for large tables.
//     // Load factor: 80% - 96-97% fairly evenly throughout.
//     const table_sizes: [9]usize = .{ 19, 31, 59, 107, 199, 491, 907, 2003, 4079 };    
//     try insertionProbingPerformance(&arena, &table_sizes, load_factor, alternatingProbe, "ALTERNATING PROBING");
//   }
// }



/////////////////////////////
// NOTE(mathias): Below we implement 4 different hash tables. Here are the tests to do:
// 1. time to insert 1k, 100k, 200k,.. unique keys into the table.
// 2. ''  ''  lookup  ''    '' keys that are in the table.
// 3. ''  ''  lookup  ''    '' keys that are NOT in the table.
// 4. '' remove a randomly chosen 1/2 of the 1k, 100k, .. elements from the table.
// 5. compare tables against the segregated's ability to batch key/value lookup.
// 6. should we do tests where the table starts at a given load bearing?

//
// All HashMaps will use u32 as their Keys, for simplicity, but also ints are common keys.
//

// This has no metadata. It places the keys and values together in a struct within
// the data table. There are 2 dummy values needed of every key type (Empty & Tombstone)
// This has no problem working as a set too. Just initialize with V as void.
//
pub fn MapCarruthTogether(comptime V: type) type
{
  return struct {
    table: []Entry,
    count: u32 = 0,
    capacity: u32 = 0,
    available: u32 = 0,

    const Self = @This();

    // here are the dummy values the type must expose
    const key_empty: u32 = 0;
    const key_tombs: u32 = 1;

    const Entry = struct {
      key:   u32 = key_empty,
      value: V   = undefined,

      fn isAvailable(self: *Entry) bool {
        return self.key < 2;
      }

      fn isEmpty(self: *Entry) bool {
        return self.key == key_empty;
      }

      fn fill(self: *Entry, key: u32, value: V) void {
        self.key = key;
        self.value = value;
      }

      fn kill(self: *Entry) void {
        self.* = Entry{};
        self.key = key_tombs;
      }
    };

    pub fn init(arena: *Arena, num_buckets: u32) Self
    {
      const slice = arena.pushArray(Entry, num_buckets) catch unreachable;

      var result = Self {
        .table = slice,
        .capacity = num_buckets,
        .available = num_buckets,
      };
      @memset(result.table, Entry{});

      return result;
    }

    pub fn insertAssumeNoMember(self: *Self, key: u32, value: V, 
                                probe_strategy: *const fn(i32)i32) !void
    {
      if (key == 0 or key == 1) return error.TheseKeyValuesAreReserved;
      const hashed = hash(u32, key);
      var probe_cnt: i32 = 0;
      var idx = hashGivenPos(hashed, probe_strategy(probe_cnt), self.capacity);

      while (!self.table[idx].isAvailable()) {
        probe_cnt += 1;
        idx = hashGivenPos(hashed, probe_strategy(probe_cnt), self.capacity);
      }
      self.table[idx].fill(key, value);
      self.count += 1;
      self.available -= 1;
    }

    pub fn get(self: *Self, key: u32, probe_strategy: *const fn(i32)i32) !?V
    {
      if (key == 0 or key == 1) return error.TheseKeyValuesAreReserved;

      const hashed = hash(u32, key);
      var i: i32 = 0;
      var idx = hashGivenPos(hashed, probe_strategy(i), self.capacity);

      while (!self.table[idx].isEmpty()) {
        if (key == self.table[idx].key) {
          return self.table[idx].value;
        }
        i += 1;
        idx = hashGivenPos(hashed, probe_strategy(i), self.capacity);
      }
      
      return null;
    }

    pub fn remove(self: *Self, key: u32, probe_strategy: *const fn(i32)i32) !bool
    {
      if (key == 0 or key == 1) return error.TheseKeyValuesAreReserved;

      const hashed = hash(u32, key);
      var i: i32 = 0;
      var idx = hashGivenPos(hashed, probe_strategy(i), self.capacity);

      while (!self.table[idx].isEmpty()) {
        if (key == self.table[idx].key) {
          self.table[idx].kill();
          self.count -= 1;
          self.available += 1;
          return true;
        }
        i += 1;
        idx = hashGivenPos(hashed, probe_strategy(i), self.capacity);
      }

      return false;
    }
  };
}


// no metadata. the keys and values are segregated into two seperate, contigeous arrays.
// array of keys is array of pure keys, same with values. Two states from the key is
// reserved for the 'bucket' states.
//
pub fn MapCarruthSeperate(comptime V: type) type {
  return struct {
    keys: [*]u32,
    values: [*]V,
    count: u32,
    capacity: u32, // this is the num elements the backing buffer 'could' hold
    available: u32, // starts at capacity, but should start at max load factor

    const Self = @This();

    const key_empty = 0;
    const key_tombs = 1;

    pub fn init(arena: *Arena, num_buckets: u32) Self
    {
      const slice_key = arena.pushArray(u32, num_buckets) catch unreachable;
      const slice_val = arena.pushArray(V, num_buckets) catch unreachable;

      const ptr_key = @ptrCast([*]u32, slice_key.ptr);
      const ptr_val = @ptrCast([*]V, slice_val.ptr);
      @memset(slice_key, key_empty);

      return Self {
        .keys = ptr_key,
        .values = ptr_val,
        .count = 0,
        .capacity = num_buckets,
        .available = num_buckets,
      };
    }

    pub fn insertAssumeNoMember(self: *Self, key: u32, value: V,
                                probe_strategy: *const fn(i32)i32) !void
    {
      std.debug.assert(self.available >= 1);
      if (key == 0 or key == 1) return error.TheseKeyValuesAreReserved;

      const hashed = hash(u32, key);
      var probe_cnt: i32 = 0;
      var idx = hashGivenPos(hashed, probe_strategy(probe_cnt), self.capacity);
      while (self.keys[idx] >= 2) {  // while occupied
        probe_cnt += 1;
        idx = hashGivenPos(hashed, probe_strategy(probe_cnt), self.capacity);
      }
      self.keys[idx] = key;
      self.values[idx] = value;
      self.count += 1;
      self.available -= 1;
    }

    pub fn get(self: *Self, key: u32, probe_strategy: *const fn(i32)i32) !?V
    {
      if (key == 0 or key == 1) return error.TheseKeyValuesAreReserved;

      const hashed = hash(u32, key);
      var probe_cnt: i32 = 0;
      var idx = hashGivenPos(hashed, probe_strategy(probe_cnt), self.capacity);
      while (self.keys[idx] != key_empty) {
        if (key == self.keys[idx]) {
          return self.values[idx];
        }
        probe_cnt += 1;
        idx = hashGivenPos(hashed, probe_strategy(probe_cnt), self.capacity);
      }

      return null;
    }

    pub fn remove(self: *Self, key: u32, probe_strategy: *const fn(i32)i32) !bool
    {
      if (key == 0 or key == 1) return error.TheseKeyValuesAreReserved;

      const hashed = hash(u32, key);
      var probe_cnt: i32 = 0;
      var idx = hashGivenPos(hashed, probe_strategy(probe_cnt), self.capacity);
      while (self.keys[idx] != key_empty) {
        if (key == self.keys[idx]) {
          self.keys[idx] = key_tombs;
          self.count -= 1;
          self.available += 1;
          return true;
        }
        probe_cnt += 1;
        idx = hashGivenPos(hashed, probe_strategy(probe_cnt), self.capacity);
      }

      return false;
    }
  };
}


// table with metadata array followed by array of key/values sharing an array.
// Tables with metadata will not require reserved values from the set of keys.
//
pub fn MapDataOrientedTogether(comptime V: type) type
{
  return struct {
    metadata: [*]Metadata,
    available: u32,
    count: u32 = 0,

    const Self = @This();

    const Header = struct {
      entries: [*]Entry,
      capacity: u32,
    };

    const Metadata = packed struct {
      fingerprint: u7 = 0,
      used: u1 = 0,
    
      fn isEmpty(self: Metadata) bool {
        return @bitCast(u8, self) == 0;
      }

      fn isFilled(self: Metadata) bool {
        return self.used == 1;
      }

      fn fill(self: *Metadata, hashed: u64) void {
        self.used = 1;
        self.fingerprint = @truncate(u7, hashed >> (64-6)); // check for off by 1 errors
      }

      fn kill(self: *Metadata) void {
        self.* = Metadata{ .fingerprint = 1, .used = 0 };
      }
    };

    comptime {
      std.debug.assert(@sizeOf(Metadata) == 1);
      std.debug.assert(@alignOf(Metadata) == 1);
      std.debug.assert(@sizeOf(Header) == 16);
      std.debug.assert(@alignOf(Header) == 8);
    }

    const Entry = packed struct { // packed only for debugging
      key: u32,
      value: V,
    };

    pub fn init(arena: *Arena, num_buckets: u32) Self
    {
      const header_start = 0;
      const meta_start = header_start + @sizeOf(Header);
      const meta_end = meta_start + (@sizeOf(Metadata) * num_buckets);
      const entries_start = mem.alignForward(meta_end, @alignOf(Entry));
      const entries_end = entries_start + (@sizeOf(Entry) * num_buckets);
      const buffer_end = mem.alignForward(entries_end, @sizeOf(usize));

      // make sure you get 8 byte aligned mem for header
      const bytes_needed = buffer_end - header_start;

      base.debugPrintD("numbytesneeded: ", bytes_needed);

      const buffer = arena.push(usize, bytes_needed) catch unreachable;

      const ptr_header = @ptrCast(*Header, buffer.ptr);
      const ptr_metadata = @ptrCast([*]Metadata, buffer.ptr + (meta_start/8));
      const ptr_entries = @ptrCast([*]Entry, buffer.ptr + (entries_start/8));

      @memset(ptr_metadata[0..num_buckets], Metadata{ .fingerprint = 0b11, .used = 0});
      
      // no need to do this since metadata negates the need to look here
      // @memset(ptr_entries[0..num_buckets], Entry{.key=0xFFCCCCFF, .value=0xBBBBBBBB});
      @memset(ptr_entries[0..num_buckets], Entry{.key=0xFFCCCCFF, .value=-1 });

      ptr_header.* = Header{
        .entries = ptr_entries,
        .capacity = num_buckets,
      };

      return Self {
        .metadata = ptr_metadata,
        .available = num_buckets,
      };
    }



  };
}


fn printTableMetadata(table: anytype) void {
  base.debugPrintD("\ncount   :", table.count);
  // base.debugPrintD("capacity:", table.capacity);
  base.debugPrintD("avaiable:", table.available);
}


test "CarruthTogether" {
  var arena = try Arena.init();

  const V: type = usize;
  var foo = MapCarruthTogether(V).init(&arena, 128);

  for (2..98) |key| {
    try foo.insertAssumeNoMember(@intCast(u32, key), 0xFFFFADDE, triangularProbe);
  }

  for (2..150) |key| {
    _ = try foo.get(@intCast(u32, key), triangularProbe);
  }

  for (2..150) |key| {
    _ = try foo.remove(@intCast(u32, key), triangularProbe);
  }

  // printTableMetadata(foo);
  // base.debugPrintMem(foo.table, 'X');
}

test "CarruthSeperate" {
  var arena = try Arena.init();

  const V: type = u32;
  var map = MapCarruthSeperate(V).init(&arena, 128);

  for (2..98) |key| {
    try map.insertAssumeNoMember(@intCast(u32, key), 0xDEADBEEF, alternatingProbe);
  }

  for (2..150) |key| {
    _ = try map.get(@intCast(u32, key), alternatingProbe);
  }

  for (2..150) |key| {
    _ = try map.remove(@intCast(u32, key), alternatingProbe);
  }

  // printTableMetadata(map);
  // base.debugPrintMem(map.keys[0..map.capacity], 'X');
  // base.debugPrintMem(map.values[0..map.capacity], 'X');
}

test "DataOrientedTogether" {
  var arena = try Arena.init();

  const V: type = i16;
  var map = MapDataOrientedTogether(V).init(&arena, 128);




  // shows where the map's memory region ends
  var mem_trash = try arena.pushArray(u8, 5);
  @memset(mem_trash, 0x42);
  
  printTableMetadata(map);
  base.debugPrintMem(map.metadata[0..64], 'X');

  // shows memory behind the metadata ptr, this will show the Header
  const temp_ptr = map.metadata - 16;
  base.debugPrintMem(temp_ptr[0..16], 'X');
}



// You will need to make sure you dont insert beyond load factor.
// if you use a power of two table size, that may create cycles for alternateProbe


/////////////////////////////
// TODO(mathias): think about how to use this API for sets


pub fn main() !void
{
}