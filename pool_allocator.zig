//
// Pool Based Allocation 
//  (also called: bins, chunks, slots, or Block Allocator)
//

 // A Pool or Block Allocator works by dividing up the backing memory into pools
 // or blocks of equal size. When an allocation is requested a fixed-size block
 // is handed out. A Free List that is in-place (within the backing memory)
 // is kept to track the location of the next available block. A Free List
 // is a linked list where each node has a next pointer. The struct keeps an
 // pointer to the head of this list. When an allocation is made, the block
 // headed by the head node is returned to the user. Also, the struct's head
 // is set to the head's next pointer. When we free an allocation we place it
 // at the head of the Free List by setting its next pointer to the head and
 // then the struct's head to this freed block. 

 // This allocator allows us to free allocations in any order we wish.
 // The allocator will also re-use these freed allocations as new
 // requests for memory are made. The operations to allocate and free
 // are both O(1). These allocators are usually used when you need to
 // allocate a group of things that will all share a single lifetime.
 // The downsides are that all allocations must be of the same size 
 // (see reset() to understand why) and cannot be resized - making
 // them inpractical for dynamic arrays, for instance.


const std = @import("std");
const debug = @import("base.zig");


pub fn BlockAllocator() type
{
  return struct {
    base_memory: []align(8) u8,
    block_size: u32 = 128,  // a ludicrously small default (important to be power of 2)
    head: ?*BlockFreeNode,
  
    const Self = @This();

    const BlockFreeNode = struct {
      next: ?*BlockFreeNode,
    };

    // Here I add the requirement that the backing memory be a multiple of the
    // block size. This ensures that none of the backing memory is wasted.
    // (this is not done in a standard implementation.)
    //
    pub fn init(backing_memory: []align(8) u8, block_size: u32) Self
    {
      std.debug.assert(backing_memory.len >= block_size);
      std.debug.assert(backing_memory.len % block_size == 0);
      // Together ensure that all blocks will be properly aligned for Free List Nodes
      std.debug.assert(isPowerOfTwo(block_size));
      std.debug.assert(block_size > @sizeOf(BlockFreeNode));

      var block_allocator = Self {
        .base_memory = backing_memory,
        .block_size = block_size,
        .head = null,
      };

      block_allocator.reset(); // creates the Free List in backing memory
      return block_allocator;
    }

    // If the backing memory is not a multiple of the default block size, the
    // the remainder after the last block will simply be unusable (wasted).
    //
    pub fn initDefaultBlockSize(backing_memory: []align(8) u8) Self
    {
      var block_allocator = Self {
        .base_memory = backing_memory,
        .head = null,
      };

      std.debug.assert(backing_memory.len >= block_allocator.block_size);

      block_allocator.reset(); // creates the Free List in backing memory
      return block_allocator;
    }

    // We don't offset the memory returned by the size of the Free Node, we
    // let the user overwrite it with their data. The Free Node will be
    // restored to the block when it is freed or when the allocator resets.
    //
    pub fn alloc(self: *Self) ![]align(8) u8
    {
      if (self.head) |head| {
        self.head = head.*.next;
        return @ptrCast([*]align(8) u8, head)[0..self.block_size];
      } else {
        return error.BlockAllocatorOutOfMemory;
      }
    }

    pub fn free(self: *Self, old_alloc: []align(8) u8) !void
    {
      const start = @ptrToInt(self.base_memory.ptr);
      const end = start + self.base_memory.len;
      if (@ptrToInt(old_alloc.ptr) < start or @ptrToInt(old_alloc.ptr) >= end) {
        return error.MemoryDoesNotBelongToThisAllocator;
      }

      const this_block = @ptrCast(*BlockFreeNode, old_alloc.ptr);
      this_block.*.next = self.head;
      self.head = this_block;
    }

    // This marks all blocks as free by building a linked list out of the first
    // 8 bytes of each block. This algorithm works backwards. It sets the last
    // block's node's next pointer to null, after that it sets it to the prior
    // visited block.
    //
    pub fn reset(self: *Self) void
    {
      const num_blocks = self.base_memory.len / self.block_size;
      self.head = null;
      var i = num_blocks;
      while (i > 0): (i -= 1) {
        const ptr_to_node = @ptrToInt(&self.base_memory.ptr[(i-1) * self.block_size]);
        @intToPtr(*BlockFreeNode, ptr_to_node).*.next = self.head;
        self.head = @intToPtr(*BlockFreeNode, ptr_to_node);
      }
    }

    fn isPowerOfTwo(x: u32) bool
    {
      return (x & (x-1)) == 0;
    }
  };
}



test "pool/block allocator - basics" {
  var needed_mem: [1024]u8 align(8) = undefined;
  var my_pool = BlockAllocator().init(&needed_mem, 1024/4);
  debug.printS("The allocator after init, before use:", "");
  debug.printMemX(my_pool.base_memory);

  var mem_1 = try my_pool.alloc();
  for (mem_1) |*ptr| {
    ptr.* = 255;
  }
  debug.printS("After using the first block", "");
  debug.printMemX(my_pool.base_memory);

  var mem_2 = try my_pool.alloc();
  for (mem_2) |*ptr| {
    ptr.* = 0xEE;
  }
  debug.printS("After using the second block", "");
  debug.printMemX(my_pool.base_memory);

  var mem_3 = try my_pool.alloc();
  for (mem_3) |*ptr| {
    ptr.* = 0xDD;
  }
  debug.printS("After using the third block", "");
  debug.printMemX(my_pool.base_memory);

  var mem_4 = try my_pool.alloc();
  for (mem_4) |*ptr| {
    ptr.* = 0xCC;
  }
  debug.printS("After using the fourth block", "");
  debug.printMemX(my_pool.base_memory);

  try my_pool.free(mem_3);
  try my_pool.free(mem_1);
  try my_pool.free(mem_4);
  debug.printS("After doing many frees", "");
  debug.printMemX(my_pool.base_memory);

  var mem_5 = try my_pool.alloc();
  var mem_6 = try my_pool.alloc();
  var mem_7 = try my_pool.alloc();
  for (mem_5) |*ptr| ptr.* = 0;
  for (mem_6) |*ptr| ptr.* = 1;
  for (mem_7) |*ptr| ptr.* = 2;
  debug.printS("After doing reallocations", "");
  debug.printMemX(my_pool.base_memory);

  my_pool.reset();
  debug.printS("After resetting the allocator", "");
  debug.printMemX(my_pool.base_memory);
}

test "pool/block allocator - default size" {
  // notice with this one that the block size will not divide evenly into the
  // backing memory. We will only get the number of blocks that will fit fully
  // into the memory; the remaining memory will be unusable.
  // In this case, we'll get 2 blocks (128*2=256) the last 44 bytes are wasted.
  var needed_mem2: [300]u8 align(8) = undefined;
  var my_pool = BlockAllocator().initDefaultBlockSize(&needed_mem2);
  debug.printMemX(my_pool.base_memory);

  var mem_1 = try my_pool.alloc();
  for (mem_1) |*ptr| {
    ptr.* = 0xFF;
  }
  debug.printS("After using the first block", "");
  debug.printMemX(my_pool.base_memory);

  var mem_2 = try my_pool.alloc();
  for (mem_2) |*ptr| {
    ptr.* = 0x66;
  }
  debug.printS("After using the second block", "");
  debug.printMemX(my_pool.base_memory);
}
