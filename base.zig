//
// basic helper functions
//

const std = @import("std");
const print = std.debug.print;


pub fn printD(text: []const u8, data: anytype) void {
  print("{s} {d}\n", .{ text, data });
}

pub fn printS(text: []const u8, data: anytype) void {
  print("{s} {s}\n", .{ text, data });
}

pub fn printX(text: []const u8, data: anytype) void {
  print("{s} {X}\n", .{ text, data });
}

pub fn printAny(text: []const u8, data: anytype) void {
  print("{s} {any}\n", .{ text, data });
}

pub fn printMemD(memory: []u8) void {
  //print header
  print("\n", .{});
  for (0..32) |i| print("{d:3} ", .{ i });
  print("\n", .{});
  for (0..127) |_| print("{s}", .{ "-" });
  print("\n", .{});

  // data section
  const full_rows = memory.len / 32;
  var i: usize = 0;
  for (0..full_rows) |_| {
    for (0..32) |_| {
      print("{d:3} ", .{ memory[i] });
      i += 1;
    }
    print("\n", .{});
  }
  const remaining_mem = memory.len % 32;
  for (0..remaining_mem) |_| {
    print("{d:3} ", .{ memory[i] });
    i += 1;
  }
  print("\n\n", .{});
}

pub fn printMemX(memory: []u8) void {
  print("\n", .{});
  for (0..32) |i| print("{d:3} ", .{ i });
  print("\n", .{});
  for (0..127) |_| print("{s}", .{ "-" });
  print("\n", .{});

  const full_rows = memory.len / 32;
  var i: usize = 0;
  for (0..full_rows) |_| {
    for (0..32) |_| {
      print("{X:3} ", .{ memory[i] });
      i += 1;
    }
    print("\n", .{});
  }
  const remaining_mem = memory.len % 32;
  for (0..remaining_mem) |_| {
    print("{X:3} ", .{ memory[i] });
    i += 1;
  }
  print("\n\n", .{});
}



// test "test base debug printers" {

//   const a: usize = 45;
//   const b: i8 = -1;

//   const c: []const u8 = "Hello, World!";

//   const Foo = struct {
//     a: u32 = 42,
//     b: bool = false,
//   };

//   printD("my usize:", a);
//   printD("my i8:", b);
//   printS("my string", c);
//   printAny("Here is a struct", Foo{});

//   var memory: [260]u8 = undefined;
//   for (&memory) |*ptr| ptr.* = 255;
//   printMemD(&memory);
//   printMemH(&memory);
// }
