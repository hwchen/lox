const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const OpCode = enum(u8) {
    op_return,

    const Self = @This();

    pub fn offset(self: Self) usize {
        return switch (self) {
            .op_return => 1,
        };
    }
};

pub const Chunk = struct {
    code: ArrayList(u8),

    const Self = @This();

    pub fn init(alloc: Allocator) Self {
        return Self{
            .code = ArrayList(u8).init(alloc),
        };
    }

    pub fn deinit(self: Self) void {
        self.code.deinit();
    }

    pub fn append_opcode(self: *Self, opcode: OpCode) !void {
        try self.code.append(@enumToInt(opcode));
    }

    pub fn disassemble(self: Self, name: []const u8) void {
        std.debug.print("== {s} chunk ==\n{any}", .{ name, self });
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        _ = fmt;
        _ = options;

        var i: usize = 0;
        while (i < self.code.items.len) {
            // undefined behavior if @intToEnum fails!
            const opcode = @intToEnum(OpCode, self.code.items[i]);
            try std.fmt.format(out_stream, "{d:0>4} {s}\n", .{ i, @tagName(opcode) });
            i += opcode.offset();
        }
    }
};
