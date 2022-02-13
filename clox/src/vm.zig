const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const OpCode = enum(u8) {
    op_return,
};

pub const Chunk = struct {
    code: ArrayList(OpCode),

    const Self = @This();

    pub fn init(alloc: Allocator) Self {
        return Self{
            .code = ArrayList(OpCode).init(alloc),
        };
    }

    pub fn deinit(self: Self) void {
        self.code.deinit();
    }

    pub fn append(self: *Self, opcode: OpCode) !void {
        try self.code.append(opcode);
    }
};
