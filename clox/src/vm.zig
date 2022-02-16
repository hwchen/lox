const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const Value = f64;

pub const OpCode = enum(u8) {
    op_return,
    op_constant,

    const Self = @This();

    pub fn offset(self: Self) usize {
        return switch (self) {
            .op_return => 1,
            .op_constant => 2,
        };
    }
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),
    lines: ArrayList(usize),

    const Self = @This();

    pub fn init(alloc: Allocator) Self {
        return Self{
            .code = ArrayList(u8).init(alloc),
            .constants = ArrayList(Value).init(alloc),
            .lines = ArrayList(usize).init(alloc),
        };
    }

    pub fn deinit(self: Self) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn writeOpCode(self: *Self, op_code: OpCode, line: usize) !void {
        try self.code.append(@enumToInt(op_code));
        try self.lines.append(line);
    }

    pub fn writeConstant(self: *Self, constant: Value, line: usize) !void {
        // undefined behavior if i is >= 256. Looks like an implicit coercion in the book?
        const i = @intCast(u8, self.constants.items.len);
        try self.constants.append(constant);
        try self.writeOpCode(.op_constant, line);
        try self.code.append(i);
        // this one is to mirror the constant_idx i
        try self.lines.append(line);
    }

    // disassembly debug output
    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        _ = fmt;
        _ = options;

        var i: usize = 0;
        while (i < self.code.items.len) {
            // First print instruction offset
            // undefined behavior if @intToEnum fails!
            const op_code = @intToEnum(OpCode, self.code.items[i]);
            try std.fmt.format(out_stream, "{d:0>4} ", .{i});

            // Then print line number
            const line = self.lines.items[i];
            if (i > 0 and line == self.lines.items[i - 1]) {
                try std.fmt.format(out_stream, "   | ", .{});
            } else {
                try std.fmt.format(out_stream, "{d:>4} ", .{line});
            }

            // Then opcode
            try std.fmt.format(out_stream, "{s}", .{@tagName(op_code)});

            // Then extra data
            switch (op_code) {
                .op_constant => {
                    const constants_idx = self.code.items[i + 1];
                    try std.fmt.format(out_stream, "{d:>16} '{d}'", .{ constants_idx, self.constants.items[constants_idx] });
                },
                else => {},
            }
            try std.fmt.format(out_stream, "\n", .{});
            i += op_code.offset();
        }
    }
};
