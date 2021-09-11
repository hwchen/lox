const std = @import("std");
const value = @import("./value.zig");

const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Value = value.Value;

pub const Environment = struct {
    values: StringHashMap(Value),

    const Self = @This();

    pub fn init(alloc: *Allocator) Self {
        return Self{
            .values = StringHashMap(Value).init(alloc),
        };
    }

    pub fn deinit(self: *Self) void {
        self.values.deinit();
    }

    pub fn define(self: *Self, name: []const u8, val: Value) !void {
        try self.values.put(name, val);
    }

    pub fn get(self: Self, name: []const u8) ?Value {
        return self.values.get(name);
    }
};
