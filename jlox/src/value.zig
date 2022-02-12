const std = @import("std");
const ArrayList = std.ArrayList;

pub const Value = union(enum) {
    number: f64,
    // TODO how to free
    string: ArrayList(u8),
    boolean: bool,
    nil,

    pub fn number(n: f64) Value {
        return .{
            .number = n,
        };
    }

    pub fn string(s: ArrayList(u8)) Value {
        return .{
            .string = s,
        };
    }

    pub fn boolean(b: bool) Value {
        return .{
            .boolean = b,
        };
    }

    pub fn is_nil(self: Value) bool {
        switch (self) {
            .nil => true,
            else => false,
        }
    }

    pub fn is_string(self: Value) bool {
        switch (self) {
            .string => true,
            else => false,
        }
    }

    // special fn called by zig compiler
    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .number => |x| try std.fmt.format(out_stream, "{d}", .{x}),
            .string => |s| try std.fmt.format(out_stream, "\"{s}\"", .{s.items}),
            .boolean => |b| try std.fmt.format(out_stream, "{}", .{b}),
            .nil => try std.fmt.format(out_stream, "nil", .{}),
        }
    }

    pub fn equals(left: Value, right: Value) bool {
        return switch (left) {
            .number => |n1| switch (right) {
                .number => |n2| n1 == n2,
                else => false,
            },
            .string => |s1| switch (right) {
                .string => |s2| std.mem.eql(u8, s1.items, s2.items),
                else => false,
            },
            .boolean => |b1| switch (right) {
                .boolean => |b2| b1 == b2,
                else => false,
            },
            .nil => switch (right) {
                .nil => true,
                else => false,
            },
        };
    }

    /// only needed for string types, will no-op for others
    pub fn deinit(self: *Value) void {
        switch (self.*) {
            .string => |s| s.deinit(),
            else => {},
        }
    }
};
