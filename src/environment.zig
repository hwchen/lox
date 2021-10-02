const std = @import("std");
const value = @import("./value.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Value = value.Value;

const Scope = StringHashMap(Value);

/// Global scope should always exist because exit_scope will never allow it.
/// So other methods don't need to check for underflow
pub const Environment = struct {
    alloc: *Allocator,
    /// index [0] is the root scope
    scopes: ArrayList(Scope),

    const Self = @This();

    pub fn init(alloc: *Allocator) !Self {
        var scopes = ArrayList(Scope).init(alloc);
        const global_scope = Scope.init(alloc);
        try scopes.append(global_scope);

        return Self{
            .alloc = alloc,
            .scopes = scopes,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.deinit();
    }

    fn len(self: Self) usize {
        return self.scopes.items.len;
    }

    pub fn enter_scope(self: *Self) void {
        self.scopes.append(Scope.init(self.alloc));
    }

    pub fn exit_scope(self: *Self) void {
        if (self.len() != 0) {
            var scope = self.scopes.pop();
            scope.deinit();
        }
    }

    fn scope_at(self: Self, idx: usize) Scope {
        return self.scopes.items[idx];
    }

    fn current_scope(self: *Self) *Scope {
        return &self.scopes.items[self.len() - 1];
    }

    pub fn define(self: *Self, name: []const u8, val: Value) !void {
        try self.current_scope().put(name, val);
    }

    /// If not in current scope, check parent scopes
    pub fn get(self: Self, name: []const u8) ?Value {
        var idx = self.len();
        while (idx > 0) {
            idx -= 1;
            if (self.scope_at(idx).get(name)) |val| {
                return val;
            }
        }
        return null;
    }

    /// If not in current scope, check parent scopes
    pub fn assign(self: *Self, name: []const u8, val: Value) !void {
        var idx = self.len();
        while (idx > 0) {
            idx -= 1;
            if (self.scope_at(idx).getEntry(name)) |kv| {
                kv.value_ptr.* = val;
                return;
            }
        }
        return error.undefined_variable;
    }
};
