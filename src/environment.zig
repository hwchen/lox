const std = @import("std");
const ast = @import("./ast.zig");
const value = @import("./value.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Node = ast.Node;
const Value = value.Value;

const Scope = struct {
    block_ref: Node.Index,
    // index of the current statement being executed
    // in the scope (in the block)
    curr_stmt: usize = 0,
    vars: StringHashMap(Value),

    const Self = @This();

    fn init(alloc: *Allocator, block_ref: Node.Index) Self {
        return .{
            .block_ref = block_ref,
            .curr_stmt = 0,
            .vars = StringHashMap(Value).init(alloc),
        };
    }

    fn deinit(self: *Self) void {
        self.vars.deinit();
    }
};

/// Global scope should always exist because exit_scope will never allow it.
/// So other methods don't need to check for underflow
pub const Environment = struct {
    alloc: *Allocator,
    /// index [0] is the root scope
    scopes: ArrayList(Scope),

    const Self = @This();

    pub fn init(alloc: *Allocator) !Self {
        var scopes = ArrayList(Scope).init(alloc);
        const global_scope = Scope.init(alloc, 0);
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

    pub fn curr(self: Self) usize {
        return self.current_scope().curr_stmt;
    }

    // returns value after increment
    pub fn increment_curr(self: *Self) usize {
        self.current_scope_mut().curr_stmt += 1;
        return self.current_scope().curr_stmt;
    }

    pub fn enter_scope(self: *Self, block_ref: Node.Index) !void {
        try self.scopes.append(Scope.init(self.alloc, block_ref));
    }

    pub fn exit_scope(self: *Self) ExitScope {
        if (self.len() > 1) {
            var scope = self.scopes.pop();
            scope.deinit();
            return .exit_local;
        }
        return .exit_global;
    }

    pub fn current_block(self: Self) Node.Index {
        return self.current_scope().block_ref;
    }

    pub fn is_current_block(self: Self, block_ref: Node.Index) bool {
        return self.current_scope().block_ref == block_ref;
    }

    fn scope_at(self: Self, idx: usize) Scope {
        return self.scopes.items[idx];
    }

    fn current_scope(self: Self) Scope {
        return self.scopes.items[self.len() - 1];
    }

    fn current_scope_mut(self: *Self) *Scope {
        return &self.scopes.items[self.len() - 1];
    }

    pub fn define(self: *Self, name: []const u8, val: Value) !void {
        try self.current_scope_mut().vars.put(name, val);
    }

    /// If not in current scope, check parent scopes
    pub fn get(self: Self, name: []const u8) ?Value {
        var idx = self.len();
        while (idx > 0) {
            idx -= 1;
            if (self.scope_at(idx).vars.get(name)) |val| {
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
            if (self.scope_at(idx).vars.getEntry(name)) |kv| {
                kv.value_ptr.* = val;
                return;
            }
        }
        return error.undefined_variable;
    }
};

pub const ExitScope = enum {
    exit_global,
    exit_local,
};
