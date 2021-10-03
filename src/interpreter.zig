//! Interpreter uses Environments to keep track of scope (of blocks).
//! Each Environment's Scopes are associated with a Block Node Index.
//!
//! Entering a Block scope is handled in evalStmt `.stmt_block`
//! Exiting a Block scope is handled in evalNextStmt, because all the statements in the current block have been executed.

const std = @import("std");
const ast = @import("./ast.zig");
const env = @import("./environment.zig");
const token = @import("./token.zig");
const value = @import("./value.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Tree = ast.Tree;
const Node = ast.Node;
const Expr = ast.Expr;
const Env = env.Environment;
const Value = value.Value;
const TokenType = token.TokenType;

/// Looks a little weird, but ExprResult is for interpreter errors, and the ! is for allocation errors
///
/// It's easy to return a ExprResult at this stage because there's no need to collect errors, either
/// a statement succeeds or fails.
pub const Interpreter = struct {
    alloc: *Allocator,
    source: []const u8,
    tree: Tree,
    env: Env,

    const Self = @This();

    // Errors that are not related to interpreting
    const ErrorSet = std.mem.Allocator.Error || error{InvalidCharacter};

    pub fn init(alloc: *Allocator, source: []const u8, tree: Tree) !Self {
        return Self{
            .alloc = alloc,
            .source = source,
            .tree = tree,
            .env = try Env.init(alloc),
        };
    }

    pub fn deinit(self: *Self) void {
        self.env.deinit();
    }

    pub fn evalNextStmt(self: *Self) ErrorSet!?StmtResult {
        std.log.debug("EVAL NEXT STMT", .{});
        // Gets statements from the current block being executed.
        var stmts = self.tree.block_stmts(self.env.current_block());
        var curr = self.env.curr();

        if (curr < stmts.len) {
            const res = try self.evalStmt(stmts[curr]);
            _ = self.env.increment_curr();
            return res;
        } else {
            // try with the parent scope's next statement after block
            // rewind up to previous block that has a statement to execute that's not the
            // end of a block
            while (curr >= stmts.len) {
                switch (self.env.exit_scope()) {
                    .exit_local => {
                        std.log.debug("BLOCK: exit scope", .{});
                        stmts = self.tree.block_stmts(self.env.current_block());
                        curr = self.env.curr();
                        continue;
                    },
                    .exit_global => {
                        std.log.debug("EXIT: global scope", .{});
                        return null;
                    },
                }
            }
            _ = self.env.increment_curr();
            const res = try self.evalStmt(stmts[curr]);
            return res;
        }
    }

    fn evalStmt(self: *Self, idx: Node.Index) ErrorSet!StmtResult {
        const stmt = self.tree.nodes.get(idx);
        std.log.debug("EVAL_STMT: {}", .{stmt});

        switch (stmt.tag) {
            .stmt_print => {
                const expr_res = try self.evalExpr(stmt.data.lhs);
                switch (expr_res) {
                    .ok => |val| return StmtResult{ .ok = Effect{ .print = val } },
                    .err => |e| return StmtResult{ .err = e },
                }
            },
            .stmt_expr => {
                const expr_res = try self.evalExpr(stmt.data.lhs);
                switch (expr_res) {
                    .ok => |_| return StmtResult{ .ok = .no_effect },
                    .err => |e| return StmtResult{ .err = e },
                }
            },
            .stmt_block => {
                // Execute a statement inside of a block
                // Need to check if the block_ref here is the same as the
                // environment's current scope. If not, add a new scope before
                // executing
                //
                // self.env.curr() automatically keeps track of the current scope.
                const block_ref = idx;
                if (!self.env.is_current_block(block_ref)) {
                    std.log.debug("BLOCK: enter scope", .{});
                    try self.env.enter_scope(block_ref);
                }
                const block_stmts = self.tree.block_stmts(block_ref);
                const curr = self.env.curr();
                if (curr < block_stmts.len) {
                    const res = try self.evalStmt(block_stmts[curr]);
                    return res;
                } else {
                    // shouldn't reach this branch; if the end of the scope is reached,
                    // it will be caught in evalNextStmt
                    unreachable;
                }
            },
            .stmt_var_decl => {
                try self.env.define(self.tree.tokenSlice(stmt.main_token), .nil);
                return StmtResult{ .ok = .no_effect };
            },
            .stmt_var_decl_init => {
                const expr_res = try self.evalExpr(stmt.data.lhs);
                switch (expr_res) {
                    .ok => |val| {
                        try self.env.define(self.tree.tokenSlice(stmt.main_token), val);
                        return StmtResult{ .ok = .no_effect };
                    },
                    .err => |e| return StmtResult{ .err = e },
                }
            },
            else => unreachable, //only statements should be handled here
        }
    }

    fn evalExpr(self: *Self, idx: Node.Index) ErrorSet!ExprResult {
        const expr = self.tree.nodes.get(idx);
        return switch (expr.tag) {
            .expr_unary => {
                return self.evalUnary(expr);
            },
            .expr_binary => {
                return self.evalBinary(expr);
            },
            .expr_variable => {
                if (self.env.get(self.tree.tokenSlice(expr.main_token))) |val| {
                    return ok(val);
                } else {
                    return err(Error.new(self.alloc, "reference to undeclared variable: {s}", .{self.tree.tokenSlice(expr.main_token)}));
                }
            },
            .expr_assignment => {
                const val_res = try self.evalExpr(expr.data.lhs);
                const val = switch (val_res) {
                    .ok => |val| val,
                    .err => |e| return err(e),
                };
                if (self.env.assign(self.tree.tokenSlice(expr.main_token), val)) |_| {
                    return ok(val);
                } else |_| {
                    return err(Error.new(self.alloc, "tried to assign to undeclared variable: {s}", .{self.tree.tokenSlice(expr.main_token)}));
                }
            },
            .expr_grouping => {
                return self.evalExpr(expr.data.lhs);
            },
            .literal_number => {
                // error should be caught earlier in synatx/parsing
                const x = try std.fmt.parseFloat(f64, self.tree.tokenSlice(expr.main_token));
                return ok(Value.number(x));
            },
            .literal_string => {
                var buf = ArrayList(u8).init(self.alloc);
                try buf.writer().print("{s}", .{self.tree.tokenSlice(expr.main_token)});
                return ok(Value.string(buf));
            },
            .literal_true => return ok(Value.boolean(true)),
            .literal_false => return ok(Value.boolean(false)),
            .literal_nil => return ok(Value.nil),
            .expr_invalid => unreachable, // because would have stopped at parsing stage
            else => unreachable, // logic error, all expr should be handled here only
        };
    }

    fn evalUnary(self: *Self, unary: Node) ErrorSet!ExprResult {
        const val_res = try self.evalExpr(unary.data.lhs);
        const val = switch (val_res) {
            .ok => |val| val,
            .err => |e| return err(e),
        };

        const unary_op = self.tree.tokenType(unary.main_token);
        return switch (unary_op) {
            .minus => switch (val) {
                .number => |n| ok(Value.number(-n)),
                else => err(Error.new(self.alloc, "value '{}' not supported for '-'", .{val})),
            },
            .bang => ok(Value.boolean(!isTruthy(val))),
            else => unreachable, // logic error
        };
    }

    fn evalBinary(self: *Self, binary: Node) ErrorSet!ExprResult {
        const left_res = try self.evalExpr(binary.data.lhs);
        const right_res = try self.evalExpr(binary.data.rhs);
        // right and left val need to be var in case they need to be deinit
        // if there's an error
        var left_val = switch (left_res) {
            .ok => |val| val,
            .err => |e| return err(e),
        };
        var right_val = switch (right_res) {
            .ok => |val| val,
            .err => |e| return err(e),
        };

        const binary_op = self.tree.tokenType(binary.main_token);
        switch (binary_op) {
            .equal_equal => return ok(Value.boolean(left_val.equals(right_val))),
            .bang_equal => return ok(Value.boolean(!left_val.equals(right_val))),
            .plus => {
                // handle both concatenation and addition
                switch (left_val) {
                    .number => |n1| switch (right_val) {
                        .number => |n2| return ok(Value.number(n1 + n2)),
                        else => return self.checkNumberOperand(&right_val, binary_op),
                    },
                    .string => |s1| switch (right_val) {
                        .string => |s2| {
                            var buf = ArrayList(u8).init(self.alloc);
                            _ = try buf.writer().write(s1.items);
                            _ = try buf.writer().write(s2.items);

                            // we can deinit immediately because values inside an expression don't need to be kept in
                            // environment
                            s1.deinit();
                            s2.deinit();

                            return ok(Value.string(buf));
                        },
                        else => {
                            s1.deinit();
                            return err(Error.new(self.alloc, "value '{}' must be a string for op {}", .{ right_val, binary_op }));
                        },
                    },
                    else => return err(Error.new(self.alloc, "value '{}' must be a number or string for op {}", .{ left_val, binary_op })),
                }
            },
            else => {
                // both sides must be numbers here
                const left = switch (left_val) {
                    .number => |n| n,
                    else => return self.checkNumberOperand(&left_val, binary_op),
                };
                const right = switch (right_val) {
                    .number => |n| n,
                    else => return self.checkNumberOperand(&right_val, binary_op),
                };

                switch (binary_op) {
                    .minus => return ok(Value.number(left - right)),
                    .star => return ok(Value.number(left * right)),
                    .slash => return ok(Value.number(left / right)),
                    .plus => unreachable, //covered one layer up
                    .greater => return ok(Value.boolean(left > right)),
                    .greater_equal => return ok(Value.boolean(left >= right)),
                    .less => return ok(Value.boolean(left < right)),
                    .less_equal => return ok(Value.boolean(left <= right)),
                    .equal_equal => unreachable, //covered one layer up
                    .bang_equal => unreachable, //covered one layer up
                    else => unreachable, // only binary ops covered here
                }
            },
        }
    }

    fn checkNumberOperand(self: Self, val: *Value, op: TokenType) ExprResult {
        const e = switch (val.*) {
            .string => |s| err(Error.new(self.alloc, "value \"{s}\" must be a number for op {}", .{ s.items, op })),
            else => err(Error.new(self.alloc, "value '{s}' must be a number for op {}", .{ val, op })),
        };
        // is it a problem to hide the deinit here?
        // we know we don't need the value immediately after writing it to error message.
        val.deinit();
        return e;
    }
};

fn isTruthy(val: Value) bool {
    return switch (val) {
        .nil => false,
        .boolean => |b| b,
        else => true,
    };
}

pub const ExprResult = union(enum) {
    ok: Value,
    err: Error,
};

pub const StmtResult = union(enum) {
    ok: Effect,
    err: Error,
};

pub const Effect = union(enum) {
    print: Value,
    no_effect,
};

// TODO get line somehow
pub const Error = struct {
    msg: ArrayList(u8),

    fn new(alloc: *Allocator, comptime fmt: []const u8, args: anytype) Error {
        var buf = ArrayList(u8).init(alloc);
        // if there's an error trying to make error msg, just exit
        buf.writer().print(fmt, args) catch unreachable;

        return Error{
            .msg = buf,
        };
    }

    pub fn write_report(self: Error) void {
        std.debug.print("{s}\n", .{self.msg.items});
    }

    pub fn deinit(self: *Error) void {
        self.msg.deinit();
    }
};

fn ok(val: Value) ExprResult {
    return .{
        .ok = val,
    };
}

fn err(e: Error) ExprResult {
    return .{
        .err = e,
    };
}
