const std = @import("std");
const ast = @import("./ast.zig");
const token = @import("./token.zig");
const value = @import("./value.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Tree = ast.Tree;
const Node = ast.Node;
const Expr = ast.Expr;
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
    curr: u64,

    const Self = @This();

    // Errors that are not related to interpreting
    const ErrorSet = std.mem.Allocator.Error || error{InvalidCharacter};

    pub fn init(alloc: *Allocator, source: []const u8, tree: Tree) Self {
        return Self{
            .alloc = alloc,
            .source = source,
            .tree = tree,
            .curr = 0,
        };
    }

    pub fn evalNextStmt(self: *Self) !?StmtResult {
        const stmts = self.tree.stmts();
        if (self.curr < stmts.len) {
            const res = try self.evalStmt(stmts[self.curr]);
            self.curr += 1;
            return res;
        } else {
            return null;
        }
    }

    fn evalStmt(self: *Self, idx: Node.Index) ErrorSet!StmtResult {
        const stmt = self.tree.nodes.get(idx);
        const stmt_type = switch (stmt.tag) {
            .stmt_print => StmtType.print,
            .stmt_expr => .expr,
            else => unreachable, //only stmts should be handled here
        };
        const expr_res = try self.evalExpr(stmt.data.lhs);

        switch (expr_res) {
            .ok => |val| return StmtResult{ .ok = Effect{ .stmt_type = stmt_type, .value = val } },
            .err => |e| return StmtResult{ .err = e },
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

pub const Effect = struct {
    stmt_type: StmtType,
    value: Value,
};

pub const StmtType = enum {
    print,
    expr,
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
