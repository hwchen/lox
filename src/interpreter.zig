const std = @import("std");
const ast = @import("./ast.zig");
const value = @import("./value.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Program = ast.Program;
const Expr = ast.Expr;
const Literal = ast.Literal;
const Unary = ast.Unary;
const Binary = ast.Binary;
const BinaryOp = ast.BinaryOp;
const Grouping = ast.Grouping;
const Stmt = ast.Stmt;
const StmtType = ast.StmtType;
const Value = value.Value;

/// Looks a little weird, but ExprResult is for interpreter errors, and the ! is for allocation errors
///
/// It's easy to return a ExprResult at this stage because there's no need to collect errors, either
/// a statement succeeds or fails.
pub const Interpreter = struct {
    alloc: *Allocator,
    source: []const u8,
    program: Program,
    curr: u64,

    const Self = @This();

    // Errors that are not related to interpreting
    const ErrorSet = std.mem.Allocator.Error || error{InvalidCharacter};

    pub fn init(alloc: *Allocator, source: []const u8, program: Program) Self {
        return Self{
            .alloc = alloc,
            .source = source,
            .program = program,
            .curr = 0,
        };
    }

    pub fn eval_next_stmt(self: *Self) !?StmtResult {
        const stmts = self.program.stmts.items;
        if (self.curr < stmts.len) {
            const res = try self.visitStmt(stmts[self.curr]);
            self.curr += 1;
            return res;
        } else {
            return null;
        }
    }

    fn visitStmt(self: *Self, stmt: Stmt) ErrorSet!StmtResult {
        const expr_res = try self.visitExpr(stmt.expr.*);

        switch (expr_res) {
            .ok => |val| return StmtResult{ .ok = Effect{ .stmt_type = stmt.stmt_type, .value = val } },
            .err => |e| return StmtResult{ .err = e },
        }
    }

    fn visitExpr(self: *Self, expr: Expr) ErrorSet!ExprResult {
        return switch (expr) {
            .literal => |n| try self.visitLiteral(n),
            .unary => |n| try self.visitUnary(n),
            .binary => |n| try self.visitBinary(n),
            .grouping => |n| try self.visitGrouping(n),
            .invalid => unreachable, // because would have stopped at parsing stage
        };
    }

    fn visitLiteral(self: *Self, literal: Literal) !ExprResult {
        const res = switch (literal) {
            .number => |span| {
                // error should be caught earlier in synatx/parsing
                const x = try std.fmt.parseFloat(f64, span.slice(self.source));
                return ok(Value.number(x));
            },
            .string => |span| {
                var buf = ArrayList(u8).init(self.alloc);
                try buf.writer().print("{s}", .{span.slice(self.source)});
                return ok(Value.string(buf));
            },
            .@"true" => return ok(Value.boolean(true)),
            .@"false" => return ok(Value.boolean(false)),
            .nil => return ok(Value.nil),
        };

        return ok(res);
    }

    fn visitUnary(self: *Self, unary: Unary) ErrorSet!ExprResult {
        const val_res = try self.visitExpr(unary.expr.*);
        const val = switch (val_res) {
            .ok => |val| val,
            .err => |e| return err(e),
        };

        return switch (unary.op) {
            .minus => switch (val) {
                .number => |n| ok(Value.number(-n)),
                else => err(Error.new(self.alloc, "value '{}' not supported for '-'", .{val})),
            },
            .bang => ok(Value.boolean(!isTruthy(val))),
        };
    }

    fn visitBinary(self: *Self, binary: Binary) !ExprResult {
        const left_res = try self.visitExpr(binary.left.*);
        const right_res = try self.visitExpr(binary.right.*);
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

        switch (binary.op) {
            .equal_equal => return ok(Value.boolean(left_val.equals(right_val))),
            .bang_equal => return ok(Value.boolean(!left_val.equals(right_val))),
            .plus => {
                // handle both concatenation and addition
                switch (left_val) {
                    .number => |n1| switch (right_val) {
                        .number => |n2| return ok(Value.number(n1 + n2)),
                        else => return self.checkNumberOperand(&right_val, binary.op),
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
                            return err(Error.new(self.alloc, "value '{}' must be a string for op {}", .{ right_val, binary.op }));
                        },
                    },
                    else => return err(Error.new(self.alloc, "value '{}' must be a number or string for op {}", .{ left_val, binary.op })),
                }
            },
            else => {
                // both sides must be numbers here
                const left = switch (left_val) {
                    .number => |n| n,
                    else => return self.checkNumberOperand(&left_val, binary.op),
                };
                const right = switch (right_val) {
                    .number => |n| n,
                    else => return self.checkNumberOperand(&right_val, binary.op),
                };

                switch (binary.op) {
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
                }
            },
        }
    }

    fn visitGrouping(self: *Self, grouping: Grouping) !ExprResult {
        return self.visitExpr(grouping.expr.*);
    }

    fn checkNumberOperand(self: Self, val: *Value, op: BinaryOp) ExprResult {
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
