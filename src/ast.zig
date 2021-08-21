const std = @import("std");

pub const Expr = union(enum) { literal: Literal, unary: Unary, binary: Binary, grouping: Grouping };

pub const Literal = union(enum) {
    number: f64,
    string: []const u8,
    @"true",
    @"false",
    nil,
};

pub const Unary = struct {
    op: UnaryOp,
    expr: *const Expr,
};

pub const UnaryOp = enum {
    minus,
    bang,
};

pub const Binary = struct {
    left: *const Expr,
    op: BinaryOp,
    right: *const Expr,
};

pub const BinaryOp = enum {
    equal_equal,
    bang_equal,
    less,
    less_equal,
    greater,
    greater_equal,
    plus,
    minus,
    star,
    slash,
};

pub const Grouping = struct {
    expr: *const Expr,
};

/// Visitor
const PrintAst = struct {
    const Self = @This();

    fn print(self: *Self, expr: Expr) void {
        self.visitExpr(expr);
    }

    fn visitExpr(self: *Self, expr: Expr) void {
        switch (expr) {
            .literal => |n| self.visitLiteral(n),
            .unary => |n| self.visitUnary(n),
            .binary => |n| self.visitBinary(n),
            .grouping => |n| self.visitGrouping(n),
        }
    }

    fn visitLiteral(self: *Self, literal: Literal) void {
        _ = self;
        switch (literal) {
            .number => |n| std.debug.print("{d}", .{n}),
            .string => |s| std.debug.print("{s}", .{s}),
            .@"true" => std.debug.print("true", .{}),
            .@"false" => std.debug.print("false", .{}),
            .nil => std.debug.print("nil", .{}),
        }
    }

    fn visitUnary(self: *Self, unary: Unary) void {
        std.debug.print("(", .{});
        std.debug.print("{s} ", .{unary.op});
        self.visitExpr(unary.expr.*);
        std.debug.print(") ", .{});
    }

    fn visitBinary(self: *Self, binary: Binary) void {
        std.debug.print("(", .{});
        std.debug.print("{s} ", .{binary.op});
        self.visitExpr(binary.left.*);
        self.visitExpr(binary.right.*);
        std.debug.print(") ", .{});
    }

    fn visitGrouping(self: *Self, grouping: Grouping) void {
        std.debug.print("(group ", .{});
        self.visitExpr(grouping.expr.*);
        std.debug.print(") ", .{});
    }
};

pub fn testPrintAst() void {
    const expr = Expr{ .binary = Binary{
        .left = &Expr{ .unary = Unary{ .op = UnaryOp.minus, .expr = &Expr{ .literal = Literal{ .number = 123 } } } },
        .op = BinaryOp.star,
        .right = &Expr{ .grouping = Grouping{ .expr = &Expr{ .literal = Literal{ .number = 46.67 } } } },
    } };

    var printer = PrintAst{};
    printer.print(expr);
}
