const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const tok = @import("./token.zig");
const Token = tok.Token;
const TokenType = tok.TokenType;

/// temporarily just an Expr
pub const Program = struct {
    alloc: *Allocator,
    stmts: ArrayList(Stmt),

    pub fn print_debug(self: Program, source: []const u8) void {
        var printer = PrintAst{ .source = source };
        printer.print(self);
    }

    pub fn deinit(self: *Program) void {
        for (self.stmts.items) |*stmt| {
            stmt.deinit(self.alloc);
        }
        self.stmts.deinit();
    }
};

pub const Stmt = struct {
    stmt_type: StmtType,
    expr: *Expr,

    pub fn deinit(self: *Stmt, alloc: *Allocator) void {
        self.expr.deinit(alloc);
        alloc.destroy(self.expr);
    }
};

pub const StmtType = enum {
    print,
    expr,
};

pub const Expr = union(enum) {
    literal: Literal,
    unary: Unary,
    binary: Binary,
    grouping: Grouping,
    // a marker so that we don't have to use exceptions
    // or leave an undefined Expr when parsing and hit an error.
    invalid,

    /// Can be called directly during failure to parse (i.e. group failing on paren)
    pub fn deinit(self: *Expr, alloc: *Allocator) void {
        switch (self.*) {
            .unary => |*n| n.deinit(alloc),
            .binary => |*n| n.deinit(alloc),
            .grouping => |*n| n.deinit(alloc),
            else => {},
        }
    }
};

pub const Literal = union(enum) {
    number: Span,
    string: Span,
    @"true",
    @"false",
    nil,

    /// Only convert on program execution
    const Span = struct {
        start: u64,
        len: u64,

        const Self = @This();

        pub fn slice(self: Self, bytes: []const u8) []const u8 {
            return bytes[self.start .. self.start + self.len];
        }
    };
};

pub const Unary = struct {
    op: UnaryOp,
    expr: *Expr,

    /// To be called only from root of AST
    fn deinit(self: *Unary, alloc: *Allocator) void {
        self.expr.deinit(alloc);
        alloc.destroy(self.expr);
    }
};

pub const UnaryOp = enum {
    minus,
    bang,

    pub fn from_token(t: Token) UnaryOp {
        return switch (t.token_type) {
            TokenType.minus => .minus,
            TokenType.bang => .bang,
            else => std.debug.panic("logic bug in caller", .{}),
        };
    }
};

pub const Binary = struct {
    left: *Expr,
    op: BinaryOp,
    right: *Expr,

    /// To be called only from root of AST
    fn deinit(self: *Binary, alloc: *Allocator) void {
        self.left.deinit(alloc);
        self.right.deinit(alloc);
        alloc.destroy(self.left);
        alloc.destroy(self.right);
    }
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

    pub fn from_token(t: Token) BinaryOp {
        return switch (t.token_type) {
            TokenType.equal_equal => .equal_equal,
            TokenType.bang_equal => .bang_equal,
            TokenType.less => .less,
            TokenType.less_equal => .less_equal,
            TokenType.greater => .greater,
            TokenType.greater_equal => .greater_equal,
            TokenType.plus => .plus,
            TokenType.minus => .minus,
            TokenType.star => .star,
            TokenType.slash => .slash,
            else => std.debug.panic("logic bug in caller", .{}),
        };
    }
};

pub const Grouping = struct {
    expr: *Expr,

    /// To be called only from root of AST
    fn deinit(self: *Grouping, alloc: *Allocator) void {
        self.expr.deinit(alloc);
        alloc.destroy(self.expr);
    }
};

/// Visitor
const PrintAst = struct {
    source: []const u8,

    const Self = @This();

    fn print(self: *Self, program: Program) void {
        std.debug.print("ast: ", .{});
        for (program.stmts.items) |stmt| {
            self.visitStmt(stmt);
            std.debug.print(";\n", .{});
        }
        std.debug.print("\n", .{});
    }

    fn visitStmt(self: *Self, stmt: Stmt) void {
        switch (stmt.stmt_type) {
            .expr => std.debug.print("exprStmt: ", .{}),
            .print => std.debug.print("printStmt: ", .{}),
        }
        self.visitExpr(stmt.expr.*);
    }

    fn visitExpr(self: *Self, expr: Expr) void {
        switch (expr) {
            .literal => |n| self.visitLiteral(n),
            .unary => |n| self.visitUnary(n),
            .binary => |n| self.visitBinary(n),
            .grouping => |n| self.visitGrouping(n),
            .invalid => std.debug.print("invalidExpr", .{}),
        }
    }

    fn visitLiteral(self: *Self, literal: Literal) void {
        _ = self;
        switch (literal) {
            .number => |span| std.debug.print("{s} ", .{span.slice(self.source)}),
            .string => |span| std.debug.print("\"{s}\" ", .{span.slice(self.source)}),
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
