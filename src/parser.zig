const std = @import("std");
const ast = @import("./ast.zig");
const tok = @import("./token.zig");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ParseFloat = std.fmt.parseFloat;
const Token = tok.Token;
const TokenType = tok.TokenType;

const Program = ast.Program;
const Expr = ast.Expr;

/// Produces a Program. It's the caller's responsibility to deallocate the Program.
pub const Parser = struct {
    alloc: *Allocator,
    tokens: []const Token,
    errors: Errors,
    curr: u64,

    const Self = @This();

    pub fn init(alloc: *Allocator, tokens: []const Token) Self {
        return Self{
            .alloc = alloc,
            .tokens = tokens,
            .errors = Errors{ .errors = ArrayList(Error).init(alloc) },
            .curr = 0,
        };
    }

    /// Always need to return an AST, otherwise memory will be leaked
    pub fn parseProgram(self: *Self) !Program {
        var expr = try self.parseExpr();
        return Program{
            .expr = expr,
            // necessary for deinit
            .alloc = self.alloc,
        };
    }

    fn parseExpr(self: *Self) !*Expr {
        return try self.parseEquality();
    }

    fn parseEquality(self: *Self) !*Expr {
        var expr = try self.parseComparison();

        while (self.match_any(&[_]TokenType{ .bang_equal, .equal_equal })) {
            const op = ast.BinaryOp.from_token(self.previous());
            const right = try self.parseComparison();
            var new_expr = try self.alloc.create(Expr);
            new_expr.* = Expr{ .binary = .{ .left = expr, .op = op, .right = right } };
            expr = new_expr;
        }

        return expr;
    }

    fn parseComparison(self: *Self) !*Expr {
        var expr = try self.parseTerm();

        while (self.match_any(&[_]TokenType{ .greater, .greater_equal, .less, .less_equal })) {
            const op = ast.BinaryOp.from_token(self.previous());
            const right = try self.parseTerm();
            var new_expr = try self.alloc.create(Expr);
            new_expr.* = Expr{ .binary = .{ .left = expr, .op = op, .right = right } };
            expr = new_expr;
        }

        return expr;
    }

    fn parseTerm(self: *Self) !*Expr {
        var expr = try self.parseFactor();

        while (self.match_any(&[_]TokenType{ .plus, .minus })) {
            const op = ast.BinaryOp.from_token(self.previous());
            const right = try self.parseFactor();
            var new_expr = try self.alloc.create(Expr);
            new_expr.* = Expr{ .binary = .{ .left = expr, .op = op, .right = right } };
            expr = new_expr;
        }

        return expr;
    }

    fn parseFactor(self: *Self) !*Expr {
        var expr = try self.parseUnary();

        while (self.match_any(&[_]TokenType{ .star, .slash })) {
            const op = ast.BinaryOp.from_token(self.previous());
            const right = try self.parseUnary();
            var new_expr = try self.alloc.create(Expr);
            new_expr.* = Expr{ .binary = .{ .left = expr, .op = op, .right = right } };
            expr = new_expr;
        }

        return expr;
    }

    fn parseUnary(self: *Self) std.mem.Allocator.Error!*Expr {
        if (self.match_any(&[_]TokenType{ .bang, .minus })) {
            const op = ast.UnaryOp.from_token(self.previous());
            var expr = try self.parseUnary();
            var new_expr = try self.alloc.create(Expr);
            new_expr.* = Expr{ .unary = .{ .op = op, .expr = expr } };
            expr = new_expr;
            return expr;
        }

        return self.parsePrimary();
    }

    // Error on allocation failure
    fn parsePrimary(self: *Self) !*Expr {
        var res = try self.alloc.create(Expr);

        if (self.match(.@"false")) {
            res.* = Expr{ .literal = .@"false" };
            return res;
        }
        if (self.match(.@"true")) {
            res.* = Expr{ .literal = .@"true" };
            return res;
        }
        if (self.match(.@"nil")) {
            res.* = Expr{ .literal = .@"nil" };
            return res;
        }

        if (self.match(.string)) {
            const token = self.previous();
            res.* = Expr{ .literal = .{
                .string = .{ .start = token.start, .len = token.len },
            } };
            return res;
        }
        if (self.match(.number)) {
            const token = self.previous();
            res.* = Expr{ .literal = .{
                .number = .{ .start = token.start, .len = token.len },
            } };
            return res;
        }

        if (self.match(.left_paren)) {
            const inner = try self.parseExpr();
            if (self.consume(.right_paren, "Expected right paren")) |err| {
                try self.errors.errors.append(err);
                inner.deinit(self.alloc);
                self.alloc.destroy(inner);
                res.* = Expr.invalid;
                return res;
            }
            res.* = Expr{ .grouping = .{
                .expr = inner,
            } };
            return res;
        }

        // TODO parse ident, keywords, etc.
        try self.errors.errors.append(Error.new(self.alloc, self.peek(), "Expected expression"));
        res.* = Expr.invalid;
        return res;
    }

    // Helpers

    fn advance(self: *Self) Token {
        const res = self.peek();
        if (!self.isAtEnd()) {
            self.curr += 1;
        }
        return res;
    }

    fn peek(self: Self) Token {
        return self.tokens[self.curr];
    }

    fn check(self: Self, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().token_type == token_type;
    }

    fn previous(self: Self) Token {
        return self.tokens[self.curr - 1];
    }

    fn isAtEnd(self: Self) bool {
        return self.peek().token_type == .EOF;
    }

    /// Consumes if matches
    fn match(self: *Self, token_type: TokenType) bool {
        if (self.check(token_type)) {
            _ = self.advance();
            return true;
        }

        return false;
    }

    /// Consumes if matches
    fn match_any(self: *Self, types: []const TokenType) bool {
        for (types) |token_type| {
            if (self.check(token_type)) {
                _ = self.advance();
                return true;
            }
        }

        return false;
    }

    /// advance, and return error if it's not the expected TokenType
    ///
    /// msg should generally be a static string. There's no provision for cleanup.
    fn consume(self: *Self, token_type: TokenType, msg: []const u8) ?Error {
        if (self.check(token_type)) {
            _ = self.advance();
            return null;
        }
        return Error.new(self.alloc, self.peek(), msg);
    }

    /// For moving to the next starting point after a parsing error
    fn synchronize(self: *Self) void {
        _ = self.advance();
        while (!self.isAtEnd()) {
            if (self.previous.token_type == .semicolon) return;
            switch (self.peek().token_type) {
                .@"class",
                .@"fun",
                .@"var",
                .@"for",
                .@"if",
                .@"while",
                .@"print",
                .@"return",
                => return,
                else => self.advance(),
            }
        }
    }
};

pub const Error = struct {
    token: Token,
    msg: ArrayList(u8),

    pub fn new(alloc: *Allocator, token: Token, msg: []const u8) Error {
        var buf = ArrayList(u8).init(alloc);
        // if there's an error trying to make error msg, just exit
        buf.writer().print("{s}", .{msg}) catch unreachable;

        return Error{
            .token = token,
            .msg = buf,
        };
    }

    pub fn write_report(self: Error, source: []const u8) void {
        if (self.token.token_type == .EOF) {
            std.debug.print("at end, {s}", .{self.msg.items});
        } else {
            std.debug.print("{} at \"{s}\", {s}", .{ self.token.line(source), self.token.lexeme(source), self.msg.items });
        }
    }

    pub fn deinit(self: *Error) void {
        self.msg.deinit();
    }
};

pub const Errors = struct {
    errors: ArrayList(Error),

    pub fn write_report(self: Errors, source: []const u8) void {
        for (self.errors.items) |err| {
            err.write_report(source);
        }
    }

    pub fn isEmpty(self: Errors) bool {
        return self.errors.items.len == 0;
    }

    pub fn deinit(self: *Errors) void {
        for (self.errors.items) |*err| {
            err.deinit();
        }
        self.errors.deinit();
    }
};
