const std = @import("std");
const ast = @import("./ast.zig");
const tok = @import("./token.zig");
const Allocator = std.mem.Allocator;
const ParseFloat = std.fmt.parseFloat;
const Token = tok.Token;
const TokenType = tok.TokenType;

const Program = ast.Program;
const Expr = ast.Expr;

/// Produces a Program. It's the caller's responsibility to deallocate the Program.
pub const Parser = struct {
    alloc: *Allocator,
    source: []const u8,
    tokens: []const Token,
    curr: u64 = 0,

    const Self = @This();

    pub fn parseProgram(self: *Self) !Program {
        return Program{
            .expr = try self.parseExpr(),
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

        return try self.parsePrimary();
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
            // TODO make error
            _ = self.match(.right_paren);
            res.* = Expr{ .grouping = .{
                .expr = inner,
            } };
            return res;
        }

        // TODO parse ident, keywords, etc.
        std.debug.panic("expected expression", .{});
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
};
