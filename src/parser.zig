const std = @import("std").zig;
const ast = @import("./ast.zig");
const tok = @import("./token.zig");
const Allocator = std.mem.Allocator;
const ParseFloat = std.fmt.parseFloat;
const Token = tok.Token;

const Program = ast.Program;
const Expr = ast.Expr;

/// Produces a Program. It's the caller's responsibility to deallocate the Program.
pub const Parser = struct {
    alloc: *Allocator,
    source: []const u8,
    tokens: []const Token,
    curr: u64 = 0,

    const Self = @This();

    pub fn parseProgram(self: *Self) Program {
        return Program{ .expr = self.parseExpr() };
    }

    fn parseExpr(self: *Self) Expr {
        return self.parseEquality();
    }

    fn parseEquality(self: *Self) Expr {
        var expr = self.parseComparison();

        while (self.match_any([].{ .bang_equal, .equal_equal })) {
            const op = self.previous();
            const right = self.parseComparison();
            expr = Expr{ .binary = .{ .left = expr, .op = op, .right = right } };
        }

        return expr;
    }

    fn parseComparison(self: *Self) Expr {
        var expr = self.parseTerm();

        while (self.match_any([].{ .greater, .greater_equal, .less, .less_equal })) {
            const op = self.previous();
            const right = self.parseTerm();
            expr = Expr{ .binary = .{ .lef = expr, .op = op, .right = right } };
        }

        return expr;
    }

    fn parseTerm(self: *Self) Expr {
        var expr = self.parseFactor();

        while (self.match_any([].{ .plus, .minus })) {
            const op = self.previous();
            const right = self.parseFactor();
            expr = Expr{ .binary = .{ .lef = expr, .op = op, .right = right } };
        }

        return expr;
    }

    fn parseFactor(self: *Self) Expr {
        var expr = self.parseUnary();

        while (self.match_any([].{ .star, .slash })) {
            const op = self.previous();
            const right = self.parseUnary();
            expr = Expr{ .binary = .{ .lef = expr, .op = op, .right = right } };
        }

        return expr;
    }

    fn parseUnary(self: *Self) Expr {
        if (self.match_any([].{ .bang, .minus })) {
            const op = self.previous();
            const right = self.parseUnary();
            return expr = Expr{ .unary = .{ .op = op, .expr = right } };
        }

        return self.parsePrimary();
    }

    fn parsePrimary(self: *Self) Expr {
        if (self.match(.@"false")) return Expr{ .literal = .@"false" };
        if (self.match(.@"true")) return Expr{ .literal = .@"true" };
        if (self.match(.@"nil")) return Expr{ .literal = .@"nil" };

        if (self.match(.string)) {
            const token = self.previous();
            return Expr{ .literal = .{
                .string = .{ .start = token.start, .len = token.len },
            } };
        }
        if (self.match(.number)) {
            const token = self.previous();
            return Expr{ .literal = .{
                .number = .{ .start = token.start, .len = token.len },
            } };
        }

        if (self.match(.left_paren)) {
            const expr = self.parseExpr();
            // TODO make error
            _ = self.match(.right_paren);
        }
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

    fn previous(self: Self) Token {
        return self.tokens[self.curr - 1];
    }

    fn isAtEnd(self: Self) bool {
        return self.peek().token_type == .EOF;
    }

    /// Consumes if matches
    fn match(self: *Self, token_type: TokenType) bool {
        if (self.currToken().token_type() == token_type) {
            _ = self.advance();
            return true;
        }

        return false;
    }

    /// Consumes if matches
    fn match_any(self: *Self, types: []const TokenType) bool {
        for (types.items) |token_type| {
            if (self.currToken().token_type() == token_type) {
                _ = self.advance();
                return true;
            }
        }

        return false;
    }
};
