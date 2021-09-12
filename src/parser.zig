const std = @import("std");
const ast = @import("./ast.zig");
const tok = @import("./token.zig");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ParseFloat = std.fmt.parseFloat;
const Token = tok.Token;
const TokenType = tok.TokenType;

const Tree = ast.Tree;
const Node = ast.Node;
const NodeList = ast.NodeList;
const TokenList = ast.TokenList;

/// Produces an AST. It's the caller's responsibility to deallocate the AST.
pub const Parser = struct {
    alloc: *Allocator,
    source: []const u8,
    tokens: []const Token,
    errors: Errors,

    nodes: NodeList,
    extra_data: ArrayList(Node.Index),
    scratch: ArrayList(Node.Index),

    curr: u32,

    const Self = @This();

    pub fn init(alloc: *Allocator, tokens: []const Token, source: []const u8) Self {
        return Self{
            .alloc = alloc,
            .source = source,
            .tokens = tokens,
            .errors = Errors{ .errors = ArrayList(Error).init(alloc) },
            .nodes = .{},
            .extra_data = ArrayList(Node.Index).init(alloc),
            .scratch = ArrayList(Node.Index).init(alloc),
            .curr = 0,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.extra_data.deinit();
        self.scratch.deinit();
    }

    fn addNode(self: *Self, node: Node) !Node.Index {
        const res = @intCast(Node.Index, self.nodes.len);
        try self.nodes.append(self.alloc, node);
        return res;
    }

    fn setNode(self: *Self, idx: usize, node: Node) Node.Index {
        self.nodes.set(idx, node);
        return @intCast(Node.Index, idx);
    }

    pub fn parse(self: *Self) !Tree {
        // program is the root node
        // But the statements it refers to must be calculated later
        _ = try self.addNode(.{
            .tag = .program,
            .main_token = 0,
            .data = .{ .lhs = 37, .rhs = 27 },
        });

        while (!self.isAtEnd()) {
            const stmt = try self.parseDeclaration();
            try self.scratch.append(stmt);
        }

        const start = self.extra_data.items.len;
        const end = start + self.scratch.items.len;
        self.nodes.items(.data)[0] = .{
            .lhs = @intCast(Node.Index, start),
            .rhs = @intCast(Node.Index, end),
        };

        try self.extra_data.appendSlice(self.scratch.items);

        return Tree{
            .alloc = self.alloc,
            .source = self.source,
            .tokens = self.tokens,
            .nodes = self.nodes,
            .extra_data = self.extra_data.toOwnedSlice(),
        };
    }

    fn parseDeclaration(self: *Self) !Node.Index {
        return if (self.match(.@"var"))
            try self.parseVarDecl()
        else
            try self.parseStmt();
    }

    fn parseVarDecl(self: *Self) !Node.Index {
        const var_ident = self.curr_idx();
        _ = self.advance();

        const var_decl = if (self.match(.equal)) blk: {
            const expr = try self.parseExpr();
            break :blk try self.addNode(.{
                .tag = .stmt_var_decl_init,
                .main_token = var_ident,
                .data = .{ .lhs = expr, .rhs = undefined },
            });
        } else try self.addNode(.{
            .tag = .stmt_var_decl,
            .main_token = var_ident,
            .data = undefined,
        });

        if (self.consume(.semicolon, "Expected ; at end of variable declaration")) |err| {
            try self.errors.errors.append(err);
            _ = self.setNode(var_decl, .{ .tag = .expr_invalid, .main_token = undefined, .data = undefined });
            _ = self.advance();
        }

        return var_decl;
    }

    fn parseStmt(self: *Self) !Node.Index {
        const main_token = self.curr_idx();
        const tag = if (self.match(.@"print")) Node.Tag.stmt_print else .stmt_expr;

        var expr = try self.parseExpr();

        if (self.consume(.semicolon, "Expected ; at end of statement")) |err| {
            try self.errors.errors.append(err);
            _ = self.setNode(expr, .{ .tag = .expr_invalid, .main_token = undefined, .data = undefined });
            // need to advance, otherwise will infinite loop
            _ = self.advance();
        }
        return try self.addNode(.{ .tag = tag, .main_token = main_token, .data = .{ .lhs = expr, .rhs = undefined } });
    }

    fn parseExpr(self: *Self) !Node.Index {
        return try self.parseAssignment();
    }

    fn parseAssignment(self: *Self) Allocator.Error!Node.Index {
        var expr = try self.parseEquality();

        // If expr is a variable and is followed by an `=`, then it's assignment.
        // Convert the `expr` node from above.
        if (self.match(.equal)) {
            const val = try self.parseAssignment();

            const expr_node = self.nodes.get(expr);
            if (expr_node.tag == .expr_variable) {
                _ = self.setNode(expr, .{
                    .tag = .expr_assignment,
                    .main_token = expr_node.main_token,
                    .data = .{ .lhs = val, .rhs = undefined },
                });
            } else {
                try self.errors.errors.append(Error.new(self.alloc, self.peek(), "Expected assignment, but lhs is not a variable"));
                return try self.addNode(.{ .tag = .expr_invalid, .main_token = undefined, .data = undefined });
            }
        }

        return expr;
    }

    fn parseEquality(self: *Self) !Node.Index {
        var expr = try self.parseComparison();

        while (self.match_any(&[_]TokenType{ .bang_equal, .equal_equal })) {
            const op = self.previous_idx();
            const right = try self.parseComparison();
            var new_expr = try self.addNode(.{ .tag = .expr_binary, .main_token = op, .data = .{ .lhs = expr, .rhs = right } });
            expr = new_expr;
        }

        return expr;
    }

    fn parseComparison(self: *Self) !Node.Index {
        var expr = try self.parseTerm();

        while (self.match_any(&[_]TokenType{ .greater, .greater_equal, .less, .less_equal })) {
            const op = self.previous_idx();
            const right = try self.parseTerm();
            var new_expr = try self.addNode(.{ .tag = .expr_binary, .main_token = op, .data = .{ .lhs = expr, .rhs = right } });
            expr = new_expr;
        }

        return expr;
    }

    fn parseTerm(self: *Self) !Node.Index {
        var expr = try self.parseFactor();

        while (self.match_any(&[_]TokenType{ .plus, .minus })) {
            const op = self.previous_idx();
            const right = try self.parseFactor();
            var new_expr = try self.addNode(.{ .tag = .expr_binary, .main_token = op, .data = .{ .lhs = expr, .rhs = right } });
            expr = new_expr;
        }

        return expr;
    }

    fn parseFactor(self: *Self) !Node.Index {
        var expr = try self.parseUnary();

        while (self.match_any(&[_]TokenType{ .star, .slash })) {
            const op = self.previous_idx();
            const right = try self.parseUnary();
            var new_expr = try self.addNode(.{ .tag = .expr_binary, .main_token = op, .data = .{ .lhs = expr, .rhs = right } });
            expr = new_expr;
        }

        return expr;
    }

    fn parseUnary(self: *Self) Allocator.Error!Node.Index {
        if (self.match_any(&[_]TokenType{ .bang, .minus })) {
            const op = self.previous_idx();
            var expr = try self.parseUnary();
            var new_expr = try self.addNode(.{ .tag = .expr_unary, .main_token = op, .data = .{ .lhs = expr, .rhs = undefined } });
            expr = new_expr;
            return expr;
        }

        return self.parsePrimary();
    }

    // Error on allocation failure
    fn parsePrimary(self: *Self) !Node.Index {
        if (self.match(.@"false")) {
            return try self.addNode(.{
                .tag = .literal_false,
                .main_token = self.previous_idx(),
                .data = undefined,
            });
        }
        if (self.match(.@"true")) {
            return try self.addNode(.{
                .tag = .literal_true,
                .main_token = self.previous_idx(),
                .data = undefined,
            });
        }
        if (self.match(.@"nil")) {
            return try self.addNode(.{
                .tag = .literal_nil,
                .main_token = self.previous_idx(),
                .data = undefined,
            });
        }
        if (self.match(.string)) {
            return try self.addNode(.{
                .tag = .literal_string,
                .main_token = self.previous_idx(),
                .data = undefined,
            });
        }
        if (self.match(.number)) {
            return try self.addNode(.{
                .tag = .literal_number,
                .main_token = self.previous_idx(),
                .data = undefined,
            });
        }
        if (self.match(.identifier)) {
            return try self.addNode(.{
                .tag = .expr_variable,
                .main_token = self.previous_idx(),
                .data = undefined,
            });
        }

        if (self.match(.left_paren)) {
            const l_paren = self.previous_idx();
            const inner = try self.parseExpr();
            if (self.consume(.right_paren, "Expected right paren")) |err| {
                try self.errors.errors.append(err);
                return try self.addNode(.{ .tag = .expr_invalid, .main_token = undefined, .data = undefined });
            }
            return try self.addNode(.{ .tag = .expr_grouping, .main_token = l_paren, .data = .{ .lhs = inner, .rhs = undefined } });
        }

        // TODO parse ident, keywords, etc.
        try self.errors.errors.append(Error.new(self.alloc, self.peek(), "Expected expression"));
        return try self.addNode(.{ .tag = .expr_invalid, .main_token = undefined, .data = undefined });
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

    fn curr_idx(self: Self) Token.Index {
        return self.curr;
    }

    fn previous_idx(self: Self) Token.Index {
        return self.curr - 1;
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
            std.debug.print(" at end, {s}\n", .{self.msg.items});
        } else {
            std.debug.print("[line {}] at \"{s}\", {s}\n", .{ self.token.line(source), self.token.lexeme(source), self.msg.items });
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
