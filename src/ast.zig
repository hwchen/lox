//! AST
//!
//! The AST (Tree) contains holds references to source and tokens also, so that:
//! - unneded conversions are avoided
//! - unnecessary allocations avoided
//! - easy indexing from AST back through tokens/source
//!
//! The implementation follows Zig's new AST memory layout, you can see notes on it
//! at https://ziglang.org/download/0.8.0/release-notes.html#Reworked-Memory-Layout.
//!
//! Note that all Nodes are kept in the NodeList. Nodes can refer to at most two other
//! nodes (lhs and rhs), so when more data needs to be referenced, the lhs and rhs are
//! repurposed so that they point to a list of node indexes. These indexes are kept in
//! a separate list of Node.Indexes called "extra_data", and they point back to Nodes
//! in the NodeList. Tag type determines which set of logic to follow.
//!
//! For example, constructs like the "program" or a "function" reference a list of nodes
//! (e.g. statements or fn params).
//!
//! In order to correctly pack the list of nodes indexes contiguously, the implementation
//! makes use of a "scratch" space when parsing. Nodes get appended to the NodeList as
//! needed, but their Node.Indexes get appended into scratch in a contiguous bunch. When
//! the scope is exited, those contiguous indexes get appended onto extra_data, and their
//! range (exclusive) is store in the lhs and rhs of the parent node.
//!
//! Using the `defer` pattern, when exiting scope the scratch list is truncated back
//! to its scope-entry state. This handles nested invocations of parsing extra_data.

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const tok = @import("./token.zig");
const Token = tok.Token;
const TokenType = tok.TokenType;

pub const NodeList = std.MultiArrayList(Node);
pub const TokenList = []const Token;

pub const Tree = struct {
    alloc: *Allocator,
    source: []const u8,
    tokens: TokenList,
    nodes: NodeList,
    extra_data: []Node.Index,

    pub fn deinit(self: *Tree) void {
        self.nodes.deinit(self.alloc);
        self.alloc.free(self.extra_data);
    }

    pub fn stmts(self: *Tree) []Node.Index {
        const root = self.nodes.get(0);
        return self.extra_data[root.data.lhs..root.data.rhs];
    }

    pub fn debug_print(self: *Tree) void {
        self.debug_print_node(0);
    }

    fn debug_print_node(self: *Tree, idx: Node.Index) void {
        const node = self.nodes.get(idx);

        switch (node.tag) {
            .program => {
                const stmt_indexes = self.extra_data[node.data.lhs..node.data.rhs];
                for (stmt_indexes) |i| {
                    self.debug_print_node(i);
                    std.debug.print(";\n", .{});
                }
            },
            .stmt_print => {
                std.debug.print("printStmt: ", .{});
                self.debug_print_node(node.data.lhs);
            },
            .stmt_expr => {
                std.debug.print("exprStmt: ", .{});
                self.debug_print_node(node.data.lhs);
            },
            .expr_unary => {
                std.debug.print("(", .{});
                std.debug.print("{s} ", .{self.tokenSlice(node.main_token)});
                self.debug_print_node(node.data.lhs);
                std.debug.print(") ", .{});
            },
            .expr_binary => {
                std.debug.print("(", .{});
                std.debug.print("{s} ", .{self.tokenSlice(node.main_token)});
                self.debug_print_node(node.data.lhs);
                self.debug_print_node(node.data.rhs);
                std.debug.print(") ", .{});
            },
            .expr_grouping => {
                std.debug.print("(group ", .{});
                self.debug_print_node(node.data.lhs);
                std.debug.print(") ", .{});
            },
            .expr_invalid => {
                std.debug.print("invalidExpr", .{});
            },
            .literal_number => {
                std.debug.print("{s} ", .{self.tokenSlice(node.main_token)});
            },
            .literal_string => {
                std.debug.print("\"{s}\" ", .{self.tokenSlice(node.main_token)});
            },
            .literal_true => {
                std.debug.print("true", .{});
            },
            .literal_false => {
                std.debug.print("false", .{});
            },
            .literal_nil => {
                std.debug.print("nil", .{});
            },
        }
    }

    pub fn tokenSlice(self: Tree, idx: Token.Index) []const u8 {
        const token = self.tokens[idx];
        return token.lexeme(self.source);
    }

    pub fn tokenType(self: Tree, idx: Token.Index) TokenType {
        const token = self.tokens[idx];
        return token.token_type;
    }
};

pub const Node = struct {
    tag: Tag,
    main_token: Token.Index,
    data: Data,

    pub const Index = u32;

    pub const Tag = enum {
        // main_token is [0], sub_list[lhs... rhs], stored in extra_data
        program,
        // main_token is `print`, lhs is expr idx
        stmt_print,
        // main_token is 1st token, lhs is expr idx
        stmt_expr,
        // main token is op, lhs is expr idx, rhs is unused
        expr_unary,
        // main token is op, lhs is lhs expr idx, rhs is rhs expr idx
        expr_binary,
        // main token is left paren, lhs is expr idx, rhs is unused
        expr_grouping,
        // all unused
        expr_invalid,

        // literals have no data
        literal_number,
        literal_string,
        literal_true,
        literal_false,
        literal_nil,
    };

    pub const Data = struct {
        lhs: Index,
        rhs: Index,
    };
};
