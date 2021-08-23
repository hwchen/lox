const util = @import("./util.zig");

pub const TokenType = enum {
    // Single-character tokens
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,

    // one or two character tokens
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    // literals
    identifier,
    string,
    number,

    // keywords,
    @"and",
    @"class",
    @"else",
    @"false",
    @"fun",
    @"for",
    @"if",
    @"nil",
    @"or",
    @"print",
    @"return",
    @"super",
    @"this",
    @"true",
    @"var",
    @"while",

    EOF,
};

pub const Token = struct {
    token_type: TokenType,
    start: u64,
    len: u64,

    pub fn line(self: Token, source: []const u8) u64 {
        return util.line(self.start, source);
    }

    pub fn lexeme(self: Token, source: []const u8) []const u8 {
        return source[self.start .. self.start + self.len];
    }

    pub fn write_debug(self: Token, writer: anytype, bytes: []const u8) !void {
        var wtr = writer.writer();
        if (self.token_type == .EOF) {
            try wtr.print("{}", .{self.token_type});
        } else {
            try wtr.print("{} \"{s}\" at {d}", .{ self.token_type, bytes[self.start .. self.start + self.len], self.start });
        }
    }
};
