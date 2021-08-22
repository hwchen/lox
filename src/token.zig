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

    pub fn write_debug(self: Token, writer: anytype, bytes: []const u8) !void {
        var wtr = writer.writer();
        if (self.start < bytes.len) {
            try wtr.print("{} \"{s}\" at {d}", .{ self.token_type, bytes[self.start .. self.start + self.len], self.start });
        } else {
            try wtr.print("{}", .{self.token_type});
        }
    }
};
