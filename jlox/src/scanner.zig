const std = @import("std");
const tok = @import("./token.zig");
const util = @import("./util.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Token = tok.Token;
const TokenType = tok.TokenType;

pub const Scanner = struct {
    alloc: Allocator,
    source: []const u8,
    start: u32 = 0,
    curr: u32 = 0,

    const ScanTokenResult = union(enum) {
        token: Token,
        err: Error,
        skip,
    };

    const ScanTokensResult = struct {
        tokens: ArrayList(Token),
        errors: Errors,
    };

    pub fn scanTokens(self: *Scanner) !ScanTokensResult {
        var tokens = ArrayList(Token).init(self.alloc);
        var errors = ArrayList(Error).init(self.alloc);

        while (!self.isAtEnd()) {
            self.start = self.curr;
            const scan_res = self.scanToken();
            switch (scan_res) {
                .token => |token| try tokens.append(token),
                .err => |err| try errors.append(err),
                .skip => continue,
            }
        }

        try tokens.append(Token{
            .token_type = .EOF,
            .start = @intCast(u32, self.source.len),
            .len = @intCast(u32, self.source.len),
        });

        return ScanTokensResult{
            .tokens = tokens,
            .errors = Errors{
                .errors = errors,
            },
        };
    }

    pub fn scanToken(self: *Scanner) ScanTokenResult {
        const c = self.advance();
        return switch (c) {
            '(' => self.makeToken(.left_paren),
            ')' => self.makeToken(.right_paren),
            '{' => self.makeToken(.left_brace),
            '}' => self.makeToken(.right_brace),
            ',' => self.makeToken(.comma),
            '.' => self.makeToken(.dot),
            '-' => self.makeToken(.minus),
            '+' => self.makeToken(.plus),
            ';' => self.makeToken(.semicolon),
            '*' => self.makeToken(.star),

            '!' => self.makeToken(if (self.match('=')) .bang_equal else .bang),
            '=' => self.makeToken(if (self.match('=')) .equal_equal else .equal),
            '<' => self.makeToken(if (self.match('=')) .less_equal else .less),
            '>' => self.makeToken(if (self.match('=')) .greater_equal else .greater),

            '/' => if (self.match('/')) self.parseComment() else self.makeToken(.slash),

            ' ', '\r', '\n', '\t' => self.makeSkip(),

            '"' => self.parseString(),

            '0'...'9' => self.parseNumber(),

            'a'...'z', 'A'...'Z', '_' => self.parseIdentifier(),

            else => self.unexpectedCharacterError(c),
        };
    }

    // consume and more to next token
    fn advance(self: *Scanner) u8 {
        const res = self.source[self.curr];
        self.curr += 1;
        return res;
    }

    // consumes if matches
    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.curr] != expected) return false;

        self.curr += 1;
        return true;
    }

    /// Like advance, but does not consume
    fn peek(self: Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.curr];
    }

    fn peek2(self: Scanner) u8 {
        const idx = self.curr + 1;

        // idx out of bounds
        if (idx >= self.source.len) return 0;

        return self.source[idx];
    }

    fn makeToken(self: *Scanner, token_type: TokenType) ScanTokenResult {
        return ScanTokenResult{ .token = Token{
            .token_type = token_type,
            .start = self.start,
            .len = self.curr - self.start,
        } };
    }

    fn unexpectedCharacterError(self: *Scanner, c: u8) ScanTokenResult {
        return ScanTokenResult{ .err = Error{
            .line = self.line(),
            .kind = .{ .unexpected_char = c },
        } };
    }

    fn makeSkip(self: Scanner) ScanTokenResult {
        _ = self;
        return ScanTokenResult.skip;
    }

    // To be used only for error reporting
    fn line(self: Scanner) u32 {
        return util.line(self.curr, self.source);
    }

    fn isAtEnd(self: Scanner) bool {
        return self.curr >= self.source.len;
    }

    fn parseComment(self: *Scanner) ScanTokenResult {
        while (self.peek() != '\n' and !self.isAtEnd()) {
            _ = self.advance();
        }
        return self.makeSkip();
    }

    /// Doesn't handle escaped double quotes
    fn parseString(self: *Scanner) ScanTokenResult {
        while (self.peek() != '"' and !self.isAtEnd()) {
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return ScanTokenResult{ .err = Error{
                .line = self.line(),
                .kind = .unterminated_string,
            } };
        }

        // advance past last double-quote
        _ = self.advance();

        return ScanTokenResult{ .token = Token{
            .token_type = .string,
            .start = self.start + 1,
            .len = self.curr - self.start - 2,
        } };
    }

    fn parseNumber(self: *Scanner) ScanTokenResult {
        const isDigit = std.ascii.isDigit;

        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        // check for one decimal point
        if (self.peek() == '.' and isDigit(self.peek2())) {
            _ = self.advance();
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(.number);
    }

    fn parseIdentifier(self: *Scanner) ScanTokenResult {
        while (isIdentChar(self.peek())) {
            _ = self.advance();
        }

        if (getKeyword(self.source[self.start..self.curr])) |keyword| {
            return self.makeToken(keyword);
        }

        return self.makeToken(.identifier);
    }
};

fn isIdentChar(c: u8) bool {
    return switch (c) {
        '0'...'9', 'a'...'z', 'A'...'Z', '_' => true,
        else => false,
    };
}

// from zig's stringToEnum. ComptimeStringMap is same strategy used for keywords in zig.
// https://github.com/ziglang/zig/pull/5359#issuecomment-634171853
//
// See usage here https://github.com/ziglang/zig/blob/6a5094872f10acc629543cc7f10533b438d0283a/lib/std/meta.zig#L62
//
// A subset of TokenType is keywords.
fn getKeyword(ident: []const u8) ?TokenType {
    const kvs = comptime build_kvs: {
        // This list should be kept in sync with the enum. It's manual, but done very rarely
        const keywords = [_]TokenType{
            .@"and",
            .@"class",
            .@"else",
            .@"false",
            .@"fun",
            .@"for",
            .@"if",
            .@"nil",
            .@"or",
            .@"print",
            .@"return",
            .@"super",
            .@"this",
            .@"true",
            .@"var",
            .@"while",
        };

        const KV = struct {
            @"0": []const u8,
            @"1": TokenType,
        };
        var kvs_array: [keywords.len]KV = undefined;
        for (keywords) |keyword, i| {
            kvs_array[i] = KV{ .@"0" = std.meta.tagName(keyword), .@"1" = keyword };
        }
        break :build_kvs kvs_array[0..];
    };
    const map = comptime std.ComptimeStringMap(TokenType, kvs);
    return map.get(ident);
}

pub const Error = struct {
    line: u32,
    kind: ErrorKind,

    pub fn format(self: Error, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self.kind) {
            .unterminated_string => try std.fmt.format(out_stream, "[line {d}] Error: unterminated string", .{self.line}),
            .unexpected_char => |c| try std.fmt.format(out_stream, "[line {d}] Error: unexpected character '{c}'", .{ self.line, c }),
        }
    }

    pub fn deinit(self: *Error) void {
        self.msg.deinit();
    }
};

pub const ErrorKind = union(enum) {
    unterminated_string,
    unexpected_char: u8,
};

pub const Errors = struct {
    errors: ArrayList(Error),

    pub fn write_report(self: Errors) void {
        for (self.errors.items) |err| {
            std.debug.print("{}\n", .{err});
        }
    }

    pub fn isEmpty(self: Errors) bool {
        return self.errors.items.len == 0;
    }

    pub fn deinit(self: *Errors) void {
        self.errors.deinit();
    }
};
