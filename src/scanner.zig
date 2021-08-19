const std = @import("std");
const tok = @import("./token.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Token = tok.Token;
const TokenType = tok.TokenType;

pub const ScannerError = struct {
    line: u64,
    message: ArrayList(u8),

    pub fn write_report(self: ScannerError) void {
        std.debug.print("[line {d}] Error: {s}", .{ self.line, self.message });
    }

    pub fn deinit(self: *ScannerError) void {
        self.message.deinit();
    }
};

pub const ScannerErrors = struct {
    errors: ArrayList(ScannerError),

    pub fn write_report(self: ScannerErrors) void {
        for (self.errors.items) |err| {
            err.write_report();
        }
    }

    pub fn isEmpty(self: ScannerErrors) bool {
        return self.errors.items.len == 0;
    }

    pub fn deinit(self: *ScannerErrors) void {
        for (self.errors.items) |*err| {
            err.deinit();
        }
        self.errors.deinit();
    }
};

pub const Scanner = struct {
    alloc: *Allocator,
    source: []const u8,
    start: u64 = 0,
    curr: u64 = 0,

    const ScanTokenResult = union(enum) {
        token: Token,
        err: ScannerError,
    };

    const ScanTokensResult = struct {
        tokens: ArrayList(Token),
        errors: ScannerErrors,
    };

    pub fn scanTokens(self: *Scanner) !ScanTokensResult {
        var tokens = ArrayList(Token).init(self.alloc);
        var errors = ArrayList(ScannerError).init(self.alloc);

        while (!self.isAtEnd()) {
            self.start = self.curr;
            const scan_res = self.scanToken();
            switch (scan_res) {
                .token => |token| try tokens.append(token),
                .err => |err| try errors.append(err),
            }
        }

        return ScanTokensResult{
            .tokens = tokens,
            .errors = ScannerErrors{
                .errors = errors,
            },
        };
    }

    pub fn scanToken(self: *Scanner) ScanTokenResult {
        const c = self.advance();
        return switch (c) {
            '(' => self.makeToken(TokenType.left_paren),
            ')' => self.makeToken(TokenType.right_paren),
            '{' => self.makeToken(TokenType.left_brace),
            '}' => self.makeToken(TokenType.right_brace),
            ',' => self.makeToken(TokenType.comma),
            '.' => self.makeToken(TokenType.dot),
            '-' => self.makeToken(TokenType.minus),
            '+' => self.makeToken(TokenType.plus),
            ';' => self.makeToken(TokenType.semicolon),
            '*' => self.makeToken(TokenType.star),
            else => self.makeError(c),
        };
    }

    fn advance(self: *Scanner) u8 {
        const res = self.source[self.curr];
        self.curr += 1;
        return res;
    }

    fn makeToken(self: *Scanner, token_type: TokenType) ScanTokenResult {
        return ScanTokenResult{ .token = Token{
            .token_type = token_type,
            .start = self.start,
            .length = self.curr - self.start,
        } };
    }

    fn makeError(self: *Scanner, c: u8) ScanTokenResult {
        var buf = ArrayList(u8).init(self.alloc);
        // if there's an error trying to make error msg, just exit
        buf.writer().print("Unexpected character {c}", .{c}) catch unreachable;

        return ScanTokenResult{ .err = ScannerError{
            .line = self.line(),
            .message = buf,
        } };
    }

    // To be used only for error reporting
    fn line(self: Scanner) u64 {
        var count: u64 = 1;
        for (self.source[0..self.curr]) |c| {
            switch (c) {
                '\n' => count += 1,
                else => {},
            }
        }
        return count;
    }

    fn isAtEnd(self: Scanner) bool {
        return self.curr >= self.source.len;
    }
};
