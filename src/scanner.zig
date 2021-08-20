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
        std.debug.print("[line {d}] Error: {s}\n", .{ self.line, self.message.items });
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
        skip,
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
                .skip => continue,
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

            '/' => blk: {
                if (self.match('/')) {
                    // comment
                    while (!self.isAtEnd() and self.peek() != '\n') {
                        _ = self.advance();
                    }
                    break :blk self.makeSkip();
                } else {
                    break :blk self.makeToken(.slash);
                }
            },

            ' ', '\r', '\n', '\t' => self.makeSkip(),

            else => self.makeError(c),
        };
    }

    fn advance(self: *Scanner) u8 {
        const res = self.source[self.curr];
        self.curr += 1;
        return res;
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.curr] != expected) return false;

        self.curr += 1;
        return true;
    }

    /// Like advance, but does not consume
    fn peek(self: *Scanner) u8 {
        return self.source[self.curr];
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

    fn makeSkip(self: Scanner) ScanTokenResult {
        _ = self;
        return ScanTokenResult.skip;
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
