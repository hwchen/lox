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
    curr_tok_start: u64 = 0,
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
            self.curr_tok_start = self.curr;
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
        _ = self;
        return ScanTokenResult{ .token = Token{
            .token_type = TokenType.EOF,
            .start = 0,
            .length = 0,
        } };
    }

    // To be used only for error reporting
    fn line(self: Scanner) u64 {
        var count = 1;
        for (self.source[0..self.curr]) |c| {
            switch (c) {
                '\n' => count += 1,
                else => {},
            }
        }
    }

    fn isAtEnd(self: Scanner) bool {
        return self.curr >= self.source.len;
    }
};
