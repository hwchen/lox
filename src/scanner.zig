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

// TODO start here: create ScannerErrors struct

pub const Scanner = struct {
    alloc: *Allocator,
    source: []const u8,
    curr_tok_start: u64 = 0,
    curr: u64 = 0,

    pub fn scanTokens(self: *Scanner, err: ?*ScannerError) ArrayList(Token) {
        _ = err;

        var tokens = ArrayList(Token).init(self.alloc);
        while (!self.isAtEnd()) {
            self.curr_tok_start = self.curr;
        }

        return tokens;
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
