const clap = @import("clap");
const scan = @import("./scanner.zig");
const std = @import("std");

const Scanner = scan.Scanner;
const ScannerError = scan.ScannerError;
const io = std.io;
const fs = std.fs;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.io.Writer;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var alloc = &gpa.allocator;

    const params = comptime [_]clap.Param(clap.Help){
        clap.parseParam("-h, --help             Display this help and exit.              ") catch unreachable,
        clap.parseParam("<PATH>                 Execute file path.                       ") catch unreachable,
    };

    var diag = clap.Diagnostic{};
    var args = clap.parse(clap.Help, &params, .{ .diagnostic = &diag }) catch |err| {
        diag.report(io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer args.deinit();

    if (args.flag("--help")) {
        var wtr = std.io.getStdOut().writer();
        _ = try wtr.write("USAGE: ");
        try clap.usage(std.io.getStdOut().writer(), &params);
        _ = try wtr.write("\n\nOPTIONS:\n");
        try clap.help(std.io.getStdOut().writer(), &params);
    }

    var lox = Lox{};

    if (args.positionals().len == 0) {
        try lox.runPrompt(alloc);
    } else if (args.positionals().len == 1) {
        try lox.runFile(alloc, args.positionals()[0]);
    } else {
        return error.IncorrectArg;
    }
}

/// Lox is main entry point, and holds state for a session
const Lox = struct {
    fn runFile(self: *Lox, alloc: *Allocator, path: []const u8) !void {
        _ = self;
        const file = try std.fs.cwd().openFile(path, .{ .read = true });
        defer file.close();

        const max_size = 1024 * 8;
        const bytes = try file.readToEndAlloc(alloc, max_size);
        defer alloc.free(bytes);

        var lox_err: ?*LoxError = null;
        try self.run(alloc, bytes, lox_err);
        if (lox_err) |err| {
            err.write_report();
            err.deinit();
            std.os.exit(65);
        }
    }

    fn runPrompt(self: *Lox, alloc: *Allocator) !void {
        _ = self;
        var buf: [1024]u8 = undefined;
        var stdin = io.getStdIn().reader();
        var rdr = io.bufferedReader(stdin).reader();
        var stdout = io.getStdOut().writer();
        var stdout_buf = io.bufferedWriter(stdout);
        var wtr = stdout_buf.writer();

        while (true) {
            _ = try wtr.write("> ");
            try stdout_buf.flush();

            if (try rdr.readUntilDelimiterOrEof(&buf, '\n')) |line| {
                var lox_err: ?*LoxError = null;
                try self.run(alloc, line, lox_err);
                if (lox_err) |err| {
                    err.write_report();
                    err.deinit();
                }
            } else {
                // EOF
                break;
            }
        }
    }

    fn run(self: *Lox, alloc: *Allocator, bytes: []const u8, lox_err: ?*LoxError) !void {
        _ = self;
        var scanner = Scanner{ .alloc = alloc, .source = bytes };
        var scanner_err: ?*ScannerError = null;

        const tokens = scanner.scanTokens(scanner_err);
        defer tokens.deinit();

        if (scanner_err) |err| {
            // Is this correct, is the error moved?
            lox_err.?.* = LoxError{ .scanner = err.* };
            return;
        }

        // TODO this printing is temporary; once we know the return value of `run`, we should remove this.
        for (tokens.items) |token| {
            var buf = ArrayList(u8).init(alloc);
            std.log.info("{s}", .{token.write_debug(&buf, bytes)});
        }
    }
};

const LoxError = union(enum) {
    scanner: ScannerError,

    fn write_report(self: LoxError) void {
        switch (self) {
            .scanner => |err| err.write_report(),
        }
    }

    fn deinit(self: *LoxError) void {
        switch (self.*) {
            .scanner => |*err| err.deinit(),
        }
    }
};
