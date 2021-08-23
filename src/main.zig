const std = @import("std");
const clap = @import("clap");
const scan = @import("./scanner.zig");
const parse = @import("./parser.zig");

const Scanner = scan.Scanner;
const ScannerErrors = scan.ScannerErrors;
const Parser = parse.Parser;
const ParserErrors = parse.Errors;

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
        const source = try file.readToEndAlloc(alloc, max_size);
        defer alloc.free(source);

        var lox_res = try self.run(alloc, source);
        switch (lox_res) {
            .ok => {},
            .err => |*err| {
                err.write_report(source);
                err.deinit();
            },
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
                var lox_res = try self.run(alloc, line);
                switch (lox_res) {
                    .ok => {},
                    .err => |*err| {
                        err.write_report(line);
                        err.deinit();
                    },
                }
            } else {
                // EOF
                break;
            }
        }
    }

    fn run(self: *Lox, alloc: *Allocator, bytes: []const u8) !LoxResult {
        _ = self;
        var scanner = Scanner{ .alloc = alloc, .source = bytes };

        var scan_res = try scanner.scanTokens();
        defer scan_res.tokens.deinit();

        if (!scan_res.errors.isEmpty()) {
            return LoxResult{ .err = .{
                .scanner = scan_res.errors,
            } };
        }

        // debug print tokens
        for (scan_res.tokens.items) |token| {
            var buf = ArrayList(u8).init(alloc);
            defer buf.deinit();
            try token.write_debug(&buf, bytes);
            std.log.info("{s}", .{buf.items});
        }

        var parser = Parser.init(
            alloc,
            scan_res.tokens.items,
        );
        var ast = try parser.parseProgram();
        ast.print_debug(bytes);
        defer ast.deinit();

        if (!parser.errors.isEmpty()) {
            return LoxResult{ .err = .{
                .parser = parser.errors,
            } };
        }

        return LoxResult.ok;
    }
};

const LoxResult = union(enum) {
    ok,
    err: LoxError,
};

const LoxError = union(enum) {
    scanner: ScannerErrors,
    parser: ParserErrors,

    fn write_report(self: LoxError, source: []const u8) void {
        switch (self) {
            .scanner => |err| err.write_report(),
            .parser => |err| err.write_report(source),
        }
        std.debug.print("\n", .{});
    }

    fn deinit(self: *LoxError) void {
        switch (self.*) {
            .scanner => |*errs| errs.deinit(),
            .parser => |*errs| errs.deinit(),
        }
    }
};
