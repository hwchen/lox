const std = @import("std");
const clap = @import("clap");
const io = std.io;
const fs = std.fs;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.io.Writer;

const bytecode = @import("./bytecode.zig");
const Chunk = bytecode.Chunk;
const Value = bytecode.Value;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var alloc = gpa.allocator();

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
        std.debug.print("Opening file: {s}\n", .{args.positionals()[0]});
        try lox.runFile(alloc, args.positionals()[0]);
    } else {
        return error.IncorrectArg;
    }
}

/// Lox is main entry point, and holds state for a session
const Lox = struct {
    fn runFile(self: *Lox, alloc: Allocator, path: []const u8) !void {
        _ = self;
        const file = try std.fs.cwd().openFile(path, .{ .read = true });
        defer file.close();

        const max_size = 1024 * 8;
        const source = try file.readToEndAlloc(alloc, max_size);
        defer alloc.free(source);

        try self.run(alloc, source);
    }

    fn runPrompt(self: *Lox, alloc: Allocator) !void {
        _ = self;
        var buf: [1024]u8 = undefined;
        var stdin = io.getStdIn().reader();
        var rdr = io.bufferedReader(stdin).reader();

        while (true) {
            std.debug.print("> ", .{});

            if (try rdr.readUntilDelimiterOrEof(&buf, '\n')) |line| {
                try self.run(alloc, line);
            } else {
                // EOF
                break;
            }
        }
    }

    fn run(self: *Lox, alloc: Allocator, source: []const u8) !void {
        _ = self;
        _ = source;

        var chunk = Chunk.init(alloc);
        defer chunk.deinit();
        try chunk.writeConstant(1.2, 123);
        try chunk.writeOpCode(.op_return, 123);
        std.debug.print("== test chunk ==\n", .{});
        std.debug.print("{}", .{chunk});
    }
};
