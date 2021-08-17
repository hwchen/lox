const clap = @import("clap");
const std = @import("std");
const io = std.io;

pub fn main() anyerror!void {
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

    for (args.positionals()) |pos| {
        std.log.info("{s}", .{pos});
    }
}
