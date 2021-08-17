const std = @import("std");
pub const pkgs = struct {
    pub const clap = std.build.Pkg{
        .name = "clap",
        .path = .{ .path = ".gyro/zig-clap-Hejsil-c5fb22823a9a4a699acaefc1e9febfee0b8e506c/pkg/clap.zig" },
    };

    pub fn addAllTo(artifact: *std.build.LibExeObjStep) void {
        @setEvalBranchQuota(1_000_000);
        inline for (std.meta.declarations(pkgs)) |decl| {
            if (decl.is_pub and decl.data == .Var) {
                artifact.addPackage(@field(pkgs, decl.name));
            }
        }
    }
};

pub const exports = struct {
};
pub const base_dirs = struct {
    pub const clap = ".gyro/zig-clap-Hejsil-c5fb22823a9a4a699acaefc1e9febfee0b8e506c/pkg";
};
