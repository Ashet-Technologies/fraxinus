const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("fraxinus", .{
        .source_file = .{ .path = "src/lib.zig" },
    });

    const test_exe = b.addTest(.{
        .root_source_file = .{ .path = "src/lib.zig" },
    });

    const test_run = b.addRunArtifact(test_exe);

    b.getInstallStep().dependOn(&test_run.step);
}
