// Copyright 2023 समीर सिंह Sameer Singh

// This file is part of BITmes.
// BITmes is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// BITmes is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with BITmes. If not, see <https:www.gnu.org/licenses/>.

const std = @import("std");
const ines = @import("ines.zig");

fn loadRom(romPath: []const u8) !void {
    var romFile = try std.fs.cwd().openFile(romPath, .{});
    defer romFile.close();

    const metadata = try romFile.metadata();

    var buffer: [2000 * 1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    const contents = try romFile.reader().readAllAlloc(
        allocator,
        metadata.inner.statx.size,
    );
    defer allocator.free(contents);

    if (std.mem.eql(u8, contents[0..4], "NES\x1a")) {
        var iNes = ines.loadiNes().init(contents[0..15]);
        iNes.setByte6();
        iNes.setByte7();
        iNes.ifINes2();
        iNes.getPrgRomSize();
        iNes.getChrRomSize();
        std.debug.print("chr: {}\n", .{iNes.byte6});
    } else {
        std.debug.print("Unsupported ROM file format.\n", .{});
    }
}
