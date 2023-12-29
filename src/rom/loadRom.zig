// Copyright 2023-2024 समीर सिंह Sameer Singh

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

const Allocator = std.mem.Allocator;

pub const Cartridge = struct {
    prg: []u8,
    chr: []u8,
    mapper: u12,
    contents: []u8,
    allocator: Allocator,

    pub fn init(allocator: Allocator, romPath: []const u8) !@This() {
        var romFile = try std.fs.cwd().openFile(romPath, .{});
        defer romFile.close();

        const metadata = try romFile.metadata();

        const contents = try romFile.reader().readAllAlloc(
            allocator,
            metadata.inner.statx.size,
        );

        if (std.mem.eql(u8, contents[0..4], "NES\x1a")) {
            const headerData = ines.loadiNes().init(contents[0..15]);

            const prg = contents[16 .. headerData.prgRomSize + 16];
            const chr = contents[headerData.prgRomSize + 16 .. headerData.prgRomSize + headerData.chrRomSize + 16];

            const mapper0 = headerData.byte6.mapperNum;
            const mapper1: u8 = headerData.byte7.mapperNum;
            const mapper2: u12 = headerData.byte8.mapperNum;
            const mapper = ((mapper2 << 8) | (mapper1 << 4)) | mapper0;

            return @This(){ .prg = prg, .chr = chr, .mapper = mapper, .contents = contents, .allocator = allocator };
        } else {
            return error.InvalidFileFormat;
        }
    }

    pub fn deinit(self: *@This()) void {
        self.allocator.free(self.contents);
    }
};
