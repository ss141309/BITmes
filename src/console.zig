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

const Cpu = @import("cpu.zig").nesCpu;
const Cartridge = @import("rom/loadRom.zig").Cartridge;
const std = @import("std");

pub const Console = struct {
    cartridge: Cartridge = undefined,
    cpu: Cpu() = undefined,
    mem: [2048]u8 = undefined,
};

pub fn main() !void {
    var buffer: [2000 * 1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var console = Console{};
    console.cartridge = try Cartridge.init(allocator, "/home/ss141309/Downloads/donkey-kong.nes");
    defer console.cartridge.deinit();
    console.cpu = Cpu().init(&console);
}
