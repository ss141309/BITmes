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

const Byte6 = packed struct(u8) {
    mirroring: bool,
    battery: bool,
    trainer: bool,
    fourScreen: bool,
    mapperNum: u4,

    pub fn fromInt(num: u8) Byte6 {
        return @bitCast(num);
    }

    pub fn toInt(s: Byte6) u8 {
        return @bitCast(s);
    }
};

const consoleTypeTag = enum(u2) {
    NES,
    VsSys,
    Playchoice10,
    Extended,
};

const Byte7 = packed struct(u8) {
    consoleType: consoleTypeTag,
    iNes2Identifer: u2,
    mapperNum: u4,

    pub fn fromInt(num: u8) Byte7 {
        return @bitCast(num);
    }

    pub fn toInt(s: Byte7) u8 {
        return @bitCast(s);
    }
};

const Byte8 = packed struct(u8) {
    mapperNum: u4,
    subMapperNum: u4,
};

pub fn loadiNes() type {
    return struct {
        isINes2: bool,
        prgRomSize: u32,
        chrRomSize: u32,
        byte6: Byte6,
        byte7: Byte7,
        byte8: Byte8,
        romData: []u8,

        pub fn init(fileContent: []u8) @This() {
            return .{
                .isINes2 = undefined,
                .prgRomSize = undefined,
                .chrRomSize = undefined,
                .byte6 = undefined,
                .byte7 = undefined,
                .byte8 = undefined,
                .romData = fileContent,
            };
        }

        pub fn ifINes2(self: *@This()) void {
            self.isINes2 = self.byte7.iNes2Identifer == 2;
        }

        pub fn getPrgRomSize(self: *@This()) void {
            const prgLo = self.romData[4];
            const prgHi: u16 = self.romData[9];

            if (self.isINes2) {
                if ((prgHi & 0x0F) == 0x0F) {
                    const multiplier: u32 = (prgLo & 0x03) * 2 + 1;
                    const exponent = std.math.pow(u32, 2, prgLo >> 2);

                    self.prgRomSize = exponent * multiplier;
                } else {
                    self.prgRomSize = @as(u32, (((prgHi & 0x0F) << 8) | prgLo)) * 0x4000;
                }
            } else {
                if (prgLo == 0) {
                    self.prgRomSize = 256 * 0x4000;
                } else {
                    self.prgRomSize = @as(u32, @intCast(prgLo)) * 0x4000;
                }
            }
        }

        pub fn getChrRomSize(self: *@This()) void {
            const chrLo = self.romData[5];
            const chrHi: u16 = self.romData[9];

            if (self.isINes2) {
                if ((chrHi & 0x0F) == 0x0F) {
                    const multiplier = (chrLo & 0x03) * 2 + 1;
                    const exponent = std.math.pow(u32, 2, chrLo >> 2);

                    self.chrRomSize = multiplier * exponent;
                } else {
                    self.chrRomSize = @as(u32, (((chrHi & 0xF0) << 4) | chrLo)) * 0x2000;
                }
            } else {
                self.chrRomSize = @as(u32, @intCast(chrLo)) * 0x2000;
            }
        }

        pub fn setByte6(self: *@This()) void {
            self.byte6 = Byte6.fromInt(self.romData[6]);
        }

        pub fn setByte7(self: *@This()) void {
            self.byte7 = Byte7.fromInt(self.romData[7]);
        }

        pub fn setByte8(self: *@This()) void {
            self.byte8 = Byte8.fromInt(self.romData[8]);
        }
    };
}
