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

pub const status = packed struct(u8) {
    carry: bool,
    zero: bool,
    interrupt: bool,
    decimal: bool,
    brk: bool,
    reserved: bool,
    overflow: bool,
    negative: bool,

    pub fn fromInt(num: u8) status {
        return @bitCast(num);
    }

    pub fn toInt(s: status) u8 {
        return @bitCast(s);
    }
};

const ByteWordTag = enum {
    byte,
    word,
};

const ByteOrWord = union(ByteWordTag) {
    byte: u8,
    word: u16,
};

const addrValPair = struct {
    addr: ByteOrWord,
    val: u8,
};

const addrMode = enum {
    absl,
    absX,
    absY,
    accm,
    immd,
    impl,
    indi,
    indX,
    indY,
    none,
    rela,
    zero,
    zroX,
    zroY,
};

pub fn nesCpu() type {
    return struct {
        pc: u16,
        sp: u8,
        a: u8,
        x: u8,
        y: u8,
        s: status,
        cycles: u8,
        mem: [64 * 1024]u8,
        currAddrMode: addrMode,
        addrTable: [256]addrMode = [256]addrMode{
            //  +00            +01            +02            +03            +04             +05           +06            +07             +08            +09            +0A            +0B            +0C            +0D            +0E            +0F
            addrMode.impl, addrMode.indX, addrMode.none, addrMode.indX, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.impl, addrMode.immd, addrMode.accm, addrMode.immd, addrMode.absl, addrMode.absl, addrMode.absl, addrMode.absl, // 00
            addrMode.rela, addrMode.indY, addrMode.none, addrMode.indY, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.impl, addrMode.absY, addrMode.impl, addrMode.absY, addrMode.absX, addrMode.absX, addrMode.absX, addrMode.absX, // 01
            addrMode.absl, addrMode.indX, addrMode.none, addrMode.indX, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.impl, addrMode.immd, addrMode.accm, addrMode.immd, addrMode.absl, addrMode.absl, addrMode.absl, addrMode.absl, // 20
            addrMode.rela, addrMode.indY, addrMode.none, addrMode.indY, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.impl, addrMode.absY, addrMode.impl, addrMode.absY, addrMode.absX, addrMode.absX, addrMode.absX, addrMode.absX, // 30
            addrMode.impl, addrMode.indX, addrMode.none, addrMode.indX, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.impl, addrMode.immd, addrMode.accm, addrMode.immd, addrMode.absl, addrMode.absl, addrMode.absl, addrMode.absl, // 40
            addrMode.rela, addrMode.indY, addrMode.none, addrMode.indY, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.impl, addrMode.absY, addrMode.impl, addrMode.absY, addrMode.absX, addrMode.absX, addrMode.absX, addrMode.absX, // 50
            addrMode.impl, addrMode.indX, addrMode.none, addrMode.indX, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.impl, addrMode.immd, addrMode.accm, addrMode.immd, addrMode.indi, addrMode.absl, addrMode.absl, addrMode.absl, // 60
            addrMode.rela, addrMode.indY, addrMode.none, addrMode.indY, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.impl, addrMode.absY, addrMode.impl, addrMode.absY, addrMode.absX, addrMode.absX, addrMode.absX, addrMode.absX, // 70
            addrMode.immd, addrMode.indX, addrMode.immd, addrMode.indX, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.impl, addrMode.immd, addrMode.accm, addrMode.immd, addrMode.absl, addrMode.absl, addrMode.absl, addrMode.absl, // 80
            addrMode.rela, addrMode.indY, addrMode.none, addrMode.indY, addrMode.zroX, addrMode.zroX, addrMode.zroY, addrMode.zroY, addrMode.impl, addrMode.absY, addrMode.impl, addrMode.absY, addrMode.absX, addrMode.absX, addrMode.absX, addrMode.absX, // 90
            addrMode.immd, addrMode.indX, addrMode.immd, addrMode.indX, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.impl, addrMode.immd, addrMode.accm, addrMode.immd, addrMode.absl, addrMode.absl, addrMode.absl, addrMode.absl, // A0
            addrMode.rela, addrMode.indY, addrMode.none, addrMode.indY, addrMode.zroX, addrMode.zroX, addrMode.zroY, addrMode.zroY, addrMode.impl, addrMode.absY, addrMode.impl, addrMode.absY, addrMode.absX, addrMode.absX, addrMode.absX, addrMode.absX, // B0
            addrMode.immd, addrMode.indX, addrMode.immd, addrMode.indX, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.impl, addrMode.immd, addrMode.accm, addrMode.immd, addrMode.absl, addrMode.absl, addrMode.absl, addrMode.absl, // C0
            addrMode.rela, addrMode.indY, addrMode.none, addrMode.indY, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.impl, addrMode.absY, addrMode.impl, addrMode.absY, addrMode.absX, addrMode.absX, addrMode.absX, addrMode.absX, // D0
            addrMode.immd, addrMode.indX, addrMode.immd, addrMode.indX, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.zero, addrMode.impl, addrMode.immd, addrMode.accm, addrMode.immd, addrMode.absl, addrMode.absl, addrMode.absl, addrMode.absl, // E0
            addrMode.rela, addrMode.indY, addrMode.none, addrMode.indY, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.impl, addrMode.absY, addrMode.impl, addrMode.absY, addrMode.absX, addrMode.absX, addrMode.absX, addrMode.absX, // F0
        },

        pub fn init(PC: u16, SP: u8, A: u8, X: u8, Y: u8, S: u8) @This() {
            return .{
                .pc = PC,
                .sp = SP,
                .a = A,
                .x = X,
                .y = Y,
                .s = status.fromInt(S),
                .cycles = 0,
                .mem = undefined,
                .currAddrMode = addrMode.none,
            };
        }

        pub fn memFetchByte(self: *@This()) u8 {
            const data = self.mem[self.pc];
            self.pc +%= 1;
            self.cycles += 1;

            return data;
        }

        fn memFetchWord(self: *@This()) u16 {
            const lo = self.memFetchByte();
            const hi: u16 = self.memFetchByte();

            return (hi << 8) | lo;
        }

        fn memReadByte(self: *@This(), comptime T: type, addr: T) u8 {
            const data = self.mem[addr];
            self.cycles += 1;

            return data;
        }

        fn memReadWord(self: *@This(), addr: u8) u16 {
            const lo = self.memReadByte(u8, addr);
            const hi: u16 = self.memReadByte(u8, addr +% 1);

            return (hi << 8) | lo;
        }

        fn memWriteByte(self: *@This(), addr: u16, val: u8) void {
            self.mem[addr] = val;
            self.cycles += 1;
        }

        fn fetchAbsolute(self: *@This()) addrValPair {
            const addr = self.memFetchWord();
            const val = self.memReadByte(u16, addr);

            return addrValPair{
                .addr = ByteOrWord{ .word = addr },
                .val = val,
            };
        }

        fn fetchAbsoluteX(self: *@This()) addrValPair {
            const addr = self.memFetchWord();
            const val = self.memReadByte(u16, addr +% self.x);

            if (checkPageCrossed(addr, self.x)) {
                self.cycles += 1;
            }

            return addrValPair{
                .addr = ByteOrWord{ .word = addr +% self.x },
                .val = val,
            };
        }

        fn fetchAbsoluteY(self: *@This()) addrValPair {
            const addr = self.memFetchWord();
            const val = self.memReadByte(u16, addr +% self.y);

            if (checkPageCrossed(addr, self.y)) {
                self.cycles += 1;
            }

            return addrValPair{
                .addr = ByteOrWord{ .word = addr +% self.y },
                .val = val,
            };
        }

        fn fetchAccumulator(self: *@This()) addrValPair {
            self.cycles += 1;

            return addrValPair{
                .addr = undefined,
                .val = undefined,
            };
        }

        fn fetchImmediate(self: *@This()) addrValPair {
            const val = self.memFetchByte();

            return addrValPair{
                .addr = undefined,
                .val = val,
            };
        }

        fn fetchIndirectX(self: *@This()) addrValPair {
            const addr = self.memReadWord(self.memFetchByte() +% self.x);
            const val = self.memReadByte(u16, addr);

            self.cycles += 1;

            return addrValPair{
                .addr = ByteOrWord{ .word = addr },
                .val = val,
            };
        }

        fn fetchIndirectY(self: *@This()) addrValPair {
            const addr = self.memReadWord(self.memFetchByte());
            const val = self.memReadByte(u16, addr +% self.y);

            if (checkPageCrossed(addr, self.y)) {
                self.cycles += 1;
            }

            return addrValPair{
                .addr = ByteOrWord{ .word = addr +% self.y },
                .val = val,
            };
        }

        fn fetchZeroPage(self: *@This()) addrValPair {
            const addr = self.memFetchByte();
            const val = self.memReadByte(u8, addr);

            return addrValPair{
                .addr = ByteOrWord{ .byte = addr },
                .val = val,
            };
        }

        fn fetchZeroPageX(self: *@This()) addrValPair {
            const addr = self.memFetchByte() +% self.x;
            const val = self.memReadByte(u8, addr);

            self.cycles += 1;

            return addrValPair{
                .addr = ByteOrWord{ .byte = addr },
                .val = val,
            };
        }

        fn fetchZeroPageY(self: *@This()) addrValPair {
            const addr = self.memFetchByte();
            const val = self.memReadByte(u8, addr);

            if (checkPageCrossed(addr, self.y)) {
                self.cycles += 1;
            }

            return addrValPair{
                .addr = ByteOrWord{ .byte = addr +% self.y },
                .val = val,
            };
        }

        pub fn fetchCurrAddrMode(self: *@This(), index: u8) void {
            self.currAddrMode = self.addrTable[index];
        }

        fn fetchOperand(self: *@This()) addrValPair {
            return switch (self.currAddrMode) {
                .absl => self.fetchAbsolute(),
                .absX => self.fetchAbsoluteX(),
                .absY => self.fetchAbsoluteY(),
                .accm => self.fetchAccumulator(),
                .immd => self.fetchImmediate(),
                .indX => self.fetchIndirectX(),
                .indY => self.fetchIndirectY(),
                .zero => self.fetchZeroPage(),
                .zroX => self.fetchZeroPageX(),
                else => .{ .addr = ByteOrWord{ .byte = 0 }, .val = 0 },
            };
        }

        fn ADD(self: *@This(), val: u8) void {
            const res = @as(u16, self.a) + @as(u16, val) + @intFromBool(self.s.carry);

            const AreSignBitsTheSame = ((self.a ^ val) & 0x80) == 0;

            self.a = @as(u8, @truncate(res));

            self.s.carry = res > 0xFF;
            self.s.overflow = AreSignBitsTheSame and (((self.a ^ val) & 0x80) != 0);

            self.setZeroNegative(self.a);
        }

        pub fn ADC(self: *@This()) void {
            const val = self.fetchOperand().val;
            self.ADD(val);
        }

        fn ASL(self: *@This(), oldVal: u8) u8 {
            self.s.carry = (oldVal & 0x80) == 0x80;

            const newVal = oldVal << 1;
            self.setZeroNegative(newVal);

            return newVal;
        }

        pub fn ASLAcc(self: *@This()) void {
            _ = self.fetchOperand();
            self.a = self.ASL(self.a);
        }

        pub fn ASLMem(self: *@This()) void {
            const opr = self.fetchOperand();

            const addr = switch (opr.addr) {
                .word => opr.addr.word,
                .byte => opr.addr.byte,
            };

            const val = opr.val;
            self.cycles += 1;
            self.memWriteByte(addr, self.ASL(val));

            // FIXME
            if (self.currAddrMode == addrMode.absX) {
                self.cycles = 7;
            }
        }

        fn CMP(self: *@This(), register: u8, val: u8) void {
            const res = register -% val;

            self.s.carry = register >= val;
            self.s.zero = register == val;
            self.s.negative = (res & 0x80) == 0x80;
        }

        pub fn CPA(self: *@This()) void {
            const val = self.fetchOperand().val;
            self.CMP(self.a, val);
        }

        pub fn CPX(self: *@This()) void {
            const val = self.fetchOperand().val;
            self.CMP(self.x, val);
        }

        pub fn CPY(self: *@This()) void {
            const val = self.fetchOperand().val;
            self.CMP(self.y, val);
        }

        fn LSR(self: *@This(), oldVal: u8) u8 {
            self.s.carry = (oldVal & 0x01) == 0x01;

            const newVal = oldVal >> 1;
            self.setZeroNegative(newVal);

            return newVal;
        }

        pub fn LSRAcc(self: *@This()) void {
            _ = self.fetchOperand();
            self.a = self.LSR(self.a);
        }

        pub fn LSRMem(self: *@This()) void {
            const opr = self.fetchOperand();

            const addr = switch (opr.addr) {
                .word => opr.addr.word,
                .byte => opr.addr.byte,
            };

            const val = opr.val;
            self.cycles += 1;
            self.memWriteByte(addr, self.LSR(val));

            // FIXME
            if (self.currAddrMode == addrMode.absX) {
                self.cycles = 7;
            }
        }

        fn ROL(self: *@This(), oldVal: u8) u8 {
            const carry: u8 = if (self.s.carry) 0x01 else 0x00;
            self.s.carry = (oldVal & 0x80) == 0x80;

            const newVal: u8 = (oldVal << 1) | carry;
            self.setZeroNegative(newVal);

            return newVal;
        }

        pub fn ROLAcc(self: *@This()) void {
            _ = self.fetchOperand();
            self.a = self.ROL(self.a);
        }

        pub fn ROLMem(self: *@This()) void {
            const opr = self.fetchOperand();

            const addr = switch (opr.addr) {
                .word => opr.addr.word,
                .byte => opr.addr.byte,
            };

            const val = opr.val;
            self.cycles += 1;
            self.memWriteByte(addr, self.ROL(val));

            // FIXME
            if (self.currAddrMode == addrMode.absX) {
                self.cycles = 7;
            }
        }

        fn ROR(self: *@This(), oldVal: u8) u8 {
            const carry: u8 = if (self.s.carry) 0x80 else 0x00;
            self.s.carry = (oldVal & 0x01) == 0x01;

            const newVal: u8 = (oldVal >> 1) | carry;
            self.setZeroNegative(newVal);

            return newVal;
        }

        pub fn RORAcc(self: *@This()) void {
            _ = self.fetchOperand();
            self.a = self.ROR(self.a);
        }

        pub fn RORMem(self: *@This()) void {
            const opr = self.fetchOperand();

            const addr = switch (opr.addr) {
                .word => opr.addr.word,
                .byte => opr.addr.byte,
            };

            const val = opr.val;
            self.cycles += 1;
            self.memWriteByte(addr, self.ROR(val));

            // FIXME
            if (self.currAddrMode == addrMode.absX) {
                self.cycles = 7;
            }
        }

        pub fn SBC(self: *@This()) void {
            const val = self.fetchOperand().val;
            self.ADD(~val);
        }

        fn checkPageCrossed(valA: u16, valB: u8) bool {
            return ((valA +% valB) & 0xFF00) != (valA & 0xFF00);
        }

        inline fn setZeroNegative(self: *@This(), register: u8) void {
            self.s.zero = register == 0;
            self.s.negative = (register & 0x80) == 0x80;
        }
    };
}

pub fn main() void {}
