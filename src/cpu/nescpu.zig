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
    reserved: bool,
    brk: bool,
    overflow: bool,
    negative: bool,

    pub fn fromInt(num: u8) status {
        return @bitCast(num);
    }

    pub fn toInt(s: status) u8 {
        return @bitCast(s);
    }
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
            addrMode.rela, addrMode.indY, addrMode.none, addrMode.indY, addrMode.zroX, addrMode.zroX, addrMode.zroY, addrMode.zroY, addrMode.impl, addrMode.absY, addrMode.impl, addrMode.absY, addrMode.absX, addrMode.absX, addrMode.absY, addrMode.absX, // B0
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

        // Read/Write
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

        fn memReadByte(self: *@This(), addr: u16) u8 {
            const data = self.mem[addr];
            self.cycles += 1;

            return data;
        }

        fn memReadWord(self: *@This(), addr: u8) u16 {
            const lo = self.memReadByte(addr);
            const hi: u16 = self.memReadByte(addr +% 1);

            return (hi << 8) | lo;
        }

        fn memWriteByte(self: *@This(), addr: u16, val: u8) void {
            self.mem[addr] = val;
            self.cycles += 1;
        }

        // Addressing Modes
        fn fetchAbsolute(self: *@This()) u16 {
            const addr = self.memFetchWord();

            return addr;
        }

        fn fetchAbsoluteX(self: *@This()) u16 {
            const addr = self.memFetchWord();

            if (checkPageCrossed(addr, self.x)) {
                self.cycles += 1;
            }

            return addr +% self.x;
        }

        fn fetchAbsoluteY(self: *@This()) u16 {
            const addr = self.memFetchWord();

            if (checkPageCrossed(addr, self.y)) {
                self.cycles += 1;
            }

            return addr +% self.y;
        }

        fn fetchAccumulator(self: *@This()) u16 {
            self.cycles += 1;

            return 0;
        }

        fn fetchImmediate(self: *@This()) u16 {
            return self.memFetchByte();
        }

        fn fetchImplied(self: *@This()) u16 {
            self.cycles += 1;

            return 0;
        }

        fn fetchIndirect(self: *@This()) u16 {
            const baseAddr = self.memFetchWord();
            if ((baseAddr & 0x00FF) == 0x00FF) {
                const lo = self.memReadByte(baseAddr);
                const hi: u16 = self.memReadByte(baseAddr - 0xFF);

                return (hi << 8) | lo;
            } else {
                const lo = self.memReadByte(baseAddr);
                const hi: u16 = self.memReadByte(baseAddr + 1);

                return (hi << 8) | lo;
            }
        }

        fn fetchIndirectX(self: *@This()) u16 {
            const addr = self.memReadWord(self.memFetchByte() +% self.x);

            self.cycles += 1;

            return addr;
        }

        fn fetchIndirectY(self: *@This()) u16 {
            const addr = self.memReadWord(self.memFetchByte());

            if (checkPageCrossed(addr, self.y)) {
                self.cycles += 1;
            }

            return addr +% self.y;
        }

        fn fetchZeroPage(self: *@This()) u16 {
            return self.memFetchByte();
        }

        fn fetchZeroPageX(self: *@This()) u16 {
            const addr = self.memFetchByte() +% self.x;

            self.cycles += 1;

            return addr;
        }

        fn fetchZeroPageY(self: *@This()) u16 {
            const addr = self.memFetchByte() +% self.y;

            self.cycles += 1;

            return addr;
        }

        pub fn fetchCurrAddrMode(self: *@This(), index: u8) void {
            self.currAddrMode = self.addrTable[index];
        }

        fn fetchAddress(self: *@This()) u16 {
            return switch (self.currAddrMode) {
                .absl => self.fetchAbsolute(),
                .absX => self.fetchAbsoluteX(),
                .absY => self.fetchAbsoluteY(),
                .accm => self.fetchAccumulator(),
                .immd => self.fetchImmediate(),
                .impl => self.fetchImplied(),
                .indi => self.fetchIndirect(),
                .indX => self.fetchIndirectX(),
                .indY => self.fetchIndirectY(),
                .rela => self.fetchImmediate(),
                .zero => self.fetchZeroPage(),
                .zroX => self.fetchZeroPageX(),
                .zroY => self.fetchZeroPageY(),
                else => 0,
            };
        }

        fn fetchOperand(self: *@This(), addr: u16) u8 {
            if (self.currAddrMode == addrMode.immd or self.currAddrMode == addrMode.rela) {
                return @as(u8, @intCast(addr));
            } else {
                return self.memReadByte(addr);
            }
        }

        // Instructions
        fn ADD(self: *@This(), val: u8) void {
            const res = @as(u16, self.a) + @as(u16, val) + @intFromBool(self.s.carry);

            const AreSignBitsTheSame = ((self.a ^ val) & 0x80) == 0;

            self.a = @as(u8, @truncate(res));

            self.s.carry = res > 0xFF;
            self.s.overflow = AreSignBitsTheSame and (((self.a ^ val) & 0x80) != 0);

            self.setZeroNegative(self.a);
        }

        pub fn ADC(self: *@This()) void {
            const val = self.fetchOperand(self.fetchAddress());
            self.ADD(val);
        }

        pub fn AND(self: *@This()) void {
            const val = self.fetchOperand(self.fetchAddress());
            self.a &= val;
            self.setZeroNegative(self.a);
        }

        fn ASL(self: *@This(), oldVal: u8) u8 {
            self.s.carry = (oldVal & 0x80) == 0x80;

            const newVal = oldVal << 1;
            self.setZeroNegative(newVal);

            return newVal;
        }

        pub fn ASLAcc(self: *@This()) void {
            _ = self.fetchAddress();
            self.a = self.ASL(self.a);
        }

        pub fn ASLMem(self: *@This()) void {
            const addr = self.fetchAddress();
            const val = self.memReadByte(addr);

            self.cycles += 1;
            self.memWriteByte(addr, self.ASL(val));

            // FIXME
            if (self.currAddrMode == addrMode.absX) {
                self.cycles = 7;
            }
        }

        pub fn BIT(self: *@This()) void {
            const val = self.memReadByte(self.fetchAddress());

            self.s.zero = (self.a & val) == 0;
            self.s.overflow = (val & 0x40) == 0x40;
            self.s.negative = (val & 0x80) == 0x80;
        }

        pub fn BCC(self: *@This()) void {
            self.BRA(!self.s.carry);
        }

        pub fn BCS(self: *@This()) void {
            self.BRA(self.s.carry);
        }

        pub fn BEQ(self: *@This()) void {
            self.BRA(self.s.zero);
        }

        pub fn BMI(self: *@This()) void {
            self.BRA(self.s.negative);
        }

        pub fn BNE(self: *@This()) void {
            self.BRA(!self.s.zero);
        }

        pub fn BPL(self: *@This()) void {
            self.BRA(!self.s.negative);
        }

        pub fn BVC(self: *@This()) void {
            self.BRA(!self.s.overflow);
        }

        pub fn BVS(self: *@This()) void {
            self.BRA(self.s.overflow);
        }

        fn BRA(self: *@This(), branch: bool) void {
            const signedAddr: i8 = @bitCast(self.fetchOperand(self.fetchAddress()));
            if (branch) {
                var temp: i32 = @as(i32, @intCast(self.pc));

                if (signedCheckPageCrossed(temp, signedAddr)) {
                    self.cycles += 1;
                }
                temp += signedAddr;
                const res: i16 = @as(i16, @truncate(temp));

                self.pc = @as(u16, @bitCast(res));
                self.cycles += 1;
            }
        }

        pub fn CLC(self: *@This()) void {
            _ = self.fetchAddress();
            self.s.carry = false;
        }

        pub fn CLD(self: *@This()) void {
            _ = self.fetchAddress();
            self.s.decimal = false;
        }

        pub fn CLI(self: *@This()) void {
            _ = self.fetchAddress();
            self.s.interrupt = false;
        }

        pub fn CLV(self: *@This()) void {
            _ = self.fetchAddress();
            self.s.overflow = false;
        }

        fn CMP(self: *@This(), register: u8) void {
            const val = self.fetchOperand(self.fetchAddress());
            const res = register -% val;

            self.s.carry = register >= val;
            self.s.zero = register == val;
            self.s.negative = (res & 0x80) == 0x80;
        }

        pub fn CPA(self: *@This()) void {
            self.CMP(self.a);
        }

        pub fn CPX(self: *@This()) void {
            self.CMP(self.x);
        }

        pub fn CPY(self: *@This()) void {
            self.CMP(self.y);
        }

        pub fn DEC(self: *@This()) void {
            const addr = self.fetchAddress();
            const val = self.fetchOperand(addr) -% 1;

            self.memWriteByte(addr, val);
            self.setZeroNegative(val);

            // FIXME
            if (self.currAddrMode == addrMode.zero) {
                self.cycles = 5;
            } else if (self.currAddrMode == addrMode.zroX or self.currAddrMode == addrMode.absl) {
                self.cycles = 6;
            } else if (self.currAddrMode == addrMode.absX) {
                self.cycles = 7;
            }
        }

        pub fn DEX(self: *@This()) void {
            _ = self.fetchAddress();
            self.x -%= 1;
            self.setZeroNegative(self.x);
        }

        pub fn DEY(self: *@This()) void {
            _ = self.fetchAddress();
            self.y -%= 1;
            self.setZeroNegative(self.y);
        }

        pub fn EOR(self: *@This()) void {
            const val = self.fetchOperand(self.fetchAddress());
            self.a ^= val;
            self.setZeroNegative(self.a);
        }

        pub fn INC(self: *@This()) void {
            const addr = self.fetchAddress();
            const val = self.fetchOperand(addr) +% 1;

            self.memWriteByte(addr, val);
            self.setZeroNegative(val);

            // FIXME
            if (self.currAddrMode == addrMode.zero) {
                self.cycles = 5;
            } else if (self.currAddrMode == addrMode.zroX or self.currAddrMode == addrMode.absl) {
                self.cycles = 6;
            } else if (self.currAddrMode == addrMode.absX) {
                self.cycles = 7;
            }
        }

        pub fn INX(self: *@This()) void {
            _ = self.fetchAddress();
            self.x +%= 1;
            self.setZeroNegative(self.x);
        }

        pub fn INY(self: *@This()) void {
            _ = self.fetchAddress();
            self.y +%= 1;
            self.setZeroNegative(self.y);
        }

        pub fn JMP(self: *@This()) void {
            self.pc = self.fetchAddress();
        }

        pub fn JSR(self: *@This()) void {
            const addr = self.fetchAddress();
            self.pushWord(self.pc - 1);
            self.pc = addr;
            self.cycles += 1;
        }

        fn LOD(self: *@This(), register: *u8) void {
            const val = self.fetchOperand(self.fetchAddress());
            register.* = val;
            self.setZeroNegative(register.*);
        }

        pub fn LDA(self: *@This()) void {
            self.LOD(&self.a);
        }

        pub fn LDX(self: *@This()) void {
            self.LOD(&self.x);
        }

        pub fn LDY(self: *@This()) void {
            self.LOD(&self.y);
        }

        fn LSR(self: *@This(), oldVal: u8) u8 {
            self.s.carry = (oldVal & 0x01) == 0x01;

            const newVal = oldVal >> 1;
            self.setZeroNegative(newVal);

            return newVal;
        }

        pub fn LSRAcc(self: *@This()) void {
            _ = self.fetchAddress();
            self.a = self.LSR(self.a);
        }

        pub fn LSRMem(self: *@This()) void {
            const addr = self.fetchAddress();
            const val = self.memReadByte(addr);

            self.cycles += 1;
            self.memWriteByte(addr, self.LSR(val));

            // FIXME
            if (self.currAddrMode == addrMode.absX) {
                self.cycles = 7;
            }
        }

        pub fn ORA(self: *@This()) void {
            const val = self.fetchOperand(self.fetchAddress());
            self.a |= val;
            self.setZeroNegative(self.a);
        }

        pub fn PHA(self: *@This()) void {
            _ = self.fetchAddress();
            self.push(self.a);
        }

        pub fn PHP(self: *@This()) void {
            _ = self.fetchAddress();
            self.push(status.toInt(self.s) | 0x30); // Set bits 4 and 5 on
        }

        pub fn PLA(self: *@This()) void {
            _ = self.fetchAddress();
            self.a = self.pop();
            self.setZeroNegative(self.a);
        }

        pub fn PLP(self: *@This()) void {
            _ = self.fetchAddress();
            self.s = status.fromInt(self.pop());
            self.s.reserved = false;
            self.s.brk = true;
        }

        fn ROL(self: *@This(), oldVal: u8) u8 {
            const carry: u8 = if (self.s.carry) 0x01 else 0x00;
            self.s.carry = (oldVal & 0x80) == 0x80;

            const newVal: u8 = (oldVal << 1) | carry;
            self.setZeroNegative(newVal);

            return newVal;
        }

        pub fn ROLAcc(self: *@This()) void {
            _ = self.fetchAddress();
            self.a = self.ROL(self.a);
        }

        pub fn ROLMem(self: *@This()) void {
            const addr = self.fetchAddress();
            const val = self.memReadByte(addr);

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
            _ = self.fetchAddress();
            self.a = self.ROR(self.a);
        }

        pub fn RORMem(self: *@This()) void {
            const addr = self.fetchAddress();
            const val = self.memReadByte(addr);

            self.cycles += 1;
            self.memWriteByte(addr, self.ROR(val));

            // FIXME
            if (self.currAddrMode == addrMode.absX) {
                self.cycles = 7;
            }
        }

        pub fn RTS(self: *@This()) void {
            _ = self.fetchAddress();
            self.pc = self.popWord() + 1;
        }

        pub fn SBC(self: *@This()) void {
            const val = self.fetchOperand(self.fetchAddress());
            self.ADD(~val);
        }

        pub fn SEC(self: *@This()) void {
            _ = self.fetchAddress();
            self.s.carry = true;
        }

        pub fn SED(self: *@This()) void {
            _ = self.fetchAddress();
            self.s.decimal = true;
        }

        pub fn SEI(self: *@This()) void {
            _ = self.fetchAddress();
            self.s.interrupt = true;
        }

        fn STR(self: *@This(), register: u8) void {
            const addr = self.fetchAddress();
            self.memWriteByte(addr, register);

            // FIXME
            if (self.currAddrMode == addrMode.indY) {
                self.cycles = 6;
            } else if (self.currAddrMode == addrMode.absY or self.currAddrMode == addrMode.absX) {
                self.cycles = 5;
            }
        }

        pub fn STA(self: *@This()) void {
            self.STR(self.a);
        }

        pub fn STX(self: *@This()) void {
            self.STR(self.x);
        }

        pub fn STY(self: *@This()) void {
            self.STR(self.y);
        }

        fn TRA(self: *@This(), reg1: *u8, reg2: u8) void {
            _ = self.fetchAddress();
            reg1.* = reg2;
            self.setZeroNegative(reg1.*);
        }

        pub fn TAX(self: *@This()) void {
            self.TRA(&self.x, self.a);
        }

        pub fn TAY(self: *@This()) void {
            self.TRA(&self.y, self.a);
        }

        pub fn TSX(self: *@This()) void {
            self.TRA(&self.x, self.sp);
        }

        pub fn TXA(self: *@This()) void {
            self.TRA(&self.a, self.x);
        }

        pub fn TXS(self: *@This()) void {
            _ = self.fetchAddress();
            self.sp = self.x;
        }

        pub fn TYA(self: *@This()) void {
            self.TRA(&self.a, self.y);
        }

        // Misc
        fn checkPageCrossed(valA: u16, valB: u8) bool {
            return ((valA +% valB) & 0xFF00) != (valA & 0xFF00);
        }

        fn signedCheckPageCrossed(valA: i32, valB: i8) bool {
            return ((valA +% valB) & 0xFF00) != (valA & 0xFF00);
        }

        fn push(self: *@This(), val: u8) void {
            const addr = @as(u16, self.sp) + 0x100;
            self.mem[addr] = val;

            self.sp -%= 1;
            self.cycles += 1;
        }

        fn pushWord(self: *@This(), val: u16) void {
            const lo: u16 = val >> 8;
            const hi: u8 = @truncate(val);

            self.push(@as(u8, @intCast(lo)));
            self.push(hi);
        }

        fn pop(self: *@This()) u8 {
            self.sp +%= 1;
            self.cycles += 1;

            const addr = @as(u16, self.sp) + 0x100;
            return self.memReadByte(addr);
        }

        fn popWord(self: *@This()) u16 {
            const lo = self.pop();
            const hi: u16 = self.pop();

            return (hi << 8) | lo;
        }

        inline fn setZeroNegative(self: *@This(), register: u8) void {
            self.s.zero = register == 0;
            self.s.negative = (register & 0x80) == 0x80;
        }
    };
}

pub fn main() void {}
