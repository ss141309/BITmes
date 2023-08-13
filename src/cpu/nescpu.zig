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

const status = packed struct(u8) {
    carry: bool = false,
    zero: bool = false,
    interrupt: bool = false,
    decimal: bool = false,
    brk: bool = false,
    reserved: bool = false,
    overflow: bool = false,
    negative: bool = false,
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

const nescpu = struct {
    pc: u16 = 0,
    sp: u8 = 0,
    a: u8 = 0,
    x: u8 = 0,
    y: u8 = 0,
    s: status = .{},
    mem: [64 * 1024]u8 = std.mem.zeroes([64 * 1024]u8),
    cycles: u16 = 0,

    fn memFetch(self: *nescpu) u8 {
        const data = self.mem[self.pc];
        self.pc += 1;
        self.cycles += 1;

        return data;
    }

    fn fetchImmediate(self: *nescpu) u8 {
        return self.memFetch();
    }

    inline fn setZeroNegative(self: *nescpu, register: u8) void {
        if (register == 0) {
            self.s.zero = true;
        } else if ((register & 0x80) == 0x80) {
            self.s.negative = true;
        }
    }

    fn ADC(self: *nescpu) void {
        const val = self.memFetch();
        const res = @as(u16, self.a) + @as(u16, val) + @intFromBool(self.s.carry);

        self.a = @as(u8, @truncate(res));

        if (res > 0xFF) {
            self.s.carry = true;
        }

        if ((~(self.a ^ val) & (self.a ^ res) & 0x80) != 0) {
            self.s.overflow = true;
        }

        self.setZeroNegative(self.a);
    }

    fn AHX(self: *nescpu) void {
        _ = self;
    }

    fn ALR(self: *nescpu) void {
        _ = self;
    }

    fn ANC(self: *nescpu) void {
        _ = self;
    }

    fn AND(self: *nescpu) void {
        _ = self;
    }

    fn ARR(self: *nescpu) void {
        _ = self;
    }

    fn ASL(self: *nescpu) void {
        _ = self;
    }

    fn AXS(self: *nescpu) void {
        _ = self;
    }

    fn BCC(self: *nescpu) void {
        _ = self;
    }

    fn BCS(self: *nescpu) void {
        _ = self;
    }

    fn BEQ(self: *nescpu) void {
        _ = self;
    }

    fn BIT(self: *nescpu) void {
        _ = self;
    }

    fn BMI(self: *nescpu) void {
        _ = self;
    }

    fn BNE(self: *nescpu) void {
        _ = self;
    }

    fn BPL(self: *nescpu) void {
        _ = self;
    }

    fn BRK(self: *nescpu) void {
        _ = self;
    }

    fn BVC(self: *nescpu) void {
        _ = self;
    }

    fn BVS(self: *nescpu) void {
        _ = self;
    }

    fn CLC(self: *nescpu) void {
        _ = self;
    }

    fn CLD(self: *nescpu) void {
        _ = self;
    }

    fn CLI(self: *nescpu) void {
        _ = self;
    }

    fn CLV(self: *nescpu) void {
        _ = self;
    }

    fn CMP(self: *nescpu) void {
        _ = self;
    }

    fn CPX(self: *nescpu) void {
        _ = self;
    }

    fn CPY(self: *nescpu) void {
        _ = self;
    }

    fn DCP(self: *nescpu) void {
        _ = self;
    }

    fn DEC(self: *nescpu) void {
        _ = self;
    }

    fn DEX(self: *nescpu) void {
        _ = self;
    }

    fn DEY(self: *nescpu) void {
        _ = self;
    }

    fn EOR(self: *nescpu) void {
        _ = self;
    }

    fn INC(self: *nescpu) void {
        _ = self;
    }

    fn INX(self: *nescpu) void {
        _ = self;
    }

    fn INY(self: *nescpu) void {
        _ = self;
    }

    fn ISC(self: *nescpu) void {
        _ = self;
    }

    fn JMP(self: *nescpu) void {
        _ = self;
    }

    fn JSR(self: *nescpu) void {
        _ = self;
    }

    fn LAS(self: *nescpu) void {
        _ = self;
    }

    fn LAX(self: *nescpu) void {
        _ = self;
    }

    fn LDA(self: *nescpu) void {
        _ = self;
    }

    fn LDX(self: *nescpu) void {
        _ = self;
    }

    fn LDY(self: *nescpu) void {
        _ = self;
    }

    fn LSR(self: *nescpu) void {
        _ = self;
    }

    fn NOP(self: *nescpu) void {
        _ = self;
    }

    fn ORA(self: *nescpu) void {
        _ = self;
    }

    fn PHA(self: *nescpu) void {
        _ = self;
    }

    fn PHP(self: *nescpu) void {
        _ = self;
    }

    fn PLA(self: *nescpu) void {
        _ = self;
    }

    fn PLP(self: *nescpu) void {
        _ = self;
    }

    fn RLA(self: *nescpu) void {
        _ = self;
    }

    fn ROL(self: *nescpu) void {
        _ = self;
    }

    fn ROR(self: *nescpu) void {
        _ = self;
    }

    fn RRA(self: *nescpu) void {
        _ = self;
    }

    fn RTI(self: *nescpu) void {
        _ = self;
    }

    fn RTS(self: *nescpu) void {
        _ = self;
    }

    fn SAX(self: *nescpu) void {
        _ = self;
    }

    fn SBC(self: *nescpu) void {
        _ = self;
    }

    fn SEC(self: *nescpu) void {
        _ = self;
    }

    fn SED(self: *nescpu) void {
        _ = self;
    }

    fn SEI(self: *nescpu) void {
        _ = self;
    }

    fn SHX(self: *nescpu) void {
        _ = self;
    }

    fn SHY(self: *nescpu) void {
        _ = self;
    }

    fn SLO(self: *nescpu) void {
        _ = self;
    }

    fn SRE(self: *nescpu) void {
        _ = self;
    }

    fn STA(self: *nescpu) void {
        _ = self;
    }

    fn STP(self: *nescpu) void {
        _ = self;
    }

    fn STX(self: *nescpu) void {
        _ = self;
    }

    fn STY(self: *nescpu) void {
        _ = self;
    }

    fn TAS(self: *nescpu) void {
        _ = self;
    }

    fn TAX(self: *nescpu) void {
        _ = self;
    }

    fn TAY(self: *nescpu) void {
        _ = self;
    }

    fn TSX(self: *nescpu) void {
        _ = self;
    }

    fn TXA(self: *nescpu) void {
        _ = self;
    }

    fn TXS(self: *nescpu) void {
        _ = self;
    }

    fn TYA(self: *nescpu) void {
        _ = self;
    }

    fn XAA(self: *nescpu) void {
        _ = self;
    }
};

pub fn fromInt(num: u8) status {
    return @bitCast(num);
}

pub fn toInt(s: status) u8 {
    return @bitCast(s);
}

pub fn main() void {
    var cpu = nescpu{};
    const opTable = [256]*const void{
        // +00         +01         +02         +03         +04         +05         +06         +07         +08          +09         +0A         +0B         +0C         +0D         +0E         +0F
        &cpu.BRK(), &cpu.ORA(), &cpu.STP(), &cpu.SLO(), &cpu.NOP(), &cpu.ORA(), &cpu.ASL(), &cpu.SLO(), &cpu.PHP(), &cpu.ORA(), &cpu.ASL(), &cpu.ANC(), &cpu.NOP(), &cpu.ORA(), &cpu.ASL(), &cpu.SLO(), // 00
        &cpu.BPL(), &cpu.ORA(), &cpu.STP(), &cpu.SLO(), &cpu.NOP(), &cpu.ORA(), &cpu.ASL(), &cpu.SLO(), &cpu.CLC(), &cpu.ORA(), &cpu.NOP(), &cpu.SLO(), &cpu.NOP(), &cpu.ORA(), &cpu.ASL(), &cpu.SLO(), // 10
        &cpu.JSR(), &cpu.AND(), &cpu.STP(), &cpu.RLA(), &cpu.BIT(), &cpu.AND(), &cpu.ROL(), &cpu.RLA(), &cpu.PLP(), &cpu.AND(), &cpu.ROL(), &cpu.ANC(), &cpu.BIT(), &cpu.AND(), &cpu.ROL(), &cpu.RLA(), // 20
        &cpu.BMI(), &cpu.AND(), &cpu.STP(), &cpu.RLA(), &cpu.NOP(), &cpu.AND(), &cpu.ROL(), &cpu.RLA(), &cpu.SEC(), &cpu.AND(), &cpu.NOP(), &cpu.RLA(), &cpu.NOP(), &cpu.AND(), &cpu.ROL(), &cpu.RLA(), // 30
        &cpu.RTI(), &cpu.EOR(), &cpu.STP(), &cpu.SRE(), &cpu.NOP(), &cpu.EOR(), &cpu.LSR(), &cpu.SRE(), &cpu.PHA(), &cpu.EOR(), &cpu.LSR(), &cpu.ALR(), &cpu.JMP(), &cpu.EOR(), &cpu.LSR(), &cpu.SRE(), // 40
        &cpu.BVC(), &cpu.EOR(), &cpu.STP(), &cpu.SRE(), &cpu.NOP(), &cpu.EOR(), &cpu.LSR(), &cpu.SRE(), &cpu.CLI(), &cpu.EOR(), &cpu.NOP(), &cpu.SRE(), &cpu.NOP(), &cpu.EOR(), &cpu.LSR(), &cpu.SRE(), // 50
        &cpu.RTS(), &cpu.ADC(), &cpu.STP(), &cpu.RRA(), &cpu.NOP(), &cpu.ADC(), &cpu.ROR(), &cpu.RRA(), &cpu.PLA(), &cpu.ADC(), &cpu.ROR(), &cpu.ARR(), &cpu.JMP(), &cpu.ADC(), &cpu.ROR(), &cpu.RRA(), // 60
        &cpu.BVS(), &cpu.ADC(), &cpu.STP(), &cpu.RRA(), &cpu.NOP(), &cpu.ADC(), &cpu.ROR(), &cpu.RRA(), &cpu.SEI(), &cpu.ADC(), &cpu.NOP(), &cpu.RRA(), &cpu.NOP(), &cpu.ADC(), &cpu.ROR(), &cpu.RRA(), // 70
        &cpu.NOP(), &cpu.STA(), &cpu.NOP(), &cpu.SAX(), &cpu.STY(), &cpu.STA(), &cpu.STX(), &cpu.SAX(), &cpu.DEY(), &cpu.NOP(), &cpu.TXA(), &cpu.XAA(), &cpu.STY(), &cpu.STA(), &cpu.STX(), &cpu.SAX(), // 80
        &cpu.BCC(), &cpu.STA(), &cpu.STP(), &cpu.AHX(), &cpu.STY(), &cpu.STA(), &cpu.STX(), &cpu.SAX(), &cpu.TYA(), &cpu.STA(), &cpu.TXS(), &cpu.TAS(), &cpu.SHY(), &cpu.STA(), &cpu.SHX(), &cpu.AHX(), // 90
        &cpu.LDY(), &cpu.LDA(), &cpu.LDX(), &cpu.LAX(), &cpu.LDY(), &cpu.LDA(), &cpu.LDX(), &cpu.LAX(), &cpu.TAY(), &cpu.LDA(), &cpu.TAX(), &cpu.LAX(), &cpu.LDY(), &cpu.LDA(), &cpu.LDX(), &cpu.LAX(), // A0
        &cpu.BCS(), &cpu.LDA(), &cpu.STP(), &cpu.LAX(), &cpu.LDY(), &cpu.LDA(), &cpu.LDX(), &cpu.LAX(), &cpu.CLV(), &cpu.LDA(), &cpu.TSX(), &cpu.LAS(), &cpu.LDY(), &cpu.LDA(), &cpu.LDX(), &cpu.LAX(), // B0
        &cpu.CPY(), &cpu.CMP(), &cpu.NOP(), &cpu.DCP(), &cpu.CPY(), &cpu.CMP(), &cpu.DEC(), &cpu.DCP(), &cpu.INY(), &cpu.CMP(), &cpu.DEX(), &cpu.AXS(), &cpu.CPY(), &cpu.CMP(), &cpu.DEC(), &cpu.DCP(), // C0
        &cpu.BNE(), &cpu.CMP(), &cpu.STP(), &cpu.DCP(), &cpu.NOP(), &cpu.CMP(), &cpu.DEC(), &cpu.DCP(), &cpu.CLD(), &cpu.CMP(), &cpu.NOP(), &cpu.DCP(), &cpu.NOP(), &cpu.CMP(), &cpu.DEC(), &cpu.DCP(), // D0
        &cpu.CPX(), &cpu.SBC(), &cpu.NOP(), &cpu.ISC(), &cpu.CPX(), &cpu.SBC(), &cpu.INC(), &cpu.ISC(), &cpu.INX(), &cpu.SBC(), &cpu.NOP(), &cpu.SBC(), &cpu.CPX(), &cpu.SBC(), &cpu.INC(), &cpu.ISC(), // E0
        &cpu.BEQ(), &cpu.SBC(), &cpu.STP(), &cpu.ISC(), &cpu.NOP(), &cpu.SBC(), &cpu.INC(), &cpu.ISC(), &cpu.SED(), &cpu.SBC(), &cpu.NOP(), &cpu.ISC(), &cpu.NOP(), &cpu.SBC(), &cpu.INC(), &cpu.ISC(), // F0
    };

    const addrTable = [256]addrMode{
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
        addrMode.rela, addrMode.indY, addrMode.none, addrMode.indY, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.zroX, addrMode.impl, addrMode.absY, addrMode.impl, addrMode.absY, addrMode.absX, addrMode.absX, addrMode.absX, addrMode.absX, // E0
    };
    _ = addrTable;
    _ = opTable;
}
