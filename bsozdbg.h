#ifndef BSOZDBG_H
#define BSOZDBG_H

#include <stdio.h>

void dump(struct cpu *cpu) {
	printf("A:%02x  X:%02x  Y:%02x  S:%02x[%02x %02x %02x %02x]  F:%02x[%c%c%c%c%c%c%c%c]  PC:%04x ",
			cpu->a, cpu->x, cpu->y, cpu->s, 
			r8(cpu, 0x100 + cpu->s+1),
			r8(cpu, 0x100 + cpu->s+2),
			r8(cpu, 0x100 + cpu->s+3),
			r8(cpu, 0x100 + cpu->s+4),
			cpu->f, 
			cpu->f & 0x80 ? 'N' : '.',
			cpu->f & 0x40 ? 'V' : '.',
			cpu->f & 0x20 ? '1' : '.',
			cpu->f & 0x10 ? 'B' : '.',
			cpu->f & 0x08 ? 'D' : '.',
			cpu->f & 0x84 ? 'I' : '.',
			cpu->f & 0x82 ? 'Z' : '.',
			cpu->f & 0x81 ? 'C' : '.',
			cpu->pc - 1);
}

uint8_t disasm(struct cpu *c, uint8_t op) {
	dump(c);
	int a = r8(c, c->pc);
	int b = r8(c, c->pc + 1);
	switch (op) {
		case 0x00: printf("[%02X] BRK IMP\n", op); break;
		case 0x01: printf("[%02X] ORA ZIX  %02X\n", op, a); break;
		case 0x05: printf("[%02X] ORA ZP   %02X\n", op, a); break;
		case 0x06: printf("[%02X] ASL ZP   %02X\n", op, a); break;
		case 0x08: printf("[%02X] PHP IMP\n", op); break;
		case 0x09: printf("[%02X] ORA IMM  %02X\n", op, a); break;
		case 0x0a: printf("[%02X] ASL ACC\n", op); break;
		case 0x0d: printf("[%02X] ORA ABS  %02X %02X\n", op, a, b); break;
		case 0x0e: printf("[%02X] ASL ABS  %02X %02X\n", op, a, b); break;
		case 0x10: printf("[%02X] BPL REL  %02X\n", op, a); break;
		case 0x11: printf("[%02X] ORA ZIY  %02X\n", op, a); break;
		case 0x15: printf("[%02X] ORA ZPX  %02X\n", op, a); break;
		case 0x16: printf("[%02X] ASL ZPX  %02X\n", op, a); break;
		case 0x18: printf("[%02X] CLC IMP\n", op); break;
		case 0x19: printf("[%02X] ORA AY   %02X %02X\n", op, a, b); break;
		case 0x1a: printf("[%02X] INC ACC\n", op); break;
		case 0x1d: printf("[%02X] ORA AX   %02X %02X\n", op, a, b); break;
		case 0x1e: printf("[%02X] ASL AX   %02X %02X\n", op, a, b); break;
		case 0x20: printf("[%02X] JSR ABS  %02X %02X\n", op, a, b); break;
		case 0x21: printf("[%02X] AND ZIX  %02X\n", op, a); break;
		case 0x24: printf("[%02X] BIT ZP   %02X\n", op, a); break;
		case 0x25: printf("[%02X] AND ZP   %02X\n", op, a); break;
		case 0x26: printf("[%02X] ROL ZP   %02X\n", op, a); break;
		case 0x28: printf("[%02X] PLP IMP\n", op); break;
		case 0x29: printf("[%02X] AND IMM  %02X\n", op, a); break;
		case 0x2a: printf("[%02X] ROL ACC\n", op); break;
		case 0x2c: printf("[%02X] BIT ABS  %02X %02X\n", op, a, b); break;
		case 0x2d: printf("[%02X] AND ABS  %02X %02X\n", op, a, b); break;
		case 0x2e: printf("[%02X] ROL ABS  %02X %02X\n", op, a, b); break;
		case 0x30: printf("[%02X] BMI REL  %02X\n", op, a); break;
		case 0x31: printf("[%02X] AND ZIY  %02X\n", op, a); break;
		case 0x34: printf("[%02X] BIT ZPX  %02X\n", op, a); break;
		case 0x35: printf("[%02X] AND ZPX  %02X\n", op, a); break;
		case 0x36: printf("[%02X] ROL ZPX  %02X\n", op, a); break;
		case 0x38: printf("[%02X] SEC IMP\n", op); break;
		case 0x39: printf("[%02X] AND AY   %02X %02X\n", op, a, b); break;
		case 0x3a: printf("[%02X] DEC ACC\n", op); break;
		case 0x3c: printf("[%02X] BIT AX   %02X %02X\n", op, a, b); break;
		case 0x3d: printf("[%02X] AND AX   %02X %02X\n", op, a, b); break;
		case 0x3e: printf("[%02X] ROL AX   %02X %02X\n", op, a, b); break;
		case 0x40: printf("[%02X] RTI IMP\n", op); break;
		case 0x41: printf("[%02X] EOR ZIX  %02X\n", op, a); break;
		case 0x45: printf("[%02X] EOR ZP   %02X\n", op, a); break;
		case 0x46: printf("[%02X] LSR ZP   %02X\n", op, a); break;
		case 0x48: printf("[%02X] PHA IMP\n", op); break;
		case 0x49: printf("[%02X] EOR IMM  %02X\n", op, a); break;
		case 0x4a: printf("[%02X] LSR ACC\n", op); break;
		case 0x4c: printf("[%02X] JMP ABS  %02X %02X\n", op, a, b); break;
		case 0x4d: printf("[%02X] EOR ABS  %02X %02X\n", op, a, b); break;
		case 0x4e: printf("[%02X] LSR ABS  %02X %02X\n", op, a, b); break;
		case 0x50: printf("[%02X] BVC REL  %02X\n", op, a); break;
		case 0x51: printf("[%02X] EOR ZIY  %02X\n", op, a); break;
		case 0x55: printf("[%02X] EOR ZPX  %02X\n", op, a); break;
		case 0x56: printf("[%02X] LSR ZPX  %02X\n", op, a); break;
		case 0x58: printf("[%02X] CLI IMP\n", op); break;
		case 0x59: printf("[%02X] EOR AY   %02X %02X\n", op, a, b); break;
		case 0x5d: printf("[%02X] EOR AX   %02X %02X\n", op, a, b); break;
		case 0x5e: printf("[%02X] LSR AX   %02X %02X\n", op, a, b); break;
		case 0x60: printf("[%02X] RTS IMP\n", op); break;
		case 0x61: printf("[%02X] ADC ZIX  %02X\n", op, a); break;
		case 0x65: printf("[%02X] ADC ZP   %02X\n", op, a); break;
		case 0x66: printf("[%02X] ROR ZP   %02X\n", op, a); break;
		case 0x68: printf("[%02X] PLA IMP\n", op); break;
		case 0x69: printf("[%02X] ADC IMM  %02X\n", op, a); break;
		case 0x6a: printf("[%02X] ROR ACC\n", op); break;
		case 0x6c: printf("[%02X] JMP IND  %02X %02X\n", op, a, b); break;
		case 0x6d: printf("[%02X] ADC ABS  %02X %02X\n", op, a, b); break;
		case 0x6e: printf("[%02X] ROR AX   %02X %02X\n", op, a, b); break;
		case 0x70: printf("[%02X] BVS REL  %02X\n", op, a); break;
		case 0x71: printf("[%02X] ADC ZIY  %02X\n", op, a); break;
		case 0x75: printf("[%02X] ADC ZPX  %02X\n", op, a); break;
		case 0x76: printf("[%02X] ROR ZPX  %02X\n", op, a); break;
		case 0x78: printf("[%02X] SEI IMP\n", op); break;
		case 0x79: printf("[%02X] ADC AY   %02X %02X\n", op, a, b); break;
		case 0x7c: printf("[%02X] JMP AX   %02X %02X\n", op, a, b); break;
		case 0x7d: printf("[%02X] ADC AX   %02X %02X\n", op, a, b); break;
		case 0x7e: printf("[%02X] ROR ABS  %02X %02X\n", op, a, b); break;
		case 0x81: printf("[%02X] STA ZIX  %02X\n", op, a); break;
		case 0x84: printf("[%02X] STY ZP   %02X\n", op, a); break;
		case 0x85: printf("[%02X] STA ZP   %02X\n", op, a); break;
		case 0x86: printf("[%02X] STX ZP   %02X\n", op, a); break;
		case 0x88: printf("[%02X] DEY IMP\n", op); break;
		case 0x89: printf("[%02X] BIT IMM  %02X\n", op, a); break;
		case 0x8a: printf("[%02X] TXA IMP\n", op); break;
		case 0x8c: printf("[%02X] STY ABS  %02X %02X\n", op, a, b); break;
		case 0x8d: printf("[%02X] STA ABS  %02X %02X\n", op, a, b); break;
		case 0x8e: printf("[%02X] STX ABS  %02X %02X\n", op, a, b); break;
		case 0x90: printf("[%02X] BCC REL  %02X\n", op, a); break;
		case 0x91: printf("[%02X] STA ZIY  %02X\n", op, a); break;
		case 0x94: printf("[%02X] STY ZPX  %02X\n", op, a); break;
		case 0x95: printf("[%02X] STA ZPX  %02X\n", op, a); break;
		case 0x96: printf("[%02X] STX ZPY  %02X\n", op, a); break;
		case 0x98: printf("[%02X] TYA IMP\n", op); break;
		case 0x99: printf("[%02X] STA AY   %02X %02X\n", op, a, b); break;
		case 0x9a: printf("[%02X] TXS IMP\n", op); break;
		case 0x9d: printf("[%02X] STA AX   %02X %02X\n", op, a, b); break;
		case 0xB0: printf("[%02X] BCS REL  %02X\n", op, a); break;
		case 0xD0: printf("[%02X] BNE REL  %02X\n", op, a); break;
		case 0xF0: printf("[%02X] BEQ REL  %02X\n", op, a); break;
		case 0xa0: printf("[%02X] LDY IMM  %02X\n", op, a); break;
		case 0xa1: printf("[%02X] LDA ZIX  %02X\n", op, a); break;
		case 0xa2: printf("[%02X] LDX IMM  %02X\n", op, a); break;
		case 0xa4: printf("[%02X] LDY ZP   %02X\n", op, a); break;
		case 0xa5: printf("[%02X] LDA ZP   %02X\n", op, a); break;
		case 0xa6: printf("[%02X] LDX ZP   %02X\n", op, a); break;
		case 0xa8: printf("[%02X] TAY  IMP\n", op); break;
		case 0xa9: printf("[%02X] LDA IMM  %02X\n", op, a); break;
		case 0xaa: printf("[%02X] TAX  IMP\n", op); break;
		case 0xac: printf("[%02X] LDY ABS  %02X %02X\n", op, a, b); break;
		case 0xad: printf("[%02X] LDA ABS  %02X %02X\n", op, a, b); break;
		case 0xae: printf("[%02X] LDX ABS  %02X %02X\n", op, a, b); break;
		case 0xb1: printf("[%02X] LDA ZIY  %02X\n", op, a); break;
		case 0xb4: printf("[%02X] LDY ZPX  %02X\n", op, a); break;
		case 0xb5: printf("[%02X] LDA ZPX  %02X\n", op, a); break;
		case 0xb6: printf("[%02X] LDX ZPY  %02X\n", op, a); break;
		case 0xb8: printf("[%02X] CLV IMP\n", op); break;
		case 0xb9: printf("[%02X] LDA AY   %02X %02X\n", op, a, b); break;
		case 0xba: printf("[%02X] TSX IMP\n", op); break;
		case 0xbc: printf("[%02X] LDY AX   %02X %02X\n", op, a, b); break;
		case 0xbd: printf("[%02X] LDA AX   %02X %02X\n", op, a, b); break;
		case 0xbe: printf("[%02X] LDX AY   %02X %02X\n", op, a, b); break;
		case 0xc0: printf("[%02X] CPY IMM  %02X\n", op, a); break;
		case 0xc1: printf("[%02X] CMP ZIX  %02X\n", op, a); break;
		case 0xc4: printf("[%02X] CPY ZP   %02X\n", op, a); break;
		case 0xc5: printf("[%02X] CMP ZP   %02X\n", op, a); break;
		case 0xc6: printf("[%02X] DEC ZP   %02X\n", op, a); break;
		case 0xc8: printf("[%02X] INY IMP\n", op); break;
		case 0xc9: printf("[%02X] CMP IMM  %02X\n", op, a); break;
		case 0xca: printf("[%02X] DEX IMP\n", op); break;
		case 0xcc: printf("[%02X] CPY ABS  %02X %02X\n", op, a, b); break;
		case 0xcd: printf("[%02X] CMP ABS  %02X %02X\n", op, a, b); break;
		case 0xce: printf("[%02X] DEC ABS  %02X %02X\n", op, a, b); break;
		case 0xd1: printf("[%02X] CMP ZIY  %02X\n", op, a); break;
		case 0xd5: printf("[%02X] CMP ZPX  %02X\n", op, a); break;
		case 0xd6: printf("[%02X] DEC ZPX  %02X\n", op, a); break;
		case 0xd8: printf("[%02X] CLD IMP\n", op); break;
		case 0xd9: printf("[%02X] CMP AY   %02X %02X\n", op, a, b); break;
		case 0xdd: printf("[%02X] CMP AX   %02X %02X\n", op, a, b); break;
		case 0xde: printf("[%02X] DEC AX   %02X %02X\n", op, a, b); break;
		case 0xe0: printf("[%02X] CPX IMM  %02X\n", op, a); break;
		case 0xe1: printf("[%02X] SBC ZIX  %02X\n", op, a); break;
		case 0xe4: printf("[%02X] CPX ZP   %02X\n", op, a); break;
		case 0xe5: printf("[%02X] SBC ZP   %02X\n", op, a); break;
		case 0xe6: printf("[%02X] INC ZP   %02X\n", op, a); break;
		case 0xe8: printf("[%02X] INX IMP\n", op); break;
		case 0xe9: printf("[%02X] SBC IMM  %02X\n", op, a); break;
		case 0xea: printf("[%02X] NOP IMP\n", op); break;
		case 0xec: printf("[%02X] CPX ABS  %02X %02X\n", op, a, b); break;
		case 0xed: printf("[%02X] SBC ABS  %02X %02X\n", op, a, b); break;
		case 0xee: printf("[%02X] INC ABS  %02X %02X\n", op, a, b); break;
		case 0xf1: printf("[%02X] SBC ZIY  %02X\n", op, a); break;
		case 0xf5: printf("[%02X] SBC ZPX  %02X\n", op, a); break;
		case 0xf6: printf("[%02X] INC ZPX  %02X\n", op, a); break;
		case 0xf8: printf("[%02X] SED IMP\n", op); break;
		case 0xf9: printf("[%02X] SBC AY   %02X %02X\n", op, a, b); break;
		case 0xfd: printf("[%02X] SBC AX   %02X %02X\n", op, a, b); break;
		case 0xfe: printf("[%02X] INC AX   %02X %02X\n", op, a, b); break;
	}
	return op;
}
#endif /* BSOZDBG_H */
