#ifndef BSOZ_H
#define BSOZ_H

#include <stdint.h> /* for uint8_t and uint16_t */

struct cpu {
	uint8_t a, x, y, s, f;
	uint16_t pc;
};

uint8_t r8(struct cpu *cpu, uint16_t addr);
uint8_t w8(struct cpu *cpu, uint16_t addr, uint8_t b);

uint8_t fetch(struct cpu *cpu) { return r8(cpu, cpu->pc++); }

uint16_t r16(struct cpu *cpu, uint16_t addr) { return r8(cpu, addr) | ((uint16_t) r8(cpu, addr + 1) << 8); }
uint16_t rb16(struct cpu *cpu, uint16_t addr) {
	return (r8(cpu, (addr & 0xff00) | ((addr + 1) & 0xff)) << 8) | r8(cpu, addr);
}

uint8_t push(struct cpu *cpu, uint8_t b) { return w8(cpu, 0x100 + cpu->s--, b); }
uint8_t pop(struct cpu *cpu) { return r8(cpu, 0x100 + (++cpu->s)); }

void pushw(struct cpu *cpu, uint16_t w) { push(cpu, w >> 8); push(cpu, w & 0xff); }
uint16_t popw(struct cpu *cpu) { return pop(cpu) | pop(cpu) << 8; }

#define IRQ_VEC 0xfffe
#define NMI_VEC 0xfffa

void rst(struct cpu *cpu) {
	cpu->pc = r16(cpu, 0xfffc);
	cpu->a = cpu->x = cpu->y = 0;
	cpu->s = 0xfd;
	cpu->f = 0x20;
}

void irq(struct cpu *cpu, uint16_t vec) {
	pushw(cpu, cpu->pc);
	push(cpu, cpu->f);
	cpu->f = cpu->f | 0x40;
	cpu->pc = r16(cpu, vec);
}

uint8_t zpg(struct cpu *cpu) { return fetch(cpu); }
uint8_t zpx(struct cpu *cpu) { return zpg(cpu) + cpu->x; }
uint8_t zpy(struct cpu *cpu) { return zpg(cpu) + cpu->y; }
uint16_t abt(struct cpu *cpu) { return (uint16_t) zpg(cpu) | ((uint16_t) zpg(cpu) << 8); }
uint16_t abx(struct cpu *cpu) { return abt(cpu) + cpu->x; }
uint16_t aby(struct cpu *cpu) { return abt(cpu) + cpu->y; }
uint16_t inx(struct cpu *cpu) { return rb16(cpu, (fetch(cpu) + cpu->x) & 0xff); }
uint16_t iny(struct cpu *cpu) { return rb16(cpu, fetch(cpu)) + cpu->y; }
uint16_t imm(struct cpu *cpu) { return cpu->pc++; }
int8_t rel(struct cpu *cpu) { return (int8_t) fetch(cpu); }

/* ??? */
/*uint16_t acc(struct cpu *cpu) { return abt(cpu) + cpu->y; }*/

/* NV1BDIZC */
uint8_t nz(struct cpu *cpu, uint8_t x) {
	cpu->f = (cpu->f & 0x7d) | ((x == 0) << 1) | (x&0x80);
	return x;
}

uint8_t inc(struct cpu *cpu, uint16_t addr, uint8_t inc) {
	return w8(cpu, addr, nz(cpu, r8(cpu, addr) + inc));
}
uint8_t cmp(struct cpu *cpu, uint16_t addr, uint8_t v) {
  uint8_t u = r8(cpu, addr);
  cpu->f = (cpu->f & 0xfe) | (v >= u);
  return nz(cpu, v - u);
}
uint8_t shl(struct cpu *cpu, uint8_t b, uint8_t rot) {
  uint8_t r = (b << 1) | ((cpu->f&1) * !!rot);
  cpu->f = (cpu->f & 0xfe) | (b >> 7);
  return nz(cpu, r);
}
uint8_t shr(struct cpu *cpu, uint8_t b, uint8_t rot) {
  uint8_t r = (b >> 1) | (((cpu->f&1) << 7) * !!rot);
  cpu->f = (cpu->f & 0xfe) | (b & 1);
	return nz(cpu, r);
}
void shlm(struct cpu *c, uint16_t addr, uint8_t rot) { w8(c, addr, shl(c, r8(c, addr), rot)); }
void shrm(struct cpu *c, uint16_t addr, uint8_t rot) { w8(c, addr, shr(c, r8(c, addr), rot)); }
void flags(struct cpu *cpu, uint16_t addr) {
	uint8_t v = r8(cpu, addr);
	cpu->f = (cpu->f & 0x3d) | (0xc0 & v) | (((v&cpu->a)==0) << 1);
}
uint8_t bit(struct cpu *cpu, uint8_t and, uint8_t or, uint8_t xor) {
	return cpu->a = nz(cpu, ((cpu->a & and) | or) ^ xor);
}
uint8_t adc(struct cpu *cpu, uint16_t addr, int neg) {
	uint8_t n = r8(cpu, addr);
	if (cpu->f & 0x08) {
		if (neg) {
#if 1
			int nd = ((n >> 4) * 10) + (n & 0x0f) + !(cpu->f & 1);
			int val = ((cpu->a >> 4) * 10) + (cpu->a & 0x0f) - nd; 
			cpu->f = cpu->f | 1;
			if (val < 0) {cpu->f &= 0xfe; val += 100;} 
			cpu->a = nz(cpu, ((val / 10) << 4) + (val % 10));
#endif
		} else {
#if 1
			uint8_t p1 = (cpu->a & 0xf) + (n & 0xf) + (cpu->f&1);
			uint8_t p2 = (p1 >= 10) + (cpu->a >> 4) + (n >> 4); 
			cpu->f = cpu->f & 0xfe;
			if (p1 >= 10) p1 -= 10; 
			if (p2 >= 10) { cpu->f = cpu->f | 1; p2 -= 10;} 
			cpu->a = nz(cpu, (p2 << 4) + p1);
#endif
		}
		return 0;
	} else { 
		uint16_t r = cpu->a + (r8(cpu, addr) ^ (0xff * neg)) + (cpu->f & 0x01);
		int v = (((0xff * !neg)^(cpu->a ^ n)) & (cpu->a ^ r) & 0x80);
		cpu->f = (cpu->f & 0xbe) | ((r & 0x100) >> 8) | (v >> 1);
		return cpu->a = nz(cpu, r & 0xff); 
	} 
}

#if BSOZ_DEBUG
#include "bsozdbg.h"
#else
#define disasm(c, op) (op)
#endif

int step(struct cpu *c) {
	uint8_t op;
	switch (op = disasm(c, fetch(c))) {
		case 0x00:
			pushw(c, c->pc+1);
			push(c, c->f | 0x10);
			c->f |= 4;
			c->pc = r16(c, 0xfffe);
			break; /* BRK,IMP,1,7,cziDBvn */
		case 0xea: break; /* NOP,IMP,1,2,czidbvn */

		case 0x69: adc(c, imm(c), 0); break; /* ADC,IMM,2,2,CZidbVN */
		case 0x65: adc(c, zpg(c), 0); break; /* ADC,ZP,2,3,CZidbVN */
		case 0x75: adc(c, zpx(c), 0); break; /* ADC,ZPX,2,4,CZidbVN */
		case 0x6d: adc(c, abt(c), 0); break; /* ADC,ABS,3,4,CZidbVN */
		case 0x7d: adc(c, abx(c), 0); break; /* ADC,AX,3,4,CZidbVN */
		case 0x79: adc(c, aby(c), 0); break; /* ADC,AY,3,4,CZidbVN */
		case 0x61: adc(c, inx(c), 0); break; /* ADC,ZIX,2,6,CZidbVN */
		case 0x71: adc(c, iny(c), 0); break; /* ADC,ZIY,2,5,CZidbVN */

		case 0xe9: adc(c, imm(c), 1); break; /* SBC,IMM,2,2,CZidbVN */
		case 0xe5: adc(c, zpg(c), 1); break; /* SBC,ZP,2,3,CZidbVN */
		case 0xf5: adc(c, zpx(c), 1); break; /* SBC,ZPX,2,4,CZidbVN */
		case 0xed: adc(c, abt(c), 1); break; /* SBC,ABS,3,4,CZidbVN */
		case 0xfd: adc(c, abx(c), 1); break; /* SBC,AX,3,4,CZidbVN */
		case 0xf9: adc(c, aby(c), 1); break; /* SBC,AY,3,4,CZidbVN */
		case 0xe1: adc(c, inx(c), 1); break; /* SBC,ZIX,2,6,CZidbVN */
		case 0xf1: adc(c, iny(c), 1); break; /* SBC,ZIY,2,5,CZidbVN */

		case 0x29: bit(c, r8(c, imm(c)), 0, 0); break; /* AND,IMM,2,2,cZidbvN */
		case 0x25: bit(c, r8(c, zpg(c)), 0, 0); break; /* AND,ZP,2,3,cZidbvN */
		case 0x35: bit(c, r8(c, zpx(c)), 0, 0); break; /* AND,ZPX,2,4,cZidbvN */
		case 0x2d: bit(c, r8(c, abt(c)), 0, 0); break; /* AND,ABS,3,4,cZidbvN */
		case 0x3d: bit(c, r8(c, abx(c)), 0, 0); break; /* AND,AX,3,4,cZidbvN */
		case 0x39: bit(c, r8(c, aby(c)), 0, 0); break; /* AND,AY,3,4,cZidbvN */
		case 0x21: bit(c, r8(c, inx(c)), 0, 0); break; /* AND,ZIX,2,6,cZidbvN */
		case 0x31: bit(c, r8(c, iny(c)), 0, 0); break; /* AND,ZIY,2,5,cZidbvN */

		case 0x09: bit(c, 0xff, r8(c, imm(c)), 0); break; /* ORA,IMM,2,2,cZidbvN */
		case 0x05: bit(c, 0xff, r8(c, zpg(c)), 0); break; /* ORA,ZP,2,3,cZidbvN */
		case 0x15: bit(c, 0xff, r8(c, zpx(c)), 0); break; /* ORA,ZPX,2,4,cZidbvN */
		case 0x0d: bit(c, 0xff, r8(c, abt(c)), 0); break; /* ORA,ABS,3,4,cZidbvN */
		case 0x1d: bit(c, 0xff, r8(c, abx(c)), 0); break; /* ORA,AX,3,4,cZidbvN */
		case 0x19: bit(c, 0xff, r8(c, aby(c)), 0); break; /* ORA,AY,3,4,cZidbvN */
		case 0x01: bit(c, 0xff, r8(c, inx(c)), 0); break; /* ORA,ZIX,2,6,cZidbvN */
		case 0x11: bit(c, 0xff, r8(c, iny(c)), 0); break; /* ORA,ZIY,2,5,cZidbvN */

		case 0x49: bit(c, 0xff, 0, r8(c, imm(c))); break; /* EOR,IMM,2,2,cZidbvN */
		case 0x45: bit(c, 0xff, 0, r8(c, zpg(c))); break; /* EOR,ZP,2,3,cZidbvN */
		case 0x55: bit(c, 0xff, 0, r8(c, zpx(c))); break; /* EOR,ZPX,2,4,cZidbvN */
		case 0x4d: bit(c, 0xff, 0, r8(c, abt(c))); break; /* EOR,ABS,3,4,cZidbvN */
		case 0x5d: bit(c, 0xff, 0, r8(c, abx(c))); break; /* EOR,AX,3,4,cZidbvN */
		case 0x59: bit(c, 0xff, 0, r8(c, aby(c))); break; /* EOR,AY,3,4,cZidbvN */
		case 0x41: bit(c, 0xff, 0, r8(c, inx(c))); break; /* EOR,ZIX,2,6,cZidbvN */
		case 0x51: bit(c, 0xff, 0, r8(c, iny(c))); break; /* EOR,ZIY,2,5,cZidbvN */

		case 0x0a: c->a = shl(c, c->a, 0); break; /* ASL,ACC,1,2,CZidbvN */
		case 0x06: shlm(c, zpg(c), 0); break; /* ASL,ZP,2,5,CZidbvN */
		case 0x16: shlm(c, zpx(c), 0); break; /* ASL,ZPX,2,6,CZidbvN */
		case 0x0e: shlm(c, abt(c), 0); break; /* ASL,ABS,3,6,CZidbvN */
		case 0x1e: shlm(c, abx(c), 0); break; /* ASL,AX,3,6/7,CZidbvN */

		case 0x4a: c->a = shr(c, c->a, 0); break; /* LSR,ACC,1,2,cZidbvN */
		case 0x46: shrm(c, zpg(c), 0); break; /* LSR,ZP,2,5,cZidbvN */
		case 0x56: shrm(c, zpx(c), 0); break; /* LSR,ZPX,2,6,cZidbvN */
		case 0x4e: shrm(c, abt(c), 0); break; /* LSR,ABS,3,6,cZidbvN */
		case 0x5e: shrm(c, abx(c), 0); break; /* LSR,AX,3,6/7,cZidbvN */

		case 0x2a: c->a = shl(c, c->a, 1); break; /* ROL,ACC,1,2,CZidbvN */
		case 0x26: shlm(c, zpg(c), 1); break; /* ROL,ZP,2,5,CZidbvN */
		case 0x36: shlm(c, zpx(c), 1); break; /* ROL,ZPX,2,6,CZidbvN */
		case 0x2e: shlm(c, abt(c), 1); break; /* ROL,ABS,3,6,CZidbvN */
		case 0x3e: shlm(c, abx(c), 1); break; /* ROL,AX,3,6/7,CZidbvN */
							 
		case 0x6a: c->a = shr(c, c->a, 1); break; /* ROR,ACC,1,2,CZidbvN */
		case 0x66: shrm(c, zpg(c), 1); break; /* ROR,ZP,2,5,CZidbvN */
		case 0x76: shrm(c, zpx(c), 1); break; /* ROR,ZPX,2,6,CZidbvN */
		case 0x7e: shrm(c, abx(c), 1); break; /* ROR,ABS,3,6,CZidbvN */
		case 0x6e: shrm(c, abt(c), 1); break; /* ROR,AX,3,6/7,CZidbvN */

		case 0xca: c->x = nz(c, c->x - 1); break; /* DEX,IMP,1,2,cZidbvN */
		case 0x88: c->y = nz(c, c->y - 1); break; /* DEY,IMP,1,2,cZidbvN */
		case 0xe8: c->x = nz(c, c->x + 1); break; /* INX,IMP,1,2,cZidbvN */
		case 0xc8: c->y = nz(c, c->y + 1); break; /* INY,IMP,1,2,cZidbvN */

		case 0xe6: inc(c, zpg(c), 1); break; /* INC,ZP,2,5,cZidbvN */
		case 0xf6: inc(c, zpx(c), 1); break; /* INC,ZPX,2,6,cZidbvN */
		case 0xee: inc(c, abt(c), 1); break; /* INC,ABS,3,6,cZidbvN */
		case 0xfe: inc(c, abx(c), 1); break; /* INC,AX,3,7,cZidbvN */
		/*case 0x1a: inc(c, acc(c), 1); break; [> INC,ACC,1,2,cZidbvN <]*/

		case 0xc6: inc(c, zpg(c), 0xff); break; /* DEC,ZP,2,5,cZidbvN */
		case 0xd6: inc(c, zpx(c), 0xff); break; /* DEC,ZPX,2,6,cZidbvN */
		case 0xce: inc(c, abt(c), 0xff); break; /* DEC,ABS,3,6,cZidbvN */
		case 0xde: inc(c, abx(c), 0xff); break; /* DEC,AX,3,7,cZidbvN */
		/*case 0x3a: inc(c, acc(c), 0xff); break; [> DEC,ACC,1,2,cZidbvN <]*/

		case 0xc9: cmp(c, imm(c), c->a); break; /* CMP,IMM,2,2,CZidbvN */
		case 0xc5: cmp(c, zpg(c), c->a); break; /* CMP,ZP,2,3,CZidbvN */
		case 0xd5: cmp(c, zpx(c), c->a); break; /* CMP,ZPX,2,4,CZidbvN */
		case 0xcd: cmp(c, abt(c), c->a); break; /* CMP,ABS,3,4,CZidbvN */
		case 0xdd: cmp(c, abx(c), c->a); break; /* CMP,AX,3,4,CZidbvN */
		case 0xd9: cmp(c, aby(c), c->a); break; /* CMP,AY,3,4,CZidbvN */
		case 0xc1: cmp(c, inx(c), c->a); break; /* CMP,ZIX,2,6,CZidbvN */
		case 0xd1: cmp(c, iny(c), c->a); break; /* CMP,ZIY,2,5,CZidbvN */

		case 0xe0: cmp(c, imm(c), c->x); break; /* CPX,IMM,2,2,CZidbvN */
		case 0xe4: cmp(c, zpg(c), c->x); break; /* CPX,ZP,2,3,CZidbvN */
		case 0xec: cmp(c, abt(c), c->x); break; /* CPX,ABS,3,4,CZidbvN */
		case 0xc0: cmp(c, imm(c), c->y); break; /* CPY,IMM,2,2,CZidbvN */
		case 0xc4: cmp(c, zpg(c), c->y); break; /* CPY,ZP,2,3,CZidbvN */
		case 0xcc: cmp(c, abt(c), c->y); break; /* CPY,ABS,3,4,CZidbvN */

		/* Status register (flags) operations */
		case 0x18: c->f = c->f & 0xfe; break; /* CLC,IMP,1,2,Czidbvn */
		case 0xd8: c->f = c->f & 0xf7; break; /* CLD,IMP,1,2,cziDbvn */
		case 0x58: c->f = c->f & 0xfb; break; /* CLI,IMP,1,2,czIdbvn */
		case 0xb8: c->f = c->f & 0xbf; break; /* CLV,IMP,1,2,czidbVn */
		case 0x38: c->f = c->f | 0x01; break; /* SEC,IMP,1,2,Czidbvn */
		case 0xf8: c->f = c->f | 0x08; break; /* SED,IMP,1,2,cziDbvn */
		case 0x78: c->f = c->f | 0x04; break; /* SEI,IMP,1,2,czIdbvn */
		case 0x24: flags(c, zpg(c)); break; /* BIT,ZP,2,3,cZidbVN */
		case 0x2c: flags(c, abt(c)); break; /* BIT,ABS,3,4,cZidbVN */
#if 0
		case 0x89: c->f = (c->f & 0x3d) | (0xc2 & r8(c, imm(c))); break; /* BIT,IMM,2,2,cZidbvn */
		case 0x34: c->f = (c->f & 0x3d) | (0xc2 & r8(c, zpx(c))); break; /* BIT,ZPX,2,4,cZidbVN */
		case 0x3c: c->f = (c->f & 0x3d) | (0xc2 & r8(c, abx(c))); break; /* BIT,AX,3,4,cZidbVN */
#endif

		/* Conditional jumps */
		case 0x90: c->pc += rel(c) * ((c->f & 0x01) == 0); break; /* BCC,REL,2,2/3,czidbvn */
		case 0xB0: c->pc += rel(c) * ((c->f & 0x01) != 0); break; /* BCS,REL,2,2/3,czidbvn */
		case 0xF0: c->pc += rel(c) * ((c->f & 0x02) != 0); break; /* BEQ,REL,2,2/3,czidbvn */
		case 0x30: c->pc += rel(c) * ((c->f & 0x80) != 0); break; /* BMI,REL,2,2/3,czidbvn */
		case 0xD0: c->pc += rel(c) * ((c->f & 0x02) == 0); break; /* BNE,REL,2,2/3,czidbvn */
		case 0x10: c->pc += rel(c) * ((c->f & 0x80) == 0); break; /* BPL,REL,2,2/3,czidbvn */
		case 0x50: c->pc += rel(c) * ((c->f & 0x40) == 0); break; /* BVC,REL,2,2/3,czidbvn */
		case 0x70: c->pc += rel(c) * ((c->f & 0x40) != 0); break; /* BVS,REL,2,2/3,czidbvn */

		/* Other jumps */
		case 0x4c: c->pc = abt(c); break; /* JMP,ABS,3,3,czidbvn */
		case 0x6c: c->pc = rb16(c, abt(c)); break; /* JMP,IND,3,5,czidbvn */
		/*case 0x7c: break; [> JMP,AX,3,6,czidbvn <]*/
		case 0x20: pushw(c, c->pc + 1); c->pc = abt(c); break; /* JSR,ABS,3,6,czidbvn */

		/* Stack operations + returns */
		case 0x48: push(c, c->a); break; /* PHA,IMP,1,3,czidbvn */
		case 0x68: c->a = nz(c, pop(c)); break; /* PLA,IMP,1,4,cZidbvN */
		case 0x08: push(c, c->f | 0x30); break; /* PHP,IMP,1,3,czidbvn */
		case 0x28: c->f = pop(c) | 0x20; break; /* PLP,IMP,1,4,CZIDBVN */
		case 0x40: c->f = pop(c) | 0x20; c->pc = popw(c); break; /* RTI,IMP,1,6,czidbvn */
		case 0x60: c->pc = popw(c) + 1; break; /* RTS,IMP,1,6,czidbvn */

		case 0xaa: c->x = nz(c, c->a); break; /* TAX,IMP,1,2,cZidbvN */
		case 0x8a: c->a = nz(c, c->x); break; /* TXA,IMP,1,2,cZidbvN */
		case 0xa8: c->y = nz(c, c->a); break; /* TAY,IMP,1,2,cZidbvN */
		case 0x98: c->a = nz(c, c->y); break; /* TYA,IMP,1,2,cZidbvN */
		case 0xba: c->x = nz(c, c->s); break; /* TSX,IMP,1,2,cZidbvN */
		case 0x9a: c->s = c->x; break; /* TXS,IMP,1,2,czidbvn */

		case 0xa9: c->a = nz(c, r8(c, imm(c))); break; /* LDA,IMM,2,2,cZidbvN */
		case 0xa5: c->a = nz(c, r8(c, zpg(c))); break; /* LDA,ZP,2,3,cZidbvN */
		case 0xb5: c->a = nz(c, r8(c, zpx(c))); break; /* LDA,ZPX,2,4,cZidbvN */
		case 0xad: c->a = nz(c, r8(c, abt(c))); break; /* LDA,ABS,3,4,cZidbvN */
		case 0xbd: c->a = nz(c, r8(c, abx(c))); break; /* LDA,AX,3,4,cZidbvN */
		case 0xb9: c->a = nz(c, r8(c, aby(c))); break; /* LDA,AY,3,4,cZidbvN */
		case 0xa1: c->a = nz(c, r8(c, inx(c))); break; /* LDA,ZIX,2,6,cZidbvN */
		case 0xb1: c->a = nz(c, r8(c, iny(c))); break; /* LDA,ZIY,2,5,cZidbvN */
		case 0xa2: c->x = nz(c, r8(c, imm(c))); break; /* LDX,IMM,2,2,cZidbvN */
		case 0xa6: c->x = nz(c, r8(c, zpg(c))); break; /* LDX,ZP,2,3,cZidbvN */
		case 0xb6: c->x = nz(c, r8(c, zpy(c))); break; /* LDX,ZPY,2,4,cZidbvN */
		case 0xae: c->x = nz(c, r8(c, abt(c))); break; /* LDX,ABS,3,4,cZidbvN */
		case 0xbe: c->x = nz(c, r8(c, aby(c))); break; /* LDX,AY,3,4,cZidbvN */
		case 0xa0: c->y = nz(c, r8(c, imm(c))); break; /* LDY,IMM,2,2,cZidbvN */
		case 0xa4: c->y = nz(c, r8(c, zpg(c))); break; /* LDY,ZP,2,3,cZidbvN */
		case 0xb4: c->y = nz(c, r8(c, zpx(c))); break; /* LDY,ZPX,2,4,cZidbvN */
		case 0xac: c->y = nz(c, r8(c, abt(c))); break; /* LDY,ABS,3,4,cZidbvN */
		case 0xbc: c->y = nz(c, r8(c, abx(c))); break; /* LDY,AX,3,4,cZidbvN */

		case 0x85: w8(c, zpg(c), c->a); break; /* STA,ZP,2,3,czidbvn */
		case 0x95: w8(c, zpx(c), c->a); break; /* STA,ZPX,2,4,czidbvn */
		case 0x8d: w8(c, abt(c), c->a); break; /* STA,ABS,3,4,czidbvn */
		case 0x9d: w8(c, abx(c), c->a); break; /* STA,AX,3,5,czidbvn */
		case 0x99: w8(c, aby(c), c->a); break; /* STA,AY,3,5,czidbvn */
		case 0x81: w8(c, inx(c), c->a); break; /* STA,ZIX,2,6,czidbvn */
		case 0x91: w8(c, iny(c), c->a); break; /* STA,ZIY,2,6,czidbvn */

		case 0x86: w8(c, zpg(c), c->x); break; /* STX,ZP,2,3,czidbvn */
		case 0x96: w8(c, zpy(c), c->x); break; /* STX,ZPY,2,4,czidbvn */
		case 0x8e: w8(c, abt(c), c->x); break; /* STX,ABS,3,4,czidbvn */

		case 0x84: w8(c, zpg(c), c->y); break; /* STY,ZP,2,3,czidbvn */
		case 0x94: w8(c, zpx(c), c->y); break; /* STY,ZPX,2,4,czidbvn */
		case 0x8c: w8(c, abt(c), c->y); break; /* STY,ABS,3,4,czidbvn */

		default: return -1;
	}
	return op;
}
#endif /* BSOZ_H */
