#define BSOZ_DEBUG 0
#include "bsoz.h"

#include <stdio.h>
#include <stdlib.h>

uint8_t mem[0x10000];
uint8_t r8(struct cpu *c, uint16_t addr) { (void)c; return mem[addr]; }
uint8_t w8(struct cpu *c, uint16_t addr, uint8_t b) { (void)c; return mem[addr] = b; }

int test_functional(void) {
	FILE *f;
	struct cpu cpu = {0};
	int lastpc = 0;

	rst(&cpu);
	cpu.pc = 0x400;

	f = fopen("test/6502test.bin", "rb");
	fread(mem, 0x10000, 1, f);
	fclose(f);

	for (;;) {
		int op;
		if ((op = step(&cpu)) < 0) {
			printf("unknown op: 0x%02x, pc=0x%04x\n", op, cpu.pc);
			return 1;
		} if (cpu.pc == lastpc) {
			if (lastpc == 0x3469) {
				printf("SUCCESS!\n");
				return 0;
			}
			printf("trap at pc=0x%04x\n", lastpc);
			return 1;
		}
		lastpc = cpu.pc;
	}
	return 0;
}

int main(void) {
	return test_functional();
}
