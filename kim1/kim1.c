#define _POSIX_C_SOURCE 199309L

#include <bsoz.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#include "rom.h"

int k = 0x15; /* pending key from hexpad */
int c = -1;   /* pending char from tty */

struct cpu cpu;
uint8_t mem[0x10000];
uint16_t memsz = 1024;

struct rriot {
  uint8_t ram[64];
  uint8_t reg[4]; /* RIOT registers: A, DA, B, DB */
  uint16_t inc;   /* timer multiplier */
  uint8_t irq;    /* timer interrupt flag */
  uint16_t count; /* timer counter */
  uint16_t value; /* timer initial value */
  uint64_t start; /* timer start in ns */
} r2, r3;
uint16_t rriot_inc[4] = {1, 8, 64, 1024};

/* now returns current timestamp in nanoseconds */
uint64_t now(void) {
  struct timespec tv;
  clock_gettime(CLOCK_REALTIME, &tv);
  return tv.tv_sec * 1000000000 + tv.tv_nsec;
}

/* timer starts a RRIOT timer with the given increment and start value */
void timer(struct rriot *r, uint16_t inc, uint8_t val) {
  r->inc = inc;
  r->irq = 0;
  r->count = r->value = val;
  r->start = now();
}

/* rriot updates a RRIOT module on every CPU step */
void rriot(struct rriot *r) {
  uint64_t ms = (now() - r->start) / 1000;
  if (r->inc == 0 || r->irq) return; /* timer is stopped or interrupted */
  if (r->value * r->inc <= ms) {
    r->irq = 0x80;
    r->count = 0;
  } else {
    r->count = r->start - ms;
  }
}

uint8_t r8(struct cpu *cpu, uint16_t addr) {
  (void)cpu;
  if (addr >= 0x1800 && addr <= 0x1bff) {
    return rriot3_rom[addr - 0x1800];
  } else if (addr >= 0x1c00 && addr <= 0x1fff) {
    return rriot2_rom[addr - 0x1c00];
  } else if (addr >= 0xff00) {
    return rriot2_rom[addr - 0xfc00];
  } else if (addr >= 0x1780 && addr <= 0x17bf) {
    return r3.ram[addr - 0x17c0];
  } else if (addr >= 0x17c0 && addr <= 0x17ff) {
    return r2.ram[addr - 0x17c0];
  } else if (addr >= 0x1700 && addr <= 0x173f) {
    /* RRIIOT 6530-003 */
    if (addr >= 0x1700 && addr <= 0x1703) {
      /* read registers */
      return r3.reg[addr - 0x1700];
    } else if (addr == 0x1706 || addr == 0x170e) {
      /* reset timer? */
      if (!r3.irq) return r3.count;
      timer(&r3, r3.inc, r3.value);
      r3.count = 0xff;
      return 0;
    } else if (addr == 0x1707) {
      /* return timer interrupt status */
      return r3.irq;
    }
  } else if (addr >= 0x1740 && addr <= 0x177f) {
    /* RRIIOT 6530-002 */
    if (addr == 0x1740) {
      uint8_t mask[] = {0xbf, 0xdf, 0xef, 0xf7, 0xfb, 0xfd, 0xfe};
      switch ((r2.reg[2] >> 1) & 0xf) {
        case 0: return k < 7 ? mask[k] : 0xff;
        case 1: return (k >= 7 && k < 14) ? mask[k - 7] : 0xff;
        case 2: return (k >= 14 && k < 21) ? mask[k - 14] : 0xff;
        case 3: return 0xff;
      }
      return 0x80;
    } else if (addr >= 0x1741 && addr <= 0x1743) {
      return r2.reg[addr - 0x1740];
    } else if (addr == 0x1746 || addr == 0x174e) {
      if (!r2.irq) return r2.count;
      timer(&r2, r2.inc, r2.value);
      return 0;
    } else if (addr == 0x1747) {
      return r2.irq;
    }
  } else if (addr < memsz) {
    return mem[addr];
  }
  return 0;
}

uint8_t w8(struct cpu *cpu, uint16_t addr, uint8_t b) {
  (void)cpu;
  if (addr >= 0x1780 && addr <= 0x17bf) {
    r3.ram[addr - 0x17c0] = b;
  } else if (addr >= 0x17c0 && addr <= 0x17ff) {
    r2.ram[addr - 0x17c0] = b;
  } else if (addr >= 0x1700 && addr <= 0x173f) {
    /* RRIOT 6530-003 */
    if (addr >= 0x1700 && addr <= 0x1703) {
      r3.reg[addr - 0x1700] = b;
    } else if (addr >= 1704 && addr <= 0x1707) {
      timer(&r3, rriot_inc[addr - 0x1704], b);
    }
  } else if (addr >= 0x1740 && addr <= 0x177f) {
    /* RRIOT 6530-002 */
    if (addr >= 0x1740 && addr <= 0x1743) {
      r2.reg[addr - 0x1740] = b;
      if (addr == 0x1742) { /* TODO: special treatment for tty */
      }
    } else if (addr >= 0x1744 && addr <= 0x1747) {
      timer(&r2, rriot_inc[addr - 0x1744], b);
    }
  } else if (addr < memsz) {
    mem[addr] = b;
  }
  return b;
}

void set_raw(void) {
  struct termios term;
  tcgetattr(0, &term);
  term.c_lflag &= ~(ICANON | ECHO | ICRNL);
  tcsetattr(0, TCSANOW, &term);
  setbuf(stdin, NULL);
}

void reset_term(void) {
  struct termios term;
  tcgetattr(0, &term);
  term.c_lflag |= ICANON | ECHO | ICRNL;
  tcsetattr(0, TCSANOW, &term);
  setbuf(stdin, NULL);
}

/* handle built-in hex keyboard input */
void hexpad(void) {
  if (k == 0x15 && (read(0, &k, 1) == 1)) {
    if (k >= 'A' && k <= 'Z') k = k - 'A' + 'a';

    if (k >= '0' && k <= '9') {
      k = k - '0';
    } else if (k >= 'a' && k <= 'f') {
      k = k - 'a' + 10;
    } else if (k == '[') { /* address mode */
      k = 0x10;
    } else if (k == ']') { /* data mode */
      k = 0x11;
    } else if (k == ' ' || k == '+') { /* next address */
      k = 0x12;
    } else if (k == 'g') { /* Go */
      k = 0x13;
    } else if (k == 'p') { /* PC */
      k = 0x14;
    } else if (k == 'r') { /* Reset */
      rst(&cpu);
      k = 0x15;
    } else if (k == 't') { /* switch to tty mode */
      /* TODO: */
    } else {
      k = 0x15;
    }
  }
}

void tty(void) { /* TODO: serial input and output */
}

/* Render digit b at position n (counted right-to-left) */
void display(int n, uint8_t b) {
  static int leds[6] = {0};
  int i;
  char digits[73]; /* 6 digits x 4 columns x 3 rows + \0 */
  if (leds[n] == b) return;
  leds[n] = b;
  memset(digits, ' ', sizeof(digits));
  digits[71] = digits[71 - 24] = digits[71 - 48] = '\n';
  for (i = 0; i < 6; i++) {
    int d = leds[5 - i];
    digits[4 * i + 1] = ((d & 0x01) ? '_' : ' ');
    digits[4 * i + 24] = ((d & 0x20) ? '|' : ' ');
    digits[4 * i + 25] = ((d & 0x40) ? '_' : ' ');
    digits[4 * i + 26] = ((d & 0x02) ? '|' : ' ');
    digits[4 * i + 48] = ((d & 0x10) ? '|' : ' ');
    digits[4 * i + 49] = ((d & 0x08) ? '_' : ' ');
    digits[4 * i + 50] = ((d & 0x04) ? '|' : ' ');
  }
  digits[72] = '\0';
  puts(digits);
}

int main(int argc, char *argv[]) {
  (void)argc;
  (void)argv;

  set_raw();
  atexit(reset_term);
  fcntl(0, F_SETFL, fcntl(0, F_GETFL) | O_NONBLOCK);

  mem[0x17fa] = mem[0x17fe] = 0;
  mem[0x17fb] = mem[0x17ff] = 0x1c;
  rst(&cpu);
  for (;;) {
    hexpad();
    tty();
    rriot(&r2);
    rriot(&r3);

    if (cpu.pc == 0x1f79 || cpu.pc == 0x1f90) {
      k = 0x15;
    } else if (cpu.pc == 0x1f56) {
      display(9 - (cpu.x >> 1), cpu.a);
      cpu.pc = 0x1f5e;
    }

    if ((step(&cpu)) < 0) { return 1; }
  }
  return 0;
}
