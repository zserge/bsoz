#include <bsoz.h>
#include <fcntl.h>
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>

#include "rom.h"

uint8_t mem[0x10000];
uint16_t memsz = 8192;
uint8_t ch;

uint8_t r8(struct cpu *c, uint16_t addr) {
  (void)c;
  if (!ch) {
    if (addr == 0xd010) return 0x80;
    if (addr == 0xd011) return 0x00;
  } else {
    if (addr == 0xd011) return 0x80;
    if (addr == 0xd010) {
      uint8_t tchar = ch;
      if (tchar >= 'a' && tchar <= 'z') { tchar = tchar - 'a' + 'A'; }
      if (tchar == '\n') tchar = '\r';
      ch = 0;
      return tchar | 0x80;
    }
  }
  if ((addr & 0xff00) == 0xff00) { return wozmon[addr & 0xff]; }
  if ((addr & 0xe000) == 0xe000) { return a1basic[addr & 0xfff]; }
  if (addr < memsz) { return mem[addr]; }
  return 0;
}

int col = 0;
uint8_t w8(struct cpu *c, uint16_t addr, uint8_t b) {
  (void)c;
  if (addr == 0xd012 || addr == 0xd0f2) {
    char o = b & 0x7f;
    col++;
    if (o == '\r') {
      o = '\n';
      col = 0;
    }
    if (o == 0x5f) o = '\b';
    printf("%c", o);
    if (col == 40) {
      printf("\n");
      col = 0;
    }
    fflush(stdout);
  } else {
    if (addr > memsz) return b;
    mem[addr] = b;
  }
  return b;
}

char *pre = NULL;
size_t presz = 0;
size_t preidx = 0;

void set_raw(void) {
  struct termios term;
  tcgetattr(0, &term);
  term.c_lflag &= ~ICANON;
  term.c_lflag &= ~ECHO;
  term.c_iflag &= ~ICRNL;
  tcsetattr(0, TCSANOW, &term);
  setbuf(stdin, NULL);
}

void reset_term(void) {
  struct termios term;
  tcgetattr(0, &term);
  term.c_lflag |= ICANON;
  term.c_lflag |= ECHO;
  term.c_iflag |= ICRNL;
  tcsetattr(0, TCSANOW, &term);
  setbuf(stdin, NULL);
}

int main(int argc, char *argv[]) {
  int c;
  struct cpu cpu = {0};

  while ((c = getopt(argc, argv, "48")) != -1) {
    switch (c) {
      case '4': memsz = 4096; break;
      case '8': memsz = 8192; break;
      case '?':
      default:  fprintf(stderr, "USAGE: %s [-4|-8] <rom>\n", argv[0]); exit(1);
    }
  }

  if (optind < argc) {
    FILE *f = fopen(argv[optind], "r");
    if (f == NULL) {
      perror(argv[1]);
      return 1;
    }
    fseek(f, 0, SEEK_END);
    presz = ftell(f);
    pre = calloc(1, presz);
    fseek(f, 0, SEEK_SET);
    if (fread(pre, presz, 1, f) < 0) {
      perror(argv[1]);
      return 1;
    }
    fclose(f);
  }

  set_raw();
  atexit(reset_term);
  fcntl(0, F_SETFL, fcntl(0, F_GETFL) | O_NONBLOCK);

  rst(&cpu);
  for (;;) {
    int op;
    if ((op = step(&cpu)) < 0) {
      printf("invalid instruction %02x at pc=%04x\n", op, cpu.pc);
      return 1;
    }
    if (!ch && preidx < presz) ch = pre[preidx++];
    if (!ch && (read(0, &ch, 1) == 1)) {
      if (ch == 0x08 || ch == 0x7f) {
        ch = 0x5F;
      } else if (ch == '') {
        rst(&cpu);
        ch = 0;
      }
    }
  }
  return 0;
}
