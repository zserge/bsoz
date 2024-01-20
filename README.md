# bsoz - Bang Some Ones & Zeros

[![Build Status](https://github.com/zserge/bsoz/actions/workflows/ci/badge.svg)](https://github.com/zserge/bsoz/actions)

bsoz is a tiny and versatile emulator suite designed to bring the nostalgic charm of vintage computers to the modern era. It provides accurate emulation for the [MOS 6502 CPU](https://en.wikipedia.org/wiki/MOS_Technology_6502) and currently supports [Apple 1](https://en.wikipedia.org/wiki/Apple_I) computer. There is ongoing development of [KIM-1](https://en.wikipedia.org/wiki/KIM-1) and hopefully [Apple II](https://en.wikipedia.org/wiki/Apple_II) emulators.

## Features

* ~300 lines of "pedantic" C89 code
* No external dependencies
* All documented 6502 instructions supported
* Correct handling of decimal mode (BCD).
* User-supplied memory-mapped I/O callbacks.
* IRQ and NMI signals.
* Multiple CPUs can be emulated (no global state).
* Instruction disassembler.
* Comprehensive test coverage.
* Designed to be cross-platform and should run on macOS, Linux and maybe even Windows.
* A full-functional lo-fi Apple 1 emulator is included!

## Usage

```c
#include "bsoz.h"

uint8_t mem[0x10000];
uint8_t r8(struct cpu *c, uint16_t addr) { (void)c; return mem[addr]; }
uint8_t w8(struct cpu *c, uint16_t addr, uint8_t b) { (void)c; return mem[addr] = b; }

int main() {
  struct cpu cpu;
  rst(&cpu);
  while (step(&cpu) >= 0);
  return 0;
}
```

Include a header, provide `r8` and `w8` callback for memory access, reset the CPU and run it step by step.

## Apple 1

To run Apple 1 emulator:

```
cd apple1
cat ./rom/hello.txt
# "tapes" are raw text input to Apple 1 WozMon monitor.
# This one loads machine code at address 0x0280 and runs is:
280:A2 C BD 8B 2 20 EF FF CA D0 F7 60 8D C4 CC D2 CF D7 A0 CF CC CC C5 C8
280.297
280
R

./apple1 ./rom/hello.txt

\
280:A2 C BD 8B 2 20 EF FF CA D0 F7 60 8D
 C4 CC D2 CF D7 A0 CF CC CC C5 C8

0280: 00
280.297

0280: A2 0C BD 8B 02 20 EF FF
0288: CA D0 F7 60 8D C4 CC D2
0290: CF D7 A0 CF CC CC C5 C8
280

0280: A2
R
HELLO WORLD
```

Alternatively, an "autorun" script may call BASIC and type a program there:

```
cd apple1
cat rom/hello.bas
E000R
PRINT "HELLO WORLD"

\
E000R

E000: 4C
>PRINT "HELLO WORLD"
HELLO WORLD

>
```

Pressing <kbd>Ctlr+R</kbd> resets Apple1 computer and loads the monitor again. However, all the memory contents are preserved and BASIC can be reloaded by typing `E2B3R` in the monitor:

```
./apple1
\
E000R

E000: 4C
>A=1

>PRINT A
1

<press Ctrl+R>
\
E2B3R  # E2B3 is BASIC entry-point that preserves RAM, while E000 clears it
>PRINT A
1
```

To learn more about Apple1 I suggest to read the original manual and endless [forums](https://applefritter.com/), it's quite a rabbit hole!

## License

This project is licensed under the MIT License. Contributions are welcome!
