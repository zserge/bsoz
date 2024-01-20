CFLAGS ?= -Wall -Wextra -pedantic -std=c89

all: test apple1

test:
	$(CC) $(CFLAGS) bsoztest.c -o bsoztest
	./bsoztest

apple1:
	make -C apple1

.PHONY: apple1 test
