all:
	make -C test
	make -C apple1

fmt:
	clang-format -i $(wildcard *.c *.c test/*.c test/*.h apple1/*.c apple1/*.h)
