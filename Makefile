all:
	make -C test
	make -C apple1
	make -C kim1

fmt:
	clang-format -i $(wildcard *.c *.c test/*.c test/*.h apple1/*.c apple1/*.h kim1/*.c kim1/*.h)
