CFLAGS=-Iinc -Wall -O2 -g
CC=gcc
all: example
example: example.coo.o
	$(CC) $(LDFLAGS) -o $@ $<
example.coo.c:
	../coo $(EXTRA) -Iinc example.coo
.PHONY: example.coo.c
