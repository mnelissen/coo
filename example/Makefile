.SUFFIXES:
SUFFIXES:=
CFLAGS=-Iinc -Wall -O2 -g
CC=gcc
FAILTESTS=$(patsubst %.coo,%.coo.c,$(wildcard fail*.coo))
all: example $(FAILTESTS)
example: example.coo.c
	$(CC) $(CFLAGS) -o $@ $<
example.coo.c: example.coo
	../coo $(EXTRA) -Iinc $<
fail%.coo.c: fail%.coo
	@../coo $< 2>&1 | diff -u fail$*.exp -
.PHONY: example.coo.c
