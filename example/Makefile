.SUFFIXES:
SUFFIXES:=
# cancel some more implicit rules
%: %,v
%: s.%
%: RCS/%
%: RCS/%,v
%: SCCS/s.%
CFLAGS=-Iinc -I.. -Wall -Wextra -Wno-unused-parameter -g
CC=gcc
TESTS:=$(patsubst %.coo,%.coo.c,$(wildcard fail*.coo))
TESTS:=$(TESTS) $(patsubst %.coo,%_exe,$(wildcard good*.coo))
TESTS:=$(TESTS) $(patsubst %.coo,%_dbg,$(wildcard good*.coo))
TESTS:=$(TESTS) $(patsubst %.coo,%.coo.c,$(wildcard good*.coo))
all: example $(TESTS)
clean:
	rm -f example $(TESTS)
example: example.coo.c
	$(CC) $(CFLAGS) -o $@ $<
example.coo.c: example.coo
	../coo $(EXTRA) -Iinc $<
coortlopt.o: ../coortl.c
	$(CC) $(CFLAGS) -O2 -c -o $@ $<
coortldbg.o: ../coortl.c
	$(CC) $(CFLAGS) -c -o $@ $<
fail%.coo.c: fail%.coo fail%.exp ../coo
	../coo $< 2>&1 | diff -u fail$*.exp -
good%.coo.c: good%.coo good%.exp ../coo
	../coo $< 2>&1
	diff -u good$*.exp good$*.coo.c
good%_exe: good%.coo.c coortlopt.o
	$(CC) $(CFLAGS) -O2 -o $@ $^
good%_dbg: good%.coo.c coortldbg.o
	$(CC) $(CFLAGS) -o $@ $^
.PHONY: example.coo.c
