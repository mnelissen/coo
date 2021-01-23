.SUFFIXES:
SUFFIXES:=
%: %,v
%: RCS/%
%: RCS/%,v
%: s.%
%: SCCS/s.%
%.c: %.w %.ch
%.tex: %.w %.ch
CC=gcc
CXX=g++
WCC32=i686-w64-mingw32-gcc
WCC=x86_64-w64-mingw32-gcc-win32
CFLAGS=-Wall -Wextra -Werror -g
all: coo coo_opt coo_cov coortl.o coortl.oo
win: coo.exe
debug: coo coortl.o
coo: main.o hash.o hasho.o
	$(CC) $(LDFLAGS) -o $@ $^
coo_opt: main.oo hash.oo hasho.oo
	$(CC) $(LDFLAGS) -o $@ $^
coo_cov: main.oc hash.oc hasho.oc
	$(CC) $(LDFLAGS) --coverage -o $@ $^
coo.exe: main.ow hash.ow hasho.ow
	$(WCC) $(LDFLAGS) -o $@ $^
t: testc
testc: testc.o
	$(CC) $(LDFLAGS) -o $@ $^
tp: testcpp
testcpp: testcpp.o
	$(CXX) $(LDFLAGS) -o $@ $^
testc.o: testc.c
	$(CC) $(CFLAGS) -ansi -pedantic -c -o $@ $<
testcpp.o: testcpp.cpp
	$(CXX) $(CFLAGS) -ansi -pedantic -c -o $@ $<
testxlist: testxlist.o
	$(CC) $(LDFLAGS) -o $@ $^
testxlist.o testxlist.oo: testxlist.c list.h
%.o: %.c $(wildcard %.h)
	$(CC) $(CFLAGS) -c -o $@ $<
%.oo: %.c $(wildcard %.h)
	$(CC) $(CFLAGS) -Os -c -o $@ $<
%.ow: %.c $(wildcard %.h)
	$(WCC) $(CFLAGS) -O2 -c -o $@ $<	
%.oc: %.c $(wildcard %.h)
	@rm -f $*.gcno $*.gcda
	gcc $(CFLAGS) --coverage -c -o $@ $<
clean:
	rm -f *.o *.oo *.oc *.ow *.gcda *.gcno coo coo_opt coo_cov testxlist testcpp testc
