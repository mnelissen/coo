.SUFFIXES:
SUFFIXES:=
CFLAGS=-Wall -Wextra -g
all: coo coortl.o coortl.oo
coo: main.o hash.o hasho.o
	gcc $(LDFLAGS) -o $@ $^
coo_opt: main.oo hash.oo hasho.oo
	gcc $(LDFLAGS) -o $@ $^
t: testc
testc: testc.o
	gcc $(LDFLAGS) -o $@ $^
tp: testcpp
testcpp: testcpp.o
	g++ $(LDFLAGS) -o $@ $^
testc.o: testc.c
	gcc $(CFLAGS) -ansi -pedantic -c -o $@ $<
testcpp.o: testcpp.cpp
	g++ $(CFLAGS) -ansi -pedantic -c -o $@ $<
%.o: %.c %.h
	gcc $(CFLAGS) -c -o $@ $<
%.oo: %.c %.h
	gcc $(CFLAGS) -O2 -c -o $@ $<
clean:
	rm -f *.o
