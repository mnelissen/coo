.SUFFIXES:
SUFFIXES:=
CFLAGS=-Wall -Wextra -g -fno-diagnostics-show-caret
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
	gcc $(CFLAGS) -c -o $@ $<
testcpp.o: testcpp.cpp
	g++ $(CFLAGS) -ansi -pedantic -c -o $@ $<
%.o: %.c
	gcc $(CFLAGS) -c -o $@ $<
%.oo: %.c
	gcc $(CFLAGS) -O2 -c -o $@ $<
clean:
	rm -f *.o
