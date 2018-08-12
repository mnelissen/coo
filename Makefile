CFLAGS=-Wall -O2 -g -fno-diagnostics-show-caret
.SUFFIXES:
coo: main.o hash.o hasho.o
	gcc $(LDFLAGS) -o $@ $^
t: testc.o
tp: testcpp.o
testc.o: testc.c
	gcc $(CFLAGS) -ansi -pedantic -c -o $@ $<
testcpp.o: testcpp.cpp
	g++ $(CFLAGS) -ansi -pedantic -c -o $@ $<
%.o: %.c
	gcc $(CFLAGS) -c -o $@ $<
clean:
	rm -f *.o
