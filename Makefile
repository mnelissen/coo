CFLAGS=-Wall -g -fno-diagnostics-show-caret
.SUFFIXES:
coo: main.o hash.o hasho.o
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
clean:
	rm -f *.o
