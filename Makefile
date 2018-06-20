CFLAGS=-Wall -g -fno-diagnostics-show-caret
.SUFFIXES:
coo: main.o hash.o
	gcc $(LDFLAGS) -o $@ $^
t: testc.o
testc.o: testc.c
	gcc $(CFLAGS) -ansi -pedantic -c -o $@ $<
%.o: %.c
	gcc $(CFLAGS) -c -o $@ $<
clean:
	rm -f *.o
