CFLAGS=-Wall -g -fno-diagnostics-show-caret
.SUFFIXES:
coo: main.o
	gcc $(LDFLAGS) -o $@ $^
t: testc.o
%.o: %.c
	gcc $(CFLAGS) -c -o $@ $<
clean:
	rm -f *.o
