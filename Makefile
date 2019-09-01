.SUFFIXES:
SUFFIXES:=
%: %,v
%: RCS/%
%: RCS/%,v
%: s.%
%: SCCS/s.%
%.c: %.w %.ch
%.tex: %.w %.ch
CFLAGS=-Wall -Wextra -g
all: coo coo_opt coortl.o coortl.oo
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
%.o: %.c $(wildcard %.h)
	gcc $(CFLAGS) -c -o $@ $<
%.oo: %.c $(wildcard %.h)
	gcc $(CFLAGS) -Os -c -o $@ $<
clean:
	rm -f *.o
