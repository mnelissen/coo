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
all: coo coo_opt coo_cov coortl.o coortl.oo
coo: main.o hash.o hasho.o
	gcc $(LDFLAGS) -o $@ $^
coo_opt: main.oo hash.oo hasho.oo
	gcc $(LDFLAGS) -o $@ $^
coo_cov: main.oc hash.oc hasho.oc
	gcc $(LDFLAGS) --coverage -o $@ $^
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
testxlist: testxlist.o
	gcc $(LDFLAGS) -o $@ $^
testxlist.o testxlist.oo: testxlist.c list.h
%.o: %.c $(wildcard %.h)
	gcc $(CFLAGS) -c -o $@ $<
%.oo: %.c $(wildcard %.h)
	gcc $(CFLAGS) -Os -c -o $@ $<
%.oc: %.c $(wildcard %.h)
	gcc $(CFLAGS) --coverage -c -o $@ $<
clean:
	rm -f *.o *.oo *.oc *.gcda *.gcno coo coo_opt coo_cov testxlist testcpp testc
