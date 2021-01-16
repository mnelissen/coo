.SUFFIXES:
SUFFIXES:=
# cancel some more implicit rules
%: %,v
%: s.%
%: RCS/%
%: RCS/%,v
%: SCCS/s.%
COO=../coo_opt
COO_COV=../coo_cov
CFLAGS=-Iinc -I.. -Wall -Wextra -Wno-unused-parameter -Werror -g
CC=gcc
TESTS:=$(patsubst %.coo,%_exe,$(wildcard good*.coo))
TESTS+=$(patsubst %.coo,%_dbg,$(wildcard good*.coo))
TESTS+=$(patsubst %.coo,%_run,$(wildcard good*.coo))
TESTS+=$(patsubst %.coo,%.coo.c,$(wildcard good*.coo))
TESTS_COV=$(patsubst %.coo,%.cov.c,$(wildcard good*.coo)) example.cov.c
TESTS_FAIL:=$(patsubst %.coo,%_run,$(wildcard fail*.coo))
all: example $(TESTS) report/index.html
clean:
	rm -f example $(TESTS)
example: example.coo.c
	$(CC) $(CFLAGS) -o $@ $<
example.coo.c: example.coo $(COO)
	$(COO) -Iinc $<
example.cov.c: example.coo $(COO_COV)
	$(COO_COV) -xs.cov.c -Iinc $<
coortlopt.o: ../coortl.c
	$(CC) $(CFLAGS) -O2 -c -o $@ $<
coortldbg.o: ../coortl.c
	$(CC) $(CFLAGS) -c -o $@ $<
fail%_run: fail%.coo fail%.exp $(COO)
	$(COO_COV) -m999 $< 2>&1 | cut -f4- -d: | diff -u fail$*.exp -
good%.coo.c: good%.coo good%.exp $(COO)
	$(COO) $< 2>&1 && touch good$*.coo.c
	diff -u good$*.exp good$*.coo.c
good%_exe: good%.coo.c coortlopt.o
	$(CC) $(CFLAGS) -O2 -o $@ $^
good%_dbg: good%.coo.c coortldbg.o
	$(CC) $(CFLAGS) -o $@ $^
good%_run: good%_exe good%.ref
	./$< | diff -u good$*.ref -
%.cov.c: %.coo $(COO_COV)
	$(COO_COV) -xs.cov.c $< 2>&1
report/index.html: report $(TESTS_COV) $(TESTS_FAIL)
	$(COO_COV) -h 2>/dev/null || true
	lcov -c -q -d .. --output-file report/coverage.info
	genhtml -q report/coverage.info --output-directory report
report:
	@mkdir report
cov:
	rm -rf report *.cov.c ../*.gcda
	@$(MAKE) --no-print-dir report/index.html
