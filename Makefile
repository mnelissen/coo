CFLAGS=-Wall -g -fno-diagnostics-show-caret
LANG=coo
.SUFFIXES:
$(LANG): main.o # $(LANG).tab.o $(LANG).lex.o
	gcc $(LDFLAGS) -o $@ $^
main.o: $(LANG).tab.c
t: testc.o
%.o: %.c
	gcc $(CFLAGS) -c -o $@ $<
$(LANG).tab.c: $(LANG).y
	bison -dv $(LANG).y
$(LANG).lex.c: $(LANG).l $(LANG).tab.c
	flex -o $@ $<
clean:
	rm -f *.o $(LANG).tab.* $(LANG).lex.*
