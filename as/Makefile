CC=gcc
CFLAGS=-O2 -g -Wall
CPPFLAGS=-I../include

ASOBJS=assemble.o hashtab.o input.o main.o output.o parse.o scan.o token.o tunit.o
LIBOBJS=../lib/pdp10-elf36.o ../lib/pdp10-extint.o ../lib/pdp10-opcodes.o ../lib/pdp10-stdio.o

as:	$(ASOBJS) $(LIBOBJS)
	$(LINK.c) -o $@ $^

assemble.o:	assemble.h tunit.h hashtab.h
hashtab.o:	hashtab.h
input.o:	hashtab.h input.h parse.h scan.h tunit.h token.h token.def
main.o:		assemble.h input.h output.h tunit.h hashtab.h
output.o:	output.h tunit.h hashtab.h
parse.o:	input.h scan.h token.h tunit.h token.def hashtab.h
scan.o:		scan.h token.h token.def
token.o:	token.h token.def
tunit.o:	hashtab.h tunit.h

clean:
	rm -f $(ASOBJS) as a.out core.*
