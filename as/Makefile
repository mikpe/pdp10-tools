CC=gcc
CFLAGS=-O2 -g -Wall
CPPFLAGS=-I../include

ASOBJS=assemble.o input.o main.o output.o parse.o scan.o token.o
LIBOBJS=../lib/pdp10-elf36.o ../lib/pdp10-extint.o ../lib/pdp10-opcodes.o ../lib/pdp10-stdio.o

as:	$(ASOBJS) $(LIBOBJS)
	$(LINK.c) -o $@ $^

input.o:	input.h parse.h scan.h token.def token.h
main.o:		assemble.h input.h output.h
output.o:	assemble.h output.h
parse.o:	input.h scan.h token.def token.h
scan.o:		scan.h token.def token.h
token.o:	token.def token.h

clean:
	rm -f $(ASOBJS) as a.out core.*
