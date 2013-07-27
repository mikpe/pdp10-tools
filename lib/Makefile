CC=gcc
CFLAGS=-O2 -g -Wall
CPPFLAGS=-I../include

LIBOBJS=pdp10-elf36.o pdp10-extint.o pdp10-opcodes.o pdp10-stdio.o

all:	$(LIBOBJS)

pdp10-elf36.o:	pdp10-elf36.c ../include/pdp10-elf36.h ../include/pdp10-extint.h ../include/pdp10-stdint.h ../include/pdp10-stdio.h
pdp10-extint.o:	pdp10-extint.c ../include/pdp10-extint.h ../include/pdp10-stdint.h
pdp10-opcode.o:	pdp10-opcode.c ../include/pdp10-opcodes.h
pdp10-stdio.o:	pdp10-stdio.c ../include/pdp10-stdio.h

clean:
	rm -f $(LIBOBJS) a.out core.*
