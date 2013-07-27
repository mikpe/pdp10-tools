CC=gcc
CFLAGS=-O2 -g -Wall
CPPFLAGS=-I../include

READELFOBJS=	readelf.o
LIBOBJS=	../lib/pdp10-elf36.o ../lib/pdp10-extint.o ../lib/pdp10-opcodes.o ../lib/pdp10-stdio.o

readelf:	$(READELFOBJS) $(LIBOBJS)
	$(LINK.c) -o $@ $^

readelf.o:	readelf.c ../include/pdp10-elf36.h ../include/pdp10-inttypes.h ../include/pdp10-opcodes.h ../include/pdp10-stdint.h ../include/pdp10-stdio.h

clean:
	rm -f $(READELFOBJS) readelf a.out core.*
