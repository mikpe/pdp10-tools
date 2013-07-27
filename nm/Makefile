CC=gcc
CFLAGS=-O2 -g -Wall
CPPFLAGS=-I../include

NMOBJS=	nm.o
LIBOBJS=../lib/pdp10-elf36.o ../lib/pdp10-extint.o ../lib/pdp10-stdio.o

nm:	$(NMOBJS) $(LIBOBJS)
	$(LINK.c) -o $@ $^

nm.o:	nm.c ../include/pdp10-elf36.h ../include/pdp10-inttypes.h ../include/pdp10-stdint.h ../include/pdp10-stdio.h

clean:
	rm -f $(NMOBJS) nm a.out core.*
