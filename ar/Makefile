CC=gcc
CFLAGS=-O2 -g -Wall
CPPFLAGS=-I../include

AROBJS=	ar.o
LIBOBJS=../lib/pdp10-stdio.o

ar:	$(AROBJS) $(LIBOBJS)
	$(LINK.c) -o $@ $^

ar.o:	ar.c ../include/pdp10-ar.h ../include/pdp10-inttypes.h ../include/pdp10-stdint.h ../include/pdp10-stdio.h

clean:
	rm -f $(AROBJS) ar a.out core.*
