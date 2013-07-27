CC=gcc
CFLAGS=-O2 -g -Wall
CPPFLAGS=-I../include

ODOBJS=	od.o
LIBOBJS=../lib/pdp10-extint.o ../lib/pdp10-stdio.o

od:	$(ODOBJS) $(LIBOBJS)
	$(LINK.c) -o $@ $^

od.o:	od.c ../include/pdp10-extint.h ../include/pdp10-inttypes.h ../include/pdp10-stdint.h ../include/pdp10-stdio.h

clean:
	rm -f $(ODOBJS) a.out core.*
