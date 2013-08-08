CC=gcc
CFLAGS=-O2 -g -Wall
CPPFLAGS=-I../include

8TO9OBJS=8to9.o
LIBOBJS=../lib/pdp10-stdio.o

8to9:	$(8TO9OBJS) $(LIBOBJS)
	$(LINK.c) -o $@ $^

8to9.o:	8to9.c ../include/pdp10-stdint.h ../include/pdp10-stdio.h

clean:
	rm -f $(8TO9OBJS) 8to9 a.out core.*
