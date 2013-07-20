CC=gcc
CFLAGS=-O2 -g -Wall
CPPFLAGS=-I../include

OBJS=	arrlst.o charbuf.o emalloc.o htab.o main.o parse.o pass1.o scan.o section.o strtab.o token.o

all:	$(OBJS)

arrlst.o:	arrlst.h emalloc.h
charbuf.o:	charbuf.h emalloc.h
emalloc.o:	emalloc.h
htab.o:		emalloc.h htab.h
main.o:		pass1.h
parse.o:	emalloc.h parse.h scan.h token.h token.def stmt.h expr.h strtab.h section.h ../include/pdp10-stdint.h ../include/pdp10-elf36.h
pass1.o:	parse.h pass1.h scan.h section.h stmt.h expr.h token.h ../include/pdp10-elf36.h arrlst.h htab.h strtab.h ../include/pdp10-stdint.h
scan.o:		charbuf.h scan.h token.h token.def ../include/pdp10-stdint.h
section.o:	emalloc.h htab.h section.h arrlst.h stmt.h expr.h strtab.h ../include/pdp10-elf36.h ../include/pdp10-stdint.h
strtab.o:	emalloc.h htab.h strtab.h
token.o:	../include/pdp10-inttypes.h token.h token.def

clean:
	rm -f $(OBJS) a.out core core.*
