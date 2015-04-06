# as/Makefile
# Copyright (C) 2013-2015  Mikael Pettersson
#
# This file is part of pdp10-tools.
#
# pdp10-tools is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pdp10-tools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with pdp10-tools.  If not, see <http://www.gnu.org/licenses/>.

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
