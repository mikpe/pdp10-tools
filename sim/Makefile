# sim/Makefile
# Copyright (C) 2018  Mikael Pettersson
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

SIMOBJS=sim.o pdp10-virt-mem.o
LIBOBJS=../lib/pdp10-elf36.o ../lib/pdp10-extint.o ../lib/pdp10-stdio.o

sim:	$(SIMOBJS) $(LIBOBJS)
	$(LINK.c) -o $@ $^

sim.o:	sim.c ../include/pdp10-elf36.h ../include/pdp10-inttypes.h ../include/pdp10-stdint.h ../include/pdp10-stdio.h

clean:
	rm -f $(SIMOBJS) sim a.out core.*
