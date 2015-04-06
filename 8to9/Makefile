# 8to9/Makefile
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

8TO9OBJS=8to9.o
LIBOBJS=../lib/pdp10-stdio.o

8to9:	$(8TO9OBJS) $(LIBOBJS)
	$(LINK.c) -o $@ $^

8to9.o:	8to9.c ../include/pdp10-stdint.h ../include/pdp10-stdio.h

clean:
	rm -f $(8TO9OBJS) 8to9 a.out core.*
