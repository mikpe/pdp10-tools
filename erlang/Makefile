# Makefile for pdp10-tools/erlang
# Copyright (C) 2018-2025  Mikael Pettersson
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

SHELL=/bin/bash
REBAR3=$(shell type -p rebar3 || echo ./rebar3)
REBAR3_GIT=https://github.com/erlang/rebar3.git
REBAR3_VSN=3.25.0

PROGRAMS=8to9 ar as elf2boot ld nm od readelf sim

default: compile link

compile:	$(REBAR3)
	$(REBAR3) do compile, xref, dialyzer

link:	$(REBAR3)
	for PROG in $(PROGRAMS); do \
		rm -f _build/$$PROG; \
		ln -s default _build/$$PROG; \
		$(REBAR3) as $$PROG escriptize; \
	done

test:
	$(REBAR3) eunit

install:
	mkdir -p ../bin
	for PROG in $(PROGRAMS); do \
		cp _build/default/bin/$$PROG ../bin/pdp10-elf-$$PROG; \
	done
	ln -sf pdp10-elf-ar ../bin/pdp10-elf-ranlib

distclean realclean:	clean
	rm -f ./rebar3

clean:	$(REBAR3)
	$(REBAR3) clean
	rm -rf _build

./rebar3:
	mkdir -p _build; \
	cd _build; \
	git clone --quiet $(REBAR3_GIT); \
	cd rebar3; \
	git checkout --quiet $(REBAR3_VSN); \
	./bootstrap; \
	mv rebar3 ../../; \
	cd ../..; \
	rm -rf _build/rebar3
