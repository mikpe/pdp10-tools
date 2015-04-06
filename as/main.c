/*
 * main.c -- as clone for pdp10-elf
 * Copyright (C) 2013-2015  Mikael Pettersson
 *
 * This file is part of pdp10-tools.
 *
 * pdp10-tools is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pdp10-tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with pdp10-tools.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <unistd.h>
#include "assemble.h"
#include "input.h"
#include "output.h"
#include "tunit.h"

#define VERSION "pdp10-tools as version 0.2, built " __DATE__ " " __TIME__ "\n"

int main(int argc, char **argv)
{
    const char *outfile = "a.out";
    struct tunit tunit;

    for (;;) {
	int ch;

	ch = getopt(argc, argv, "vo:VQ:");
	switch (ch) {
	case 'Q':	/* SVR4 compat, ignored */
	    continue;
	case 'V':	/* SVR4 compat, alias for -v */
	case 'v':
	    printf(VERSION);
	    continue;
	case 'o':
	    outfile = optarg;
	    continue;
	case -1:
	    break;
	default:
	    fprintf(stderr, "Usage: %s [-v] [-o outfile] [files..]\n", argv[0]);
	    return 1;
	}
	break;
    }

    if (tunit_init(&tunit, argv[0]) < 0)
	return 1;

    if (input(&argv[optind], argc - optind, &tunit) < 0)
	return 1;

    if (assemble(&tunit) < 0)
	return 1;

    if (output(&tunit, outfile) < 0)
	return 1;

    tunit_fini(&tunit);

    return 0;
}
