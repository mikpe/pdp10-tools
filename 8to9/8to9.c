/*
 * 8to9.c -- convert octet files to nonet files
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
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "pdp10-stdio.h"

#define VERSION "pdp10-tools 8to9 version 0.0, built " __DATE__ " " __TIME__ "\n"

int main(int argc, char **argv)
{
    const char *infile, *outfile;
    FILE *infp;
    PDP10_FILE *outfp;
    int ch;

    infile = NULL;
    outfile = NULL;

    for (;;) {
	ch = getopt(argc, argv, "Vi:o:");
	switch (ch) {
	case 'V':
	    printf(VERSION);
	    return 0;
	case 'i':
	    infile = optarg;
	    continue;
	case 'o':
	    outfile = optarg;
	    continue;
	case -1:
	    break;
	default:
	    fprintf(stderr, "%s -o OUTFILE [-i INFILE] [-V]\n", argv[0]);
	    return 1;
	}
	break;
    }

    /* XXX: We currently require a named output file because pdp10-stdio.c doesn't
       support fdopen:ing non-seekable files.  That should be fixed at some point.  */
    if (!outfile) {
	fprintf(stderr, "%s: no OUTFILE specified\n", argv[0]);
	return 1;
    } else {
	outfp = pdp10_fopen(outfile, "wb");
	if (!outfp) {
	    fprintf(stderr, "%s: %s: failed to open for writing: %s\n", argv[0], outfile, strerror(errno));
	    return 1;
	}
    }

    if (!infile) {
	infp = stdin;
    } else {
	infp = fopen(infile, "rb");
	if (!infp) {
	    fprintf(stderr, "%s: %s: failed to open for reading: %s\n", argv[0], infile, strerror(errno));
	    return 1;
	}
    }
    
    while ((ch = fgetc(infp)) != EOF)
	if (pdp10_fputc(ch, outfp) == EOF) {
	    fprintf(stderr, "%s: %s: failed to write: %s\n", argv[0], outfile, strerror(errno));
	    return 1;
	}

    if (pdp10_fclose(outfp) == EOF) {
	fprintf(stderr, "%s: %s: failed to close: %s\n", argv[0], outfile, strerror(errno));
	return 1;
    }

    return 0;
}
