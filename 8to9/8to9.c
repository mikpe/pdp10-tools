/*
 * 8to9.c
 *
 * Convert octet files to nonet files.
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
