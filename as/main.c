/*
 * main.c
 *
 * as clone for PDP10 with Elf36 object files.
 */
#include <stdio.h>
#include <unistd.h>
#include "assemble.h"
#include "input.h"
#include "output.h"

#define VERSION "pdp10-tools as version 0.1, built " __DATE__ " " __TIME__ "\n"

int main(int argc, char **argv)
{
    const char *outfile = "a.out";
    struct iunit iunit;
    struct aunit aunit;

    for (;;) {
	int ch;

	ch = getopt(argc, argv, "vo:");
	switch (ch) {
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

    if (input(argv[0], &argv[optind], argc - optind, &iunit) < 0)
	return 1;

    if (assemble(argv[0], &iunit, &aunit) < 0)
	return 1;

    /* XXX: iunit_fini(&iunit) */

    if (output(argv[0], &aunit, outfile) < 0)
	return 1;

    return 0;
}
