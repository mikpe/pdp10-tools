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
