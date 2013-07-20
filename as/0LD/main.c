/*
 * main.c
 */
#include <stdio.h>
#include <unistd.h>
#include "pass1.h"

int main(int argc, char **argv)
{
    int ch;
    const char *outfile = "a.out";
    const char *infile = NULL;

    for (;;) {
	ch = getopt(argc, argv, "o:");
	switch (ch) {
	case 'o':
	    outfile = optarg;
	    continue;
	case -1:
	    break;
	default:
	    fprintf(stderr, "as: invalid option '%c'\n", ch);
	    return 1;
	}
    }
    if (optind + 1 == argc)
	infile = argv[optind];

    if (pass1(infile) < 0)
	return 1;

    if (pass2() < 0)
	return 1;

    if (pass3(outfile) < 0)
	return 1;

    return 0;
}
