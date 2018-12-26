/*
 * od.c -- od clone for binary files with 9-bit bytes
 * Copyright (C) 2013-2015, 2018  Mikael Pettersson
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
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>	/* getopt() */
#include "pdp10-extint.h"
#include "pdp10-inttypes.h"
#include "pdp10-stdio.h"

#define VERSION "pdp10-tools od version 0.2, built " __DATE__ " " __TIME__ "\n"

struct options {
    /* user options: */
    char address_radix;			/* -A/--address-radix= */
    unsigned long skip_bytes;		/* -j/--skip-bytes= */
    unsigned long read_bytes;		/* -N/--read-bytes= */
    char output_type;			/* 'c', 'd', 'o', 'u', 'x'; 'a' and 'f' are NYI */
    unsigned char output_z;		/* type has trailing 'z' */
    unsigned int bytes_per_datum;	/* 1, 2, or 4 */
    unsigned long width;		/* -w/--width= */
    unsigned char version;		/* -V */
    /* compiled options: */
    char numfmt[8];			/* "%0*{,l,ll}[doux]\0" */
    int chars_per_datum;
    unsigned int datums_per_line;
};

struct input {
    unsigned long read_bytes;
    char **files;
    unsigned int nrfiles;
    PDP10_FILE *pdp10fp;
};

static void input_init(struct input *input, unsigned long read_bytes, char **files, unsigned int nrfiles)
{
    input->read_bytes = read_bytes;
    input->files = files;
    input->nrfiles = nrfiles;
    input->pdp10fp = NULL;
}

static void input_fini(struct input *input)
{
    if (input->pdp10fp)
	pdp10_fclose(input->pdp10fp);
}

static int input_fgetc_raw(struct input *input, const char *progname)
{
    int ch;
    const char *filename;

    for (;;) {
	if (input->pdp10fp) {
	    ch = pdp10_fgetc(input->pdp10fp);
	    if (ch != EOF)
		return ch;
	    pdp10_fclose(input->pdp10fp);
	    input->pdp10fp = NULL;
	}

	if (input->nrfiles == 0)
	    return EOF;

	filename = input->files[0];
	++input->files;
	--input->nrfiles;

	/* XXX: call pdp10_fdopen() if filename is "-" */
	if (strcmp(filename, "-") == 0)
	    filename = "/dev/stdin";
	input->pdp10fp = pdp10_fopen(filename, "rb");
	if (!input->pdp10fp) {
	    fprintf(stderr, "%s: %s: failed to open: %s\n", progname, filename, strerror(errno));
	    input->nrfiles = 0;
	    return EOF;
	}
    }
}

static int input_fgetc_limited(struct input *input, const char *progname)
{
    int ch;

    if (input->read_bytes == 0)
	return EOF;

    ch = input_fgetc_raw(input, progname);
    if (ch == EOF)
	return ch;

    if (input->read_bytes != -1UL)
	--input->read_bytes;

    return ch;
}

static int od1(const char *progname, struct options *options, struct input *input)
{
    pdp10_uint36_t offset;
    const unsigned int bytes_per_line = options->bytes_per_datum * options->datums_per_line;
    pdp10_uint9_t line_bytes[bytes_per_line];
    unsigned int nrbytes, b;
    int ch;
    pdp10_uint36_t datum;

    offset = 0;

    for (; options->skip_bytes > 0; ++offset, --options->skip_bytes) {
	if (input_fgetc_raw(input, progname) == EOF) {
	    fprintf(stderr, "%s: cannot skip past end of combined input\n", progname);
	    return 1;
	}
    }

    for (;;) {

	switch (options->address_radix) {
	case 'o':
	    printf("%011" PDP10_PRIo36, offset);
	    break;
	case 'd':
	    printf("%010" PDP10_PRIu36, offset);
	    break;
	case 'x':
	    printf("%09" PDP10_PRIx36, offset);
	    break;
	}

	for (nrbytes = 0; nrbytes < bytes_per_line; ++nrbytes) {
	    ch = input_fgetc_limited(input, progname);
	    if (ch == EOF)
		break;
	    line_bytes[nrbytes] = ch;
	}
	for (b = nrbytes; b < bytes_per_line; ++b)
	    line_bytes[b] = 0;

	if (nrbytes == 0) {
	    printf("\n");
	    break;
	}

	for (b = 0; b < nrbytes; b += options->bytes_per_datum) {
	    if (options->output_type == 'c') {
		if ((unsigned char)line_bytes[b] == line_bytes[b]
		    && isprint(line_bytes[b])) {
		    printf("   %c", line_bytes[b]);
		    continue;
		}
		switch (line_bytes[b]) {
		case '\0':
		    printf("  \\0");
		    break;
		case '\t':
		    printf("  \\t");
		    break;
		case '\r':
		    printf("  \\r");
		    break;
		case '\n':
		    printf("  \\n");
		    break;
		case '\f':
		    printf("  \\f");
		    break;
		case '\e':
		    printf("  \\e");
		    break;
		default:
		    printf(" %03o", line_bytes[b]);
		    break;
		}
		continue;
	    }
	    switch (options->bytes_per_datum) {
	    default:
	    case 1:
		datum = line_bytes[b];
		if (options->output_type == 'd') {
		    const pdp10_uint36_t PDP10_UINT9_SBIT = ~(PDP10_UINT9_MAX >> 1) & PDP10_UINT9_MAX;
		    datum = ((datum & PDP10_UINT9_MAX) ^ PDP10_UINT9_SBIT) - PDP10_UINT9_SBIT;
		}
		break;
	    case 2:
	    {
		struct pdp10_ext_uint18 ext18;
		ext18.nonet[0] = line_bytes[b + 0];
		ext18.nonet[1] = line_bytes[b + 1];
		datum = pdp10_uint18_from_ext(&ext18);
		if (options->output_type == 'd') {
		    const pdp10_uint36_t PDP10_UINT18_SBIT = ~(PDP10_UINT18_MAX >> 1) & PDP10_UINT18_MAX;
		    datum = ((datum & PDP10_UINT18_MAX) ^ PDP10_UINT18_SBIT) - PDP10_UINT18_SBIT;
		}
		break;
	    }
	    case 4:
	    {
		struct pdp10_ext_uint36 ext36;
		ext36.nonet[0] = line_bytes[b + 0];
		ext36.nonet[1] = line_bytes[b + 1];
		ext36.nonet[2] = line_bytes[b + 2];
		ext36.nonet[3] = line_bytes[b + 3];
		datum = pdp10_uint36_from_ext(&ext36);
		if (options->output_type == 'd') {
		    const pdp10_uint36_t PDP10_UINT36_SBIT = ~(PDP10_UINT36_MAX >> 1) & PDP10_UINT36_MAX;
		    datum = ((datum & PDP10_UINT36_MAX) ^ PDP10_UINT36_SBIT) - PDP10_UINT36_SBIT;
		}
		break;
	    }
	    }
	    printf(" ");
	    printf(options->numfmt, options->chars_per_datum, datum);
	}

	while (b < bytes_per_line) {
	    printf("%*s", options->chars_per_datum + 1, "");
	    b += options->bytes_per_datum;
	}

	if (options->output_z) {
	    printf("  >");
	    for (b = 0; b < nrbytes; ++b) {
		if ((unsigned char)line_bytes[b] == line_bytes[b]
		    && isprint(line_bytes[b]))
		    printf("%c", line_bytes[b]);
		else
		    printf(".");
	    }
	    printf("<");
	}
	printf("\n");

	offset += nrbytes;
    }

    return 0;
}

static int od(const char *progname, struct options *options, char **files, int nrfiles)
{
    struct input input;
    char fake_file[2];
    char *fake_files[1];
    int i;
    int status;

    /* getopt() doesn't diagnose invalid short options, so we'll do that here */
    for (i = 0; i < nrfiles; ++i) {
	if (files[i][0] == '-' && files[i][1] != '\0') {
	    fprintf(stderr, "%s: invalid option '%s'\n", progname, files[i]);
	    return 1;
	}
    }

    if (options->version)
	printf(VERSION);

    if (nrfiles <= 0) {
	fake_file[0] = '-';
	fake_file[1] = '\0';
	fake_files[0] = fake_file;
	files = fake_files;
	nrfiles = 1;
    }

    input_init(&input, options->read_bytes, files, nrfiles);
    status = od1(progname, options, &input);
    input_fini(&input);
    return status;
}

/*
 * Command-line interface.
 */

static void usage(const char *progname)

{
    fprintf(stderr,
	    "Usage: %s [-V] [-bcdDiloOsSxX] [-t [bcdoux][1248][z]] [-A RADIX] [-j BYTES] [-N BYTES] [-w [BYTES]] [files..]\n",
	    progname);
}

static int parse_radix(const char *progname, struct options *options, const char *string)
{
    switch (string[0]) {
    case 'd':
    case 'o':
    case 'x':
    case 'n':
	if (string[1] == '\0') {
	    options->address_radix = string[0];
	    return 0;
	}
	/*FALLTHROUGH*/
    default:
	fprintf(stderr, "%s: invalid radix '%s'\n", progname, string);
	return -1;
    }
}

static int parse_bytes(const char *progname, unsigned long *dst, const char *string)
{
    unsigned long val;
    char *end;

    errno = 0;
    val = strtoul(string, &end, 0);
    if ((val == 0 || val == ULONG_MAX) && errno != 0) {
	fprintf(stderr, "%s: invalid number '%s': %s\n", progname, string, strerror(errno));
	return -1;
    }
    switch (*end) {
    case 'b':
	val *= 512;
	++end;
	break;
    case 'K':
	val *= 1024;
	++end;
	break;
    case 'M':
	val *= 1024 * 1024;
	++end;
	break;
    case 'G':
	val *= 1024 * 1024 * 1024;
	++end;
	break;
    }
    if (*end != '\0') {
	fprintf(stderr, "%s: invalid multiplier in '%s'\n", progname, string);
	return -1;
    }
    *dst = val;
    return 0;
}

static int parse_type(const char *progname, struct options *options, const char *string)
{
    const char *s;

    s = string;
    switch (*s) {
    case 'c':
    case 'd':
    case 'o':
    case 'u':
    case 'x':
	options->output_type = *s;
	break;
    default:
	fprintf(stderr, "%s: invalid type letter '%c' in type specifier '%s'\n", progname, *s, string);
	return -1;
    }

    if (*s == 'c') {
	++s;
	options->bytes_per_datum = 1;
    } else {
	++s;
	switch (*s) {
	case '1':
	    options->bytes_per_datum = 1;
	    ++s;
	    break;
	case '2':
	    options->bytes_per_datum = 2;
	    ++s;
	    break;
	case '4':
	    options->bytes_per_datum = 4;
	    ++s;
	    break;
	case 'z':
	case '\0':
	    options->bytes_per_datum = 4;
	    break;
	default:
	    fprintf(stderr, "%s: invalid type size '%c' in type specifier '%s'\n", progname, *s, string);
	    return -1;
	}
    }

    if (*s == 'z') {
	options->output_z = 1;
	++s;
    }

    if (*s != '\0') {
	fprintf(stderr, "%s: invalid suffix '%c' in type specifier '%s'\n", progname, *s, string);
	return -1;
    }

    return 0;
}

static int compile_options(const char *progname, struct options *options)
{
    pdp10_uint36_t maxval;
    char tmpbuf[16];

    options->datums_per_line = options->width / options->bytes_per_datum;
    if (options->datums_per_line * options->bytes_per_datum != options->width) {
	fprintf(stderr, "%s: line width %lu is not a multiple of the input datum size %u\n",
		progname, options->width, options->bytes_per_datum);
	return -1;
    }

    switch (options->output_type) {
    case 'd':
	sprintf(options->numfmt, "%% *%s", PDP10_PRId36);
	break;
    case 'u':
	sprintf(options->numfmt, "%% *%s", PDP10_PRIu36);
	break;
    case 'o':
	sprintf(options->numfmt, "%%0*%s", PDP10_PRIo36);
	break;
    case 'x':
	sprintf(options->numfmt, "%%0*%s", PDP10_PRIx36);
	break;
    default:
	options->chars_per_datum = 3;	/* for -c */
	return 0;
    }

    maxval = PDP10_UINT36_MAX;
    if (options->bytes_per_datum < 4)
	maxval >>= 18;
    if (options->bytes_per_datum < 2)
	maxval >>= 9;

    sprintf(tmpbuf, options->numfmt, 1, maxval);
    options->chars_per_datum = strlen(tmpbuf);

    return 0;
}

int main(int argc, char **argv)
{
    struct options options;

    memset(&options, 0, sizeof options);
    options.address_radix = 'o';
    options.read_bytes = -1UL;
    options.output_type = 'o';
    options.bytes_per_datum = 2;
    options.width = 16;

    for (;;) {
	int ch;

	ch = getopt(argc, argv, "VbcdDiloOsxXA:j:N:t:w::");
	switch (ch) {
	case 'V':
	    options.version = 1;
	    continue;
	case 'b':	/* == -t o1 */
	    options.output_type = 'o';
	    options.output_z = 0;
	    options.bytes_per_datum = 1;
	    break;
	case 'c':	/* == -t c */
	    options.output_type = 'c';
	    options.output_z = 0;
	    options.bytes_per_datum = 1;
	    continue;
	case 'd':	/* == -t u2 */
	    options.output_type = 'u';
	    options.output_z = 0;
	    options.bytes_per_datum = 2;
	    continue;
	case 'D':	/* == -t u4 */
	    options.output_type = 'u';
	    options.output_z = 0;
	    options.bytes_per_datum = 4;
	    continue;
	case 'o':	/* == -t o2 */
	    options.output_type = 'o';
	    options.output_z = 0;
	    options.bytes_per_datum = 2;
	    continue;
	case 'O':	/* == -t o4 */
	    options.output_type = 'o';
	    options.output_z = 0;
	    options.bytes_per_datum = 4;
	    continue;
	case 's':	/* == -t d2 */
	    options.output_type = 'd';
	    options.output_z = 0;
	    options.bytes_per_datum = 2;
	    continue;
	case 'i':	/* == -t d<sizeof(int)> */
	case 'l':	/* == -t d<sizeof(long)> */
	    options.output_type = 'd';
	    options.output_z = 0;
	    options.bytes_per_datum = 4;
	    continue;
	case 'x':	/* == -t x2 */
	    options.output_type = 'x';
	    options.output_z = 0;
	    options.bytes_per_datum = 2;
	    continue;
	case 'X':	/* == -t x4 */
	    options.output_type = 'x';
	    options.output_z = 0;
	    options.bytes_per_datum = 4;
	    continue;
	case 't':
	    if (parse_type(argv[0], &options, optarg) < 0)
		goto do_usage;
	    continue;
	case 'A':
	    if (parse_radix(argv[0], &options, optarg) < 0)
		goto do_usage;
	    continue;
	case 'j':
	    if (parse_bytes(argv[0], &options.skip_bytes, optarg) < 0)
		goto do_usage;
	    continue;
	case 'N':
	    if (parse_bytes(argv[0], &options.read_bytes, optarg) < 0)
		goto do_usage;
	    continue;
	case 'w':
	    if (!optarg)
		options.width = 32;
	    else if (parse_bytes(argv[0], &options.width, optarg) < 0)
		goto do_usage;
	    continue;
	case -1:
	    break;
	default:
	do_usage:
	    usage(argv[0]);
	    return 1;
	}
	break;
    }

    if (compile_options(argv[0], &options) < 0)
	return 1;

    return od(argv[0], &options, &argv[optind], argc - optind);
}
