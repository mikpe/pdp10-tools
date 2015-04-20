/*
 * scan.c
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
#include <limits.h>	/* XXX: for UCHAR_MAX, deleteme */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "scan.h"
#include "token.h"

void scan_init(struct scan_state *scan_state, const char *progname)
{
    scan_state->progname = progname;
    scan_state->filename = "<stdin>";
    scan_state->linenr = 1;
}

int scan_open(struct scan_state *scan_state, const char *filename)
{
    if (filename[0] == '-' && filename[1] == '-' && filename[2] == '\0') {
	scan_state->filename = "<stdin>";
	filename = "/dev/stdin";
    } else
	scan_state->filename = filename;

    if (freopen(filename, "r", stdin) == NULL) {
	fprintf(stderr, "%s: Error opening %s: %s\n", scan_state->progname, filename, strerror(errno));
	return -1;
    }

    return 0;
}

static void scan_ungetc(struct scan_state *scan_state, int ch)
{
    if (ch != EOF && ungetc(ch, stdin) == EOF)
	fprintf(stderr, "%s: %s line %u: ungetc %d failed: %s\n",
		scan_state->progname, scan_state->filename, scan_state->linenr, ch, strerror(errno));
}

static int scan_getchar(void)
{
    return fgetc(stdin);
}

static void badchar(struct scan_state *scan_state, int ch, const char *context)
{
    char buf[7];

    if (ch == EOF) {
	buf[0] = '<';
	buf[1] = 'E';
	buf[2] = 'O';
	buf[3] = 'F';
	buf[4] = '>';
	buf[5] = '\0';
    } else if (' ' <= ch && ch <= '~') {
	buf[0] = '\'';
	buf[1] = ch;
	buf[2] = '\'';
	buf[3] = '\0';
    } else {
	buf[0] = '\'';
	buf[1] = '\\';
	buf[2] = '0' + ((ch >> 6) & 3);
	buf[3] = '0' + ((ch >> 3) & 7);
	buf[4] = '0' + (ch & 7);
	buf[5] = '\'';
	buf[6] = '\0';
    }

    fprintf(stderr, "%s: %s line %u: invalid character %s%s\n",
	    scan_state->progname, scan_state->filename, scan_state->linenr, buf, context);
}

static unsigned int get_chval(int ch)
{
    if ('0' <= ch && ch <= '9')
	return ch - '0';
    if ('A' <= ch && ch <= 'F')
	return ch - ('A' - 10);
    if ('a' <= ch && ch <= 'f')
	return ch - ('a' - 10);
    return -1U;
}

static int is_octal_digit(int ch)
{
    return ch >= '0' && ch <= '7';
}

static int do_escape(struct scan_state *scan_state)
{
    int ch;

    ch = scan_getchar();
    switch (ch) {
    case 'n':
	return '\n';
    case 't':
	return '\t';
    case 'f':
	return '\f';
    case 'r':
	return '\r';
    case 'b':
	return '\b';
    case '\\':
    case '\'':
    case '"':
	return ch;
    default:
	if (is_octal_digit(ch)) {
	    unsigned int val = ch - '0';
	    ch = scan_getchar();
	    if (is_octal_digit(ch)) {
		val = val * 8 + (ch - '0');
		ch = scan_getchar();
		if (is_octal_digit(ch))
		    val = val * 8 + (ch - '0');
		else
		    scan_ungetc(scan_state, ch);
	    } else
		scan_ungetc(scan_state, ch);
	    /* XXX: this should be PDP10_UINT9_MAX, but our string elements are still char not pdp10_uint9_t for now */
	    if (val > UCHAR_MAX) {
		fprintf(stderr, "%s: %s line %u: out of range character escape value %#x\n",
			scan_state->progname, scan_state->filename, scan_state->linenr, val);
		return EOF;
	    }
	    return val & UCHAR_MAX;
	}
	break;
    }
    badchar(scan_state, ch, "in \\ character escape");
    if (ch == '\n')
	++scan_state->linenr;
    return EOF;
}

/* XXX: string literals should be sequences of pdp10_uint9_t, not sequences of char */

static enum token do_string(struct scan_state *scan_state, union token_attribute *token_attr)
{
    char charbuf[4096];	/* 4095 char + NUL, XXX: make it dynamic */
    unsigned int len;
    char *text;
    int ch;

    len = 0;
    for (;;) {
	ch = scan_getchar();
	switch (ch) {
	case '"':
	    text = malloc(len + 1);
	    if (!text) {
		fprintf(stderr, "%s: %s line %u: malloc(%u) failed: %s\n",
			scan_state->progname, scan_state->filename, scan_state->linenr, len + 1, strerror(errno));
		return T_ERROR;
	    }
	    strcpy(text, charbuf);
	    token_attr->text = text;
	    return T_STRING;
	case '\\':
	    ch = do_escape(scan_state);
	    if (ch == EOF)
		return T_ERROR;
	    break;
	case EOF:
	case '\n':
	    badchar(scan_state, ch, "in string literal");
	    if (ch == '\n')
		++scan_state->linenr;
	    return T_ERROR;
	default:
	    break;
	}
	if (len >= sizeof charbuf - 1) {
	    fprintf(stderr, "%s: %s line %u: too long string literal\n",
		    scan_state->progname, scan_state->filename, scan_state->linenr);
	    return T_ERROR;
	}
	charbuf[len] = ch;
	++len;
    }
}

static int is_symbol_internal_char(int ch)
{
    return
	('A' <= ch && ch <= 'Z')
	|| ('a' <= ch && ch <= 'z')
	|| ('0' <= ch && ch <= '9')
	|| ch == '_'
	|| ch == '$'
	|| ch == '.';
}

static enum token do_symbol(struct scan_state *scan_state, union token_attribute *token_attr, int ch)
{
    char charbuf[128];	/* 127 chars + NUL, XXX: make it dynamic */
    unsigned int len;
    char *text;

    len = 0;
    do {
	if (len >= sizeof charbuf - 1) {
	    fprintf(stderr, "%s: %s line %u: too long symbol\n",
		    scan_state->progname, scan_state->filename, scan_state->linenr);
	    return T_ERROR;
	}
	charbuf[len] = ch;
	++len;
	ch = scan_getchar();
    } while (is_symbol_internal_char(ch));
    charbuf[len] = '\0';
    scan_ungetc(scan_state, ch);
    
    if (charbuf[0] == '.') {
	enum token low, high;

	if (charbuf[1] == '\0')
	    return T_DOT;

	/* see token.def, reserved symbols occupy tokens [0,T_SYMBOL[ */
	low = 0;
	high = T_SYMBOL;

	while (low < high) {
	    enum token middle;
	    int cmp;

	    middle = (low + high) / 2;
	    cmp = strcmp(charbuf, token_info[middle].print_name);

	    if (cmp < 0)
		high = middle;
	    else if (cmp > 0)
		low = middle + 1;
	    else
		return middle;
	}
    }

    text = malloc(len + 1);
    if (!text) {
	fprintf(stderr, "%s: %s line %u: malloc(%u) failed: %s\n",
		scan_state->progname, scan_state->filename, scan_state->linenr, len + 1, strerror(errno));
	return T_ERROR;
    }
    strcpy(text, charbuf);
    token_attr->text = text;
    return T_SYMBOL;
}

static enum token do_number(struct scan_state *scan_state, union token_attribute *token_attr, int ch)
{
    unsigned int base, chval;
    pdp10_uint36_t numval;

    base = (ch == '0') ? 8 : 10;
    numval = ch - '0';

    /* handle 0x<first hexdig> */
    ch = scan_getchar();
    if (base == 8 && (ch == 'x' || ch == 'X')) {
	base = 16;
	/* must have at least one hex digit after 0x */
	ch = scan_getchar();
	chval = get_chval(ch);
	if (chval >= 16) {
	    badchar(scan_state, ch, " after 0x in hexadecimal literal");
	    return T_ERROR;
	}
	numval = chval;
	ch = scan_getchar();
    }

    /* the number is non-empty, consume and accumulate trailing
       characters as long as they are valid in the base */
    for (;;) {
	chval = get_chval(ch);
	if (chval >= base)
	    break;
	numval = numval * base + chval;	/* XXX: check for overflow */
	ch = scan_getchar();
    }

    /* XXX: check for <decimal>{b,f} which is a local label reference */

    /* plain integer literal */
    scan_ungetc(scan_state, ch);
    token_attr->uint = numval;
    return T_UINTEGER;
}

static int do_line_comment(struct scan_state *scan_state)
{
    for (;;) {
	int ch = scan_getchar();
	switch (ch) {
	case '\n':
	    ++scan_state->linenr;
	    return T_NEWLINE;
	case EOF:
	    badchar(scan_state, ch, "in line comment");
	    return T_ERROR;
	default:
	    continue;
	}
    }
}

enum token scan_token(struct scan_state *scan_state, union token_attribute *token_attr)
{
    int ch;

    ch = scan_getchar();

    for (;; ch = scan_getchar()) {
	switch (ch) {
	case ' ':
	case '\t':
	case '\r':
	case '\f':
	    continue;
	case '\n':
	    ++scan_state->linenr;
	    return T_NEWLINE;
	case '#':
	    return do_line_comment(scan_state);
	case EOF:
	    return T_EOF;
	case '@':
	    return T_AT;
	case ':':
	    return T_COLON;
	case ',':
	    return T_COMMA;
	case '(':
	    return T_LPAREN;
	case ')':
	    return T_RPAREN;
	case '"':
	    return do_string(scan_state, token_attr);
	case '-':
	    return T_MINUS;
	case '.':
	    /* Dot may start a floating point literal, but tests show that
	       gcc always outputs floating point values as integer literals,
	       so we shouldn't have to support floating point literals at all.  */
	case '$':
	case '_':
	    return do_symbol(scan_state, token_attr, ch);
	default:
	    if ('0' <= ch && ch <= '9')	/* number or <decimal>{b,f} */
		return do_number(scan_state, token_attr, ch);
	    if (('A' <= ch && ch <= 'Z') ||
		('a' <= ch && ch <= 'z'))
		return do_symbol(scan_state, token_attr, ch);
	    break;
	}
	badchar(scan_state, ch, "");
	return T_ERROR;
    }
}
