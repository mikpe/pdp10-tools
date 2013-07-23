/*
 * scan.c
 */
#include <errno.h>
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

    fprintf(stderr, "%s: %s, line %u: invalid character %s%s\n",
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
