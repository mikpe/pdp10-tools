/*
 * scan.c
 */
#include <errno.h>
#include <stdio.h>	/* host stdio since we're dealing with plain text */
#include <stdlib.h>
#include <string.h>
#include "charbuf.h"
#include "scan.h"
#include "token.h"

/* XXX: we should have a pdp10-limits.h */
#define PDP10_UCHAR_MAX PDP10_UINT9_MAX

const char *scan_filename = "<stdin>";

int scan_freopen(const char *filename)
{
    if (filename != NULL) {
	if (freopen(filename, "r", stdin) == NULL) {
	    fprintf(stderr, "as: Error opening %s: %s\n", filename, strerror(errno));
	    return -1;
	}
	scan_filename = filename;
    }
    return 0;
}

unsigned int scan_linenr;

static void scan_ungetc(int ch)
{
    if (ch != EOF && ungetc(ch, stdin) == EOF)
	perror("ungetc");
}

static int scan_getchar(void)
{
    return fgetc(stdin);
}

static void badchar(int ch, const char *context)
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

    fprintf(stderr, "as: %s, line %u: invalid character %s %s\n", scan_filename, scan_linenr, buf, context);
}

static int is_decimal_digit(char ch)
{
    return '0' <= ch && ch <= '9';
}

static int is_octal_digit(char ch)
{
    return '0' <= ch && ch <= '7';
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

static int do_escape(void)
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
	return ch;
    case '\'':
	return ch;
    case '"':
	return ch;
    case 'x':
    case 'X':
    {
	unsigned int chval;

	ch = scan_getchar();
	chval = get_chval(ch);
	if (chval <= 15) {
	    unsigned int val = 0;
	    do {
		val = val * 16 + chval;
		ch = scan_getchar();
		chval = get_chval(ch);
	    } while (chval <= 15);
	    scan_ungetc(ch);
	    if (val > PDP10_UCHAR_MAX)
		fprintf(stderr, "as: %s, line %u: truncating escaped value from %#x to %#x\n", scan_filename, scan_linenr, val, val & PDP10_UCHAR_MAX);
	    return val & PDP10_UCHAR_MAX;
	}
	break;
    }
    case EOF:
	break;
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
		    scan_ungetc(ch);
	    } else
		scan_ungetc(ch);
	    if (val > PDP10_UCHAR_MAX)
		fprintf(stderr, "as: %s, line %u: truncating escaped value from %#x to %#x\n", scan_filename, scan_linenr, val, val & PDP10_UCHAR_MAX);
	    return val & PDP10_UCHAR_MAX;
	}
	break;
    }
    badchar(ch, "in \\ character escape sequence");
    return ch;
}

static enum token do_char(union token_attribute *token_attr)
{
    int ch;

    ch = scan_getchar();
    switch (ch) {
    case '\\':
	ch = do_escape();
	if (ch == EOF)
	    return T_ERROR;
	break;
    case '\'':
    case EOF:
	badchar(ch, "in character literal");
	return T_ERROR;
    default:
	break;
    }
    token_attr->uint = ch;
    ch = scan_getchar();
    if (ch != '\'') {
	badchar(ch, "after character literal");
	return T_ERROR;
    }
    return T_UINTEGER;
}

/* XXX: strings should be sequences of uint9_t not sequences of unsigned char */

static enum token do_string(union token_attribute *token_attr, struct charbuf *charbuf)
{
    int ch;

    for (;;) {
	ch = scan_getchar();
	switch (ch) {
	case '"':
	    token_attr->text = charbuf_string(charbuf);
	    return T_STRING;
	case '\\':
	    ch = do_escape();
	    if (ch == EOF)
		return T_ERROR;
	    break;
	case EOF:
	case '\n':
	    badchar(ch, "in string literal");
	    if (ch == '\n')
		++scan_linenr;
	    return T_ERROR;
	default:
	    break;
	}
	charbuf_append(charbuf, ch);
    }
}

static const struct {
    enum token token;
    const char *name;
} directives[] = {
    { T_DOT_ALIGN, ".align" },
    { T_DOT_ASCII, ".ascii" },
    { T_DOT_ASCIZ, ".asciz" },
    { T_DOT_BALIGN, ".balign" },
    { T_DOT_BSS, ".bss" },
    { T_DOT_BYTE, ".byte" },
    { T_DOT_COMM, ".comm" },
    { T_DOT_DATA, ".data" },
    { T_DOT_FILE, ".file" },
    { T_DOT_GLOBL, ".globl" },
    { T_DOT_HIDDEN, ".hidden" },
    { T_DOT_IDENT, ".ident" },
    { T_DOT_INTERNAL, ".internal" },
    { T_DOT_LOCAL, ".local" },
    { T_DOT_LONG, ".long" },
    { T_DOT_ORG, ".org" },
    { T_DOT_P2ALIGN, ".p2align" },
    { T_DOT_POPSECTION, ".popsection" },
    { T_DOT_PREVIOUS, ".previous" },
    { T_DOT_PROTECTED, ".protected" },
    { T_DOT_PUSHSECTION, ".pushsection" },
    { T_DOT_RODATA, ".rodata" },
    { T_DOT_SECTION, ".section" },
    { T_DOT_SET, ".set" },
    { T_DOT_SHORT, ".short" },
    { T_DOT_SIZE, ".size" },
    { T_DOT_SUBSECTION, ".subsection" },
    { T_DOT_SYMVER, ".symver" },
    { T_DOT_TEXT, ".text" },
    { T_DOT_TYPE, ".type" },
    { T_DOT_WEAK, ".weak" },
    { T_DOT_WEAKREF, ".weakref" },
};

static enum token mk_symbol(union token_attribute *token_attr, const struct charbuf *charbuf)
{
    if (charbuf->head.buf[0] == '.') {			/* check for <.directive> */
	unsigned int low, high;

	low = 0;
	high = sizeof directives / sizeof directives[0];

	while (low < high) {
	    unsigned int middle;
	    int cmp;

	    middle = (low + high) / 2;
	    cmp = charbuf_strcmp(charbuf, directives[middle].name);

	    if (cmp < 0)
		high = middle;
	    else if (cmp > 0)
		low = middle + 1;
	    else
		return directives[middle].token;
	}
    } else if (charbuf->head.buf[0] == '$'
	       && charbuf->head.next == NULL) {		/* check for $<reg> */
	if (charbuf->pos == 2
	    && is_decimal_digit(charbuf->head.buf[1])) {
	    token_attr->uint = charbuf->head.buf[1] - '0';
	    return T_REGISTER;
	} else if (charbuf->pos == 3) {
	    if (is_decimal_digit(charbuf->head.buf[1])
		&& is_decimal_digit(charbuf->head.buf[2])) {
		unsigned int val;

		val = (charbuf->head.buf[1] - '0') * 10 + (charbuf->head.buf[2] - '0');
		if (val < 16) {
		    token_attr->uint = val;
		    return T_REGISTER;
		}
	    } else if (charbuf->head.buf[1] == 's'
		       && charbuf->head.buf[2] == 'p') {
		token_attr->uint = 15;
		return T_REGISTER;
	    }
	}
    }

    token_attr->text = charbuf_string(charbuf);
    return T_SYMBOL;
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

static enum token do_symbol(union token_attribute *token_attr, int ch, struct charbuf *charbuf)
{
    do {
	charbuf_append(charbuf, ch);
	ch = scan_getchar();
    } while (is_symbol_internal_char(ch));
    scan_ungetc(ch);
    return mk_symbol(token_attr, charbuf);
}

static enum token do_number(union token_attribute *token_attr, int ch)
{
    unsigned int base, chval;
    pdp10_uint36_t numval;

    base = (ch == '0') ? 8 : 10;
    numval = ch - '0';

    ch = scan_getchar();
    /* handle 0x<first hexdig> */
    if (ch == 'x' || ch == 'X') {
	base = 16;
	/* must have at least one hex digit after 0x */
	ch = scan_getchar();
	chval = get_chval(ch);
	if (chval <= 15)
	    numval = chval;
	else {
	    badchar(ch, "after 0x in hexadecimal literal");
	    return T_ERROR;
	}
	ch = scan_getchar();
    }
    /* the number is non-empty, consume and accumulate trailing
       characters as long as they are valid in the base */
    for (;;) {
	chval = get_chval(ch);
	if (chval >= base)
	    break;
	numval = numval * base + chval;
	ch = scan_getchar();
    }
    /* check for <local label>{b,f} */
    if (base <= 10 && (ch == 'b' || ch == 'f')) {
	/* represent the local label + direction in sign-magnitude with
	   the sign in the least significant bit; using sign-magnitude
	   allows to distinguish 0f from 0b (i.e., +0 from -0); storing
	   the sign in the least significant bit makes us independent of
	   word size */
	token_attr->uint = (numval << 1) | (ch == 'f' ? 1 : 0);
	return T_LOCAL_LABEL;
    }
    /* plain integer literal */
    scan_ungetc(ch);
    token_attr->uint = numval;
    return T_UINTEGER;
}

static enum token do_eq(void)
{
    int ch;

    ch = scan_getchar();
    switch (ch) {
    case '=':
	return T_EQEQ;
    default:
	scan_ungetc(ch);
	return T_EQ;
    }
}

static enum token do_ampersand(void)
{
    int ch;

    ch = scan_getchar();
    switch (ch) {
    case '&':
	return T_ANDAND;
    default:
	scan_ungetc(ch);
	return T_AND;
    }
}

static enum token do_bar(void)
{
    int ch;

    ch = scan_getchar();
    switch (ch) {
    case '|':
	return T_OROR;
    default:
	scan_ungetc(ch);
	return T_OR;
    }
}

static enum token do_gt(void)
{
    int ch;

    ch = scan_getchar();
    switch (ch) {
    case '>':
	return T_RSHIFT;
    case '=':
	return T_GE;
    default:
	scan_ungetc(ch);
	return T_GT;
    }
}

static enum token do_lt(void)
{
    int ch;

    ch = scan_getchar();
    switch (ch) {
    case '<':
	return T_LSHIFT;
    case '=':
	return T_LE;
    case '>':	/* <> is the same as != */
	return T_NEQ;
    default:
	scan_ungetc(ch);
	return T_LT;
    }
}

static enum token do_c_comment(void)
{
    int ch;

    for (;;) {
	ch = scan_getchar();
	switch (ch) {
	case EOF:
	    badchar(ch, "in /**/-style comment");
	    return T_ERROR;
	case '*':
	    for (;;) {
		ch = scan_getchar();
		switch (ch) {
		case '*':
		    continue;
		case '/':
		    return T_EOF;	/* fake token for a C comment */
		case EOF:
		    badchar(ch, "in /**/-style comment");
		    return T_ERROR;
		case '\n':
		    ++scan_linenr;
		    /*FALLTHROUGH*/
		default:
		    break;
		}
		break;
	    }
	    continue;
	case '\n':
	    ++scan_linenr;
	    /*FALLTHROUGH*/
	default:
	    continue;
	}
    }
}

static enum token do_slash(void)
{
    int ch;

    ch = scan_getchar();
    switch (ch) {
    case '*':
	return do_c_comment();
    default:
	scan_ungetc(ch);
	return T_DIV;
    }
}

static enum token do_bang(void)
{
    int ch;

    ch = scan_getchar();
    switch (ch) {
    case '=':
	return T_NEQ;
    default:
	scan_ungetc(ch);
	return T_BANG;
    }
}

static int do_line_comment(void)
{
    int ch;

    for (;;) {
	ch = scan_getchar();
	switch (ch) {
	case '\n':
	    ++scan_linenr;
	    return 0;
	case EOF:
	    badchar(ch, "in line comment");
	    return -1;
	default:
	    continue;
	}
    }
}

static enum token do_scan(union token_attribute *token_attr, struct charbuf *charbuf)
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
	    ++scan_linenr;
	    return T_NEWLINE;
	case '#':
	    if (do_line_comment() != 0)
		return T_ERROR;
	    return T_NEWLINE;
	case ';':
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
	case '~':
	    return T_TILDE;
	case '*':
	    return T_MUL;
	case '/':	/* "/""*", "/" */
	    switch (do_slash()) {
	    case T_DIV:
		return T_DIV;
	    case T_EOF:	/* fake token for a C comment */
		continue;
	    default:	/* error, eof in comment */
		return T_ERROR;
	    }
	case '%':
	    return T_REM;
	case '<':	/* <<, <=, < */
	    return do_lt();
	case '>':	/* >>, >=, > */
	    return do_gt();
	case '|':	/* ||, | */
	    return do_bar();
	case '&':	/* &&, & */
	    return do_ampersand();
	case '^':
	    return T_CARET;
	case '!':	/* !=, ! */
	    return do_bang();
	case '+':
	    return T_PLUS;
	case '-':
	    return T_MINUS;
	case '=':	/* ==, = */
	    return do_eq();
	case '"':
	    return do_string(token_attr, charbuf);
	case '\'':
	    return do_char(token_attr);
	case '.':
	    /* Dot may start a floating point literal, but tests show that
	       gcc always outputs floating point values as integer literals,
	       so we shouldn't have to support floating point literals at all.  */
	case '$':
	case '_':
	    return do_symbol(token_attr, ch, charbuf);
	default:
	    if ('0' <= ch && ch <= '9')	/* number or <decimal>{b,f} */
		return do_number(token_attr, ch);
	    if (('A' <= ch && ch <= 'Z') ||
		('a' <= ch && ch <= 'z'))
		return do_symbol(token_attr, ch, charbuf);
	}
	badchar(ch, "");
	return T_ERROR;
    }
}

enum token scan(union token_attribute *token_attr)
{
    struct charbuf charbuf;
    enum token token;

    charbuf_init(&charbuf);
    token = do_scan(token_attr, &charbuf);
    charbuf_fini(&charbuf);

    return token;
}
