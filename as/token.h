/*
 * token.h
 */
#ifndef TOKEN_H
#define TOKEN_H

#include <stdio.h>
#include "pdp10-stdint.h"

enum token {
#define TOKEN(T,P,F)	T,
#include "token.def"
#undef TOKEN
};

enum {
    TAFMT_NONE = 0,
    TAFMT_UINT = 1,
    TAFMT_SYMBOL = 2,
    TAFMT_STRING = 3,
};

struct token_info {
    char print_name[15];
    unsigned char attribute_fmt;
};

/* token_info[] is indexed by token and is used by token_print() to print tokens;
   it is also public so the scanner can map directive names to tokens without
   duplicating the names or the name-to-token mapping */
extern const struct token_info token_info[];

union token_attribute {
    const char *text;		/* symbol, string */
    pdp10_uint36_t uint;	/* uinteger */
};

void token_print(FILE *fp, enum token token, const union token_attribute *token_attr);

#endif /* TOKEN_H */
