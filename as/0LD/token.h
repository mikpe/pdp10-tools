/*
 * token.h
 */
#ifndef TOKEN_H
#define TOKEN_H

#include "pdp10-stdint.h"

enum token {
#define TOKEN(T,P,F)	T,
#include "token.def"
#undef TOKEN
};

union token_attribute {
    const char *text;		/* symbol, string */
    pdp10_uint36_t uint;	/* uinteger */
};

void token_print(FILE *fp, enum token token, const union token_attribute *token_attr);

#endif /* TOKEN_H */
