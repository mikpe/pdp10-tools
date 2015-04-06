/*
 * token.h
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
