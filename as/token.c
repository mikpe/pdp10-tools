/*
 * token.c
 */
#include <stdio.h>
#include "pdp10-inttypes.h"
#include "token.h"

enum {
    FMT_NONE = 0,
    FMT_UINT = 1,
    FMT_SYMBOL = 2,
    FMT_STRING = 3,
};

struct token_info {
    char print_name[15];
    unsigned char attribute_fmt;
};

static const struct token_info token_info[] = {
#define TOKEN(T,P,F) { P, F },
#include "token.def"
#undef TOKEN
};

void token_print(FILE *fp, enum token token, const union token_attribute *token_attr)
{
    const struct token_info *ti;

    if (token >= sizeof token_info / sizeof token_info[0]) {
	fprintf(fp, "<invalid token %u>", token);
	return;
    }

    ti = &token_info[token];
    fprintf(fp, "%.*s", (int) sizeof ti->print_name, ti->print_name);

    if (!token_attr)
	return;

    switch (ti->attribute_fmt) {
    case FMT_UINT:
	fprintf(fp, " [%" PDP10_PRIu36 "u]", token_attr->uint);
	break;
    case FMT_SYMBOL:
	fprintf(fp, " [%s]", token_attr->text);
	break;
    case FMT_STRING:
	fprintf(fp, " [\"%s\"]", token_attr->text);
	break;
    default:
	break;
    }
}
