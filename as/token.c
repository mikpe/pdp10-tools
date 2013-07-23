/*
 * token.c
 */
#include <stdio.h>
#include "pdp10-inttypes.h"
#include "token.h"

const struct token_info token_info[] = {
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
    case TAFMT_UINT:
	fprintf(fp, " [%" PDP10_PRIu36 "]", token_attr->uint);
	break;
    case TAFMT_SYMBOL:
	fprintf(fp, " [%s]", token_attr->text);
	break;
    case TAFMT_STRING:
	fprintf(fp, " [\"%s\"]", token_attr->text);
	break;
    default:
	break;
    }
}
