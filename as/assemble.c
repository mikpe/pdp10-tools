/*
 * assemble.c
 */
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "assemble.h"
#include "hashtab.h"
#include "tunit.h"

static int assemble_section(struct hashnode *hashnode, void *data)
{
    struct section *section = (struct section*)hashnode;	/*XXX*/
    struct tunit *tunit = data;
    unsigned long dot;
    struct stmt *stmt;

    /* if it's not .text-like then we have nothing to do (for now) */
    if (section->sh_type != SHT_PROGBITS
	|| section->sh_flags != (SHF_ALLOC | SHF_EXECINSTR))
	return 0;

    section->dot = (section->dot + 3) & ~(unsigned long)3;

    section->image_words = malloc((section->dot / 4) * sizeof(pdp10_uint36_t));
    if (!section->image_words) {
	fprintf(stderr, "%s: %s: failed to allocate %zu bytes for text image: %s\n",
		tunit->progname, __FUNCTION__, (section->dot / 4) * sizeof(pdp10_uint36_t), strerror(errno));
	return -1;
    }

    dot = 0;
    for (stmt = section->head; stmt; stmt = stmt->next) {
	switch (stmt->tag) {
	case S_LABEL:
	{
	    struct symbol *symbol;

	    symbol = tunit_symbol_lookup(tunit, stmt->u.symbol.name);
	    if (!symbol)
		return -1;

	    if (symbol->section != section
		|| !symbol->defined
		|| symbol->st_value != dot)
		return -1;

	    break;
	}
	case S_INSN:
	{
	    if (dot >= section->dot) {
		fprintf(stderr, "%s: %s: internal error: text image overflow\n", tunit->progname, __FUNCTION__);
		return -1;
	    }
	    section->image_words[dot / 4] =
		((pdp10_uint36_t)(stmt->u.insn.opcode & 0x1FF) << (36 - 9)
		 | ((stmt->u.insn.accumulator & 0xF) << (36 - 13))
		 | ((stmt->u.insn.at & 1) << (36 - 14))
		 | ((stmt->u.insn.indexreg & 0xF) << (36 - 18))
		 | (stmt->u.insn.address & PDP10_UINT18_MAX));
	    dot += 4;
	    break;
	}
	default:
	    break;
	}
    }

    if (dot != section->dot) {
	fprintf(stderr, "%s: %s: internal error: text image size mismatch\n", tunit->progname, __FUNCTION__);
	return -1;
    }

    return 0;
}

int assemble(struct tunit *tunit)
{
    return hashtab_enumerate(&tunit->sections, assemble_section, tunit);
}
