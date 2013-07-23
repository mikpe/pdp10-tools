/*
 * assemble.c
 */
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "assemble.h"
#include "input.h"

static struct aunit_symbol *symbol(const char *progname, struct aunit *aunit, const char *name)
{
    struct aunit_symbol *sym;

    for (sym = aunit->symbols; sym; sym = sym->next)
	if (strcmp(name, sym->name) == 0)
	    return sym;

    sym = malloc(sizeof *sym);
    if (!sym) {
	fprintf(stderr, "%s: failed to allocate %zu bytes for aunit_symbol: %s\n", progname, sizeof *sym, strerror(errno));
	return NULL;
    }

    sym->name = name;
    sym->text_offset = 0;
    sym->is_global = 0;
    sym->is_defined = 0;

    sym->next = aunit->symbols;
    aunit->symbols = sym;

    return sym;
}

int assemble(const char *progname, struct iunit *iunit, struct aunit *aunit)
{
    struct stmt *stmt;
    struct aunit_symbol *sym;
    pdp10_uint36_t i, n;

    aunit->text_words = NULL;
    aunit->text_nr_words = 0;
    aunit->symbols = NULL;

    n = 0;
    for (stmt = iunit->text.head; stmt; stmt = stmt->next) {
	switch (stmt->tag) {
	case S_DOT_GLOBL:
	    sym = symbol(progname, aunit, stmt->u.symbol.name);
	    if (!sym)
		return -1;
	    sym->is_global = 1;
	    break;
	case S_LABEL:
	    (void)symbol(progname, aunit, stmt->u.symbol.name);
	    break;
	case S_INSN:
	    ++n;
	    break;
	default:
	    break;
	}
    }

    aunit->text_nr_words = n;
    aunit->text_words = malloc(n * sizeof(pdp10_uint36_t));
    if (!aunit->text_words) {
	fprintf(stderr, "%s: failed to allocate %zu bytes for text image: %s\n", progname, n * sizeof(pdp10_uint36_t), strerror(errno));
	return -1;
    }

    i = 0;
    for (stmt = iunit->text.head; stmt; stmt = stmt->next) {
	switch (stmt->tag) {
	case S_LABEL:
	    sym = symbol(progname, aunit, stmt->u.symbol.name);
	    if (!sym)
		return -1;
	    sym->is_defined = 1;
	    sym->text_offset = i * 4;
	    break;
	case S_INSN:
	    if (i >= n) {
		fprintf(stderr, "%s: internal error: text image overflow\n", progname);
		return -1;
	    }
	    aunit->text_words[i] =
		((pdp10_uint36_t)(stmt->u.insn.opcode & 0x1FF) << (36 - 9)
		 | ((stmt->u.insn.accumulator & 0xF) << (36 - 13))
		 | ((stmt->u.insn.at & 1) << (36 - 14))
		 | ((stmt->u.insn.indexreg & 0xF) << (36 - 18))
		 | (stmt->u.insn.address & PDP10_UINT18_MAX));
	    ++i;
	    break;
	default:
	    break;
	}
    }
    if (i != n) {
	fprintf(stderr, "%s: internal error: text image size mismatch\n", progname);
	return -1;
    }

    return 0;
}
