/*
 * assemble.c
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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "assemble.h"
#include "hashtab.h"
#include "tunit.h"
#include "pdp10-extint.h"

static int assemble_section(struct hashnode *hashnode, void *data)
{
    struct section *section = container_of(hashnode, struct section, hashnode);
    struct tunit *tunit = data;
    unsigned long dot;
    struct stmt *stmt;

    /* if it's not .text-like then we have nothing to do (for now) */
    if (section->sh_type != SHT_PROGBITS
	|| section->sh_flags != (SHF_ALLOC | SHF_EXECINSTR))
	return 0;

    section->dot = (section->dot + 3) & ~(unsigned long)3;

    section->image = malloc(section->dot * sizeof(pdp10_uint9_t));
    if (!section->image) {
	fprintf(stderr, "%s: %s: failed to allocate %zu bytes for text image: %s\n",
		tunit->progname, __FUNCTION__, section->dot * sizeof(pdp10_uint9_t), strerror(errno));
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
	    pdp10_uint36_t word;
	    struct pdp10_ext_uint36 ext36;
	    unsigned int i;

	    if (dot >= section->dot) {
		fprintf(stderr, "%s: %s: internal error: text image overflow\n", tunit->progname, __FUNCTION__);
		return -1;
	    }
	    word =
		((pdp10_uint36_t)(stmt->u.insn.opcode & 0x1FF) << (36 - 9)
		 | ((stmt->u.insn.accumulator & 0xF) << (36 - 13))
		 | ((stmt->u.insn.at & 1) << (36 - 14))
		 | ((stmt->u.insn.indexreg & 0xF) << (36 - 18))
		 | (stmt->u.insn.address & PDP10_UINT18_MAX));
	    pdp10_uint36_to_ext(word, &ext36);
	    for (i = 0; i < 4; ++i)
		section->image[dot + i] = ext36.nonet[i];
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
