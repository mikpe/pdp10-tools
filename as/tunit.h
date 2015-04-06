/*
 * tunit.h
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
#ifndef TUNIT_H
#define TUNIT_H

#include "pdp10-elf36.h"
#include "hashtab.h"

/*
 * A directive, label, or instruction is parsed to a statement, which is
 * either interpreted immediately or appended to the representation of the
 * current section.
 */

enum stmt_tag {
    /* directives */
    S_DOT_FILE,
    S_DOT_GLOBL,
    S_DOT_SIZE,
    S_DOT_TEXT,
    S_DOT_TYPE_FUNCTION,
    /* non-directives */
    S_LABEL,
    S_INSN,
};

struct stmt {
    struct stmt *next;
    enum stmt_tag tag;
    union {
	struct {	/* S_DOT_FILE */
	    const char *text;	/* XXX: should be pdp10_uint9_t* */
	} string;
	struct {	/* S_DOT_GLOBL, S_LABEL, S_DOT_SIZE, S_DOT_TYPE_FUNCTION */
	    const char *name;
	} symbol;
	struct {	/* S_INSN */
	    unsigned int opcode;
	    unsigned int accumulator;
	    int at;
	    unsigned int address;	/* XXX: relocatable expr */
	    unsigned int indexreg;
	} insn;
    } u;
};

struct section {
    struct hashnode hashnode;
    const char *name;
    struct stmt *head, **tailptr;
    unsigned long dot;
    pdp10_uint9_t *image;		/* assigned during assembly */
    Elf36_Word st_shndx;		/* assigned during output */
    Elf36_Word sh_name;			/* assigned during output */
    Elf36_Word sh_type;
    Elf36_Word sh_flags;
    Elf36_Word sh_offset;		/* assigned during output */
    Elf36_Word sh_link;			/* assigned during output */
    Elf36_Word sh_addralign;
    Elf36_Word sh_entsize;		/* assigned during output */
};

void section_init(struct section *section, const char *name);

struct symbol {
    struct hashnode hashnode;
    const char *name;
    struct section *section;	/* NULL if UNDEF or ABS, otherwise non-NULL */
    unsigned char defined;
    Elf36_Addr st_value;
    Elf36_Word st_size;
    Elf36_Uchar st_info;
    Elf36_Word st_name;			/* assigned during output */
};

/*
 * The translation unit object is the top-level container for the
 * representation of the sections, other information collected from
 * the input, and information synthesized during assembly.
 */

struct tunit {
    const char *progname;
    struct hashtab sections;
    struct section *cursect;
    struct hashtab symbols;
};

int tunit_init(struct tunit *tunit, const char *progname);
void tunit_fini(struct tunit *tunit);
struct section *tunit_section_enter(struct tunit *tunit, const char *name);
struct symbol *tunit_symbol_lookup(struct tunit *tunit, const char *name);
struct symbol *tunit_symbol_enter(struct tunit *tunit, const char *name);

#endif /* TUNIT_H */
