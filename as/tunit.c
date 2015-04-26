/*
 * tunit.c
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hashtab.h"
#include "tunit.h"

/*
 * String hash function, used for both section names and symbol names.
 */
static uintptr_t string_hash(const char *string)
{
    const unsigned char *s;
    uintptr_t h;
    unsigned char c;

    s = (const unsigned char*)string;
    h = 0;

    while ((c = *s++) != '\0')
	h = (h << 5) + h + c;

    return h;
}

/*
 * Sections hash table.
 */
static struct section *section_from_hashnode(const struct hashnode *hashnode)
{
    /* hashnode is first in section, so no need to mess with offsetof */
    return (struct section*)hashnode;
}

static int section_eq(const struct hashnode *hashnode, const void *data)
{
    return strcmp(section_from_hashnode(hashnode)->name, (const char*)data) == 0;
}

void section_init(struct section *section, const char *name)
{
    section->hashnode.hashval = 0;
    section->hashnode.next = NULL;

    section->name = name;
    section->head = NULL;
    section->tailptr = &section->head;
    section->dot = 0;
    section->output = NULL;
    section->image = NULL;
    section->st_shndx = 0;
    section->sh_name = 0;
    section->sh_type = SHT_NULL;
    section->sh_flags = 0;
    section->sh_offset = 0;
    section->sh_link = 0;
    section->sh_addralign = 1;
    section->sh_entsize = 0;
}

struct section *tunit_section_enter(struct tunit *tunit, const char *name)
{
    struct section *section;
    uintptr_t hashval;

    hashval = string_hash(name);
    section = section_from_hashnode(hashtab_lookup(&tunit->sections, hashval, name));
    if (section)
	return section;

    section = malloc(sizeof *section);
    if (!section) {
	fprintf(stderr, "%s: %s: malloc(%zu) failed: %s\n", tunit->progname, __FUNCTION__, sizeof *section, strerror(errno));
	return NULL;
    }

    section_init(section, name);

    section->hashnode.hashval = hashval;
    if (hashtab_insert(&tunit->sections, &section->hashnode) < 0)
	return NULL;

    return section;
}

static int sections_init(struct tunit *tunit)
{
    struct section *section;

    if (hashtab_init(&tunit->sections, 2, section_eq) < 0)
	return -1;

    section = tunit_section_enter(tunit, ".text");
    if (!section)
	return -1;

    section->sh_type = SHT_PROGBITS;
    section->sh_flags = SHF_ALLOC | SHF_EXECINSTR;
    section->sh_addralign = 4;	/* XXX: PDP10-specific */

    tunit->cursect = section;

    return 0;
}

/*
 * Symbols hash table.
 */
static struct symbol *symbol_from_hashnode(const struct hashnode *hashnode)
{
    /* hashnode is first in symbol, so no need to mess with offsetof */
    return (struct symbol*)hashnode;
}

static int symbol_eq(const struct hashnode *hashnode, const void *data)
{
    return strcmp(symbol_from_hashnode(hashnode)->name, (const char*)data) == 0;
}

struct symbol *tunit_symbol_lookup(struct tunit *tunit, const char *name)
{
    uintptr_t hashval;
    struct symbol *symbol;

    hashval = string_hash(name);
    symbol = symbol_from_hashnode(hashtab_lookup(&tunit->symbols, hashval, name));
    if (!symbol)
	fprintf(stderr, "%s: %s: symbol %s not found\n", tunit->progname, __FUNCTION__, name);
    return symbol;
}

struct symbol *tunit_symbol_enter(struct tunit *tunit, const char *name)
{
    uintptr_t hashval;
    struct symbol *symbol;

    hashval = string_hash(name);
    symbol = symbol_from_hashnode(hashtab_lookup(&tunit->symbols, hashval, name));
    if (symbol)
	return symbol;

    symbol = malloc(sizeof *symbol);
    if (!symbol) {
	fprintf(stderr, "%s: %s: malloc(%zu) failed: %s\n", tunit->progname, __FUNCTION__, sizeof *symbol, strerror(errno));
	return NULL;
    }

    symbol->hashnode.hashval = hashval;
    symbol->name = name;
    symbol->section = NULL;
    symbol->defined = 0;
    symbol->st_value = 0;
    symbol->st_size = 0;
    symbol->st_info = 0;
    symbol->st_name = 0;

    if (hashtab_insert(&tunit->symbols, &symbol->hashnode) < 0)
	return NULL;

    return symbol;
}

static int symbols_init(struct tunit *tunit)
{
    return hashtab_init(&tunit->symbols, 8, symbol_eq);
}

/*
 * Translation unit init and fini ops.
 */
int tunit_init(struct tunit *tunit, const char *progname)
{
    tunit->progname = progname;
    if (sections_init(tunit) < 0)
	return -1;
    if (symbols_init(tunit) < 0)
	return -1;
    return 0;
}

void tunit_fini(struct tunit *tunit)
{
}
