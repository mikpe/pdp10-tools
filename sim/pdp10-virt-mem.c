/*
 * pdp10-virt-mem.c -- virtual memory simulation for PDP10
 * Copyright (C) 2018  Mikael Pettersson
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

#include <string.h>
#include "pdp10-virt-mem.h"

int pdp10_vmem_init(struct pdp10_vmem *vmem)
{
    memset(vmem, 0, sizeof *vmem);
    return 0;
}

static inline pdp10_uint36_t pdp10_mword_read(uint64_t *mword)
{
    return *mword;
}

static inline void pdp10_mword_write(uint64_t *mword, pdp10_uint36_t word)
{
    /* TODO: assert high bits are clear */
    *mword = word;
}

static inline uint64_t *deref(struct pdp10_vmem *vmem, uint32_t address)
{
    struct pdp10_section *section;
    struct pdp10_page *page;

    section = vmem->section[address >> 18];
    if (section) {
	page = section->page[(address >> 9) & ((1 << 9) - 1)];
	if (page)
	    return &page->mword[address & ((1 << 9) - 1)];
    }
    return NULL;
}

uint64_t pdp10_vmem_read(struct pdp10_vmem *vmem, uint32_t address)
{
    uint64_t *mword;

    mword = deref(vmem, address);
    if (!mword)
	return (uint64_t)-1;

    return pdp10_mword_read(mword);
}

int pdp10_vmem_write(struct pdp10_vmem *vmem, uint32_t address, pdp10_uint36_t word)
{
    uint64_t *mword;

    mword = deref(vmem, address);
    if (!mword)
	return -1;

    pdp10_mword_write(mword, word);
    return 0;
}
