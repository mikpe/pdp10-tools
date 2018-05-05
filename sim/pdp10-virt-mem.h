/*
 * pdp10-virt-mem.h -- virtual memory simulation for PDP10
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
#ifndef PDP10_VIRT_MEM_H
#define PDP10_VIRT_MEM_H

#include <stdint.h>
#include "pdp10-stdint.h"

/*
 * A fully-extended PDP10 has 30-bit addresses, divided into
 * - 12 bit section number (4096 sections)
 * - 9 bit page number (512 pages per section)
 * - 9 bit page offset (512 36-bit words per page)
 *
 * Each 36-bit word is currently stored in the low 36 bits of an uint64_t.
 *
 * (Other options that pack the 36-bit words more tightly have drawbacks
 * such as more complex address arithmetic, requiring multiple loads or stores
 * per access, or requiring writes to perform read-modify-write operations.)
 */

struct pdp10_page {
    uint64_t mword[512];
};

struct pdp10_section {
    struct pdp10_page *page[512];
};

struct pdp10_vmem {
    struct pdp10_section *section[4096];
};

/* Returns non-zero on failure, zero on success. */
int pdp10_vmem_init(struct pdp10_vmem *vmem);

/* Returns (uint64_t)-1 on failure (page fault), a pdp10_uint36_t word on success. */
uint64_t pdp10_vmem_read(struct pdp10_vmem *vmem, uint32_t address);

/* Returns non-zero on failure, zero on success. */
int pdp10_vmem_write(struct pdp10_vmem *vmem, uint32_t address, pdp10_uint36_t word);

#endif /* PDP10_VIRT_MEM_H */
