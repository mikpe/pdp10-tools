/*
 * pdp10-ea.h -- PDP10 Effective-Address Calculation
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
#ifndef PDP10_EA_H
#define PDP10_EA_H

#include "pdp10-core.h"

/* This performs Effective Address Calculation, but not Instruction Fetch
 * or looping on XCT instructions; the caller is assumed to handle that.
 * Returns (uint64_t)-1 on failure (page fault), otherwise a pdp10_vaddr_t
 * (30 bits) and a 1-bit local(0)/global(1) flag in the MSB (2^30).
 */
uint64_t pdp10_ea(struct pdp10_cpu *cpu, uint64_t MB);

static inline int pdp10_ea_is_page_fault(uint64_t ea)
{
    return ea == (uint64_t)-1;
}

static inline int pdp10_ea_is_global(uint64_t ea)
{
    return ea & (1UL << 30);
}

#endif /* PDP10_EA_H */
