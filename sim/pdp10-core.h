/*
 * pdp10-core.h -- core definitions for PDP10 simulation
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
#ifndef PDP10_CORE_H
#define PDP10_CORE_H

#include <assert.h>
#include "pdp10-stdint.h"
#include "pdp10-virt-mem.h"

/*
 * Words are 36 bits wide.  The bits are numbered 0-35, left to right (most
 * significant to least significant).  The left (more significant) half word
 * is bits 0-17, and the right (less significant) half word is bits 18-36.
 * Numbers use twos-complement representation, with bit 0 as the sign.
 *
 * A double word is two adjacent words treated as a single 72-bit entity, where
 * the word with the lower address is the more significant.
 *
 * Bits are numbered in decimal, most other numbers are in octal, in particular
 * accumulator numbers, addresses, and instruction codes are in octal.
 *
 * A virtual address on a fully extended processor is 30 bits, composed of a 12
 * bit section number (upper 12 bits) and an 18-bit section offset (lower 18
 * bits).  Paging hardware divides each section into 512 pages of 512 words each.
 * The address bits are numbered according to the right-justified position of an
 * address in a word.  Thus bits 6-17 contain the section number, and bits 18-35
 * the section offset.
 *
 * Single-section processors (PDP6, KA10, KI10, non-extended KL10, KS10) have no
 * section number in their virtual addresses, while the extended KL10 (KL10B)
 * only has 5 bit section numbers (bits 13-17).
 *
 * The program counter register, PC, contains a virtual address.  Incrementing PC
 * increments its segment offset without propagating any carry into the section
 * number.
 *
 * Instructions are words.  In the basic instruction format, bits 0-8 specify the
 * operation, and bits 9-12 (A) address an accumulator.  The rest of the word
 * specifies how to calculate the effective address (E).  Bit 13 (I) specifies the
 * type of addressing (indirect or not), bits 14-17 (X) specify an index register,
 * and bits 18-35 (Y) specify a memory location.  Some instructions use bits 9-12
 * to address flags or as extension of the instruction code.
 *
 * If an instruction does not use some part of the instruction word, that part is
 * reserved and MUST BE ZERO, unless otherwise specified.
 */

/*
 * BITS36(X, LEFT, RIGHT)
 *
 * Extract bits LEFT through RIGHT (inclusive) from a 36-bit number X.
 * Requires 0 <= LEFT <= RIGHT <= 35.
 */
static inline pdp10_uint36_t BITS36(pdp10_uint36_t X, unsigned int LEFT, unsigned int RIGHT)
{
    assert(0 <= LEFT && LEFT <= RIGHT && RIGHT <= 35);
    return (X >> (35 - RIGHT)) & ((1UL << (1 + RIGHT - LEFT)) - 1);
}

static inline uint32_t BITS36LOW18(pdp10_uint36_t X)
{
    return BITS36(X, 18, 35);
}

/*
 * BIT36(X, BIT)
 *
 * Extract bit BIT from a 36-bit number X.
 * Requires 0 <= BIT <= 35.
 */
static inline unsigned int BIT36(pdp10_uint36_t X, unsigned int BIT)
{
    return BITS36(X, BIT, BIT);
}

typedef uint32_t pdp10_vaddr_t; /* 30 bits: 12-bit section number, 18-bit section offset */

struct pdp10_cpu {
    pdp10_vaddr_t PC;
    pdp10_uint36_t AC[017]; /* copy of ACS[CAB] */
    pdp10_uint36_t ACS[8][017];
    unsigned int flags:13;
    unsigned int CAB:3; /* Current AC Block */
    /*
     * Previous Context
     */
    unsigned int PCS:12; /* Previous Context Section */
    unsigned int PCU:1; /* Previous Context User */
    unsigned int PAB:3; /* Previous AC Block */
    /*
     * Memory
     */
    struct pdp10_vmem vmem;
};

#endif /* PDP10_CORE_H */
