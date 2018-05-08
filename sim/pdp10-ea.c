/*
 * pdp10-ea.c -- PDP10 Effective-Address Calculation
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

#include "pdp10-core.h"
#include "pdp10-arith.h"

/* c.f. Figure 1.9 in the Toad-1 Architecture Manual */
static
uint64_t pdp10_section_zero_ea(struct pdp10_cpu *cpu, uint64_t MB)
{

    /* Instruction Fetch (not done here)
     *
     * MB := C(PC)
     * IR := MB_<0:12>
     */

    for (;;) {
	uint32_t Y = BITS36(MB, 18, 35); /* Y := MB_<18:35> */
	unsigned int X = BITS36(MB, 14, 17); /* X := MB_<14:17> */
	unsigned int I = BIT36(MB, 13); /* I := MB_<13> */
	uint32_t E; /* 18 bits */

	if (X == 0) {
	    E = Y;
	} else {
	    E = BITS36LOW18(Y + BITS36LOW18(cpu->AC[X])); /* E := Y + C(X)_<18:35> */
	}

	if (I == 0) {
	    return E;
	}

	/* MB := C(E), but handle ACs */
	if (E <= 017) {
	    MB = cpu->AC[E];
	} else {
	    MB = pdp10_vmem_read(&cpu->vmem, E);
	    if (MB == (uint64_t)-1)
		return MB; /* propagate page fault */
	}
    }
}

/* c.f. Figure 1.11 in the Toad-1 Architecture Manual */
static
uint64_t pdp10_extended_ea(struct pdp10_cpu *cpu, uint64_t MB)
{
    uint32_t E; /* 30 bits */

    /* Instruction Fetch (not done here)
     * 
     * if PC_<18:31> == 0 then MB := C(PC_<32:35>)
     * else MB := C(PC_<6:35>)
     * IR := MB_<0:12>
     */
    E = BITS36(cpu->PC, 6, 17) << 18; /* E_<6:17> := PC_<6:17> */

    /* Local-Format Address Word */
    /* A Local Address is in the same section as this Address Word */

    for (;;) {
	unsigned int I = BIT36(MB, 13); /* I := MB_<13> */
	{
	    uint32_t Y = BITS36(MB, 18, 35); /* Y_<18:35> := MB_<18:35> */
	    unsigned int X = BITS36(MB, 14, 17); /* X := MB_<14:17> */
	    /* Indexed Address? Test X field. */
	    if (X == 0) {
		/* No Indexing */
		E = BITS36(E, 6, 17) | Y; /* E_<18:35> := Y_<18:35> */
	    } else {
		/* X field != 0 */
		pdp10_uint36_t CX = cpu->AC[X];
		/* Test Section Number in E_<6:17> */
		if (BITS36(E, 6, 17) == 0) { /* Section 0 */
		    /* Local Index */
		    E = BITS36(E, 6, 17) | BITS36LOW18(CX + Y); /* E_<18:35> := C(X)_<18:35> + Y_<18:35> */
		} else {
		    /* Section != 0 */
		    /* Test C(X). Global Index when (C(X)_<0> == 0) && (C(X)_<6:17> != 0) */
		    if (!(BIT36(CX, 0) == 0 && BITS36(CX, 6, 17) != 0)) {
			/* Local Index */
			E = BITS36(E, 6, 17) | BITS36LOW18(CX + Y); /* E_<18:35> := C(X)_<18:35> + Y_<18:35> */
		    } else {
			/* Global Index */
			Y = pdp10_sext_uint18(Y); /* Y_<6:17> := 07777 * Y_<18> */
			E = BITS36((CX + Y), 6, 35); /* E_<6:35> := C(X)_<6:35> + Y_<6:35> */
		    }
		}
	    }
	}

	for (;;) {
	    /* Indirect Addressing?  Test I bit. */
	    if (I == 0) { /* Done! */
		/* E is the Effective Address */
		/* The "XCT Continues" loop is implemented by the caller. */
		return E;
	    }
	    /* Fetch the Indirect Word: MB := C(E) */
	    /* FIXME: handle ACs? */
	    MB = pdp10_vmem_read(&cpu->vmem, E);
	    if (MB == (uint64_t)-1)
		return MB; /* propagate page fault */
	    /* Non-Zero Section? Test E_<6:17> */
	    if (BITS36(E, 6, 17) == 0) {
		/* Section 0 */
		break;
	    } else {
		/* Section != 0 */
		/* Decode Indirect Word MB_<0:1> */
		switch (BITS36(MB, 0, 1)) {
		case 2:
		    /* Local Indirect */
		    break;
		case 3:
		    /* Page Failure */
		    return (uint64_t)-1;
		case 0:
		case 1: {
		    /* Global Indirect Word */
		    uint32_t Y = BITS36(MB, 6, 35); /* Y := MB_<6:35> */
		    unsigned int X = BITS36(MB, 2, 5); /* X := MB_<2:5> */
		    I = BIT36(MB, 1); /* I := MB_<1> */
		    /* Indexed Address? Test X field. */
		    if (X == 0) {
			E = Y; /* E_<6:35> := Y_<6:35> */
		    } else {
			E = BITS36((cpu->AC[X] + Y), 6, 35); /* E_<6:35> := C(X)_6:35 + Y_<6:35> */
		    }
		    continue;
		}
		}
	    }
	    break;
	}
    }
}

static
int pdp10_section_zero_p(struct pdp10_cpu *cpu)
{
    /* My interpretation is that the choice of EA procedure is only based on
     * whether the CPU is extended or not, so an extended CPU should always
     * use the extended EA procedure, regardless of which section PC is in.
     */
    return 0;
}

uint64_t pdp10_ea(struct pdp10_cpu *cpu, uint64_t MB)
{
    if (pdp10_section_zero_p(cpu))
	return pdp10_section_zero_ea(cpu, MB);
    else
	return pdp10_extended_ea(cpu, MB);
}
