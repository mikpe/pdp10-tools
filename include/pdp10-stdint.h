/*
 * pdp10-stdint.h -- stdint.h clone for PDP10
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
/*
 * Provide stdint.h-like type names and macros for 9, 18, and 36-bit unsigned
 * integer types.
 *
 * Standard uint<N>_t types must not contain any any extraneous bits, but that
 * cannot be guaranteed for these 9, 18, and 36-bit types when they are embedded
 * in larger 16, 32, and 64-bit host types.  For arithmetic on these types, use
 * the operations provided by pdp10-arith.h.
 *
 * Do not use these 18 or 36-bit types for file-level binary data structures,
 * instead use the pdp10-extint.h and pdp10-stdio.h facilities to explicitly
 * convert between file-level and host-level binary data structures.
 */
#ifndef PDP10_STDINT_H
#define PDP10_STDINT_H

#include <stdint.h>

#if	defined(UINT9_MAX)

typedef uint9_t			pdp10_uint9_t;
#define PDP10_UINT9_MAX		UINT9_MAX
#define PDP10_UINT9_C(c)	UINT9_C(c)

#else	/* !UINT9_MAX */

typedef uint16_t		pdp10_uint9_t;
#define PDP10_UINT9_MAX		((1U << 9) - 1)
#define PDP10_UINT9_C(c)	c

#endif	/* !UINT9_MAX */

#if	defined(UINT18_MAX)

typedef uint18_t		pdp10_uint18_t;
#define PDP10_UINT18_MAX	UINT18_MAX
#define PDP10_UINT18_C(c)	UINT18_C(c)

#else	/* !UINT18_MAX */

typedef uint32_t		pdp10_uint18_t;
#define PDP10_UINT18_MAX	((1UL << 18) - 1)
#define PDP10_UINT18_C(c)	c ## U

#endif	/* !UINT18_MAX */

#if	defined(UINT36_MAX)

typedef uint36_t		pdp10_uint36_t;
#define PDP10_UINT36_MAX	UINT36_MAX
#define PDP10_UINT36_C(c)	UINT36_C(c)

typedef int36_t			pdp10_int36_t;
#define PDP10_INT36_MAX		INT36_MAX
#define PDP10_INT36_C(c)	INT36_C(c)

#else	/* !UINT36_MAX */

typedef uint64_t		pdp10_uint36_t;
#define PDP10_UINT36_MAX	((1ULL << 36) - 1)
#define PDP10_UINT36_C(c)	c ## ULL

typedef int64_t			pdp10_int36_t;
#define PDP10_INT36_MAX		((1LL << (36 - 1)) - 1)
#define PDP10_INT36_C(c)	c ## LL

#endif	/* !UINT36_MAX */

#endif /* PDP10_STDINT_H */
