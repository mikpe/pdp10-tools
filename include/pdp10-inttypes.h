/*
 * pdp10-inttypes.h -- inttypes.h clone for PDP10
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
 * Provide format conversions for 18 and 36-bit integers.
 * For 9-bit integers, pdp_uint9_t, just use the regular
 * int-sized d/o/u/x formats.
 */
#ifndef PDP10_INTTYPES_H
#define PDP10_INTTYPES_H

#include <inttypes.h>
#include "pdp10-stdint.h"

#if defined(UINT18_MAX)
#define PDP10_PRId18	PRId18
#define PDP10_PRIo18	PRIo18
#define PDP10_PRIu18	PRIu18
#define PDP10_PRIx18	PRIx18
#else
#define PDP10_PRId18	PRId32
#define PDP10_PRIo18	PRIo32
#define PDP10_PRIu18	PRIu32
#define PDP10_PRIx18	PRIx32
#endif

#if defined(UINT36_MAX)
#define PDP10_PRId36	PRId36
#define PDP10_PRIo36	PRIo36
#define PDP10_PRIu36	PRIu36
#define PDP10_PRIx36	PRIx36
#else
#define PDP10_PRId36	PRId64
#define PDP10_PRIo36	PRIo64
#define PDP10_PRIu36	PRIu64
#define PDP10_PRIx36	PRIx64
#endif

#endif /* PDP10_INTTYPES_H */
