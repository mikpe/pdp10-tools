/*
 * pdp10-extint.h
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
 * Provide types and procedures for converting 18 and 36-bit integers
 * to and from arrays of 9-bit bytes (nonets).  Use these together with
 * pdp10_fread() and pdp10_fwrite() to convert 18 and 36-bit integers
 * between host-level and file-level binary representations.
 */
#ifndef PDP10_EXTINT_H
#define PDP10_EXTINT_H

#include "pdp10-stdint.h"

struct pdp10_ext_uint18 {
    pdp10_uint9_t nonet[2];
};

void pdp10_uint18_to_ext(pdp10_uint18_t val, struct pdp10_ext_uint18 *ext);
pdp10_uint18_t pdp10_uint18_from_ext(const struct pdp10_ext_uint18 *ext);

struct pdp10_ext_uint36 {
    pdp10_uint9_t nonet[4];
};

void pdp10_uint36_to_ext(pdp10_uint36_t val, struct pdp10_ext_uint36 *ext);
pdp10_uint36_t pdp10_uint36_from_ext(const struct pdp10_ext_uint36 *ext);

#endif /* PDP10_EXTINT_H */
