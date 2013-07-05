/*
 * pdp10-extint.h
 *
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
