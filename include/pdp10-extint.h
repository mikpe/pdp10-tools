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

typedef struct {
    pdp10_uint9_t x[2];
} pdp10_ext_uint18_t;

void pdp10_uint18_to_ext(pdp10_uint18_t val, pdp10_ext_uint18_t *ext);
pdp10_uint18_t pdp10_uint18_from_ext(const pdp10_ext_uint18_t *ext);

typedef struct {
    pdp10_uint9_t x[4];
} pdp10_ext_uint36_t;

void pdp10_uint36_to_ext(pdp10_uint36_t val, pdp10_ext_uint36_t *ext);
pdp10_uint36_t pdp10_uint36_from_ext(const pdp10_ext_uint36_t *ext);

#endif /* PDP10_EXTINT_H */
