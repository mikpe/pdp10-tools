/*
 * pdp10-arith.h
 *
 * Provide functions for performing arithmetic operations on PDP10 integer types.
 * Currently only 36-bit signed operations are supported.
 */
#ifndef PDP10_ARITH_H
#define PDP10_ARITH_H

#include "pdp10-stdint.h"

/* Zero-extend a pdp10_{u,}int36_t to the full width of its representation type.
 * Use this to prepare operands before unsigned operations, or to correct results
 * after signed operations.
 */
static inline pdp10_uint36_t pdp10_zext_uint36(pdp10_uint36_t x)
{
    return x & PDP10_UINT36_MAX;
}

/* Sign-extend a pdp10_int36_t to the full width of its representation type.
 * Use this to prepare operands before signed operations.
 *
 * Based on the following trick for sign-extending an octet x: ((x & 0xff) ^ 0x80) - 0x80,
 * c.f. <http://sourceware.org/ml/binutils/2001-05/msg00093.html>.
 */
static inline pdp10_int36_t pdp10_sext_int36(pdp10_uint36_t x)
{
    const pdp10_uint36_t PDP10_UINT36_SBIT = ~(PDP10_UINT36_MAX >> 1) & PDP10_UINT36_MAX;

    return ((x & PDP10_UINT36_MAX) ^ PDP10_UINT36_SBIT) - PDP10_UINT36_SBIT;
}

static inline pdp10_uint36_t pdp10_neg_int36(pdp10_uint36_t x)
{
    return pdp10_zext_uint36(-pdp10_sext_int36(x));
}

static inline pdp10_uint36_t pdp10_not_int36(pdp10_uint36_t x)
{
    return pdp10_zext_uint36(~pdp10_sext_int36(x));
}

static inline pdp10_uint36_t pdp10_add_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_zext_uint36(pdp10_sext_int36(x) + pdp10_sext_int36(y));
}

static inline pdp10_uint36_t pdp10_sub_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_zext_uint36(pdp10_sext_int36(x) - pdp10_sext_int36(y));
}

static inline pdp10_uint36_t pdp10_mul_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_zext_uint36(pdp10_sext_int36(x) * pdp10_sext_int36(y));
}

static inline pdp10_uint36_t pdp10_div_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_zext_uint36(pdp10_sext_int36(x) / pdp10_sext_int36(y));
}

static inline pdp10_uint36_t pdp10_rem_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_zext_uint36(pdp10_sext_int36(x) % pdp10_sext_int36(y));
}

static inline pdp10_uint36_t pdp10_lsl_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_zext_uint36(pdp10_zext_uint36(x) << pdp10_zext_uint36(y));
}

static inline pdp10_uint36_t pdp10_lsr_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_zext_uint36(pdp10_zext_uint36(x) >> pdp10_zext_uint36(y));
}

static inline pdp10_uint36_t pdp10_asr_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_zext_uint36(pdp10_sext_int36(x) >> pdp10_zext_uint36(y));
}

static inline pdp10_uint36_t pdp10_or_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return (pdp10_zext_uint36(x) | pdp10_zext_uint36(y));
}

static inline pdp10_uint36_t pdp10_and_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return (pdp10_zext_uint36(x) & pdp10_zext_uint36(y));
}

static inline pdp10_uint36_t pdp10_xor_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return (pdp10_zext_uint36(x) ^ pdp10_zext_uint36(y));
}

static inline int pdp10_eq_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_zext_uint36(x) == pdp10_zext_uint36(y);
}

static inline int pdp10_ne_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_zext_uint36(x) != pdp10_zext_uint36(y);
}

static inline int pdp10_lt_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_sext_int36(x) < pdp10_sext_int36(y);
}

static inline int pdp10_gt_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_sext_int36(x) > pdp10_sext_int36(y);
}

static inline int pdp10_ge_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_sext_int36(x) >= pdp10_sext_int36(y);
}

static inline int pdp10_le_int36(pdp10_uint36_t x, pdp10_uint36_t y)
{
    return pdp10_sext_int36(x) <= pdp10_sext_int36(y);
}

static inline int pdp10_nonzero_int36(pdp10_uint36_t x)
{
    return pdp10_zext_uint36(x) != 0;
}

#endif /* PDP10_ARITH_H */
