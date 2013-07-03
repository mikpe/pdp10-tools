/*
 * pdp10-inttypes.h
 *
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
