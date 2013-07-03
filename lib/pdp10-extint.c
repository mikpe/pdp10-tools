/*
 * pdp10-extint.c
 *
 * Provide types and procedures for converting 18 and 36-bit integers
 * to and from arrays of 9-bit bytes (nonets).  Use these together with
 * pdp10_fread() and pdp10_fwrite() to convert 18 and 36-bit integers
 * between host-level and file-level binary representations.
 */
#include "pdp10-extint.h"

/*
 * The behaviour of the PDP10's byte pointers implies a big-endian storage model,
 * as does the layout of its 72-bit long integers.
 */

void pdp10_uint18_to_ext(pdp10_uint18_t val, pdp10_ext_uint18_t *ext)
{
    ext->x[0] = (val >> 9) & 0x1FF;
    ext->x[1] = val & 0x1FF;
}

pdp10_uint18_t pdp10_uint18_from_ext(const pdp10_ext_uint18_t *ext)
{
    return
	((pdp10_uint18_t)(ext->x[0] & 0x1FF) << 9)
	| (ext->x[1] & 0x1FF);
}

void pdp10_uint36_to_ext(pdp10_uint36_t val, pdp10_ext_uint36_t *ext)
{
    ext->x[0] = (val >> 27) & 0x1FF;
    ext->x[1] = (val >> 18) & 0x1FF;
    ext->x[2] = (val >> 9) & 0x1FF;
    ext->x[3] = val & 0x1FF;
}

pdp10_uint36_t pdp10_uint36_from_ext(const pdp10_ext_uint36_t *ext)
{
    return
	((pdp10_uint36_t)(ext->x[0] & 0x1FF) << 27)
	| ((pdp10_uint36_t)(ext->x[1] & 0x1FF) << 18)
	| ((pdp10_uint36_t)(ext->x[2] & 0x1FF) << 9)
	| (ext->x[3] & 0x1FF);
}
