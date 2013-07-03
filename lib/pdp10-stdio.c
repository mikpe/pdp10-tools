/*
 * pdp10-stdio.h
 *
 * Provide stdio.h-like interface for I/O to and from files with 9-bit logical bytes (nonets),
 * represented by native files with 8-bit physical bytes (octets).
 *
 * Theory of operation:
 *
 * - The state of a pdp10 file is composed of: a FILE* for an underlying octet file,
 *   the current read/write position in the nonet file, a 16-bit shift register buffering
 *   partial octets (writes) or partial nonets (reads), a counter indicating the number
 *   of bits in the shift register (which may be negative after a call to pdp10_fseek),
 *   and a boolean flag indicating if there may be unwritten buffered output.
 *
 * - Write streams: pdp10_fputc adds 9 bits to shiftreg and 9 to shiftreg_nr_bits, then each
 *   complete group of 8 bits in shiftreg is shifted out and written to the octet file.
 *   Between pdp10_fputc calls shiftreg contains between 0 and 7 bits, inclusive, during a
 *   pdp10_fputc it may temporarily contain up to 7+9 == 16 bits.
 *
 * - Read streams: pdp10_fgetc reads an octet from the octet file and adds 8 bits to shiftreg
 *   and 8 to shiftreg_nr_bits; this is repeated once more if needed to make shiftreg
 *   contains at least 9 bits.  Then 9 bits are shifted out of shiftreg and returned.
 *   Between pdp10_fgetc calls shiftreg contains between 0 and 7 bits, inclusive, during an
 *   fgetc it may contain up to 8+8 == 16 bits.
 *
 * - An output operation (pdp10_fputc or pdp10_fwrite) may not be directly followed by an
 *   input operation (pdp10_fgetc or pdp10_fread) without an intervening call to pdp10_fflush
 *   or pdp10_fseeko, and an input operation may not be directly followed by an output
 *   operation without an intervening call to pdp10_fseeko, unless the input operation
 *   encountered end-of-file.  (Same restriction as ANSI/ISO C.)
 *
 * - A pdp_fseeko repositions the octet file to the closest octet boundary at or before the
 *   requested nonet boundary, and sets shiftreg_nr_bits to the bit difference, as a number
 *   between 0 and -7, inclusive.  A subsequent pdp10_fgetc or pdp10_fputc detects this
 *   special state and reinitializes shiftreg as appropriate for that I/O direction.
 */
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "pdp10-stdio.h"

struct pdp10_file {
    FILE *octet_fp;
    off_t nonet_pos;		/* current read or write nonet offset */
    unsigned int shiftreg;	/* contains 0 to 16 buffered bits */
    int shiftreg_nr_bits;
    int writing;		/* non-zero if shiftreg may contain pending output data */
};

PDP10_FILE *pdp10_fopen(const char *path, const char *mode)
{
    PDP10_FILE *pdp10fp;

    /* "a+" won't work, and "a" is not yet implemented */
    if (mode[0] == 'a') {
	errno = EINVAL;
	return NULL;
    }

    pdp10fp = malloc(sizeof *pdp10fp);
    if (!pdp10fp)
	return NULL;

    pdp10fp->octet_fp = fopen(path, mode);
    if (!pdp10fp->octet_fp) {
	int oerrno = errno;
	free(pdp10fp);
	errno = oerrno;
	return NULL;
    }

    pdp10fp->nonet_pos = 0;
    pdp10fp->shiftreg = 0;
    pdp10fp->shiftreg_nr_bits = 0;
    pdp10fp->writing = 0;

    return pdp10fp;
}

static int pdp10_flush_buffered_write(PDP10_FILE *pdp10fp)
{
    int octet_ch;

    if (!pdp10fp->writing)
	return 0;

    if (pdp10fp->shiftreg_nr_bits <= 0)
	return 0;

    /* read the next octet which we will partially overwrite */
    if (fseeko(pdp10fp->octet_fp, 0, SEEK_CUR) == -1)
	return EOF;
    octet_ch = fgetc(pdp10fp->octet_fp);

    /* rewind by one octet, or by zero octets if we read EOF above */
    if (fseeko(pdp10fp->octet_fp, octet_ch == EOF ? 0 : -1, SEEK_CUR) == -1)
	return EOF;

    if (octet_ch == EOF)
	octet_ch = 0;

    octet_ch &= (1 << (8 - pdp10fp->shiftreg_nr_bits)) - 1;
    octet_ch |= (pdp10fp->shiftreg << (8 - pdp10fp->shiftreg_nr_bits)) & 0xFF;

    if (fputc(octet_ch, pdp10fp->octet_fp) == EOF)
	return EOF;

    /* rewind by one octet to permit further writes; XXX: this is unnecessary
       when the flush is called from fclose() or fseeko() */
    if (fseeko(pdp10fp->octet_fp, -1, SEEK_CUR) == -1)
	return EOF;

    return 0;
}

int pdp10_fflush(PDP10_FILE *pdp10fp)
{
    if (pdp10_flush_buffered_write(pdp10fp) == EOF)
	return EOF;
    return fflush(pdp10fp->octet_fp);
}

int pdp10_fclose(PDP10_FILE *pdp10fp)
{
    int status;
    FILE *octet_fp;

    status = pdp10_flush_buffered_write(pdp10fp);
    octet_fp = pdp10fp->octet_fp;
    free(pdp10fp);
    if (fclose(octet_fp) == EOF)
	status = EOF;
    return status;
}

static int pdp10_fgetc_one_octet(PDP10_FILE *pdp10fp)
{
    int octet_ch;

    octet_ch = fgetc(pdp10fp->octet_fp);
    if (octet_ch == EOF)
	return -1;	/* incomplete nonets are discarded */

    /* XXX: big-endian conversion */
    pdp10fp->shiftreg = (pdp10fp->shiftreg << 8) | (octet_ch & 0xFF);
    pdp10fp->shiftreg_nr_bits += 8;

    return 0;
}

int pdp10_fgetc(PDP10_FILE *pdp10fp)
{
    uint16_t nonet_ch;

    pdp10fp->writing = 0;

    if (pdp10fp->shiftreg_nr_bits < 9) {
	/*
	 * There are three cases to consider here:
	 *
	 * 1. 1 <= shiftreg_nr_bits <= 8.
	 *    We have a partially filled nonet in the buffer.
	 *    We'll read one octet.
	 *
	 * 2. shiftreg_nr_bits == 0.
	 *    The last read took us to a 72-bit boundary, emptying the buffer.
	 *    We'll read two octets.
	 *
	 * 3. -7 <= shiftreg_nr_bits <= -1.
	 *    An fseek placed octet_pos 1 to 7 bits before nonet_pos.
	 *    We'll read two octets, but the first -shiftreg_nr_bits
	 *    bits will be discarded.
	 *
	 * Either way we read one or two octets, append them to the buffer,
	 * and increment shiftreg_nr_bits by the number of bits read.
	 *
	 * An EOF during read permits the next operation to be a write, without
	 * an intervening fflush() or fseeko().  Therefore we must reposition
	 * octet_pos before nonet_pos if an EOF occurs here.
	 */
	if (pdp10_fgetc_one_octet(pdp10fp) < 0
	    || (pdp10fp->shiftreg_nr_bits < 9
		&& pdp10_fgetc_one_octet(pdp10fp) < 0)) {
	    if (pdp10fp->shiftreg_nr_bits > 0) {
		/* if this fseeko() fails then presumably subsequent fseeko()s
		   will also fail; if not, then data may not be read or written
		   where we expect it to be XXX */
		(void)fseeko(pdp10fp->octet_fp, -1, SEEK_CUR);
		pdp10fp->shiftreg_nr_bits -= 8;
	    }
	    return EOF;
	}
    }
    /* XXX: big-endian conversion */
    nonet_ch = (pdp10fp->shiftreg >> (pdp10fp->shiftreg_nr_bits - 9)) & 0x1FF;
    pdp10fp->shiftreg_nr_bits -= 9;
    pdp10fp->nonet_pos += 1;
    return nonet_ch;
}

static int pdp10_fputc_one_octet(PDP10_FILE *pdp10fp)
{
    unsigned char rest_bits;
    unsigned char octet_ch;

    rest_bits = pdp10fp->shiftreg_nr_bits - 8;
    octet_ch = (pdp10fp->shiftreg >> rest_bits) & 0xFF;

    if (fputc((char)octet_ch, pdp10fp->octet_fp) == EOF)
	return -1;

    pdp10fp->shiftreg_nr_bits = rest_bits;

    return 0;
}

int pdp10_fputc(uint16_t nonet_ch, PDP10_FILE *pdp10fp)
{
    if (pdp10fp->shiftreg_nr_bits < 0) {
	int octet_ch;

	/*
	 * -7 <= shiftreg_nr_bits <= -1.
	 * An fseek placed octet_pos 1 to 7 bits before nonet_pos.
	 * We will peek at the octet at octet_pos, and preload shiftreg with the
	 * -shiftreg_nr_bits high bits from the octet.
	 */

	/* read the next octet, which we will partially overwrite */
#if 0	/* XXX: the pdp10_fseek did that already */
	if (fseeko(pdp10fp->octet_fp, 0, SEEK_CUR) == -1)
	    return EOF;
#endif
	octet_ch = fgetc(pdp10fp->octet_fp);

	/* rewind by one octet, or by zero octets if we read EOF above */
	if (fseeko(pdp10fp->octet_fp, octet_ch == EOF ? 0 : -1, SEEK_CUR) == -1)
	    return EOF;

	if (octet_ch == EOF)
	    octet_ch = 0;

	pdp10fp->shiftreg_nr_bits = -pdp10fp->shiftreg_nr_bits;
	pdp10fp->shiftreg = (octet_ch & 0xFF) >> (8 - pdp10fp->shiftreg_nr_bits);
    }

    pdp10fp->writing = 1;
    pdp10fp->shiftreg = (pdp10fp->shiftreg << 9) | (nonet_ch & 0x1FF);
    pdp10fp->shiftreg_nr_bits += 9;
    if (pdp10_fputc_one_octet(pdp10fp) < 0)
	return EOF;
    if (pdp10fp->shiftreg_nr_bits == 8
	&& pdp10_fputc_one_octet(pdp10fp) < 0)
	return EOF;
    pdp10fp->nonet_pos += 1;
    return nonet_ch & 0x1FF;
}

int pdp10_fseeko(PDP10_FILE *pdp10fp, off_t offset, int whence)
{
    off_t octet_pos, nonet_pos;

    if (pdp10_flush_buffered_write(pdp10fp) == EOF)
	return -1;

    switch (whence) {
    case PDP10_SEEK_SET:
	nonet_pos = 0;
	break;
    case PDP10_SEEK_CUR:
	nonet_pos = pdp10fp->nonet_pos;
	break;
    case PDP10_SEEK_END:
	if (fseeko(pdp10fp->octet_fp, 0, SEEK_END) == -1)
	    return -1;
	octet_pos = ftello(pdp10fp->octet_fp);
	if (octet_pos == -1)
	    return -1;

	/*
	 * Compute 'nonet_pos = (octet_pos * 8) / 9;' without
	 * overflowing the intermediate term.
	 *
	 * Let octet_pos = A * 9 + B, where A = octet_pos / 9 and B = octet_pos % 9.
	 *
	 * (octet_pos * 8) / 9
	 * == ((A * 9 + B) * 8) / 9
	 * == (A * 9 * 8 + B * 8) / 9
	 * == A * 8 + (B * 8) / 9
	 * == (octet_pos / 9) * 8 + ((octet_pos % 9) * 8) / 9
	 */
	nonet_pos = (octet_pos / 9) * 8 + ((octet_pos % 9) * 8) / 9;
	break;
    default:
	errno = EINVAL;
	return -1;
    }

    nonet_pos += offset;

    /*
     * Compute 'octet_pos = (nonet_pos * 9) / 8;' without
     * overflowing the intermediate term.
     *
     * Let nonet_pos = C * 8 + D, where C = nonet_pos / 8 and D = nonet_pos % 8.
     *
     * (nonet_pos * 9) / 8
     * == ((C * 8 + D) * 9) / 8
     * == (C * 8 * 9 + D * 9) / 8
     * == C * 9 + (D * 9) / 8
     * == (nonet_pos / 8) * 9 + ((nonet_pos % 8) * 9) / 8
     */
    octet_pos = (nonet_pos / 8) * 9 + ((nonet_pos % 8) * 9) / 8;

    if (fseeko(pdp10fp->octet_fp, octet_pos, SEEK_SET) == -1)
	return -1;

    pdp10fp->nonet_pos = nonet_pos;

    /*
     * Now octet_pos will be from 0 to 7 bits before nonet_pos.
     * Depending on whether the next I/O is a read or a write,
     * different actions need to be taken.  Set shiftreg_nr_bits
     * to the negation of the number of "slack" bits to signal
     * this case.
     */
    pdp10fp->shiftreg = 0;
    pdp10fp->shiftreg_nr_bits = -(nonet_pos % 8);
    pdp10fp->writing = 0;

    return 0;
}

/*
 * On an octet-based host, in-core data structures representing nonet-based
 * target data will in fact contain oversize octet-based host data.  For
 * example, 9/18/36-bit target integers are typically stored in 16/32/64-bit
 * host integers.
 *
 * This means that I/O of aggreate structures must be avoided, and instead
 * be performed on each primitive data field individually, using explicit
 * marshalling code for multi-nonet primitive data types.
 *
 * To detect mistakes in I/O, fread and fwrite only accepts strings (size == 1)
 * and single marshalled primitive data values (nmemb == 1, size == 1, 2, or 4).
 */
static int pdp10_freadwrite_bad_params(size_t size, size_t nmemb)
{
    return !(size == 1 || (nmemb == 1 && (size == 2 || size == 4)));
}

size_t pdp10_fread(uint16_t *ptr, size_t size, size_t nmemb, PDP10_FILE *pdp10fp)
{
    size_t i, nr_nonets;
    int nonet_ch;

    if (size == 0 || nmemb == 0)
	return nmemb;

    if (pdp10_freadwrite_bad_params(size, nmemb)) {
	errno = EINVAL;
	return 0;
    }

    nr_nonets = size * nmemb;

    for (i = 0; i < nr_nonets; ++i) {
	nonet_ch = pdp10_fgetc(pdp10fp);
	if (nonet_ch == EOF)
	    break;
	ptr[i] = nonet_ch & 0x1FF;
    }

    return i / size;
}

size_t pdp10_fwrite(const uint16_t *ptr, size_t size, size_t nmemb, PDP10_FILE *pdp10fp)
{
    size_t i, nr_nonets;

    if (size == 0 || nmemb == 0)
	return nmemb;

    if (pdp10_freadwrite_bad_params(size, nmemb)) {
	errno = EINVAL;
	return 0;
    }

    nr_nonets = size * nmemb;

    for (i = 0; i < nr_nonets; ++i)
	if (pdp10_fputc(ptr[i] & 0x1FF, pdp10fp) == EOF)
	    break;

    return i / size;
}
