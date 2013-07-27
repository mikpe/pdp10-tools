/*
 * pdp10-ar.h
 *
 * This is essentially the standard SVR4/GNU AR format, with 'char'
 * replaced by 'pdp10_uint9_t' throughout.
 * Groups of 4 bytes in the symbol table denote pdp10_uint36_t
 * values rather than plain int or uint32_t values.
 */
#ifndef PDP10_AR_H
#define PDP10_AR_H

#include "pdp10-stdint.h"	/* pdp10_{u,}int{9,18,36}_t */

#define PDP10_ARMAG	{ '!', '<', 'a', 'r', 'c', 'h', '>', '\n' }	/* String that begins an archive file.  */
#define PDP10_SARMAG	8						/* Size of that string.  */

#define PDP10_ARFMAG	{ '`', '\n' }					/* String in ar_fmag at end of each header.  */

struct pdp10_ar_hdr {
    pdp10_uint9_t ar_name[16];	/* Member file name, sometimes / terminated.  */
    pdp10_uint9_t ar_date[12];	/* File date, decminal seconds since Epoch.  */
    pdp10_uint9_t ar_uid[6];	/* User ID, in ASCII decimal.  */
    pdp10_uint9_t ar_gid[6];	/* Group ID, in ASCII decimal.  */
    pdp10_uint9_t ar_mode[8];	/* File mode, in ASCII octal.  */
    pdp10_uint9_t ar_size[10];	/* File size, in ASCII decimal.  */
    pdp10_uint9_t ar_fmag[2];	/* Always contains ARFMAG.  */
};

#define PDP10_ARHDR_SIZEOF	60

/*
 * Additional information summarized from the FreeBSD ar(5) manual page
 * <http://howtounix.info/man/FreeBSD/man5/ar.5>.
 *
 * DESCRIPTION
 *
 * Th e archive file starts with an identifying byte sequence of the seven ASCII characters
 * '!<arch>' followed by an ASCII linefeed character (see the constant "ARMAG" in the header
 * file <ar.h>).
 * Archive members follow the initial identifying byte sequence.  Each archive member is
 * prefixed by a fixed size heade describing the file attributes associated with the member.
 *
 * Archive Headers
 *
 * Archive headers are placed at an even offset in the archive file.  If the data for an
 * archive member ends at an odd byte offset, then a padding byte with value 0x0A is used
 * to position the next archive header on an even byte offset.
 *
 * Unused bytes in the fields of an archive header are set to the value 0x20.
 *
 * Representing File Names
 *
 * SVR4/GNU
 *	File names that are up to 15 characters long are stored directly in the ar_name
 *	field of the header, terminated by a "/" character.
 *	If the file name is larger than would fit in the space for the ar_name field,
 *	then the actual file name is kept in the archive string table, and the decimal
 *	offset of the file name in the string table is stored in the ar_name field,
 *	prefixed by a "/" character.
 *
 * Special Archive Members
 *
 * The following archive members are special.
 *
 * "/"
 *	In the SVR4/GNU variant of the archive format, the archive member with name "/"
 *	denotes an archive symbol table.  If present, this member will be the very first
 *	member of the archive.
 *
 * "//"
 *	In the SVR4/GNU variant of the archive format, the archive member with name "//"
 *	denotes the archive string table.  This special member is used to hold filenames
 *	that do not fit in the file name field of the header.  If present, this member
 *	immediately follows the archive symbol table if an archive symbol table is present,
 *	or is the first member otherwise.
 *
 * Archive String Tables
 *
 * An archive string table is used in the SVR4/GNU archive format to hold file names that
 * are too large to fit into the constraints of the ar_name field of the archive header.
 * An archive string table contains a sequence of file names.  Each file name in the archive
 * string table is terminated by the sequence 0x2F, 0x0A (the ARCII string "/\n").  No
 * padding is used to separate adjacent file names.
 *
 * Archive Symbol Tables
 *
 * Archive symbol tables are used to speed up link editing by providing a mapping between
 * the program symbols defined in the archive and the corresponding archive members.
 * Archive symbol tables are managed by the ranlib utility.  The format of archive symbol
 * tables is as follows:
 *
 * SVR4/GNU
 *	In the SVR4/GNU archive format, the archive symbol table starts with a 4-byte binary
 *	values consisting of the number of entries contained in the archive symbol table.
 *	This could of entries is stored most significant byte first.  Next, there are 'count'
 *	4-byte numbers, each stored most significant byte first.  Each number is a binary
 *	offset to the archive header for the member in the archive file for the corresponding
 *	symbol table entry.  After the binary offset values, there are 'count' NUL-terminated
 *	strings in sequence, holding the symbol names for the corresponding symbol table entries.
 */

/*
 * Further references:
 * <https://blogs.oracle.com/ali/entry/64_bit_archives_needed>
 * <http://linux.die.net/man/1/llvm-ar>
 * "man -s 3HEAD ar.h" on Solaris 10
 */

#endif /* PDP10_AR_H */
