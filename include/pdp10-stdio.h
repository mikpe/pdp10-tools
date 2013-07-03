/*
 * pdp10-stdio.h
 *
 * Provide stdio.h-like interface for I/O to and from files with 9-bit logical bytes (nonets),
 * represented by native files with 8-bit physical bytes (octets).
 */
#ifndef PDP10_STDIO_H
#define PDP10_STDIO_H

#include <stdint.h>

struct pdp10_file;
typedef struct pdp10_file PDP10_FILE;

/* append modes are not permitted */
PDP10_FILE *pdp10_fopen(const char *path, const char *mode);

int pdp10_fflush(PDP10_FILE *pdp10fp);
int pdp10_fclose(PDP10_FILE *pdp10fp);
int pdp10_fgetc(PDP10_FILE *pdp10fp);	/* returns a nonet, [0-511], or EOF */
int pdp10_fputc(uint16_t nonet_ch, PDP10_FILE *pdp10fp);

enum {
    PDP10_SEEK_SET = 0,
    PDP10_SEEK_CUR = 1,
    PDP10_SEEK_END = 2,
};
int pdp10_fseeko(PDP10_FILE *pdp10fp, off_t offset, int whence);

/* pdp10_fread() and pdp10_fwrite() deliberately only permit transfers of strings
 * (size == 1), marshalled 9/18/36-bit primitives (nmemb == 1, size == 1, 2, or 4),
 * or empty objects (size == 0 || nmemb == 0).  To transfer structures, transfer
 * their primitive fields individually.
 */
size_t pdp10_fread(uint16_t *ptr, size_t size, size_t nmemb, PDP10_FILE *pdp10fp);
size_t pdp10_fwrite(const uint16_t *ptr, size_t size, size_t nmemb, PDP10_FILE *pdp10fp);

#endif /* PDP10_STDIO_H */
