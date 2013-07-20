/*
 * emalloc.c
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "emalloc.h"

void *emalloc(size_t nrbytes)
{
    void *p;

    p = malloc(nrbytes);
    if (!p) {
	fprintf(stderr, "malloc(%zu) failed: %s\n", nrbytes, strerror(errno));
	exit(1);
    }
    return p;
}
