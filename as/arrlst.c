/*
 * arrlst.c
 */
#include <stdlib.h>
#include <string.h>
#include "arrlst.h"
#include "emalloc.h"

enum {
    ARRLST_CHUNK_NRELEM = 128,
};

struct arrlst {
    void *head;
    void *tail;
    unsigned int eltsz;
    unsigned int tailpos;

    /* We only allow a single iterator per arrlst, so we
       allocate the iterator in the arrlst itself.  */
    struct {
	void *chunk;
	unsigned int chunkpos;
	unsigned int chunklen;
    } iter;
};

static void **arrlst_chunk_nextp(const void *chunk)
{
    return (void**)chunk;
}

static unsigned int eltsz_chunk_header_nrelem(unsigned int eltsz)
{
    unsigned int nrelem;

    /* compute how may eltsz elements are needed to cover a void* */
    nrelem = sizeof(void*) / eltsz;
    if (nrelem * eltsz < sizeof(void*))
	++nrelem;

    return nrelem;
}

static unsigned int arrlst_chunk_header_nrelem(const struct arrlst *arrlst)
{
    return eltsz_chunk_header_nrelem(arrlst->eltsz);
}

static void *arrlst_chunk_element(const struct arrlst *arrlst, void *chunk, unsigned int eltnr)
{
    return (char*)chunk + eltnr * arrlst->eltsz;
}

static void *arrlst_alloc_chunk(const struct arrlst *arrlst)
{
    unsigned int nrbytes;
    void *chunk;

    nrbytes = arrlst->eltsz * ARRLST_CHUNK_NRELEM;
    chunk = emalloc(nrbytes);
    return chunk;
}

struct arrlst *arrlst_alloc(size_t eltsz)
{
    struct arrlst *arrlst;

    if (eltsz == 0
	|| eltsz > (unsigned int)-1
	|| eltsz_chunk_header_nrelem(eltsz) >= ARRLST_CHUNK_NRELEM)
	return NULL;

    arrlst = emalloc(sizeof *arrlst);

    /* these fields will be adjusted in the first call to append() */
    arrlst->head = NULL;
    arrlst->tail = NULL;
    arrlst->eltsz = eltsz;
    arrlst->tailpos = ARRLST_CHUNK_NRELEM;

    return arrlst;
}

void arrlst_free(struct arrlst *arrlst)
{
    void *chunk, *next;

    chunk = arrlst->head;
    while (chunk) {
	next = *arrlst_chunk_nextp(chunk);
	free(chunk);
	chunk = next;
    }

    free(arrlst);
}

size_t arrlst_length(const struct arrlst *arrlst)
{
    const void *chunk;
    size_t length;
    size_t chunk_header_nrelem;

    chunk = arrlst->head;
    if (!chunk)
	return 0;
    length = 0;
    chunk_header_nrelem = arrlst_chunk_header_nrelem(arrlst);
    for (;;) {
	chunk = *arrlst_chunk_nextp(chunk);
	if (!chunk)
	    break;
	length += ARRLST_CHUNK_NRELEM - chunk_header_nrelem;
    }
    return length + arrlst->tailpos - chunk_header_nrelem;
}

void *arrlst_append(struct arrlst *arrlst)
{
    void *tail;
    void *elt;

    tail = arrlst->tail;

    if (arrlst->tailpos >= ARRLST_CHUNK_NRELEM) {
	void *new_tail;

	new_tail = arrlst_alloc_chunk(arrlst);
	if (!new_tail)
	    return NULL;

	if (tail)
	    *arrlst_chunk_nextp(tail) = new_tail;
	else {
	    arrlst->head = new_tail;
	    arrlst->tail = new_tail;
	}

	arrlst->tailpos = arrlst_chunk_header_nrelem(arrlst);

	tail = new_tail;
    }

    elt = arrlst_chunk_element(arrlst, tail, arrlst->tailpos);
    ++arrlst->tailpos;

    return elt;
}

void arrlst_iter_rewind(struct arrlst *arrlst)
{
    /* these fields will be adjusted in the first call to next() */
    arrlst->iter.chunk = NULL;
    arrlst->iter.chunklen = 0;
    arrlst->iter.chunkpos = 0;
}

void *arrlst_iter_next(struct arrlst *arrlst)
{
    void *chunk;
    void *elt;

    chunk = arrlst->iter.chunk;
    if (arrlst->iter.chunkpos >= arrlst->iter.chunklen) {
	if (!chunk)
	    chunk = arrlst->head;
	else
	    chunk = *arrlst_chunk_nextp(chunk);
	if (!chunk)
	    return NULL;
	arrlst->iter.chunk = chunk;

	if (*arrlst_chunk_nextp(chunk))
	    arrlst->iter.chunklen = ARRLST_CHUNK_NRELEM;
	else
	    arrlst->iter.chunklen = arrlst->tailpos;

	arrlst->iter.chunkpos = arrlst_chunk_header_nrelem(arrlst);
    }

    elt = arrlst_chunk_element(arrlst, chunk, arrlst->iter.chunkpos);
    ++arrlst->iter.chunkpos;

    return elt;
}
