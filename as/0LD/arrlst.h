/*
 * arrlst.h
 */
#ifndef ARRLST_H
#define ARRLST_H

#include <stdlib.h>	/* size_t */

struct arrlst;

struct arrlst *arrlst_alloc(size_t eltsz);
void arrlst_free(struct arrlst *arrlst);
size_t arrlst_length(const struct arrlst *arrlst);
void *arrlst_append(struct arrlst *arrlst);

/* for now there is only one iterator per arrlst */
void arrlst_iter_rewind(struct arrlst *arrlst);
void *arrlst_iter_next(struct arrlst *arrlst);

#endif /* ARRLST_H */
