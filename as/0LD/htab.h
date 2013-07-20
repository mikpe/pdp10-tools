/*
 * htab.h
 */
#ifndef HTAB_H
#define HTAB_H

#include <stdint.h>	/* uintptr_t */

struct hnode {
    uintptr_t hval;
    struct hnode *hnext;
};

typedef int (*htab_cmpfn_t)(const struct hnode *hnode, const void *data);

struct htab {
    unsigned int log2size;
    unsigned int mask;		/* INV: mask == (1 << log2size) - 1 */
    unsigned int used;
    htab_cmpfn_t cmpfn;
    struct hnode **bucket;
};

void htab_init(struct htab *htab, unsigned int log2size, htab_cmpfn_t cmpfn);

struct hnode *htab_lookup(const struct htab *htab, uintptr_t hval, const void *data);

struct hnode *htab_reset(struct htab *htab);

void htab_enumerate(const struct htab *htab,
		    void (*callback)(struct hnode *hnode, void *data),
		    void *data);

void htab_insert(struct htab *htab, struct hnode *hnode);

#endif /* HTAB_H */
