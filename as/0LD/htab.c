/*
 * htab.c
 */
#include <stdlib.h>
#include <string.h>
#include "emalloc.h"
#include "htab.h"

static struct hnode **htab_alloc_bucket(unsigned int size)
{
    size_t nrbytes;
    struct hnode **bucket;

    nrbytes = size * sizeof(struct hnode*);
    bucket = emalloc(nrbytes);
    memset(bucket, 0, nrbytes);
    return bucket;
}

void htab_init(struct htab *htab, unsigned int log2size, htab_cmpfn_t cmpfn)
{
    unsigned int size;

    size = 1 << log2size;
    htab->log2size = log2size;
    htab->mask = size - 1;
    htab->used = 0;
    htab->cmpfn = cmpfn;
    htab->bucket = htab_alloc_bucket(size);
}

struct hnode *htab_lookup(const struct htab *htab, uintptr_t hval, const void *data)
{
    htab_cmpfn_t cmpfn;
    unsigned int i;
    struct hnode *hnode;

    cmpfn = htab->cmpfn;
    i = hval & htab->mask;

    hnode = htab->bucket[i];
    while (hnode != NULL) {
	if (hnode->hval == hval
	    && (cmpfn == NULL || (*cmpfn)(hnode, data) == 0))
	    break;
	hnode = hnode->hnext;
    }

    return hnode;
}

static void htab_grow(struct htab *htab)
{
    unsigned int old_size, new_size, new_mask;
    struct hnode **old_bucket, **new_bucket;
    unsigned int i;

    old_size = 1 << htab->log2size;
    htab->log2size += 1;
    new_size = 1 << htab->log2size;
    new_mask = new_size - 1;
    htab->mask = new_mask;
    old_bucket = htab->bucket;
    new_bucket = htab_alloc_bucket(new_size);
    htab->bucket = new_bucket;
    for (i = 0; i < old_size; ++i) {
	struct hnode *hnode = old_bucket[i];
	while (hnode != NULL) {
	    struct hnode *hnext = hnode->hnext;
	    unsigned int j = hnode->hval & new_mask;
	    hnode->hnext = new_bucket[j];
	    new_bucket[j] = hnode;
	    hnode = hnext;
	}
    }
    free(old_bucket);
}

void htab_insert(struct htab *htab, struct hnode *hnode)
{
    unsigned int i;
    unsigned int size;

    i = hnode->hval & htab->mask;
    hnode->hnext = htab->bucket[i];
    htab->bucket[i] = hnode;
    htab->used += 1;
    size = 1 << htab->log2size;
    if (htab->used > (4 * size) / 5)	/* rehash at 80% */
	htab_grow(htab);
}

#if 0
struct hash_node *am_hash_reset(struct hash_table *hash_table)
{
    unsigned int i;
    unsigned int size;
    struct hash_node * volatile *bucket, *all_nodes, *head, *tail;

    all_nodes = NULL;
    bucket = hash_table->bucket;
    size = 1 << hash_table->log2size;
    for (i = 0; i < size; ++i) {
	head = bucket[i];
	if (head) {
	    bucket[i] = NULL;
	    tail = head;
	    while (tail->next)
		tail = tail->next;
	    tail->next = all_nodes;
	    all_nodes = head;
	}
    }
    hash_table->used = 0;
    return all_nodes;
}

void am_hash_enumerate(const struct hash_table *hash_table,
                       void (*callback)(struct hash_node *hash_node, void *data),
                       void *data)
{
    unsigned int i;
    unsigned int size;
    struct hash_node * volatile *bucket, *head;

    bucket = hash_table->bucket;
    size = 1 << hash_table->log2size;
    for (i = 0; i < size; ++i) {
	head = bucket[i];
	while (head != NULL) {
            callback(head, data);
            head = head->next;
	}
    }
}
#endif
