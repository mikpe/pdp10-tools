/*
 * hashtab.c
 * Copyright (C) 2013-2015  Mikael Pettersson
 *
 * This file is part of pdp10-tools.
 *
 * pdp10-tools is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pdp10-tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with pdp10-tools.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "hashtab.h"

static struct hashnode **hashtab_alloc_bucket(unsigned int nrelem)
{
    size_t nrbytes;
    struct hashnode **bucket;
    unsigned int i;

    nrbytes = nrelem * sizeof(struct hashnode*);
    bucket = malloc(nrbytes);
    if (!bucket) {
	fprintf(stderr, "%s: malloc(%zu) failed: %s\n", __FUNCTION__, nrbytes, strerror(errno));
	return NULL;
    }
    for (i = 0; i < nrelem; ++i)
	bucket[i] = NULL;
    return bucket;
}

int hashtab_init(struct hashtab *hashtab, unsigned int log2size, hashtab_eq_func_t eq_func)
{
    unsigned int size;

    size = 1 << log2size;
    hashtab->log2size = log2size;
    hashtab->mask = size - 1;
    hashtab->used = 0;
    hashtab->eq_func = eq_func;
    hashtab->bucket = hashtab_alloc_bucket(size);
    return hashtab->bucket ? 0 : -1;
}

struct hashnode *hashtab_lookup(const struct hashtab *hashtab, uintptr_t hashval, const void *data)
{
    unsigned int i;
    struct hashnode *hashnode;

    i = hashval & hashtab->mask;

    hashnode = hashtab->bucket[i];
    while (hashnode != NULL) {
	if (hashnode->hashval == hashval
	    && (hashtab->eq_func == NULL || (*hashtab->eq_func)(hashnode, data) != 0))
	    break;
	hashnode = hashnode->next;
    }

    return hashnode;
}

static int hashtab_grow(struct hashtab *hashtab)
{
    unsigned int old_size, new_size, new_mask, i;
    struct hashnode **old_bucket, **new_bucket;

    old_size = 1 << hashtab->log2size;
    new_size = old_size << 1;
    new_bucket = hashtab_alloc_bucket(new_size);
    if (!new_bucket)
	return -1;

    old_bucket = hashtab->bucket;
    hashtab->log2size += 1;
    new_mask = new_size - 1;
    hashtab->mask = new_mask;
    hashtab->bucket = new_bucket;

    for (i = 0; i < old_size; ++i) {
	struct hashnode *hashnode = old_bucket[i];
	while (hashnode != NULL) {
	    struct hashnode *next = hashnode->next;
	    unsigned int j = hashnode->hashval & new_mask;
	    hashnode->next = new_bucket[j];
	    new_bucket[j] = hashnode;
	    hashnode = next;
	}
    }

    free(old_bucket);
    return 0;
}

int hashtab_insert(struct hashtab *hashtab, struct hashnode *hashnode)
{
    unsigned int i, size;

    i = hashnode->hashval & hashtab->mask;
    hashnode->next = hashtab->bucket[i];
    hashtab->bucket[i] = hashnode;
    hashtab->used += 1;
    size = 1 << hashtab->log2size;
    if (hashtab->used > (4 * size) / 5)	/* rehash at 80% */
	return hashtab_grow(hashtab);
    return 0;
}

struct hashnode *hashtab_reset(struct hashtab *hashtab)
{
    unsigned int i, size;
    struct hashnode **bucket, *all_nodes, *head, *tail;

    all_nodes = NULL;
    bucket = hashtab->bucket;
    size = 1 << hashtab->log2size;

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

    hashtab->used = 0;
    return all_nodes;
}

int hashtab_enumerate(const struct hashtab *hashtab,
		      int (*func)(struct hashnode *hashnode, void *data),
		      void *data)
{
    unsigned int i, size;
    struct hashnode **bucket, *head;
    int status;

    bucket = hashtab->bucket;
    size = 1 << hashtab->log2size;

    for (i = 0; i < size; ++i) {
	head = bucket[i];
	while (head != NULL) {
            status = (*func)(head, data);
	    if (status != 0)
		return status;
            head = head->next;
	}
    }

    return 0;
}
