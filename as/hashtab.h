/*
 * hashtab.h
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
#ifndef HASHTAB_H
#define HASHTAB_H

#include <stdint.h>	/* uintptr_t */

struct hashnode {
    uintptr_t hashval;
    struct hashnode *next;
};

typedef int (*hashtab_eq_func_t)(const struct hashnode *hashnode, const void *data);

struct hashtab {
    unsigned int log2size;
    unsigned int mask;		/* INV: mask == (1 << log2size) - 1 */
    unsigned int used;
    hashtab_eq_func_t eq_func;
    struct hashnode **bucket;
};

int hashtab_init(struct hashtab *hashtab, unsigned int log2size, hashtab_eq_func_t eq_func);

struct hashnode *hashtab_lookup(const struct hashtab *hashtab, uintptr_t hashval, const void *data);

int hashtab_insert(struct hashtab *hashtab, struct hashnode *hashnode);

struct hashnode *hashtab_reset(struct hashtab *hashtab);

int hashtab_enumerate(const struct hashtab *hashtab,
		      int (*func)(struct hashnode *hashnode, void *data),
		      void *data);

#endif /* HASHTAB_H */
