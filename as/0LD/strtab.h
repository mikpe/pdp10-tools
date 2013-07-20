/*
 * strtab.h
 */
#ifndef STRTAB_H
#define STRTAB_H

#include "htab.h"

struct strnode {
    struct hnode hnode;
    char string[];
};

void strtab_init(void);
const struct strnode *strtab_enter(const char *string);

#endif /* STRTAB */
