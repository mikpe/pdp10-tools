/*
 * section.h
 */
#ifndef SECTION_H
#define SECTION_H

#include "pdp10-elf36.h"

#include "arrlst.h"
#include "htab.h"
#include "stmt.h"
#include "strtab.h"

struct subsection {
    struct hnode hnode;	/* hnode.hval == subsect nr */
    struct arrlst *stmts;
};

struct section {
    struct hnode hnode;	/* hnode.hval == struct strnode* */
    Elf36_Shdr e_shdr;
    struct htab subsects;
    const struct strnode *groupname;
    const struct strnode *linkage;
    unsigned int dot;	/* Elf36_Off? */
};

#define SECTION_ABS	((struct section*)0)
#define SECTION_UNDEF	((struct section*)1)

void section_init(void);
struct section *section_enter(const struct strnode *strnode);
struct subsection *subsection_enter(struct section *section, int subsectnr);

#endif /* SECTION_H */
