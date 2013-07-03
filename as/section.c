/*
 * section.c
 */
#include <string.h>
#include "emalloc.h"
#include "htab.h"
#include "section.h"

static struct section *section_from_hnode(const struct hnode *hnode)
{
    /* hnode is first in section, so no need to mess with offsetof() */
    return (struct section*)hnode;
}

static struct htab section_htab;

void section_init(void)
{
    htab_init(&section_htab, 8, NULL);
}

struct section *section_enter(const struct strnode *strnode)
{
    struct section *section;

    section = section_from_hnode(htab_lookup(&section_htab, (uintptr_t)strnode, NULL));

    if (section == NULL) {
	section = emalloc(sizeof *section);
	memset(section, '\0', sizeof *section);
	section->hnode.hval = (uintptr_t)strnode;
	htab_init(&section->subsects, 4, NULL);
	htab_insert(&section_htab, &section->hnode);
    }

    return section;
}

static struct subsection *subsection_from_hnode(const struct hnode *hnode)
{
    /* hnode is first in subsection, so no need to mess with offsetof() */
    return (struct subsection*)hnode;
}

struct subsection *subsection_enter(struct section *section, int subsectnr)
{
    struct subsection *subsection;

    subsection = subsection_from_hnode(htab_lookup(&section->subsects, (uintptr_t)subsectnr, NULL));

    if (subsection == NULL) {
	subsection = emalloc(sizeof *subsection);
	subsection->hnode.hval = (uintptr_t)subsectnr;
	subsection->stmts = arrlst_alloc(sizeof(struct stmt));
	htab_insert(&section->subsects, &subsection->hnode);
    }

    return subsection;
}
