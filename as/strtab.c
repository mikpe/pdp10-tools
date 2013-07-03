/*
 * strtab.c
 */
#include <string.h>
#include "emalloc.h"
#include "htab.h"
#include "strtab.h"

static struct strnode *strnode_from_hnode(const struct hnode *hnode)
{
    /* hnode is first in strnode, so no need to mess with offsetof() */
    return (struct strnode*)hnode;
}

static int strtab_cmpfn(const struct hnode *hnode, const void *data)
{
    const struct strnode *strnode = strnode_from_hnode(hnode);
    const char *string = data;

    return strcmp(strnode->string, string);
}

static struct htab strtab_htab;

void strtab_init(void)
{
    htab_init(&strtab_htab, 64, strtab_cmpfn);
}

static uintptr_t strtab_hash(const char *string)
{
    const unsigned char *s;
    uintptr_t h;
    unsigned char c;

    s = (const unsigned char*)string;
    h = 0;

    for (;;) {
	c = *s++;
	if (c == '\0')
	    break;
	h = (h << 5) + h + c;
    }

    return h;
}

const struct strnode *strtab_enter(const char *string)
{
    uintptr_t hval;
    struct strnode *strnode;

    hval = strtab_hash(string);
    strnode = strnode_from_hnode(htab_lookup(&strtab_htab, hval, string));

    if (strnode == NULL) {
	strnode = emalloc(offsetof(struct strnode, string) + strlen(string) + 1);
	strnode->hnode.hval = hval;
	strcpy(strnode->string, string);
	htab_insert(&strtab_htab, &strnode->hnode);
    }

    return strnode;
}
