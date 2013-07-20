/*
 * stmt.h
 */
#ifndef STMT_H
#define STMT_H

#include "pdp10-elf36.h"
#include "expr.h"
#include "strtab.h"

struct expr_list {
    struct expr *expr;
    struct expr_list *next;
};

struct string_list {
    const struct strnode *string;
    struct string_list *next;
};

enum stmt_tag {
    /* directives */
    S_ALIGN,		/* .align, .balign, and .p2align map to this */
    S_ASCII,
    S_ASCIZ,
    S_BYTE,
    S_COMM,
    S_FILE,
    S_GLOBL,
    S_HIDDEN,
    S_IDENT,
    S_INTERNAL,
    S_LOCAL,
    S_LONG,
    S_ORG,
    S_POPSECTION,	/* no attribute */
    S_PREVIOUS,		/* no attribute */
    S_PROTECTED,
    S_PUSHSECTION,
    S_SECTION,		/* .bss, .data, .rodata, and .text also map to this */
    S_SET,
    S_SHORT,
    S_SIZE,
    S_SUBSECTION,
    S_SYMVER,
    S_TYPE,
    S_WEAK,
    S_WEAKREF,
    /* non-directives */
    S_LABEL,
    S_INSN,
};

struct stmt {
    enum stmt_tag tag;
    union {
	struct {
	    unsigned char flags;	/* p2 vs b, none/w/l */
	    struct expr *balign;
	    struct expr *fill;
	    struct expr *maxskip;
	} s_align;
	struct {
	    struct string_list *list;
	} s_string_list;
	struct {
	    struct expr_list *list;
	} s_expr_list;
	struct {
	    const struct strnode *name;
	    struct expr *length;
	    struct expr *balign;
	} s_comm;
	struct {
	    const struct strnode *string;
	} s_string;
	struct {
	    struct expr *newlc;
	    struct expr *fill;
	} s_org;
	struct {
	    const struct strnode *name;
	    struct expr *subsectnr;
	    Elf36_Word sh_flags;
	    Elf36_Word sh_type;
	    struct expr *sh_entsize;
	    const struct strnode *groupname;
	    const struct strnode *linkage;
	} s_section;
	struct {
	    const struct strnode *name;
	    struct expr *expr;
	} s_setsize;
	struct {
	    struct expr *expr;
	} s_subsection;
	struct {
	    const struct strnode *name1;
	    const struct strnode *name2;
	    const struct strnode *name3;
	    unsigned char nrats;	/* 1, 2, or 3 */
	} s_symver;
	struct {
	    const struct strnode *name;
	    unsigned char st_type;
	} s_type;
	struct {
	    const struct strnode *alias;
	    const struct strnode *target;
	} s_weakref;
	struct {
	    const struct strnode *name;
	    unsigned int accumulator;
	    int at;
	    struct expr *expr;
	    unsigned int indexreg;
	} s_insn;

    } u;
};

#endif /* STMT_H */
