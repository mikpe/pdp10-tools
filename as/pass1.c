/*
 * pass1.c
 */
#include <stdio.h>
#include <stdlib.h>
#include "emalloc.h"
#include "parse.h"
#include "pass1.h"
#include "scan.h"
#include "section.h"

struct section_and_subsection {
    struct section *section;
    struct subsection *subsection;
};

struct current_and_previous_sections {
    struct section_and_subsection cursec;
    struct section_and_subsection prevsec;
};

struct sections_stack_element {
    struct current_and_previous_sections sects;
    struct sections_stack_element *next;
};

struct pass1_state {
    struct current_and_previous_sections sects;
    struct sections_stack_element *sects_stack;
};

static int pass1_s_popsection(struct pass1_state *state)
{
    struct sections_stack_element *top;

    top = state->sects_stack;
    if (top == NULL) {
	fprintf(stderr, "as: %s, line %u: .popsection with no previous .pushsection\n", scan_filename, scan_linenr);
	return -1;
    }

    state->sects = top->sects;
    state->sects_stack = top->next;
    free(top);

    return 0;
}

static int pass1_s_previous(struct pass1_state *state)
{
    struct section_and_subsection prevsec;

    prevsec = state->sects.prevsec;
    if (prevsec.section == NULL) {
	fprintf(stderr, "as: %s, line %u: .previous with no previous .section\n", scan_filename, scan_linenr);
	return -1;
    }

    state->sects.prevsec = state->sects.cursec;
    state->sects.cursec = prevsec;

    return 0;
}

static int pass1_s_section(struct pass1_state *state, struct stmt *stmt, int push)
{
    struct section *section;
    struct subsection *subsection;
    int subsectnr;

    section = section_enter(stmt->u.s_section.name);

    if (stmt->u.s_section.sh_type != 0) {
	if (section->e_shdr.sh_type == 0)
	    section->e_shdr.sh_type = stmt->u.s_section.sh_type;
	else if (section->e_shdr.sh_type != stmt->u.s_section.sh_type) {
	    fprintf(stderr, "as: %s, line %u: section type mismatch\n", scan_filename, scan_linenr);
	    return -1;
	}
    }

    section->e_shdr.sh_flags |= stmt->u.s_section.sh_flags;

    if (stmt->u.s_section.sh_entsize != NULL) {
	pdp10_uint36_t offset;

	if (eval_abs_verbose(stmt->u.s_section.sh_entsize, &offset) < 0)
	    return -1;
	if (section->e_shdr.sh_entsize == 0)
	    section->e_shdr.sh_entsize = offset;
	else if (section->e_shdr.sh_entsize != offset) {
	    fprintf(stderr, "as: %s, line %u: section <entsize> mismatch\n", scan_filename, scan_linenr);
	    return -1;
	}
    }

    if (stmt->u.s_section.groupname != NULL) {
	if (section->groupname == NULL)
	    section->groupname = stmt->u.s_section.groupname;
	else if (stmt->u.s_section.groupname != section->groupname) {
	    fprintf(stderr, "as: %s, line %u: section <groupname> mismatch\n", scan_filename, scan_linenr);
	    return -1;
	}
    }

    if (stmt->u.s_section.linkage != NULL) {
	if (section->linkage == NULL)
	    section->linkage = stmt->u.s_section.linkage;
	else if (stmt->u.s_section.linkage != section->linkage) {
	    fprintf(stderr, "as: %s, line %u: section <linkage> mismatch\n", scan_filename, scan_linenr);
	    return -1;
	}
    }

    if (push && stmt->u.s_section.subsectnr != NULL) {
	pdp10_uint36_t offset;

	if (eval_abs_verbose(stmt->u.s_section.subsectnr, &offset) < 0)
	    return -1;
	subsectnr = offset;
    } else
	subsectnr = 0;

    subsection = subsection_enter(section, subsectnr);

    if (push) {
	struct sections_stack_element *top;

	top = emalloc(sizeof *top);
	top->sects = state->sects;
	top->next = state->sects_stack;
	state->sects_stack = top;
    }

    state->sects.prevsec = state->sects.cursec;
    state->sects.cursec.section = section;
    state->sects.cursec.subsection = subsection;

    return 0;
}

static int pass1_s_subsection(struct pass1_state *state, struct stmt *stmt)
{
    pdp10_uint36_t offset;
    struct subsection *subsection;

    if (eval_abs_verbose(stmt->u.s_subsection.expr, &offset) < 0)
	return -1;

    subsection = subsection_enter(state->sects.cursec.section, (int)(pdp10_int36_t)offset);

    state->sects.prevsec = state->sects.cursec;
    state->sects.cursec.subsection = subsection;

    return 0;
}

static int pass1_interpret(struct pass1_state *state, struct stmt *stmt)
{
    switch (stmt->tag) {
	/* in pass1 we have to deal with section-altering directives */
    case S_POPSECTION:
	return pass1_s_popsection(state);
    case S_PREVIOUS:
	return pass1_s_previous(state);
    case S_PUSHSECTION:
	return pass1_s_section(state, stmt, 1);
    case S_SECTION:
	return pass1_s_section(state, stmt, 0);
    case S_SUBSECTION:
	return pass1_s_subsection(state, stmt);

	/* remaining directives, and the non-directives, enter data
	   into sections or manipulate symbols; delay these for pass2 */
	XXX;
	/* XXX: wrong, symbol values, even section-relative, are needed as soon
	   as possible, so all statements must be interpreted right away */
    case S_ALIGN:
    case S_ASCII:
    case S_ASCIZ:
    case S_BYTE:
    case S_COMM:
    case S_FILE:
    case S_GLOBL:
    case S_HIDDEN:
    case S_IDENT:
    case S_INTERNAL:
    case S_LOCAL:
    case S_LONG:
    case S_ORG:
    case S_PROTECTED:
    case S_SET:
    case S_SHORT:
    case S_SIZE:
    case S_SYMVER:
    case S_TYPE:
    case S_WEAK:
    case S_WEAKREF:
    case S_LABEL:
    case S_INSN:
    {
	struct stmt *stmt2;

	stmt2 = arrlst_append(state->sects.cursec.subsection->stmts);
	/* XXX: error check */
	*stmt2 = *stmt;
	return 0;
    }
    default:
	fprintf(stderr, "as: %s, line %u: %s(): unknown stmt tag %u\n", scan_filename, scan_linenr, __FUNCTION__, stmt->tag);
	return -1;
    }
}

static void pass1_init_state(struct pass1_state *state)
{
    struct section *text;

    text = section_enter(strtab_enter(".text"));

    text->e_shdr.sh_type = SHT_PROGBITS;
    text->e_shdr.sh_flags = SHF_ALLOC | SHF_EXECINSTR;
    text->e_shdr.sh_addralign = 4;	/* XXX: PDP10-specific */

    state->sects.cursec.section = text;
    state->sects.cursec.subsection = subsection_enter(text, 0);

    state->sects.prevsec.section = NULL;
    state->sects.prevsec.subsection = NULL;

    state->sects_stack = NULL;
}

static void pass1_fini_state(struct pass1_state *state)
{
    struct sections_stack_element *here, *next;

    here = state->sects_stack;
    while (here != NULL) {
	next = here->next;
	free(here);
	here = next;
    }
}

int pass1(const char *filename)
{
    struct pass1_state state;
    struct stmt stmt;
    int status;

    if (scan_freopen(filename) < 0)
	return -1;

    pass1_init_state(&state);

    for (;;) {
	status = parse_stmt(&stmt);
	if (status < 0)
	    return -1;
	else if (status == 0)
	    break;
	else if (pass1_interpret(&state, &stmt) < 0)
	    return -1;
    }

    pass1_fini_state(&state);

    return 0;
}
