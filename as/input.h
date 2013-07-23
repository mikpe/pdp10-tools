/*
 * input.h
 */
#ifndef INPUT_H
#define INPUT_H

/*
 * A directives, label, or instruction is parsed to a statement, which is
 * either interpreted immediately or appended to the representation of the
 * current section.
 */

enum stmt_tag {
    /* directives */
    S_DOT_GLOBL,
    S_DOT_TEXT,
    /* non-directives */
    S_LABEL,
    S_INSN,
};

struct stmt {
    struct stmt *next;
    enum stmt_tag tag;
    union {
	struct {	/* S_DOT_GLOBL, S_LABEL */
	    const char *name;
	} symbol;
	struct {	/* S_INSN */
	    unsigned int opcode;
	    unsigned int accumulator;
	    int at;
	    unsigned int address;	/* XXX: relocatable expr */
	    unsigned int indexreg;
	} insn;
    } u;
};

/*
 * The input unit object is the top-level container for the representation
 * of the sections, and all other information collected from the input.
 */

struct iunit {
    struct {
	struct stmt *head;
	struct stmt **tailptr;
    } text;
};

int input(const char *progname, char **files, int nrfiles, struct iunit *iunit);

#endif /* INPUT_H */
