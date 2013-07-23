/*
 * input.c
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "input.h"
#include "parse.h"
#include "scan.h"

static int interpret(struct scan_state *scan_state, struct iunit *iunit, struct stmt *stmt)
{
    struct stmt *stmt2;

    switch (stmt->tag) {
    case S_DOT_GLOBL:
	break;
    case S_DOT_TEXT:
	return 0;	/* XXX: nothing to do yet */
    case S_LABEL:
	break;
    case S_INSN:
	break;
    default:
	fprintf(stderr, "%s: %s line %u: parser returned unexpected stmt->tag %u\n",
		scan_state->progname, scan_state->filename, scan_state->linenr, stmt->tag);
	return -1;
    }

    stmt2 = malloc(sizeof *stmt2);
    if (!stmt2) {
	fprintf(stderr, "%s: %s line %u: malloc(%zu) failed: %s\n",
		scan_state->progname, scan_state->filename, scan_state->linenr, sizeof *stmt2, strerror(errno));
	return -1;
    }

    *stmt2 = *stmt;
    stmt2->next = NULL;

    *iunit->text.tailptr = stmt2;
    iunit->text.tailptr = &stmt2->next;

    return 0;
}

int input(const char *progname, char **files, int nrfiles, struct iunit *iunit)
{
    char fake_file[3];
    char *fake_files[1];
    struct scan_state scan_state;
    int i;
    struct stmt stmt;
    int status;

    if (nrfiles <= 0) {
	fake_file[0] = '-';
	fake_file[1] = '-';
	fake_file[2] = '\0';
	fake_files[0] = fake_file;
	files = fake_files;
	nrfiles = 1;
    }

    iunit->text.head = NULL;
    iunit->text.tailptr = &iunit->text.head;

    scan_init(&scan_state, progname);

    for (i = 0; i < nrfiles; ++i) {
	if (scan_open(&scan_state, files[i]) < 0)
	    return -1;
	for (;;) {
	    status = parse_stmt(&scan_state, &stmt);
	    if (status < 0)
		return -1;
	    if (status == 0)
		break;
	    if (interpret(&scan_state, iunit, &stmt) < 0)
		return -1;
	}
    }

    return 0;
}
