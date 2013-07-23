/*
 * scan.h
 */
#ifndef SCAN_H
#define SCAN_H

#include "token.h"

struct scan_state {
    const char *progname;	/* for diagnostics, does not change after scan_init() */
    const char *filename;	/* for diagnostics, set by scan_open() */
    unsigned int linenr;
};

void scan_init(struct scan_state *scan_state, const char *progname);
int scan_open(struct scan_state *scan_state, const char *filename);
enum token scan_token(struct scan_state *scan_state, union token_attribute *token_attr);

#endif /* SCAN_H */
