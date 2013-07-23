/*
 * parse.h
 */
#ifndef PARSE_H
#define PARSE_H

#include "input.h"	/* for struct stmt */
#include "scan.h"

int parse_stmt(struct scan_state *scan_state, struct stmt *stmt);

#endif /* PARSE_H */
