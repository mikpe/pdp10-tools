/*
 * scan.h
 */
#ifndef SCAN_H
#define SCAN_H

#include "token.h"

const char *scan_filename;
int scan_freopen(const char *filename);

unsigned int scan_linenr;
enum token scan(union token_attribute *token_attr);

#endif /* SCAN_H */
