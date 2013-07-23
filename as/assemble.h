/*
 * assemble.h
 */
#ifndef ASSEMBLE_H
#define ASSEMBLE_H

#include "pdp10-stdint.h"
#include "input.h"

struct aunit_symbol {
    struct aunit_symbol *next;
    const char *name;
    pdp10_uint36_t text_offset;
    int is_global;
    int is_defined;
};

struct aunit {
    pdp10_uint36_t *text_words;
    pdp10_uint36_t text_nr_words;
    struct aunit_symbol *symbols;
};

int assemble(const char *progname, struct iunit *iunit, struct aunit *aunit);

#endif /* ASSEMBLE_H */
