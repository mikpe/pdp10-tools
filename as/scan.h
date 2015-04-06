/*
 * scan.h
 * Copyright (C) 2013-2015  Mikael Pettersson
 *
 * This file is part of pdp10-tools.
 *
 * pdp10-tools is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pdp10-tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with pdp10-tools.  If not, see <http://www.gnu.org/licenses/>.
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
