/*
 * simtest1.s
 * Copyright (C) 2020  Mikael Pettersson
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

/*
 * int _start(int argc, char **argv, char **envp) { exit_group(42); asm("halt"); }
 */
	.file	"test9c.c"
	.text
	.globl	_start
	.type	_start,@function
_start:
	movei	1,42
	jsys	0136 # __NR_exit_group
	halt	0
	.size	_start,.-_start
