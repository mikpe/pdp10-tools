/*
 * test1.s
 * Copyright (C) 2013-2019  Mikael Pettersson
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

/* int foo(void) { return 27; }
 */
	.file	"test1.c"
	.text
	.globl	foo
	.type	foo,@function
foo:
	movei	1,033
	popj	017,
	.size	foo,.-foo
	.ident	"GCC: (GNU) 4.3.0.- for XKL-2 (XKL LLC, Kirkland, WA, USA)  Built 2013-08-15 23:03 +0200 on porter by mikpe"
