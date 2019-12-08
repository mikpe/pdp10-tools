/*
 * test4.s
 * Copyright (C) 2019  Mikael Pettersson
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

/* This exercises the section-switching directives and should assemble
   to a sequence of moves with monotonically insreasing immediates.  */

	.text	0		# cur 0, prev -, stack []
	.globl	foo
	.type	foo,@function
foo:
	movei	0, 0
	.pushsection .text, 1	# cur 1, prev 0, stack [{0,-}]
	movei	0, 2
	.text	2		# cur 2, prev 1, stack [{0,-}]
	movei	0, 4
	.previous		# cur 1, prev 2, stack [{0,-}]
	movei	0, 3
	.popsection		# cur 0, prev -, stack []
	movei	0, 1
	.size	foo,.-foo
