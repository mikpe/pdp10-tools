/* PDP-10 opcode list.
   Copyright (C) 2000 Free Software Foundation, Inc.

This file is part of GDB and GAS.

GDB and GAS are free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GDB and GAS are distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GDB or GAS; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef PDP10_OPCODES_H
#define PDP10_OPCODES_H

/*
 * PDP-10 notational conventions.
 *
 * A word is 36 bits wide, with bits numbered 0 to 35, left to right
 * (most significant to least significant).
 *
 * A doubleword is two adjacent words treated as a single 72-bit entity,
 * where the word with the lower address is on the left.  The direction
 * from more to less significance is always from lower to higher addresses.
 *
 * Bit numbers are always displayed in decimal.  Values in bit fields,
 * especially opcodes, are always displayed in octal.
 */

/*
 * PDP-10 instruction types.
 *
 * Most PDP-10 instructions have this format:
 *
 *             111 1 1111 112222222222333333
 *  012345678 9012 3 4567 890123456789012345
 * +---------+----+-+----+------------------+
 * |  opcode |  A |I|  X |         Y        |
 * +---------+----+-+----+------------------+
 *    9 bits    4  1   4        18 bits
 *
 * A usually specifies the accumulator.
 * I indicates indirect addressing.
 * X specifies the index register.
 * Y specifies the address.
 *
 * I, X, and Y are used together to calculate E, the effective address.
 *
 * PDP10_A_OPCODE means that A is used together with the opcode field
 * to specify the operation.
 *
 * PDP10_A_UNUSED means that A is unused and should be set to zero.
 *
 * PDP10_E_UNUSED means that I, X, and Y are unused and should be set to zero.
 *
 * PDP10_IO means that the instruction format looks like this:
 *
 *              111 1 1111 112222222222333333
 *  012 3456789 012 3 4567 890123456789012345
 * +---+-------+---+-+----+------------------+
 * |op1| device|op2|I|  X |         Y        |
 * +---+-------+---+-+----+------------------+
 *   3   7 bits   3 1   4       18 bits
 *
 * op1 and op2 are used together to specify the operation.
 *
 * I, X, and Y as above.
 *
 * PDP10_EXTEND means that this the first word of a two-word
 * instruction.  The second word is located at the effective address,
 * E, of the first instruction word.
 */

#define PDP10_BASIC	0x00
#define PDP10_A_OPCODE	0x01
#define PDP10_A_UNUSED	0x02
#define PDP10_E_UNUSED	0x04
#define PDP10_IO	0x08
#define PDP10_EXTEND	0x10

/*
 * PDP-10 CPU models.
 */

#define PDP10_NONE	0x00000000
#define PDP6_166	0x00000001	/* DEC PDP-6 Type 166 Arithmetic Processor */
#define PDP10_KA10	0x00000002	/* DEC PDP-10 KA10 */
#define PDP10_KA10_ITS	0x00000004	/* DEC PDP-10 KA10, modifications for ITS */
#define PDP10_KI10	0x00000008	/* DEC PDP-10 KI10 */
#define PDP10_KL10	0x00000010	/* DEC PDP-10 KL10 */
#define PDP10_KL10_ITS	0x00000020	/* DEC PDP-10 KL10, ITS microcode */
#define PDP10_KL10_271	0x00000040	/* DEC PDP-10 KL10, microcode version >= 271 */
#define PDP10_KS10	0x00000080	/* DEC PDP-10 KS10 */
#define PDP10_KS10_ITS	0x00000100	/* DEC PDP-10 KS10, ITS microcode */
#define PDP10_KC10	0x00000200	/* DEC PDP-10 KC10 */
#define PDP10_XKL1	0x00000400	/* XKL TOAD-1 XKL-1 */
#define PDP10_ALL	0xffffffff

struct pdp10_instruction
{
  const char *name;
  unsigned int opcode;
  unsigned int type;
  unsigned int model;
};

struct pdp10_device
{
  const char *name;
  unsigned number;
  unsigned int model;
};

extern const struct pdp10_instruction pdp10_instruction[];
extern const unsigned int pdp10_num_instructions;

extern const struct pdp10_device pdp10_device[];
extern const unsigned int pdp10_num_devices;

extern const struct pdp10_instruction pdp10_alias[];
extern const unsigned int pdp10_num_aliases;

extern const struct pdp10_instruction pdp10_extended_instruction[];
extern const unsigned int pdp10_num_extended_instructions;

#endif /* PDP10_OPCODES_H */
