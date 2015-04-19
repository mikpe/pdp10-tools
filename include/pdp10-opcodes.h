/*
 * pdp10-opcodes.h
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
/*
 * Word Representation
 * ===================
 *
 *
 *            11111111112222222222333333
 *  012345678901234567890123456789012345
 * +------------------------------------+
 * |                                    |
 * +------------------------------------+
 *
 * The basic storage unit is a 36-bit wide word.  Its bits are numbered 0
 * to 35, in left-to-right order, with bit 0 being the most significant
 * and bit 35 the least significant.  (Similar to IBM's bit numbering but
 * opposite to most modern processors.)
 *
 * The architecture supports sub-word storage units via special instructions
 * and specially formatted "byte" pointers, where a byte may be from 0 to 36
 * bits wide.  Incrementing a byte pointer moves it right over a word towards
 * its less significant bits, implying a big-endian storage model.
 *
 * A 72-bit long integer is composed of two adjacent words.  It stores the most
 * significant bits in first word (lower address) and the least significant bits
 * in the second word (higher address), again implying a big-endian storage model.
 *
 *
 * Instruction Representation
 * ==========================
 *
 * Basic instructions are stored in 36-bit words with the following format:
 *
 *             111 1 1111 112222222222333333
 *  012345678 9012 3 4567 890123456789012345
 * +---------+----+-+----+------------------+
 * |  opcode |  A |I|  X |         Y        |
 * +---------+----+-+----+------------------+
 *    9 bits    4  1   4        18 bits
 *
 * A 9-bit opcode is stored in the high 9 bits.
 * A is a 4-bit field specifying the accumulator (a register).
 * I is a 1-bit field specifying indirect addressing.
 * X is a 4-bit field specifying the index register.
 * Y is an 18-bit field specifying an address or offset.
 *
 * E, the effective addreess, is computed from I, X, and Y.
 *
 * In some instructions A contains further opcode bits.
 *
 * In some instructions A is unused and should be zero.
 *
 * Instructions that not compute an effective address E
 * should have I, X, and Y set to zero.
 *
 * IO instructions have a slightly different format:
 *
 *              111 1 1111 112222222222333333
 *  012 3456789 012 3 4567 890123456789012345
 * +---+-------+---+-+----+------------------+
 * |op1| device|op2|I|  X |         Y        |
 * +---+-------+---+-+----+------------------+
 *   3   7 bits   3 1   4       18 bits
 *
 * The op1 field is all-bits-one (7), the device field addresses the selected device,
 * and the op2 field specifies the operation.  Both devices internal to the processor
 * and devices attached via external buses can be accessed.
 *
 * Some non-IO instructions also have a 7 in their high three bits.
 *
 * Extended instructions consist of two separate instruction words:
 *
 * A:
 *             111 1 1111 112222222222333333
 *  012345678 9012 3 4567 890123456789012345
 * +---------+----+-+----+------------------+
 * |    0123 |  A |I|  X |         Y        |
 * +---------+----+-+----+------------------+
 *    9 bits    4  1   4        18 bits
 *
 * E0:
 *             111 1 1111 112222222222333333
 *  012345678 9012 3 4567 890123456789012345
 * +---------+----+-+----+------------------+
 * | xopcode |0000|I|  X |         Y        |
 * +---------+----+-+----+------------------+
 *    9 bits    4  1   4        18 bits
 *
 * The first word is stored at address A in the instruction stream in the basic
 * format with opcode 0123.  The second word is stored at the effective address
 * E0 specified by the the first word.  Its accumulator field is unused and must
 * be zero for compatibility with future extensions.
 */

/*
 * Known PDP10 CPU models, each represented by a distinct bit value.
 *
 * These are combined with bit-wise 'and', 'or', and 'not' operations
 * to form sets of CPU models, used to check if a given mnemonic or
 * opcode is available for a selected set of CPUs.
 */

enum {
    /*
     * DEC processors.
     */

    PDP10_NONE		= 0,
    PDP6_166		= 1 << 0,	/* DEC PDP-6 Type 166 Arithmetic Processor */
    PDP10_KA10		= 1 << 1,	/* DEC PDP-10 KA10 */
    PDP10_KA10_ITS	= 1 << 2,	/* DEC PDP-10 KA10, ITS microcode */
    PDP10_KI10		= 1 << 3,	/* DEC PDP-10 KI10 */
    PDP10_KL10		= 1 << 4,	/* DEC PDP-10 KL10 */
    PDP10_KL10_ITS	= 1 << 5,	/* DEC PDP-10 KL10, ITS microcode */
    PDP10_KL10_271	= 1 << 6,	/* DEC PDP-10 KL10, microcode version >= 271 (many extensions) */
    PDP10_KS10		= 1 << 7,	/* DEC PDP-10 KS10 */
    PDP10_KS10_ITS	= 1 << 8,	/* DEC PDP-10 KS10, ITS microcode */
    PDP10_KC10		= 1 << 9,	/* DEC PDP-10 KC10 (Jupiter, full extended addressing) */
    PDP10_KD10		= 1 << 10,	/* DEC PDP-10 KD10 (Minnow, KS10 extended to match KC10 specs) */

    /*
     * XKL Processors.
     *
     * The XKL-2 is believed to have been built, and to contain some instruction set
     * extensions, but no details are known about it at this time.
     */

    PDP10_XKL1		= 1 << 11,	/* XKL TOAD-1 XKL-1, KL10 clone with KC10-like full extended addressing */

    /*
     * Other clones, not yet supported.
     *
     * System Concepts SC-20, SC-25, SC-30M, SC-40 (KC10-like)
     *
     * Foonly F-1, F-2, F-3, F-4 (KI10/KL10-hybrid)
     *
     * Xerox PARC MAXC (KI10-like?)
     */

    /*
     * Convenience constants for combinations of CPU models.
     */

    PDP10_ALL = PDP10_XKL1 | (PDP10_XKL1 - 1), /* XXX: depends on XKL1 being last above */

    PDP10_KL10_271up = PDP10_KL10_271 | PDP10_XKL1,
    PDP10_KL10any = PDP10_KL10 | PDP10_KL10_ITS | PDP10_KL10_271up,
    PDP10_KL10up = PDP10_KL10any | PDP10_KS10,

    PDP10_KI10_to_KL10 = PDP10_KI10 | PDP10_KL10any,
    PDP10_KI10up = PDP10_KI10 | PDP10_KL10up,

    PDP10_KA10any = PDP10_KA10 | PDP10_KA10_ITS,
    PDP10_KA10up = PDP10_KA10any | PDP10_KI10up,
    PDP10_KA10_to_KI10 = PDP10_KA10 | PDP10_KI10, /* XXX: should that be KA10_any? */
    PDP10_KA10_to_KL10 = PDP10_KA10_to_KI10 | PDP10_KL10any,

    PDP10_not_KS10_or_XKL1 = PDP10_ALL & ~(PDP10_KS10 | PDP10_XKL1), /* XXX: should that be KS10_any? */

    PDP10_ITS = PDP10_KA10_ITS | PDP10_KL10_ITS | PDP10_KS10_ITS,

    PDP6_166_to_PDP10_KI10 = PDP6_166 | PDP10_KA10_to_KI10,
};

typedef unsigned short pdp10_cpu_models_t;

/*
 * Device names for IO instructions.
 */

struct pdp10_cpu_device {
    const char *name;
    unsigned char device;	/* device field in bits 3-9 of IO instructions */
    pdp10_cpu_models_t models;
};

const struct pdp10_cpu_device *
pdp10_cpu_device_from_name(pdp10_cpu_models_t models, const char *name);

/*
 * Instructions.
 */

enum {
    /* Each instruction belongs to exactly one of these primary categories,
       which determine how the high 13 bits are to be interpreted:  */
    /* XXX: change this to separate mutually exclusive bits to simplify usage */
    PDP10_INSN_BASIC = 0,
    PDP10_INSN_A_OPCODE = 1,
    PDP10_INSN_A_UNUSED = 2,
    PDP10_INSN_IO = 3,

    /* Flag set to indicate that E is unused.  */
    PDP10_INSN_E_UNUSED = 4,

    /* Flag set to indicate that this is the second word of an extended instruction.  */
    PDP10_INSN_EXTENDED = 8,
};

typedef unsigned char pdp10_insn_fmt_t;

struct pdp10_insn {
    const char *name;
    /*
     * The high13 field is 13 bits, formatted as:
     * <9 bit opcode><0000>	BASIC, A_UNUSED
     * <9 + 4 bit opcode>	A_OPCODE
     * <111><0000000><3 bit op> IO
     *
     * An extended instruction uses the BASIC format with opcode 0123 for
     * the first word, and the A_UNUSED | EXTENDED format for the second word.
     */
    unsigned short high13;
    pdp10_insn_fmt_t fmt;
    pdp10_cpu_models_t models;
};

/* for assembly */
const struct pdp10_insn *
pdp10_insn_from_name(pdp10_cpu_models_t models, const char *name);

/* for disassembly */
const struct pdp10_insn *
pdp10_insn_from_high13(pdp10_cpu_models_t models, unsigned int high13, int extended);
