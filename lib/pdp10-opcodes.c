/*
 * pdp10-opcodes.c
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
#include <string.h>
#include "pdp10-opcodes.h"

/*
 * Macros to initialize both high13 and fmt in instruction entries.
 * May be followed by | PDP10_INSN_E_UNUSED and/or | PDP10_INSN_EXTENDED.
 *
 * The convention in documentation is to list opcodes as three or five-digit
 * octal numbers, with zeros in IO device subfields.  Five-digit numbers have
 * two excess bits, which are removed by the macros to produce 13 bits.
 */

#define BASIC(OPCODE9)		((OPCODE9) << 4), PDP10_INSN_BASIC
#define A_OPCODE(OPCODE15)	((OPCODE15) >> 2), PDP10_INSN_A_OPCODE
#define A_UNUSED(OPCODE9)	((OPCODE9) << 4), PDP10_INSN_A_UNUSED
#define IO(OPCODE15)		((OPCODE15) >> 2), PDP10_INSN_IO

/* Much of the contents of these tables is based on code in
   Lars Brinkhoff's pdp10-its-disassembler, but the code has
   since been completely rewritten and extended.  */

static const struct pdp10_insn pdp10_insn[] = {
    /* name,		high13,	fmt,		models */

    /* 000: ILLEGAL */
    /* 001-037: LUUOs */

    /* ITS MUUOs */
    { ".iot",		BASIC(0040),		PDP10_ITS },
    { ".open",		BASIC(0041),		PDP10_ITS },
    { ".oper",		BASIC(0042),		PDP10_ITS },
    { ".call",		A_OPCODE(004300),	PDP10_ITS },
    { ".dismis",	A_OPCODE(004304),	PDP10_ITS },
    { ".lose",		A_OPCODE(004310),	PDP10_ITS }, /* XXX: .trans? */
    { ".tranad",	A_OPCODE(004314),	PDP10_ITS },
    { ".value",		A_OPCODE(004320),	PDP10_ITS },
    { ".utran",		A_OPCODE(004324),	PDP10_ITS },
    { ".core",		A_OPCODE(004330),	PDP10_ITS },
    { ".trand",		A_OPCODE(004334),	PDP10_ITS },
    { ".dstart",	A_OPCODE(004340),	PDP10_ITS },
    { ".fdele",		A_OPCODE(004344),	PDP10_ITS },
    { ".dstrtl",	A_OPCODE(004350),	PDP10_ITS },
    { ".suset",		A_OPCODE(004354),	PDP10_ITS },
    { ".ltpen",		A_OPCODE(004360),	PDP10_ITS },
    { ".vscan",		A_OPCODE(004364),	PDP10_ITS },
    { ".potset",	A_OPCODE(004370),	PDP10_ITS },
    { ".uset",		BASIC(0044),		PDP10_ITS },
    { ".break",		BASIC(0045),		PDP10_ITS },
    { ".status",	BASIC(0046),		PDP10_ITS },
    { ".access",	BASIC(0047),		PDP10_ITS },

    /* TOPS-10 MUUOs (formats and models guesstimates) [XXX: all should be & ~PDP10_ITS] */
    { ".call",		BASIC(0040),		PDP10_KA10up },
    { ".init",		BASIC(0041),		PDP10_KA10up },
    /* 042-046: reserved MUUOs */
    { ".calli",		BASIC(0047),		PDP10_KA10up },
    { ".open",		BASIC(0050),		PDP10_KA10up },
    { ".ttcall",	BASIC(0051),		PDP10_KA10up },
    { ".rename",	BASIC(0055),		PDP10_KA10up },
    { ".in",		BASIC(0056),		PDP10_KA10up },
    { ".out",		BASIC(0057),		PDP10_KA10up },
    { ".setsts",	BASIC(0060),		PDP10_KA10up },
    { ".stato",		BASIC(0061),		PDP10_KA10up },
    { ".status",	BASIC(0062),		PDP10_KA10up },
    { ".getsts",	BASIC(0062),		PDP10_KA10up }, /* XXX: alias for .status? */
    { ".statz",		BASIC(0063),		PDP10_KA10up },
    { ".inbuf",		BASIC(0064),		PDP10_KA10up },
    { ".outbuf",	BASIC(0065),		PDP10_KA10up },
    { ".input",		BASIC(0066),		PDP10_KA10up },
    { ".output",	BASIC(0067),		PDP10_KA10up },
    { ".close",		BASIC(0070),		PDP10_KA10up },
    { ".releas",	BASIC(0071),		PDP10_KA10up },
    { ".mtape",		BASIC(0072),		PDP10_KA10up },
    { ".ugetf",		BASIC(0073),		PDP10_KA10up },
    { ".useti",		BASIC(0074),		PDP10_KA10up },
    { ".useto",		BASIC(0075),		PDP10_KA10up },
    { ".lookup",	BASIC(0076),		PDP10_KA10up },
    { ".enter",		BASIC(0077),		PDP10_KA10up },
    { ".ujen",		BASIC(0100),		PDP10_KA10 }, /* XXX: or KI10up??? */

    /* 101: unassigned */
    { "gfad",		BASIC(0102),		PDP10_KL10_271 }, /* XXX: or 271up??? */
    { "gfsb",		BASIC(0103),		PDP10_KL10_271 }, /* XXX: or 271up??? */

    /* TOPS-20 MUUO */
    { "jsys",		BASIC(0104),		PDP10_KI10up }, /* XXX: & ~PDP10_ITS */

    { "adjsp",		BASIC(0105),		PDP10_KL10up },
    { "gfmp",		BASIC(0106),		PDP10_KL10_271 }, /* XXX: or 271up??? */
    { "gfdv",		BASIC(0107),		PDP10_KL10_271 }, /* XXX: or 271up??? */
    { "dfad",		BASIC(0110),		PDP10_KI10up },
    { "dfsb",		BASIC(0111),		PDP10_KI10up },
    { "dfmp",		BASIC(0112),		PDP10_KI10up },
    { "dfdv",		BASIC(0113),		PDP10_KI10up },
    { "dadd",		BASIC(0114),		PDP10_KL10up },
    { "dsub",		BASIC(0115),		PDP10_KL10up },
    { "dmul",		BASIC(0116),		PDP10_KL10up },
    { "ddiv",		BASIC(0117),		PDP10_KL10up },
    { "dmove",		BASIC(0120),		PDP10_KI10up },
    { "dmovn",		BASIC(0121),		PDP10_KI10up },
    { "fix",		BASIC(0122),		PDP10_KI10up },
    { "extend",		BASIC(0123),		PDP10_KL10up },
    { "dmovem",		BASIC(0124),		PDP10_KI10up },
    { "dmovnm",		BASIC(0125),		PDP10_KI10up },
    { "fixr",		BASIC(0126),		PDP10_KI10up },
    { "fltr",		BASIC(0127),		PDP10_KI10up },
    { "ufa",		BASIC(0130),		PDP10_KA10_to_KI10 }, /* XXX: and TOPS-10 KL10 */
    { "dfn",		BASIC(0131),		PDP10_KA10_to_KI10 }, /* XXX: and TOPS-10 KL10 */
    { "fsc",		BASIC(0132),		PDP10_ALL },
    { "ibp",		A_OPCODE(013300),	PDP10_ALL },
    { "adjbp",		BASIC(0133),		PDP10_KL10up }, /* A != 0 */
    { "ildb",		BASIC(0134),		PDP10_ALL },
    { "ldb",		BASIC(0135),		PDP10_ALL },
    { "idpb",		BASIC(0136),		PDP10_ALL },
    { "dpb",		BASIC(0137),		PDP10_ALL },
    { "fad",		BASIC(0140),		PDP10_ALL },
    { "fadl",		BASIC(0141),		PDP6_166_to_PDP10_KI10 }, /* XXX: and TOPS-10 KL10, not the PDP6? */
    { "fadm",		BASIC(0142),		PDP10_ALL },
    { "fadb",		BASIC(0143),		PDP10_ALL },
    { "fadr",		BASIC(0144),		PDP10_ALL },
    { "fadri",		BASIC(0145),		PDP10_KA10up },
    { "fadrl",		BASIC(0145),		PDP6_166 }, /* XXX: conflicts with fadri? */
    { "fadrm",		BASIC(0146),		PDP10_ALL },
    { "fadrb",		BASIC(0147),		PDP10_ALL },
    { "fsb",		BASIC(0150),		PDP10_ALL },
    { "fsbl",		BASIC(0151),		PDP6_166_to_PDP10_KI10 }, /* XXX: and TOPS-10 KL10 */
    { "fsbm",		BASIC(0152),		PDP10_ALL },
    { "fsbb",		BASIC(0153),		PDP10_ALL },
    { "fsbr",		BASIC(0154),		PDP10_ALL },
    { "fsbri",		BASIC(0155),		PDP10_KA10up },
    { "fsbrl",		BASIC(0155),		PDP6_166 }, /* XXX: conflicts with fsbri? */
    { "fsbrm",		BASIC(0156),		PDP10_ALL },
    { "fsbrb",		BASIC(0157),		PDP10_ALL },
    { "fmp",		BASIC(0160),		PDP10_ALL },
    { "fmpl",		BASIC(0161),		PDP6_166_to_PDP10_KI10 }, /* XXX: and TOPS-10 KL10 */
    { "fmpm",		BASIC(0162),		PDP10_ALL },
    { "fmpb",		BASIC(0163),		PDP10_ALL },
    { "fmpr",		BASIC(0164),		PDP10_ALL },
    { "fmpri",		BASIC(0165),		PDP10_KA10up },
    { "fmprl",		BASIC(0165),		PDP6_166 }, /* XXX: conflicts with fmpri? */
    { "fmprm",		BASIC(0166),		PDP10_ALL },
    { "fmprb",		BASIC(0167),		PDP10_ALL },
    { "fdv",		BASIC(0170),		PDP10_ALL },
    { "fdvl",		BASIC(0171),		PDP6_166_to_PDP10_KI10 }, /* XXX: and TOPS-10 KL10 */
    { "fdvm",		BASIC(0172),		PDP10_ALL },
    { "fdvb",		BASIC(0173),		PDP10_ALL },
    { "fdvr",		BASIC(0174),		PDP10_ALL },
    { "fdvri",		BASIC(0175),		PDP10_KA10up },
    { "fdvrl",		BASIC(0175),		PDP6_166 }, /* XXX: conflicts with fdvri? */
    { "fdvrm",		BASIC(0176),		PDP10_ALL },
    { "fdvrb",		BASIC(0177),		PDP10_ALL },
    { "move",		BASIC(0200),		PDP10_ALL },
    { "movei",		BASIC(0201),		PDP10_ALL },
    { "movem",		BASIC(0202),		PDP10_ALL },
    { "moves",		BASIC(0203),		PDP10_ALL },
    { "movs",		BASIC(0204),		PDP10_ALL },
    { "movsi",		BASIC(0205),		PDP10_ALL },
    { "movsm",		BASIC(0206),		PDP10_ALL },
    { "movss",		BASIC(0207),		PDP10_ALL },
    { "movn",		BASIC(0210),		PDP10_ALL },
    { "movni",		BASIC(0211),		PDP10_ALL },
    { "movnm",		BASIC(0212),		PDP10_ALL },
    { "movns",		BASIC(0213),		PDP10_ALL },
    { "movm",		BASIC(0214),		PDP10_ALL },
    { "movmi",		BASIC(0215),		PDP10_ALL },
    { "movmm",		BASIC(0216),		PDP10_ALL },
    { "movms",		BASIC(0217),		PDP10_ALL },
    { "imul",		BASIC(0220),		PDP10_ALL },
    { "imuli",		BASIC(0221),		PDP10_ALL },
    { "imulm",		BASIC(0222),		PDP10_ALL },
    { "imulb",		BASIC(0223),		PDP10_ALL },
    { "mul",		BASIC(0224),		PDP10_ALL },
    { "muli",		BASIC(0225),		PDP10_ALL },
    { "mulm",		BASIC(0226),		PDP10_ALL },
    { "mulb",		BASIC(0227),		PDP10_ALL },
    { "idiv",		BASIC(0230),		PDP10_ALL },
    { "idivi",		BASIC(0231),		PDP10_ALL },
    { "idivm",		BASIC(0232),		PDP10_ALL },
    { "idivb",		BASIC(0233),		PDP10_ALL },
    { "div",		BASIC(0234),		PDP10_ALL },
    { "divi",		BASIC(0235),		PDP10_ALL },
    { "divm",		BASIC(0236),		PDP10_ALL },
    { "divb",		BASIC(0237),		PDP10_ALL },
    { "ash",		BASIC(0240),		PDP10_ALL },
    { "rot",		BASIC(0241),		PDP10_ALL },
    { "lsh",		BASIC(0242),		PDP10_ALL },
    { "jffo",		BASIC(0243),		PDP10_KA10up },
    { "ashc",		BASIC(0244),		PDP10_ALL },
    { "rotc",		BASIC(0245),		PDP10_ALL },
    { "lshc",		BASIC(0246),		PDP10_ALL },
    /* 247: MUUO (XKL-1, KD10, KC10), unassigned/trapping (KI10, KL10, KS10?), unassigned/nop (KA10, PDP-6?) */
    { "exch",		BASIC(0250),		PDP10_ALL },
    { "blt",		BASIC(0251),		PDP10_ALL },
    { "aobjp",		BASIC(0252),		PDP10_ALL },
    { "aobjn",		BASIC(0253),		PDP10_ALL },

    /*
     * 254: JRST instruction family.
     * Special cases first, followed by the generic entry.
     * 25414, 25440 (jrstil?), 25444, 25454, 25464, 25470, 25470: unassigned
     */
    { "portal",		A_OPCODE(025404),	PDP10_ALL },
    { "jrstf",		A_OPCODE(025410),	PDP10_ALL },
    { "halt",		A_OPCODE(025420),	PDP10_ALL },
    { "xjrstf",		A_OPCODE(025424),	PDP10_KL10up },
    { "xjen",		A_OPCODE(025430),	PDP10_KL10up },
    { "xpcw",		A_OPCODE(025434),	PDP10_KL10up },
    { "jen",		A_OPCODE(025450),	PDP10_ALL },
    { "sfm",		A_OPCODE(025460),	PDP10_KL10up },
    { "jrst",		A_UNUSED(0254),		PDP10_ALL },

    /*
     * 255: JFCL instruction family.
     * Special cases first, followed by the generic entry.
     * 25514, 25524, 25534, 25544, 25550, 25554, 25560, 25564, 25570, 25574: unassigned
     */
    { "nop",		A_OPCODE(025500) | PDP10_INSN_E_UNUSED,	PDP10_ALL },
    { "jfov",		A_OPCODE(025504),	PDP10_KA10up },
    { "jpcch",		A_OPCODE(025504),	PDP6_166 }, /* XXX: CHECKME, conflicts with jfov? */
    { "jcry1",		A_OPCODE(025510),	PDP10_ALL },
    { "jcry0",		A_OPCODE(025520),	PDP10_ALL },
    { "jcry",		A_OPCODE(025530),	PDP10_ALL },
    { "jov",		A_OPCODE(025540),	PDP10_ALL },
    { "jfcl",		BASIC(0255),		PDP10_ALL },

    { "xct",		A_UNUSED(0256),		PDP10_ALL }, /* A zero, or in user mode, or is a KA10 */
    { "pxct",		BASIC(0256),		PDP10_ALL }, /* A non-zero and in executive mode */
    { "map",		BASIC(0257),		PDP10_KA10_to_KI10 }, /* XXX: and TOPS-10 KL10, nop on KA10 */
    { "pushj",		BASIC(0260),		PDP10_ALL },
    { "push",		BASIC(0261),		PDP10_ALL },
    { "pop",		BASIC(0262),		PDP10_ALL },
    { "popj",		BASIC(0263) | PDP10_INSN_E_UNUSED,	PDP10_ALL },
    { "jsr",		A_UNUSED(0264),		PDP10_ALL },
    { "jsp",		BASIC(0265),		PDP10_ALL },
    { "jsa",		BASIC(0266),		PDP10_ALL },
    { "jra",		BASIC(0267),		PDP10_ALL },
    { "add",		BASIC(0270),		PDP10_ALL },
    { "addi",		BASIC(0271),		PDP10_ALL },
    { "addm",		BASIC(0272),		PDP10_ALL },
    { "addb",		BASIC(0273),		PDP10_ALL },
    { "sub",		BASIC(0274),		PDP10_ALL },
    { "subi",		BASIC(0275),		PDP10_ALL },
    { "subm",		BASIC(0276),		PDP10_ALL },
    { "subb",		BASIC(0277),		PDP10_ALL },
    { "cai",		BASIC(0300),		PDP10_ALL },
    { "cail",		BASIC(0301),		PDP10_ALL },
    { "caie",		BASIC(0302),		PDP10_ALL },
    { "caile",		BASIC(0303),		PDP10_ALL },
    { "caia",		BASIC(0304),		PDP10_ALL },
    { "caige",		BASIC(0305),		PDP10_ALL },
    { "cain",		BASIC(0306),		PDP10_ALL },
    { "caig",		BASIC(0307),		PDP10_ALL },
    { "cam",		BASIC(0310),		PDP10_ALL },
    { "caml",		BASIC(0311),		PDP10_ALL },
    { "came",		BASIC(0312),		PDP10_ALL },
    { "camle",		BASIC(0313),		PDP10_ALL },
    { "cama",		BASIC(0314),		PDP10_ALL },
    { "camge",		BASIC(0315),		PDP10_ALL },
    { "camn",		BASIC(0316),		PDP10_ALL },
    { "camg",		BASIC(0317),		PDP10_ALL },
    { "jump",		BASIC(0320),		PDP10_ALL },
    { "jumpl",		BASIC(0321),		PDP10_ALL },
    { "jumpe",		BASIC(0322),		PDP10_ALL },
    { "jumple",		BASIC(0323),		PDP10_ALL },
    { "jumpa",		BASIC(0324),		PDP10_ALL },
    { "jumpge",		BASIC(0325),		PDP10_ALL },
    { "jumpn",		BASIC(0326),		PDP10_ALL },
    { "jumpg",		BASIC(0327),		PDP10_ALL },
    { "skip",		BASIC(0330),		PDP10_ALL },
    { "skipl",		BASIC(0331),		PDP10_ALL },
    { "skipe",		BASIC(0332),		PDP10_ALL },
    { "skiple",		BASIC(0333),		PDP10_ALL },
    { "skipa",		BASIC(0334),		PDP10_ALL },
    { "skipge",		BASIC(0335),		PDP10_ALL },
    { "skipn",		BASIC(0336),		PDP10_ALL },
    { "skipg",		BASIC(0337),		PDP10_ALL },
    { "aoj",		BASIC(0340),		PDP10_ALL },
    { "aojl",		BASIC(0341),		PDP10_ALL },
    { "aoje",		BASIC(0342),		PDP10_ALL },
    { "aojle",		BASIC(0343),		PDP10_ALL },
    { "aoja",		BASIC(0344),		PDP10_ALL },
    { "aojge",		BASIC(0345),		PDP10_ALL },
    { "aojn",		BASIC(0346),		PDP10_ALL },
    { "aojg",		BASIC(0347),		PDP10_ALL },
    { "aos",		BASIC(0350),		PDP10_ALL },
    { "aosl",		BASIC(0351),		PDP10_ALL },
    { "aose",		BASIC(0352),		PDP10_ALL },
    { "aosle",		BASIC(0353),		PDP10_ALL },
    { "aosa",		BASIC(0354),		PDP10_ALL },
    { "aosge",		BASIC(0355),		PDP10_ALL },
    { "aosn",		BASIC(0356),		PDP10_ALL },
    { "aosg",		BASIC(0357),		PDP10_ALL },
    { "soj",		BASIC(0360),		PDP10_ALL },
    { "sojl",		BASIC(0361),		PDP10_ALL },
    { "soje",		BASIC(0362),		PDP10_ALL },
    { "sojle",		BASIC(0363),		PDP10_ALL },
    { "soja",		BASIC(0364),		PDP10_ALL },
    { "sojge",		BASIC(0365),		PDP10_ALL },
    { "sojn",		BASIC(0366),		PDP10_ALL },
    { "sojg",		BASIC(0367),		PDP10_ALL },
    { "sos",		BASIC(0370),		PDP10_ALL },
    { "sosl",		BASIC(0371),		PDP10_ALL },
    { "sose",		BASIC(0372),		PDP10_ALL },
    { "sosle",		BASIC(0373),		PDP10_ALL },
    { "sosa",		BASIC(0374),		PDP10_ALL },
    { "sosge",		BASIC(0375),		PDP10_ALL },
    { "sosn",		BASIC(0376),		PDP10_ALL },
    { "sosg",		BASIC(0377),		PDP10_ALL },
    { "setz",		BASIC(0400) | PDP10_INSN_E_UNUSED,	PDP10_ALL },
    { "setzi",		BASIC(0401) | PDP10_INSN_E_UNUSED,	PDP10_ALL },
    { "setzm",		A_UNUSED(0402),		PDP10_ALL },
    { "setzb",		BASIC(0403),		PDP10_ALL },
    { "and",		BASIC(0404),		PDP10_ALL },
    { "andi",		BASIC(0405),		PDP10_ALL },
    { "andm",		BASIC(0406),		PDP10_ALL },
    { "andb",		BASIC(0407),		PDP10_ALL },
    { "andca",		BASIC(0410),		PDP10_ALL },
    { "andcai",		BASIC(0411),		PDP10_ALL },
    { "andcam",		BASIC(0412),		PDP10_ALL },
    { "andcab",		BASIC(0413),		PDP10_ALL },
    { "setm",		BASIC(0414),		PDP10_ALL },
    { "xmovei",		BASIC(0415),		PDP10_KL10up }, /* in non-zero section, setmi in zero section */
    { "setmi",		BASIC(0415),		PDP10_ALL }, /* on KL10up, depends on current section */
    { "setmm",		A_UNUSED(0416),		PDP10_ALL },
    { "setmb",		BASIC(0417),		PDP10_ALL },
    { "andcm",		BASIC(0420),		PDP10_ALL },
    { "andcmi",		BASIC(0421),		PDP10_ALL },
    { "andcmm",		BASIC(0422),		PDP10_ALL },
    { "andcmb",		BASIC(0423),		PDP10_ALL },
    { "seta",		BASIC(0424) | PDP10_INSN_E_UNUSED,	PDP10_ALL },
    { "setai",		BASIC(0425) | PDP10_INSN_E_UNUSED,	PDP10_ALL },
    { "setam",		BASIC(0426),		PDP10_ALL },
    { "setab",		BASIC(0427),		PDP10_ALL },
    { "xor",		BASIC(0430),		PDP10_ALL },
    { "xori",		BASIC(0431),		PDP10_ALL },
    { "xorm",		BASIC(0432),		PDP10_ALL },
    { "xorb",		BASIC(0433),		PDP10_ALL },
    { "ior",		BASIC(0434),		PDP10_ALL },
    { "or",		BASIC(0434),		PDP10_ALL }, /* alias for ior */
    { "iori",		BASIC(0435),		PDP10_ALL },
    { "ori",		BASIC(0435),		PDP10_ALL }, /* alias for iori */
    { "iorm",		BASIC(0436),		PDP10_ALL },
    { "orm",		BASIC(0436),		PDP10_ALL }, /* alias for iorm */
    { "iorb",		BASIC(0437),		PDP10_ALL },
    { "orb",		BASIC(0437),		PDP10_ALL }, /* alias for iorb */
    { "andcb",		BASIC(0440),		PDP10_ALL },
    { "andcbi",		BASIC(0441),		PDP10_ALL },
    { "andcbm",		BASIC(0442),		PDP10_ALL },
    { "andcbb",		BASIC(0443),		PDP10_ALL },
    { "eqv",		BASIC(0444),		PDP10_ALL },
    { "eqvi",		BASIC(0445),		PDP10_ALL },
    { "eqvm",		BASIC(0446),		PDP10_ALL },
    { "eqvb",		BASIC(0447),		PDP10_ALL },
    { "setca",		BASIC(0450) | PDP10_INSN_E_UNUSED,	PDP10_ALL },
    { "setcai",		BASIC(0451) | PDP10_INSN_E_UNUSED,	PDP10_ALL },
    { "setcam",		BASIC(0452),		PDP10_ALL },
    { "setcab",		BASIC(0453),		PDP10_ALL },
    { "orca",		BASIC(0454),		PDP10_ALL },
    { "orcai",		BASIC(0455),		PDP10_ALL },
    { "orcam",		BASIC(0456),		PDP10_ALL },
    { "orcab",		BASIC(0457),		PDP10_ALL },
    { "setcm",		BASIC(0460),		PDP10_ALL },
    { "setcmi",		BASIC(0461),		PDP10_ALL },
    { "setcmm",		A_UNUSED(0462),		PDP10_ALL },
    { "setcmb",		BASIC(0463),		PDP10_ALL },
    { "orcm",		BASIC(0464),		PDP10_ALL },
    { "orcmi",		BASIC(0465),		PDP10_ALL },
    { "orcmm",		BASIC(0466),		PDP10_ALL },
    { "orcmb",		BASIC(0467),		PDP10_ALL },
    { "orcb",		BASIC(0470),		PDP10_ALL },
    { "orcbi",		BASIC(0471),		PDP10_ALL },
    { "orcbm",		BASIC(0472),		PDP10_ALL },
    { "orcbb",		BASIC(0473),		PDP10_ALL },
    { "seto",		BASIC(0474) | PDP10_INSN_E_UNUSED,	PDP10_ALL },
    { "setoi",		BASIC(0475) | PDP10_INSN_E_UNUSED,	PDP10_ALL },
    { "setom",		A_UNUSED(0476),		PDP10_ALL },
    { "setob",		BASIC(0477),		PDP10_ALL },
    { "hll",		BASIC(0500),		PDP10_ALL },
    { "xhlli",		BASIC(0501),		PDP10_KL10up }, /* in non-zero section, hlli in zero section */
    { "hlli",		BASIC(0501),		PDP10_ALL }, /* on KL10up, depends on current section */
    { "hllm",		BASIC(0502),		PDP10_ALL },
    { "hlls",		BASIC(0503),		PDP10_ALL },
    { "hrl",		BASIC(0504),		PDP10_ALL },
    { "hrli",		BASIC(0505),		PDP10_ALL },
    { "hrlm",		BASIC(0506),		PDP10_ALL },
    { "hrls",		BASIC(0507),		PDP10_ALL },
    { "hllz",		BASIC(0510),		PDP10_ALL },
    { "hllzi",		BASIC(0511),		PDP10_ALL },
    { "hllzm",		BASIC(0512),		PDP10_ALL },
    { "hllzs",		BASIC(0513),		PDP10_ALL },
    { "hrlz",		BASIC(0514),		PDP10_ALL },
    { "hrlzi",		BASIC(0515),		PDP10_ALL },
    { "hrlzm",		BASIC(0516),		PDP10_ALL },
    { "hrlzs",		BASIC(0517),		PDP10_ALL },
    { "hllo",		BASIC(0520),		PDP10_ALL },
    { "hlloi",		BASIC(0521),		PDP10_ALL },
    { "hllom",		BASIC(0522),		PDP10_ALL },
    { "hllos",		BASIC(0523),		PDP10_ALL },
    { "hrlo",		BASIC(0524),		PDP10_ALL },
    { "hrloi",		BASIC(0525),		PDP10_ALL },
    { "hrlom",		BASIC(0526),		PDP10_ALL },
    { "hrlos",		BASIC(0527),		PDP10_ALL },
    { "hlle",		BASIC(0530),		PDP10_ALL },
    { "hllei",		BASIC(0531),		PDP10_ALL },
    { "hllem",		BASIC(0532),		PDP10_ALL },
    { "hlles",		BASIC(0533),		PDP10_ALL },
    { "hrle",		BASIC(0534),		PDP10_ALL },
    { "hrlei",		BASIC(0535),		PDP10_ALL },
    { "hrlem",		BASIC(0536),		PDP10_ALL },
    { "hrles",		BASIC(0537),		PDP10_ALL },
    { "hrr",		BASIC(0540),		PDP10_ALL },
    { "hrri",		BASIC(0541),		PDP10_ALL },
    { "hrrm",		BASIC(0542),		PDP10_ALL },
    { "hrrs",		BASIC(0543),		PDP10_ALL },
    { "hlr",		BASIC(0544),		PDP10_ALL },
    { "hlri",		BASIC(0545),		PDP10_ALL },
    { "hlrm",		BASIC(0546),		PDP10_ALL },
    { "hlrs",		BASIC(0547),		PDP10_ALL },
    { "hrrz",		BASIC(0550),		PDP10_ALL },
    { "hrrzi",		BASIC(0551),		PDP10_ALL },
    { "hrrzm",		BASIC(0552),		PDP10_ALL },
    { "hrrzs",		BASIC(0553),		PDP10_ALL },
    { "hlrz",		BASIC(0554),		PDP10_ALL },
    { "hlrzi",		BASIC(0555),		PDP10_ALL },
    { "hlrzm",		BASIC(0556),		PDP10_ALL },
    { "hlrzs",		BASIC(0557),		PDP10_ALL },
    { "hrro",		BASIC(0560),		PDP10_ALL },
    { "hrroi",		BASIC(0561),		PDP10_ALL },
    { "hrrom",		BASIC(0562),		PDP10_ALL },
    { "hrros",		BASIC(0563),		PDP10_ALL },
    { "hlro",		BASIC(0564),		PDP10_ALL },
    { "hlroi",		BASIC(0565),		PDP10_ALL },
    { "hlrom",		BASIC(0566),		PDP10_ALL },
    { "hlros",		BASIC(0567),		PDP10_ALL },
    { "hrre",		BASIC(0570),		PDP10_ALL },
    { "hrrei",		BASIC(0571),		PDP10_ALL },
    { "hrrem",		BASIC(0572),		PDP10_ALL },
    { "hrres",		BASIC(0573),		PDP10_ALL },
    { "hlre",		BASIC(0574),		PDP10_ALL },
    { "hlrei",		BASIC(0575),		PDP10_ALL },
    { "hlrem",		BASIC(0576),		PDP10_ALL },
    { "hlres",		BASIC(0577),		PDP10_ALL },
    { "trn",		BASIC(0600),		PDP10_ALL },
    { "tln",		BASIC(0601),		PDP10_ALL },
    { "trne",		BASIC(0602),		PDP10_ALL },
    { "tlne",		BASIC(0603),		PDP10_ALL },
    { "trna",		BASIC(0604),		PDP10_ALL },
    { "tlna",		BASIC(0605),		PDP10_ALL },
    { "trnn",		BASIC(0606),		PDP10_ALL },
    { "tlnn",		BASIC(0607),		PDP10_ALL },
    { "tdn",		BASIC(0610),		PDP10_ALL },
    { "tsn",		BASIC(0611),		PDP10_ALL },
    { "tdne",		BASIC(0612),		PDP10_ALL },
    { "tsne",		BASIC(0613),		PDP10_ALL },
    { "tdna",		BASIC(0614),		PDP10_ALL },
    { "tsna",		BASIC(0615),		PDP10_ALL },
    { "tdnn",		BASIC(0616),		PDP10_ALL },
    { "tsnn",		BASIC(0617),		PDP10_ALL },
    { "trz",		BASIC(0620),		PDP10_ALL },
    { "tlz",		BASIC(0621),		PDP10_ALL },
    { "trze",		BASIC(0622),		PDP10_ALL },
    { "tlze",		BASIC(0623),		PDP10_ALL },
    { "trza",		BASIC(0624),		PDP10_ALL },
    { "tlza",		BASIC(0625),		PDP10_ALL },
    { "trzn",		BASIC(0626),		PDP10_ALL },
    { "tlzn",		BASIC(0627),		PDP10_ALL },
    { "tdz",		BASIC(0630),		PDP10_ALL },
    { "tsz",		BASIC(0631),		PDP10_ALL },
    { "tdze",		BASIC(0632),		PDP10_ALL },
    { "tsze",		BASIC(0633),		PDP10_ALL },
    { "tdza",		BASIC(0634),		PDP10_ALL },
    { "tsza",		BASIC(0635),		PDP10_ALL },
    { "tdzn",		BASIC(0636),		PDP10_ALL },
    { "tszn",		BASIC(0637),		PDP10_ALL },
    { "trc",		BASIC(0640),		PDP10_ALL },
    { "tlc",		BASIC(0641),		PDP10_ALL },
    { "trce",		BASIC(0642),		PDP10_ALL },
    { "tlce",		BASIC(0643),		PDP10_ALL },
    { "trca",		BASIC(0644),		PDP10_ALL },
    { "tlca",		BASIC(0645),		PDP10_ALL },
    { "trcn",		BASIC(0646),		PDP10_ALL },
    { "tlcn",		BASIC(0647),		PDP10_ALL },
    { "tdc",		BASIC(0650),		PDP10_ALL },
    { "tsc",		BASIC(0651),		PDP10_ALL },
    { "tdce",		BASIC(0652),		PDP10_ALL },
    { "tsce",		BASIC(0653),		PDP10_ALL },
    { "tdca",		BASIC(0654),		PDP10_ALL },
    { "tsca",		BASIC(0655),		PDP10_ALL },
    { "tdcn",		BASIC(0656),		PDP10_ALL },
    { "tscn",		BASIC(0657),		PDP10_ALL },
    { "tro",		BASIC(0660),		PDP10_ALL },
    { "tlo",		BASIC(0661),		PDP10_ALL },
    { "troe",		BASIC(0662),		PDP10_ALL },
    { "tloe",		BASIC(0663),		PDP10_ALL },
    { "troa",		BASIC(0664),		PDP10_ALL },
    { "tloa",		BASIC(0665),		PDP10_ALL },
    { "tron",		BASIC(0666),		PDP10_ALL },
    { "tlon",		BASIC(0667),		PDP10_ALL },
    { "tdo",		BASIC(0670),		PDP10_ALL },
    { "tso",		BASIC(0671),		PDP10_ALL },
    { "tdoe",		BASIC(0672),		PDP10_ALL },
    { "tsoe",		BASIC(0673),		PDP10_ALL },
    { "tdoa",		BASIC(0674),		PDP10_ALL },
    { "tsoa",		BASIC(0675),		PDP10_ALL },
    { "tdon",		BASIC(0676),		PDP10_ALL },
    { "tson",		BASIC(0677),		PDP10_ALL },

    /*
     * I/O and system instructions.  Lots of model-specifics here.
     * XXX: these are all & ~PDP10_ITS
     */
    { "aprid",		A_OPCODE(070000),	PDP10_KL10any | PDP10_KS10 | PDP10_XKL1 }, /* alias for BLKI APR, */
    { "rsw",		A_OPCODE(070004),	PDP10_KA10_to_KI10 }, /* alias for DATAI APR, */
    { "wrfil",		A_OPCODE(070010),	PDP10_KL10any }, /* alias for BLKO APR, */
    { "wrapr",		A_OPCODE(070020),	PDP10_KS10 | PDP10_XKL1 }, /* alias for CONO APR, */
    { "rdapr",		A_OPCODE(070024),	PDP10_KS10 | PDP10_XKL1 }, /* alias for CONI APR, */
    { "rdera",		A_OPCODE(070040),	PDP10_KL10any }, /* alias for BLKI PI, */
    { "sbdiag",		A_OPCODE(070050),	PDP10_KL10any }, /* alias for BLKO PI, */
    { "wrpi",		A_OPCODE(070060),	PDP10_KS10 | PDP10_XKL1 }, /* alias for CONO PI, */
    { "rdpi",		A_OPCODE(070064),	PDP10_KS10 | PDP10_XKL1 }, /* alias for CONI PI, */
    { "rdubr",		A_OPCODE(070104),	PDP10_KS10 | PDP10_XKL1 }, /* alias for DATAI PAG, */
    { "clrpt",		A_OPCODE(070110),	PDP10_KL10any | PDP10_KS10 | PDP10_XKL1 }, /* alias for BLKO PAG, */
    { "wrubr",		A_OPCODE(070114),	PDP10_KS10 | PDP10_XKL1 }, /* alias for DATAO PAG, */
    { "wrebr",		A_OPCODE(070120),	PDP10_KS10 }, /* alias for CONO PAG, */
    { "rdebr",		A_OPCODE(070124),	PDP10_KS10 }, /* alias for CONI PAG, */
    { "swpia",		A_OPCODE(070144) | PDP10_INSN_E_UNUSED,	PDP10_KL10any | PDP10_XKL1 }, /* alias for DATAI CCA, */
    { "swpva",		A_OPCODE(070150) | PDP10_INSN_E_UNUSED,	PDP10_KL10any | PDP10_XKL1 }, /* alias for BLKO CCA, */
    { "swpua",		A_OPCODE(070154) | PDP10_INSN_E_UNUSED,	PDP10_KL10any | PDP10_XKL1 }, /* alias for DATAO CCA, */
    { "swpio",		A_OPCODE(070164),	PDP10_KL10any | PDP10_XKL1 }, /* alias for CONI CCA, */
    { "swpvo",		A_OPCODE(070170),	PDP10_KL10any | PDP10_XKL1 }, /* alias for CONSZ CCA, */
    { "swpuo",		A_OPCODE(070174),	PDP10_KL10any | PDP10_XKL1 }, /* alias for CONSO CCA, */
    { "rdperf",		A_OPCODE(070200),	PDP10_KL10any }, /* alias for BLKI TIM, */
    { "rdspb",		A_OPCODE(070200),	PDP10_KS10 | PDP10_XKL1 },
    { "rdtime",		A_OPCODE(070204),	PDP10_KL10any }, /* alias for DATAI TIM, */
    { "rdcsb",		A_OPCODE(070204),	PDP10_KS10 | PDP10_XKL1 },
    { "wrpae",		A_OPCODE(070210),	PDP10_KL10any }, /* alias for BLKO TIM, */
    { "rdpur",		A_OPCODE(070210),	PDP10_KS10 | PDP10_XKL1 },
    { "rdcstm",		A_OPCODE(070214),	PDP10_KS10 | PDP10_XKL1 },
    { "rdtim",		A_OPCODE(070220),	PDP10_KS10 }, /* alias for CONO TIM, */
    { "rdint",		A_OPCODE(070224),	PDP10_KS10 }, /* alias for CONI TIM, */
    { "rdhsb",		A_OPCODE(070230),	PDP10_KS10 },
    { "rdmact",		A_OPCODE(070240),	PDP10_KL10any }, /* alias for BLKI MTR, */
    { "wrspb",		A_OPCODE(070240),	PDP10_KS10 | PDP10_XKL1 },
    { "rdeact",		A_OPCODE(070244),	PDP10_KL10any }, /* alias for DATAI MTR, */
    { "wrcsb",		A_OPCODE(070244),	PDP10_KS10 | PDP10_XKL1 },
    { "wrpur",		A_OPCODE(070250),	PDP10_KS10 | PDP10_XKL1 },
    { "wrcstm",		A_OPCODE(070254),	PDP10_KS10 | PDP10_XKL1 },
    { "wrtime",		A_OPCODE(070260),	PDP10_KL10any }, /* alias for CONO MTR, */
    { "wrtim",		A_OPCODE(070260),	PDP10_KS10 },
    { "wrint",		A_OPCODE(070264),	PDP10_KS10 },
    { "wrhsb",		A_OPCODE(070270),	PDP10_KS10 },
    { "umove",		BASIC(0704),		PDP10_KS10 },
    { "umovem",		BASIC(0705),		PDP10_KS10 },
    { "tioe",		BASIC(0710),		PDP10_KS10 },
    { "tion",		BASIC(0711),		PDP10_KS10 },
    { "rdio",		BASIC(0712),		PDP10_KS10 },
    { "wrio",		BASIC(0713),		PDP10_KS10 },
    { "bsio",		BASIC(0714),		PDP10_KS10 },
    { "bcio",		BASIC(0715),		PDP10_KS10 },
    { "tioeb",		BASIC(0720),		PDP10_KS10 },
    { "tionb",		BASIC(0721),		PDP10_KS10 },
    { "rdiob",		BASIC(0722),		PDP10_KS10 },
    { "wriob",		BASIC(0723),		PDP10_KS10 },
    { "bsiob",		BASIC(0724),		PDP10_KS10 },
    { "bciob",		BASIC(0725),		PDP10_KS10 },

    /*
     * KA10/KL10 ITS system instructions.
     */
    { "lpm",		A_OPCODE(010200),	PDP10_KA10_ITS | PDP10_KL10_ITS },
    { "spm",		A_OPCODE(010204),	PDP10_KA10_ITS | PDP10_KL10_ITS },
    { "lpmr",		A_OPCODE(010210),	PDP10_KA10_ITS | PDP10_KL10_ITS },
    { "lpmri",		A_OPCODE(010230),	PDP10_KA10_ITS | PDP10_KL10_ITS },

    /*
     * KA10 ITS system instructions.
     */
    { "xctr",		A_OPCODE(010300),	PDP10_KA10_ITS },
    { "xctri",		A_OPCODE(010320),	PDP10_KA10_ITS },

    /*
     * KL10 ITS system instructions.
     */
    { "xctr",		BASIC(0074),		PDP10_KL10_ITS },
    { "xctri",		BASIC(0075),		PDP10_KL10_ITS },
    { "lpmr",		BASIC(0076),		PDP10_KL10_ITS },
    { "spm",		BASIC(0077),		PDP10_KL10_ITS },

    /*
     * KS10 ITS system instructions.
     */
    { "xctr",		BASIC(0102),		PDP10_KS10_ITS },
    { "xctri",		BASIC(0103),		PDP10_KS10_ITS },
    { "aprid",		A_OPCODE(070000),	PDP10_KS10_ITS },
#if 0
    /* ITS appears to prefer CONO and CONI over these mnemonics */
    { "wrapr",		A_OPCODE(070020),	PDP10_KS10_ITS },
    { "rdapr",		A_OPCODE(070024),	PDP10_KS10_ITS },
    { "wrpi",		A_OPCODE(070060),	PDP10_KS10_ITS },
    { "rdpi",		A_OPCODE(070064),	PDP10_KS10_ITS },
#endif
    { "clrcsh",		A_OPCODE(070100),	PDP10_KS10_ITS },
    { "rdubr",		A_OPCODE(070104),	PDP10_KS10_ITS },
    { "clrpt",		A_OPCODE(070110),	PDP10_KS10_ITS },
    { "wrubr",		A_OPCODE(070114),	PDP10_KS10_ITS },
    { "wrebr",		A_OPCODE(070120),	PDP10_KS10_ITS },
    { "rdebr",		A_OPCODE(070124),	PDP10_KS10_ITS },
    { "sdbr1",		A_OPCODE(070200),	PDP10_KS10_ITS },
    { "sdbr2",		A_OPCODE(070204),	PDP10_KS10_ITS },
    { "sdbr3",		A_OPCODE(070210),	PDP10_KS10_ITS },
    { "sdbr4",		A_OPCODE(070214),	PDP10_KS10_ITS },
    { "rdtim",		A_OPCODE(070220),	PDP10_KS10_ITS },
    { "rdint",		A_OPCODE(070224),	PDP10_KS10_ITS },
    { "rdhsb",		A_OPCODE(070230),	PDP10_KS10_ITS },
    { "spm",		A_OPCODE(070234),	PDP10_KS10_ITS },
    { "ldbr1",		A_OPCODE(070240),	PDP10_KS10_ITS },
    { "ldbr2",		A_OPCODE(070244),	PDP10_KS10_ITS },
    { "ldbr3",		A_OPCODE(070250),	PDP10_KS10_ITS },
    { "ldbr4",		A_OPCODE(070254),	PDP10_KS10_ITS },
    { "wrtim",		A_OPCODE(070260),	PDP10_KS10_ITS },
    { "wrint",		A_OPCODE(070264),	PDP10_KS10_ITS },
    { "wrhsb",		A_OPCODE(070270),	PDP10_KS10_ITS },
    { "lpmr",		A_OPCODE(070274),	PDP10_KS10_ITS },
    { "umove",		BASIC(0704),		PDP10_KS10_ITS },
    { "umovem",		BASIC(0705),		PDP10_KS10_ITS },
    { "iordi",		BASIC(0710),		PDP10_KS10_ITS },
    { "iordq",		BASIC(0711),		PDP10_KS10_ITS },
    { "iord",		BASIC(0712),		PDP10_KS10_ITS },
    { "iowr",		BASIC(0713),		PDP10_KS10_ITS },
    { "iowri",		BASIC(0714),		PDP10_KS10_ITS },
    { "iowrq",		BASIC(0715),		PDP10_KS10_ITS },
    { "bltbu",		BASIC(0716),		PDP10_KS10_ITS },
    { "bltub",		BASIC(0717),		PDP10_KS10_ITS },
    { "iordbi",		BASIC(0720),		PDP10_KS10_ITS },
    { "iordbq",		BASIC(0721),		PDP10_KS10_ITS },
    { "iordb",		BASIC(0722),		PDP10_KS10_ITS },
    { "iowrb",		BASIC(0723),		PDP10_KS10_ITS },
    { "iowrbi",		BASIC(0724),		PDP10_KS10_ITS },
    { "iowrbq",		BASIC(0725),		PDP10_KS10_ITS },

    /*
     * XKL-1 system instructions.
     */
    { "rdadb",		A_OPCODE(070004),	PDP10_XKL1 },
    { "sysid",		A_OPCODE(070010),	PDP10_XKL1 },
    { "wradb",		A_OPCODE(070014),	PDP10_XKL1 },
    { "szapr",		A_OPCODE(070030),	PDP10_XKL1 },
    { "snapr",		A_OPCODE(070034),	PDP10_XKL1 },
    { "wctrlf",		A_OPCODE(070040),	PDP10_XKL1 },
    { "rctrlf",		A_OPCODE(070044),	PDP10_XKL1 },
    { "simird",		A_OPCODE(070050),	PDP10_XKL1 },
    { "wrkpa",		A_OPCODE(070054),	PDP10_XKL1 },
    { "szpi",		A_OPCODE(070070),	PDP10_XKL1 },
    { "snpi",		A_OPCODE(070074),	PDP10_XKL1 },
    { "apr0",		BASIC(0700),		PDP10_XKL1 },
    { "clrpt",		A_OPCODE(070110),	PDP10_XKL1 },
    { "wrerr",		A_OPCODE(070120),	PDP10_XKL1 },
    { "rderr",		A_OPCODE(070124),	PDP10_XKL1 },
    { "wrctx",		A_OPCODE(070130),	PDP10_XKL1 },
    { "rdctx",		A_OPCODE(070134),	PDP10_XKL1 },
    { "rddcsh",		A_OPCODE(070140),	PDP10_XKL1 },
    { "dwrcsh",		A_OPCODE(070160),	PDP10_XKL1 },
    { "swpio",		A_OPCODE(070164),	PDP10_XKL1 },
    { "swpvo",		A_OPCODE(070170),	PDP10_XKL1 },
    { "swpuo",		A_OPCODE(070174),	PDP10_XKL1 },
    { "apr1",		BASIC(0701),		PDP10_XKL1 },
    { "rditm",		A_OPCODE(070220),	PDP10_XKL1 },
    { "rdtime",		A_OPCODE(070224),	PDP10_XKL1 },
    { "drdptb",		A_OPCODE(070230),	PDP10_XKL1 },
    { "wrtime",		A_OPCODE(070234),	PDP10_XKL1 },
    { "writm",		A_OPCODE(070260),	PDP10_XKL1 },
    { "dwrptb",		A_OPCODE(070270),	PDP10_XKL1 },
    { "apr2",		BASIC(0702),		PDP10_XKL1 },
    { "rdcty",		A_OPCODE(070304),	PDP10_XKL1 },
    { "wrcty",		A_OPCODE(070314),	PDP10_XKL1 },
    { "wrctys",		A_OPCODE(070320),	PDP10_XKL1 },
    { "rdctys",		A_OPCODE(070324),	PDP10_XKL1 },
    { "szcty",		A_OPCODE(070330),	PDP10_XKL1 },
    { "sncty",		A_OPCODE(070334),	PDP10_XKL1 },
    { "apr3",		BASIC(0703),		PDP10_XKL1 },
    { "pmove",		BASIC(0704),		PDP10_XKL1 },
    { "pmovem",		BASIC(0705),		PDP10_XKL1 },
    { "nmove",		BASIC(0706),		PDP10_XKL1 },
    { "nmovem",		BASIC(0707),		PDP10_XKL1 },
    { "ldlpn",		BASIC(0710),		PDP10_XKL1 },
    { "rdcfg",		BASIC(0711),		PDP10_XKL1 },
    { "amove",		BASIC(0714),		PDP10_XKL1 },
    { "amovem",		BASIC(0715),		PDP10_XKL1 },
    { "umove",		BASIC(0716),		PDP10_XKL1 },
    { "umovem",		BASIC(0717),		PDP10_XKL1 },

    /*
     * 166 / KA10 / KI10 / KL10 I/O instructions.
     *
     * These take an operand which determines which device to access.
     * Most of the system instructions above are instances of these with
     * pre-determined values for the device operands.
     */
    { "blki",		IO(070000),		PDP10_not_KS10_or_XKL1 },
    { "datai",		IO(070004),		PDP10_not_KS10_or_XKL1 },
    { "blko",		IO(070010),		PDP10_not_KS10_or_XKL1 },
    { "datao",		IO(070014),		PDP10_not_KS10_or_XKL1 },
    { "cono",		IO(070020),		PDP10_not_KS10_or_XKL1 },
    { "coni",		IO(070024),		PDP10_not_KS10_or_XKL1 },
    { "consz",		IO(070030),		PDP10_not_KS10_or_XKL1 },
    { "conso",		IO(070034),		PDP10_not_KS10_or_XKL1 },
};

/*
 * Internal device names for IO instructions.
 *
 * The convention in documentation is to list 7-bit device codes as three-digit
 * octal numbers.  Three-digit numbers have two excess bits, which are removed
 * the the DEVICE() macro to produce 7 bits.
 */

#define DEVICE(DEVICE9)	((DEVICE9) >> 2)

static const struct pdp10_cpu_device pdp10_cpu_device[] = {
    /* name,	device,		models */
    { "apr",	DEVICE(0000),	PDP10_KA10_to_KL10 },	/* Arithmetic processor */
    { "pi",	DEVICE(0004),	PDP10_KA10_to_KL10 },	/* Priority interrupt */
    { "pag",	DEVICE(0010),	PDP10_KI10_to_KL10 },	/* Pager */
    { "cca",	DEVICE(0014),	PDP10_KL10any },	/* Cache */
    { "tim",	DEVICE(0020),	PDP10_KL10any },	/* Timer */
    { "mtr",	DEVICE(0024),	PDP10_KL10any },	/* Meters */
};

/*
 * Extended instructions, second word.
 */

#define EXTENDED(OPCODE9)	A_UNUSED(OPCODE9) | PDP10_INSN_EXTENDED

static const struct pdp10_insn pdp10_extended_insn[] = {
    /* name,		high13,	fmt,				models */
    { "cmpsl",		EXTENDED(0001) | PDP10_INSN_E_UNUSED,	PDP10_KL10up },
    { "cmpse",		EXTENDED(0002) | PDP10_INSN_E_UNUSED,	PDP10_KL10up },
    { "cmpsle",		EXTENDED(0003) | PDP10_INSN_E_UNUSED,	PDP10_KL10up },
    { "edit",		EXTENDED(0004) | PDP10_INSN_E_UNUSED,	PDP10_KL10up },
    { "cmpsge",		EXTENDED(0005) | PDP10_INSN_E_UNUSED,	PDP10_KL10up },
    { "cmpsn",		EXTENDED(0006) | PDP10_INSN_E_UNUSED,	PDP10_KL10up },
    { "cmpsg",		EXTENDED(0007) | PDP10_INSN_E_UNUSED,	PDP10_KL10up },
    { "cvtdbo",		EXTENDED(0010),				PDP10_KL10up },
    { "cvtdbt",		EXTENDED(0011),				PDP10_KL10up },
    { "cvtbdo",		EXTENDED(0012),				PDP10_KL10up },
    { "cvtbdt",		EXTENDED(0013),				PDP10_KL10up },
    { "movso",		EXTENDED(0014),				PDP10_KL10up },
    { "movst",		EXTENDED(0015),				PDP10_KL10up },
    { "movslj",		EXTENDED(0016) | PDP10_INSN_E_UNUSED,	PDP10_KL10up },
    { "movsrj",		EXTENDED(0017) | PDP10_INSN_E_UNUSED,	PDP10_KL10up },
    { "xblt",		EXTENDED(0020) | PDP10_INSN_E_UNUSED,	PDP10_KL10up },
    { "gsngl",		EXTENDED(0021),				PDP10_KL10_271 },
    { "gdble",		EXTENDED(0022),				PDP10_KL10_271 },
    { "gdfix",		EXTENDED(0023),				PDP10_KL10_271 },
    { "gdfixr",		EXTENDED(0025),				PDP10_KL10_271 },
    { "gfix",		EXTENDED(0024),				PDP10_KL10_271 },
    { "gfixr",		EXTENDED(0026),				PDP10_KL10_271 },
    { "dgfltr",		EXTENDED(0027),				PDP10_KL10_271 },
    { "gfltr",		EXTENDED(0030),				PDP10_KL10_271 },
    { "gfsc",		EXTENDED(0031),				PDP10_KL10_271 },
};

#define ARRAY_SIZE(A) (sizeof(A) / sizeof((A)[0]))

static const struct pdp10_insn *
insn_from_name(const struct pdp10_insn *insn, unsigned int nr_insn, pdp10_cpu_models_t models, const char *name)
{
    unsigned int i;

    for (i = 0; i < nr_insn; ++i)
	if (strcmp(name, insn[i].name) == 0
	    && (insn[i].models & models) == models)
	    return &insn[i];

    return (const struct pdp10_insn *)0;
}

const struct pdp10_insn *
pdp10_insn_from_name(pdp10_cpu_models_t models, const char *name)
{
    const struct pdp10_insn *insn;

    insn = insn_from_name(pdp10_insn, ARRAY_SIZE(pdp10_insn), models, name);
    if (!insn)
	insn = insn_from_name(pdp10_extended_insn, ARRAY_SIZE(pdp10_extended_insn), models, name);
    return insn;
}

const struct pdp10_cpu_device *
pdp10_cpu_device_from_name(pdp10_cpu_models_t models, const char *name)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(pdp10_cpu_device); ++i)
	if (strcmp(name, pdp10_cpu_device[i].name) == 0
	    && (pdp10_cpu_device[i].models & models) == models)
	    return &pdp10_cpu_device[i];

    return (const struct pdp10_cpu_device *)0;
}

static int high13_matches(unsigned int high13, const struct pdp10_insn *insn)
{
    switch (insn->fmt & 3) {
    case PDP10_INSN_BASIC:
	/* ignore A field (low 4 bits) */
	high13 &= ~017;
	break;
    case PDP10_INSN_A_OPCODE:
	/* all bits significant, nothing to ignore */
	break;
    case PDP10_INSN_A_UNUSED:
	/* A should be zero, don't ignore it */
	break;
    case PDP10_INSN_IO:
	/* ignore device field (middle 7 bits) */
	high13 &= ~01770;
	break;
    }
    return high13 == insn->high13;
}

const struct pdp10_insn *
pdp10_insn_from_high13(pdp10_cpu_models_t models, unsigned int high13, int extended)
{
    unsigned int i, nr_insn;
    const struct pdp10_insn *insn;

    if (extended) {
	insn = pdp10_extended_insn;
	nr_insn = ARRAY_SIZE(pdp10_extended_insn);
    } else {
	insn = pdp10_insn;
	nr_insn = ARRAY_SIZE(pdp10_insn);
    }

    for (i = 0; i < nr_insn; ++i)
	if (high13_matches(high13, &insn[i])
	    && (insn[i].models & models) == models)
	    return &insn[i];

    return (const struct pdp10_insn *)0;
}
