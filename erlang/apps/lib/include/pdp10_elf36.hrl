%%% -*- erlang-indent-level: 2 -*-
%%%
%%% pdp10_elf36.hrl -- ELF definitions for PDP10
%%% Copyright (C) 2013-2025  Mikael Pettersson
%%%
%%% This file is part of pdp10-tools.
%%%
%%% pdp10-tools is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% pdp10-tools is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with pdp10-tools.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% This is essentially the standard Elf32 declarations with 8/16/32-bit
%%% primitive types replaced by 9/18/36-bit primitive types, and Elf32
%%% replaced by elf36.

-ifndef(PDP10_ELF36_HRL).
-define(PDP10_ELF36_HRL, 1).

-include("pdp10_stdint.hrl").

-type elf36_Addr()  :: uint36_t().
-type elf36_Half()  :: uint18_t().
-type elf36_Off()   :: uint36_t().
-type elf36_Sword() :: int36_t().
-type elf36_Uchar() :: uint9_t().
-type elf36_Word()  :: uint36_t().

%% ELF Header

-define(EI_NIDENT, 16).

-record(elf36_Ehdr,
        { e_ident       :: [elf36_Uchar()]      % ELF magic, ?EI_NIDENT elements
        , e_type        :: elf36_Half()         % Identifies object file type
        , e_machine     :: elf36_Half()         % Specifies required architecture
        , e_version     :: elf36_Word()         % Identifies object file version
        , e_entry       :: elf36_Addr()         % Entry point virtual address
        , e_phoff       :: elf36_Off()          % Program header table file offset
        , e_shoff       :: elf36_Off()          % Section header table file offset
        , e_flags       :: elf36_Word()         % Processor-specific flags
        , e_ehsize      :: elf36_Half()         % ELF header size in bytes
        , e_phentsize   :: elf36_Half()         % Program header table entry size
        , e_phnum       :: elf36_Half()         % Program header table entry count
        , e_shentsize   :: elf36_Half()         % Section header table entry size
        , e_shnum       :: elf36_Half()         % Section header table entry count
        , e_shstrndx    :: elf36_Half()         % Section header string table index
        }).

-define(ELF36_EHDR_SIZEOF, (8 * 2 + 5 * 4 + ?EI_NIDENT)).

%% e_ident[] identification indexes

-define(EI_MAG0,        0).     % File identification byte 0 index
-define(EI_MAG1,        1).     % File identification byte 1 index
-define(EI_MAG2,        2).     % File identification byte 2 index
-define(EI_MAG3,        3).     % File identification byte 3 index
-define(EI_CLASS,       4).     % File class
-define(EI_DATA,        5).     % Data encoding
-define(EI_VERSION,     6).     % File version
-define(EI_OSABI,       7).     % Operating System/ABI indication
-define(EI_ABIVERSION,  8).     % ABI version
-define(EI_PAD,         9).     % Start of padding bytes

-define(ELFMAG0,        16#7F). % Magic number byte 0
-define(ELFMAG1,           $E). % Magic number byte 1
-define(ELFMAG2,           $L). % Magic number byte 2
-define(ELFMAG3,           $F). % Magic number byte 3

-define(ELFCLASSNONE,       0). % Invalid class
-define(ELFCLASS32,         1). % 32-bit objects
-define(ELFCLASS64,         2). % 64-bit objects
-define(ELFCLASS36,        36). % 36-bit objects (Elf36 extension)

-define(ELFDATANONE,        0). % Invalid data encoding
-define(ELFDATA2LSB,        1). % 2's complement, little endian
-define(ELFDATA2MSB,        2). % 2's complement, big endian

-define(ELFOSABI_NONE,          0). % UNIX System V ABI
-define(ELFOSABI_HPUX,          1). % HP-UX operating system
-define(ELFOSABI_NETBSD,        2). % NetBSD
-define(ELFOSABI_GNU,           3). % GNU
-define(ELFOSABI_LINUX,         3). % Alias for ELFOSABI_GNU
-define(ELFOSABI_SOLARIS,       6). % Solaris
-define(ELFOSABI_AIX,           7). % AIX
-define(ELFOSABI_IRIX,          8). % IRIX
-define(ELFOSABI_FREEBSD,       9). % FreeBSD
-define(ELFOSABI_TRU64,        10). % TRU64 UNIX
-define(ELFOSABI_MODESTO,      11). % Novell Modesto
-define(ELFOSABI_OPENBSD,      12). % OpenBSD
-define(ELFOSABI_OPENVMS,      13). % OpenVMS
-define(ELFOSABI_NSK,          14). % Hewlett-Packard Non-Stop Kernel
-define(ELFOSABI_AROS,         15). % AROS
-define(ELFOSABI_FENIXOS,      16). % FenixOS
-define(ELFOSABI_CLOUDABI,     17). % Nuxi CloudABI
-define(ELFOSABI_OPENVOS,      18). % Stratus Technologies OpenVOS

-define(ELFOSABI_CUDA,         51). % NVIDIA CUDA architecture.
-define(ELFOSABI_C6000_ELFABI, 64). % Bare-metal TMS320C6000
-define(ELFOSABI_AMDGPU_HSA,   64). % AMD HSA Runtime
-define(ELFOSABI_C6000_LINUX,  65). % Linux TMS320C6000
-define(ELFOSABI_AMDGPU_PAL,   65). % AMD PAL Runtime
-define(ELFOSABI_ARM_FDPIC,    65). % ARM FDPIC
-define(ELFOSABI_AMDGPU_MESA3D,66). % AMD Mesa3D Runtime
-define(ELFOSABI_ARM,          97). % ARM
-define(ELFOSABI_STANDALONE,  255). % Standalone (embedded) application

%% Values for e_type, which identifies the object file type.

-define(ET_NONE,        0).       % No file type
-define(ET_REL,         1).       % Relocatable file
-define(ET_EXEC,        2).       % Position-dependent executable file
-define(ET_DYN,         3).       % Position-independent executable or shared object file
-define(ET_CORE,        4).       % Core file
-define(ET_LOOS,        16#FE00). % Operating system-specific
-define(ET_HIOS,        16#FEFF). % Operating system-specific
-define(ET_LOPROC,      16#FF00). % Processor-specific
-define(ET_HIPROC,      16#FFFF). % Processor-specific

%% Values for e_machine, which identifies the architecture.  These numbers
%% are officially assigned by registry@sco.com.  See below for a list of
%% ad-hoc numbers used during initial development.

-define(EM_NONE,          0).   % No machine
-define(EM_M32,           1).   % AT&T WE 32100
-define(EM_SPARC,         2).   % SUN SPARC
-define(EM_386,           3).   % Intel 80386
-define(EM_68K,           4).   % Motorola m68k family
-define(EM_88K,           5).   % Motorola m88k family
-define(EM_IAMCU,         6).   % Intel MCU (was: Intel 80486)
-define(EM_860,           7).   % Intel 80860
-define(EM_MIPS,          8).   % MIPS R3000 (officially, big-endian only)
-define(EM_S370,          9).   % IBM System/370
-define(EM_MIPS_RS3_LE,  10).   % MIPS R3000 little-endian (Oct 4 1999 Draft).  Deprecated.
-define(EM_OLD_SPARCV9,  11).   % Old version of Sparc v9, from before the ABI.  Deprecated.
-define(EM_res011,       11).   % Reserved
-define(EM_res012,       12).   % Reserved
-define(EM_res013,       13).   % Reserved
-define(EM_res014,       14).   % Reserved
-define(EM_PARISC,       15).   % HPPA
-define(EM_res016,       16).   % Reserved
-define(EM_PPC_OLD,      17).   % Old version of PowerPC.  Deprecated.
-define(EM_VPP550,       17).   % Fujitsu VPP500
-define(EM_SPARC32PLUS,  18).   % Sun's "v8plus"
-define(EM_960,          19).   % Intel 80960
-define(EM_PPC,          20).   % PowerPC
-define(EM_PPC64,        21).   % 64-bit PowerPC
-define(EM_S390,         22).   % IBM S/390
-define(EM_SPU,          23).   % Sony/Toshiba/IBM SPU
-define(EM_res024,       24).   % Reserved
-define(EM_res025,       25).   % Reserved
-define(EM_res026,       26).   % Reserved
-define(EM_res027,       27).   % Reserved
-define(EM_res028,       28).   % Reserved
-define(EM_res029,       29).   % Reserved
-define(EM_res030,       30).   % Reserved
-define(EM_res031,       31).   % Reserved
-define(EM_res032,       32).   % Reserved
-define(EM_res033,       33).   % Reserved
-define(EM_res034,       34).   % Reserved
-define(EM_res035,       35).   % Reserved
-define(EM_V800,         36).   % NEC V800 series
-define(EM_FR20,         37).   % Fujitsu FR20
-define(EM_RH32,         38).   % TRW RH32
-define(EM_MCORE,        39).   % Motorola M*Core (May also be taken by Fujitsu MMA)
-define(EM_RCE,          39).   % Old name for MCore
-define(EM_ARM,          40).   % ARM
-define(EM_OLD_ALPHA,    41).   % Digital Alpha
-define(EM_SH,           42).   % Renesas (formerly Hitachi) / SuperH SH
-define(EM_SPARCV9,      43).   % SPARC v9 64-bit
-define(EM_TRICORE,      44).   % Siemens Tricore embedded processor
-define(EM_ARC,          45).   % ARC Cores
-define(EM_H8_300,       46).   % Renesas (formerly Hitachi) H8/300
-define(EM_H8_300H,      47).   % Renesas (formerly Hitachi) H8/300H
-define(EM_H8S,          48).   % Renesas (formerly Hitachi) H8S
-define(EM_H8_500,       49).   % Renesas (formerly Hitachi) H8/500
-define(EM_IA_64,        50).   % Intel IA-64 Processor
-define(EM_MIPS_X,       51).   % Stanford MIPS-X
-define(EM_COLDFIRE,     52).   % Motorola Coldfire
-define(EM_68HC12,       53).   % Motorola M68HC12
-define(EM_MMA,          54).   % Fujitsu Multimedia Accelerator
-define(EM_PCP,          55).   % Siemens PCP
-define(EM_NCPU,         56).   % Sony nCPU embedded RISC processor
-define(EM_NDR1,         57).   % Denso NDR1 microprocessor
-define(EM_STARCORE,     58).   % Motorola Star*Core processor
-define(EM_ME16,         59).   % Toyota ME16 processor
-define(EM_ST100,        60).   % STMicroelectronics ST100 processor
-define(EM_TINYJ,        61).   % Advanced Logic Corp. TinyJ embedded processor
-define(EM_X86_64,       62).   % Advanced Micro Devices X86-64 processor
-define(EM_PDSP,         63).   % Sony DSP Processor
-define(EM_PDP10,        64).   % Digital Equipment Corp. PDP-10
-define(EM_PDP11,        65).   % Digital Equipment Corp. PDP-11
-define(EM_FX66,         66).   % Siemens FX66 microcontroller
-define(EM_ST9PLUS,      67).   % STMicroelectronics ST9+ 8/16 bit microcontroller
-define(EM_ST7,          68).   % STMicroelectronics ST7 8-bit microcontroller
-define(EM_68HC16,       69).   % Motorola MC68HC16 Microcontroller
-define(EM_68HC11,       70).   % Motorola MC68HC11 Microcontroller
-define(EM_68HC08,       71).   % Motorola MC68HC08 Microcontroller
-define(EM_68HC05,       72).   % Motorola MC68HC05 Microcontroller
-define(EM_SVX,          73).   % Silicon Graphics SVx
-define(EM_ST19,         74).   % STMicroelectronics ST19 8-bit cpu
-define(EM_VAX,          75).   % Digital VAX
-define(EM_CRIS,         76).   % Axis Communications 32-bit embedded processor
-define(EM_JAVELIN,      77).   % Infineon Technologies 32-bit embedded cpu
-define(EM_FIREPATH,     78).   % Element 14 64-bit DSP processor
-define(EM_ZSP,          79).   % LSI Logic's 16-bit DSP processor
-define(EM_MMIX,         80).   % Donald Knuth's educational 64-bit processor
-define(EM_HUANY,        81).   % Harvard's machine-independent format
-define(EM_PRISM,        82).   % SiTera Prism
-define(EM_AVR,          83).   % Atmel AVR 8-bit microcontroller
-define(EM_FR30,         84).   % Fujitsu FR30
-define(EM_D10V,         85).   % Mitsubishi D10V
-define(EM_D30V,         86).   % Mitsubishi D30V
-define(EM_V850,         87).   % Renesas V850 (formerly NEC V850)
-define(EM_M32R,         88).   % Renesas M32R (formerly Mitsubishi M32R)
-define(EM_MN10300,      89).   % Matsushita MN10300
-define(EM_MN10200,      90).   % Matsushita MN10200
-define(EM_PJ,           91).   % picoJava
-define(EM_OR1K,         92).   % OpenRISC 1000 32-bit embedded processor
-define(EM_ARC_COMPACT,  93).   % ARC International ARCompact processor
-define(EM_XTENSA,       94).   % Tensilica Xtensa Architecture
-define(EM_SCORE_OLD,    95).   % Old Sunplus S+core7 backend magic number. Written in the absence of an ABI.
-define(EM_VIDEOCORE,    95).   % Alphamosaic VideoCore processor
-define(EM_TMM_GPP,      96).   % Thompson Multimedia General Purpose Processor
-define(EM_NS32K,        97).   % National Semiconductor 32000 series
-define(EM_TPC,          98).   % Tenor Network TPC processor
-define(EM_PJ_OLD,       99).   % Old value for picoJava.  Deprecated.
-define(EM_SNP1K,        99).   % Trebia SNP 1000 processor
-define(EM_ST200,       100).   % STMicroelectronics ST200 microcontroller
-define(EM_IP2K,        101).   % Ubicom IP2022 micro controller
-define(EM_MAX,         102).   % MAX Processor
-define(EM_CR,          103).   % National Semiconductor CompactRISC
-define(EM_F2MC16,      104).   % Fujitsu F2MC16
-define(EM_MSP430,      105).   % TI msp430 micro controller
-define(EM_BLACKFIN,    106).   % ADI Blackfin
-define(EM_SE_C33,      107).   % S1C33 Family of Seiko Epson processors
-define(EM_SEP,         108).   % Sharp embedded microprocessor
-define(EM_ARCA,        109).   % Arca RISC Microprocessor
-define(EM_UNICORE,     110).   % Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University
-define(EM_EXCESS,      111).   % eXcess: 16/32/64-bit configurable embedded CPU
-define(EM_DXP,         112).   % Icera Semiconductor Inc. Deep Execution Processor
-define(EM_ALTERA_NIOS2,113).   % Altera Nios II soft-core processor
-define(EM_CRX,         114).   % National Semiconductor CRX
-define(EM_CR16_OLD,    115).   % Old, value for National Semiconductor CompactRISC.  Deprecated.
-define(EM_XGATE,       115).   % Motorola XGATE embedded processor
-define(EM_C166,        116).   % Infineon C16x/XC16x processor
-define(EM_M16C,        117).   % Renesas M16C series microprocessors
-define(EM_DSPIC30F,    118).   % Microchip Technology dsPIC30F Digital Signal Controller
-define(EM_CE,          119).   % Freescale Communication Engine RISC core
-define(EM_M32C,        120).   % Renesas M32C series microprocessors
-define(EM_res121,      121).   % Reserved
-define(EM_res122,      122).   % Reserved
-define(EM_res123,      123).   % Reserved
-define(EM_res124,      124).   % Reserved
-define(EM_res125,      125).   % Reserved
-define(EM_res126,      126).   % Reserved
-define(EM_res127,      127).   % Reserved
-define(EM_res128,      128).   % Reserved
-define(EM_res129,      129).   % Reserved
-define(EM_res130,      130).   % Reserved
-define(EM_TSK3000,     131).   % Altium TSK3000 core
-define(EM_RS08,        132).   % Freescale RS08 embedded processor
-define(EM_SHARC,       133).   % Analog Devices SHARC family of 32-bit DSP processors
-define(EM_ECOG2,       134).   % Cyan Technology eCOG2 microprocessor
-define(EM_SCORE,       135).   % Sunplus Score
-define(EM_SCORE7,      135).   % Sunplus S+core7 RISC processor
-define(EM_DSP24,       136).   % New Japan Radio (NJR) 24-bit DSP Processor
-define(EM_VIDEOCORE3,  137).   % Broadcom VideoCore III processor
-define(EM_LATTICEMICO32,138).  % RISC processor for Lattice FPGA architecture
-define(EM_SE_C17,      139).   % Seiko Epson C17 family
-define(EM_TI_C6000,    140).   % Texas Instruments TMS320C6000 DSP family
-define(EM_TI_C2000,    141).   % Texas Instruments TMS320C2000 DSP family
-define(EM_TI_C5500,    142).   % Texas Instruments TMS320C55x DSP family
-define(EM_TI_ARP32,    143).   % Texas Instruments Application Specific RISC Processor, 32bit fetch
-define(EM_TI_PRU,      144).   % Texas Instruments Programmable Realtime Unit
-define(EM_res145,      145).   % Reserved
-define(EM_res146,      146).   % Reserved
-define(EM_res147,      147).   % Reserved
-define(EM_res148,      148).   % Reserved
-define(EM_res149,      149).   % Reserved
-define(EM_res150,      150).   % Reserved
-define(EM_res151,      151).   % Reserved
-define(EM_res152,      152).   % Reserved
-define(EM_res153,      153).   % Reserved
-define(EM_res154,      154).   % Reserved
-define(EM_res155,      155).   % Reserved
-define(EM_res156,      156).   % Reserved
-define(EM_res157,      157).   % Reserved
-define(EM_res158,      158).   % Reserved
-define(EM_res159,      159).   % Reserved
-define(EM_MMDSP_PLUS,  160).   % STMicroelectronics 64bit VLIW Data Signal Processor
-define(EM_CYPRESS_M8C, 161).   % Cypress M8C microprocessor
-define(EM_R32C,        162).   % Renesas R32C series microprocessors
-define(EM_TRIMEDIA,    163).   % NXP Semiconductors TriMedia architecture family
-define(EM_QDSP6,       164).   % QUALCOMM DSP6 Processor
-define(EM_8051,        165).   % Intel 8051 and variants
-define(EM_STXP7X,      166).   % STMicroelectronics STxP7x family
-define(EM_NDS32,       167).   % Andes Technology compact code size embedded RISC processor family
-define(EM_ECOG1,       168).   % Cyan Technology eCOG1X family
-define(EM_ECOG1X,      168).   % Cyan Technology eCOG1X family
-define(EM_MAXQ30,      169).   % Dallas Semiconductor MAXQ30 Core Micro-controllers
-define(EM_XIMO16,      170).   % New Japan Radio (NJR) 16-bit DSP Processor
-define(EM_MANIK,       171).   % M2000 Reconfigurable RISC Microprocessor
-define(EM_CRAYNV2,     172).   % Cray Inc. NV2 vector architecture
-define(EM_RX,          173).   % Renesas RX family
-define(EM_METAG,       174).   % Imagination Technologies Meta processor architecture
-define(EM_MCST_ELBRUS, 175).   % MCST Elbrus general purpose hardware architecture
-define(EM_ECOG16,      176).   % Cyan Technology eCOG16 family
-define(EM_CR16,        177).   % National Semiconductor CompactRISC 16-bit processor
-define(EM_ETPU,        178).   % Freescale Extended Time Processing Unit
-define(EM_SLE9X,       179).   % Infineon Technologies SLE9X core
-define(EM_L1OM,        180).   % Intel L1OM
-define(EM_K1OM,        181).   % Intel K1OM
-define(EM_INTEL182,    182).   % Reserved by Intel
-define(EM_AARCH64,     183).   % ARM 64-bit architecture
-define(EM_ARM184,      184).   % Reserved by ARM
-define(EM_AVR32,       185).   % Atmel Corporation 32-bit microprocessor family
-define(EM_STM8,        186).   % STMicroeletronics STM8 8-bit microcontroller
-define(EM_TILE64,      187).   % Tilera TILE64 multicore architecture family
-define(EM_TILEPRO,     188).   % Tilera TILEPro multicore architecture family
-define(EM_MICROBLAZE,  189).   % Xilinx MicroBlaze 32-bit RISC soft processor core
-define(EM_CUDA,        190).   % NVIDIA CUDA architecture
-define(EM_TILEGX,      191).   % Tilera TILE-Gx multicore architecture family
-define(EM_CLOUDSHIELD, 192).   % CloudShield architecture family
-define(EM_COREA_1ST,   193).   % KIPO-KAIST Core-A 1st generation processor family
-define(EM_COREA_2ND,   194).   % KIPO-KAIST Core-A 2nd generation processor family
-define(EM_ARC_COMPACT2,195).   % Synopsys ARCompact V2
-define(EM_OPEN8,       196).   % Open8 8-bit RISC soft processor core
-define(EM_RL78,        197).   % Renesas RL78 family.
-define(EM_VIDEOCORE5,  198).   % Broadcom VideoCore V processor
-define(EM_78K0R,       199).   % Renesas 78K0R.
-define(EM_56800EX,     200).   % Freescale 56800EX Digital Signal Controller (DSC)
-define(EM_BA1,         201).   % Beyond BA1 CPU architecture
-define(EM_BA2,         202).   % Beyond BA2 CPU architecture
-define(EM_XCORE,       203).   % XMOS xCORE processor family
-define(EM_MCHP_PIC,    204).   % Microchip 8-bit PIC(r) family
-define(EM_INTELGT,     205).   % Intel Graphics Technology
-define(EM_INTEL206,    206).   % Reserved by Intel
-define(EM_INTEL207,    207).   % Reserved by Intel
-define(EM_INTEL208,    208).   % Reserved by Intel
-define(EM_INTEL209,    209).   % Reserved by Intel
-define(EM_KM32,        210).   % KM211 KM32 32-bit processor
-define(EM_KMX32,       211).   % KM211 KMX32 32-bit processor
-define(EM_KMX16,       212).   % KM211 KMX16 16-bit processor
-define(EM_KMX8,        213).   % KM211 KMX8 8-bit processor
-define(EM_KVARC,       214).   % KM211 KVARC processor
-define(EM_CDP,         215).   % Paneve CDP architecture family
-define(EM_COGE,        216).   % Cognitive Smart Memory Processor
-define(EM_COOL,        217).   % Bluechip Systems CoolEngine
-define(EM_NORC,        218).   % Nanoradio Optimized RISC
-define(EM_CSR_KALIMBA, 219).   % CSR Kalimba architecture family
-define(EM_Z80,         220).   % Zilog Z80
-define(EM_VISIUM,      221).   % Controls and Data Services VISIUMcore processor
-define(EM_FT32,        222).   % FTDI Chip FT32 high performance 32-bit RISC architecture
-define(EM_MOXIE,       223).   % Moxie processor family
-define(EM_AMDGPU,      224).   % AMD GPU architecture
%% 225-242: reserved
-define(EM_RISCV,       243).   % RISC-V
-define(EM_LANAI,       244).   % Lanai 32-bit processor.
-define(EM_CEVA,        245).   % CEVA Processor Architecture Family
-define(EM_CEVA_X2,     246).   % CEVA X2 Processor Family
-define(EM_BPF,         247).   % Linux BPF - in-kernel virtual machine.
-define(EM_GRAPHCORE_IPU,248).  % Graphcore Intelligent Processing Unit
-define(EM_IMG1,        249).   % Imagination Technologies
-define(EM_NFP,         250).   % Netronome Flow Processor.
-define(EM_VE,          251).   % NEC Vector Engine
-define(EM_CSKY,        252).   % C-SKY processor family.
-define(EM_ARC_COMPACT3_64,253).% Synopsys ARCv2.3 64-bit
-define(EM_MCS6502,     254).   % MOS Technology MCS 6502 processor
-define(EM_ARC_COMPACT3,255).   % Synopsys ARCv2.3 32-bit
-define(EM_KVX,         256).   % Kalray VLIW core of the MPPA processor family
-define(EM_65816,       257).   % WDC 65816/65C816
-define(EM_LOONGARCH,   258).   % LoongArch
-define(EM_KF32,        259).   % ChipON KungFu32
-define(EM_U16_U8CORE,  260).   % LAPIS nX-U16/U8
-define(EM_TACHYUM,     261).   % Tachyum
-define(EM_56800EF,     262).   % NXP 56800EF Digital Signal Controller (DSC)

%% If it is necessary to assign new unofficial EM_* values, please pick large
%% random numbers (16#8523, 16#a7f2, etc.) to minimize the chances of collision
%% with official or non-GNU unofficial values.
%%
%% NOTE: Do not just increment the most recent number by one.
%% Somebody else somewhere will do exactly the same thing, and you
%% will have a collision.  Instead, pick a random number.
%%
%% Normally, each entity or maintainer responsible for a machine with an
%% unofficial e_machine number should eventually ask registry@sco.com for
%% an officially blessed number to be added to the list above.

%% AVR magic number.  Written in the absense of an ABI.
-define(EM_AVR_OLD,             16#1057).

%% MSP430 magic number.  Written in the absense of everything.
-define(EM_MSP430_OLD,          16#1059).

%% Morpho MT.   Written in the absense of an ABI.
-define(EM_MT,                  16#2530).

%% FR30 magic number - no EABI available.
-define(EM_CYGNUS_FR30,         16#3330).

%% Unofficial value for Web Assembly binaries, as used by LLVM.
-define(EM_WEBASSEMBLY,         16#4157).

%% Freescale S12Z.   The Freescale toolchain generates elf files with this value.
-define(EM_S12Z,                16#4DEF).

%% DLX magic number.  Written in the absense of an ABI.
-define(EM_DLX,                 16#5aa5).

%% FRV magic number - no EABI available??.
-define(EM_CYGNUS_FRV,          16#5441).

%% Infineon Technologies 16-bit microcontroller with C166-V2 core.
-define(EM_XC16X,               16#4688).

%% D10V backend magic number.  Written in the absence of an ABI.
-define(EM_CYGNUS_D10V,         16#7650).

%% D30V backend magic number.  Written in the absence of an ABI.
-define(EM_CYGNUS_D30V,         16#7676).

%% Ubicom IP2xxx;   Written in the absense of an ABI.
-define(EM_IP2K_OLD,            16#8217).

%% Cygnus PowerPC ELF backend.  Written in the absence of an ABI.
-define(EM_CYGNUS_POWERPC,      16#9025).

%% Alpha backend magic number.  Written in the absence of an ABI.
-define(EM_ALPHA,               16#9026).

%% Cygnus M32R ELF backend.  Written in the absence of an ABI.
-define(EM_CYGNUS_M32R,         16#9041).

%% V850 backend magic number.  Written in the absense of an ABI.
-define(EM_CYGNUS_V850,         16#9080).

%% old S/390 backend magic number. Written in the absence of an ABI.
-define(EM_S390_OLD,            16#a390).

%% Old, unofficial value for Xtensa.
-define(EM_XTENSA_OLD,          16#abc7).

-define(EM_XSTORMY16,           16#ad45).

%% mn10200 and mn10300 backend magic numbers.
%% Written in the absense of an ABI.
-define(EM_CYGNUS_MN10300,      16#beef).
-define(EM_CYGNUS_MN10200,      16#dead).

%% Renesas M32C and M16C.
-define(EM_M32C_OLD,            16#FEB0).

%% Vitesse IQ2000.
-define(EM_IQ2000,              16#FEBA).

%% NIOS magic number - no EABI available.
-define(EM_NIOS32,              16#FEBB).

-define(EM_CYGNUS_MEP,          16#F00D).  % Toshiba MeP

%% Old, unofficial value for Moxie.
-define(EM_MOXIE_OLD,           16#FEED).

-define(EM_MICROBLAZE_OLD,      16#baab).  % Old MicroBlaze

-define(EM_ADAPTEVA_EPIPHANY,   16#1223).  % Adapteva's Epiphany architecture.

%% Old constant that might be in use by some software.
-define(EM_OPENRISC,            ?EM_OR1K).

%% C-SKY historically used 39, the same value as MCORE, from which the
%% architecture was derived.
-define(EM_CSKY_OLD,            ?EM_MCORE).

%% See the above comment before you add a new EM_* value here.

%% Values for e_version.

-define(EV_NONE,        0).             % Invalid ELF version
-define(EV_CURRENT,     1).             % Current version

%% Value for e_phnum.
-define(PN_XNUM,        16#ffff).       % Extended numbering

%% Section header

-record(elf36_Shdr,
        { sh_name       :: elf36_Word()         % Section name, index in string tbl
                         | string()             % Name (in-core only)
        , sh_type       :: elf36_Word()         % Type of section
        , sh_flags      :: elf36_Word()         % Miscellaneous section attributes
        , sh_addr       :: elf36_Addr()         % Section virtual addr at execution
        , sh_offset     :: elf36_Off()          % Section file offset
        , sh_size       :: elf36_Word()         % Size of section in bytes
        , sh_link       :: elf36_Word()         % Index of another section
        , sh_info       :: elf36_Word()         % Additional section information
        , sh_addralign  :: elf36_Word()         % Section alignment
        , sh_entsize    :: elf36_Word()         % Entry size if section holds table
        }).

-define(ELF36_SHDR_SIZEOF, (10 * 4)).

%% Special section indices, which may show up in st_shndx fields, among
%% other places.

-define(SHN_UNDEF,      16#0000).       % Undefined, missing, irrelevant, or meaningless
-define(SHN_LORESERVE,  16#FF00).       % Begin range of reserved indices
-define(SHN_LOPROC,     16#FF00).       % Begin range of appl-specific
-define(SHN_HIPROC,     16#FF1F).       % End range of appl-specific
-define(SHN_LOOS,       16#FF20).       % OS specific semantics, lo
-define(SHN_HIOS,       16#FF3F).       % OS specific semantics, hi
-define(SHN_ABS,        16#FFF1).       % Associated symbol is absolute
-define(SHN_COMMON,     16#FFF2).       % Associated symbol is in common
-define(SHN_XINDEX,     16#FFFF).       % Section index is held elsewhere
-define(SHN_HIRESERVE,  16#FFFF).       % End range of reserved indices

%% Values for section header, sh_type field.

-define(SHT_NULL,       0).             % Section header table entry unused
-define(SHT_PROGBITS,   1).             % Program specific (private) data
-define(SHT_SYMTAB,     2).             % Link editing symbol table
-define(SHT_STRTAB,     3).             % A string table
-define(SHT_RELA,       4).             % Relocation entries with addends
-define(SHT_HASH,       5).             % A symbol hash table
-define(SHT_DYNAMIC,    6).             % Information for dynamic linking
-define(SHT_NOTE,       7).             % Information that marks file
-define(SHT_NOBITS,     8).             % Section occupies no space in file
-define(SHT_REL,        9).             % Relocation entries, no addends
-define(SHT_SHLIB,      10).            % Reserved, unspecified semantics
-define(SHT_DYNSYM,     11).            % Dynamic linking symbol table

-define(SHT_INIT_ARRAY,   14).          % Array of ptrs to init functions
-define(SHT_FINI_ARRAY,   15).          % Array of ptrs to finish functions
-define(SHT_PREINIT_ARRAY,16).          % Array of ptrs to pre-init funcs
-define(SHT_GROUP,        17).          % Section contains a section group
-define(SHT_SYMTAB_SHNDX, 18).          % Indices for SHN_XINDEX entries
-define(SHT_RELR,         19).          % RELR relative relocations

-define(SHT_LOOS,       16#60000000).   % First of OS specific semantics
-define(SHT_HIOS,       16#6fffffff).   % Last of OS specific semantics

-define(SHT_ANDROID_REL,                16#60000001).
-define(SHT_ANDROID_RELA,               16#60000002).

-define(SHT_GNU_INCREMENTAL_INPUTS,     16#6fff4700). % Incremental build data

-define(SHT_LLVM_ODRTAB,                16#6fff4c00). % LLVM ODR table.
-define(SHT_LLVM_LINKER_OPTIONS,        16#6fff4c01). % LLVM Linker Options.
-define(SHT_LLVM_ADDRSIG,               16#6fff4c03). % List of address-significant symbols for safe ICF.
-define(SHT_LLVM_DEPENDENT_LIBRARIES,   16#6fff4c04). % LLVM Dependent Library Specifiers.
-define(SHT_LLVM_SYMPART,               16#6fff4c05). % Symbol partition specification.
-define(SHT_LLVM_PART_EHDR,             16#6fff4c06). % ELF header for loadable partition.
-define(SHT_LLVM_PART_PHDR,             16#6fff4c07). % Phdrs for loadable partition.
-define(SHT_LLVM_BB_ADDR_MAP_V0,        16#6fff4c08). % LLVM Basic Block Address Map.
-define(SHT_LLVM_CALL_GRAPH_PROFILE,    16#6fff4c09). % LLVM Call Graph Profile.
-define(SHT_LLVM_BB_ADDR_MAP,           16#6fff4c0a). % LLVM Basic Block Address Map.
-define(SHT_LLVM_OFFLOADING,            16#6fff4c0b). % LLVM device offloading data.
-define(SHT_LLVM_LTO,                   16#6fff4c0c). % .llvm.lto for fat LTO.

-define(SHT_ANDROID_RELR,               16#6fffff00).

-define(SHT_GNU_ATTRIBUTES,             16#6ffffff5). % Object attributes
-define(SHT_GNU_HASH,                   16#6ffffff6). % GNU style symbol hash table
-define(SHT_GNU_LIBLIST,                16#6ffffff7). % List of prelink dependencies
-define(SHT_CHECKSUM,                   16#6ffffff8). % Checksum for DSO content.
-define(SHT_GNU_OBJECT_ONLY,            16#6ffffff9). % Object only

-define(SHT_SUNW_move,                  16#6ffffffa).
-define(SHT_SUNW_COMDAT,                16#6ffffffb).
-define(SHT_SUNW_syminfo,               16#6ffffffc).
%% The next three section types are defined by Solaris, and are named
%% SHT_SUNW*.  We use them in GNU code, so we also define SHT_GNU*
%% versions.
-define(SHT_SUNW_verdef,                16#6ffffffd). % Versions defined by file
-define(SHT_SUNW_verneed,               16#6ffffffe). % Versions needed by file
-define(SHT_SUNW_versym,                16#6fffffff). % Symbol versions

-define(SHT_GNU_verdef,  ?SHT_SUNW_verdef).
-define(SHT_GNU_verneed, ?SHT_SUNW_verneed).
-define(SHT_GNU_versym,  ?SHT_SUNW_versym).

-define(SHT_LOPROC,     16#70000000).   % Processor-specific semantics, lo
-define(SHT_HIPROC,     16#7FFFFFFF).   % Processor-specific semantics, hi

-define(SHT_LOUSER,     16#80000000).   % Application-specific semantics
%%-define(SHT_HIUSER,   16#8FFFFFFF).   % Application-specific semantics
-define(SHT_HIUSER,     16#FFFFFFFF).   % New value, defined in Oct 4, 1999 Draft

%% Values for section header, sh_flags field.

-define(SHF_WRITE,      (1 bsl 0)).     % Writable data during execution
-define(SHF_ALLOC,      (1 bsl 1)).     % Occupies memory during execution
-define(SHF_EXECINSTR,  (1 bsl 2)).     % Executable machine instructions
-define(SHF_MERGE,      (1 bsl 4)).     % Data in this section can be merged
-define(SHF_STRINGS,    (1 bsl 5)).     % Contains null terminated character strings
-define(SHF_INFO_LINK,  (1 bsl 6)).     % sh_info holds section header table index
-define(SHF_LINK_ORDER, (1 bsl 7)).     % Preserve section ordering when linking
-define(SHF_OS_NONCONFORMING, (1 bsl 8)). % OS specific processing required
-define(SHF_GROUP,      (1 bsl 9)).     % Member of a section group
-define(SHF_TLS,        (1 bsl 10)).    % Thread local storage section
-define(SHF_COMPRESSED, (1 bsl 11)).    % Data in this section is compressed

%%-define(SHF_MASKOS,   16#0F000000).   % OS-specific semantics
-define(SHF_MASKOS,     16#0FF00000).   % New value, Oct 4, 1999 Draft
-define(SHF_GNU_RETAIN, (1 bsl 21)).    % Section should not be garbage collected by the linker.
-define(SHF_GNU_MBIND,  (1 bsl 24)).    % Mbind section.

-define(SHF_MASKPROC,   16#F0000000).   % Processor-specific semantics
%% This used to be implemented as a processor specific section flag.
%% We just make it generic.  The definition is:	the link editor is
%% to exclude this section from executable and shared libraries that
%% it builds when those objects are not to be further relocated.
-define(SHF_EXCLUDE,    (1 bsl 31)).

%% Size of SHT_GROUP section entry.

-define(GRP_ENTRY_SIZE,         4).

%% Section Group Flags.

-define(GRP_COMDAT,            16#1).   % A COMDAT group
-define(GRP_MASKOS,     16#0ff00000).   % Reserved for OS-specific semantics
-define(GRP_MASKPROC,   16#f0000000).   % Reserved for processor-specific semantics

%% Compression Header

-record(elf36_Chdr,
        { ch_type       :: elf36_Word() % Type of compression
        , ch_size       :: elf36_Word() % Size of uncompressed data in bytes
        , ch_addralign  :: elf36_Word() % Alignment of uncompressed data
        }).

%% Compression types.
-define(ELFCOMPRESS_ZLIB,                 1).   % Compressed with zlib.
-define(ELFCOMPRESS_ZSTD,                 2).   % Compressed with zstd
                                                % (see http://www.zstandard.org).
-define(ELFCOMPRESS_LOOS,       16#60000000).   % OS-specific semantics, lo
-define(ELFCOMPRESS_HIOS,       16#6FFFFFFF).   % OS-specific semantics, hi
-define(ELFCOMPRESS_LOPROC,     16#70000000).   % Processor-specific semantics, lo
-define(ELFCOMPRESS_HIPROC,     16#7FFFFFFF).   % Processor-specific semantics, hi

%% Symbol table entry

-record(elf36_Sym,
        { st_name       :: elf36_Word()         % Symbol name, index in string tbl
                         | string()             % Name (in-core only)
        , st_value      :: elf36_Addr()         % Value of the symbol
        , st_size       :: elf36_Word()         % Associated symbol size
        , st_info       :: elf36_Uchar()        % Type and binding attributes
        , st_other      :: elf36_Uchar()        % No defined meaning, 0
        , st_shndx      :: elf36_Half()         % Associated section index
        }).

-define(ELF36_SYM_SIZEOF,       (3 * 4 + 2 + 1 + 1)).

-ifdef(notdef).
-record(elf_External_Sym_Shndx,
        { est_shndx :: [uint9_t()]      % Section index, 4 elements
        }).
-endif.

-define(STN_UNDEF,      0).             % Undefined symbol index

%% These three macros disassemble and assemble a symbol table st_info field,
%% which contains the symbol binding and symbol type.  The STB_ and STT_
%% defines identify the binding and type.

-define(ELF_ST_BIND(Val),               (((Val) band ?PDP10_UINT9_MAX) bsr 4)).
-define(ELF_ST_TYPE(Val),               ((Val) band 16#F)).
-define(ELF_ST_INFO(Bind, Type),        (((Bind) bsl 4) + ((Type) band 16#F))).

%% The 64bit, 36bit, and 32bit versions of these macros are identical, but
%% the ELF spec defines them, so here they are.
-define(ELF36_ST_BIND(Val),             ?ELF_ST_BIND((Val))).
-define(ELF36_ST_TYPE(Val),             ?ELF_ST_TYPE((Val))).
-define(ELF36_ST_INFO(Bind, Type),      ?ELF_ST_INFO((Bind), (Type))).

-define(STB_LOCAL,      0).     % Symbol not visible outside obj
-define(STB_GLOBAL,     1).     % Symbol visible outside obj
-define(STB_WEAK,       2).     % Like globals, lower precedence
-define(STB_LOOS,       10).    % OS-specific semantics
-define(STB_GNU_UNIQUE, 10).    % Symbol is unique in namespace
-define(STB_HIOS,       12).    % OS-specific semantics
-define(STB_LOPROC,     13).    % Processor-specific semantics
-define(STB_HIPROC,     15).    % Processor-specific semantics

-define(STT_NOTYPE,     0).     % Symbol type is unspecified
-define(STT_OBJECT,     1).     % Symbol is a data object
-define(STT_FUNC,       2).     % Symbol is a code object
-define(STT_SECTION,    3).     % Symbol associated with a section
-define(STT_FILE,       4).     % Symbol gives a file name
-define(STT_COMMON,     5).     % An uninitialised common block
-define(STT_TLS,        6).     % Thread local data object
-define(STT_RELC,       8).     % Complex relocation expression
-define(STT_SRELC,      9).     % Signed Complex relocation expression
-define(STT_LOOS,       10).    % OS-specific semantics
-define(STT_GNU_IFUNC,  10).    % Symbol is an indirect code object
-define(STT_HIOS,       12).    % OS-specific semantics
-define(STT_LOPROC,     13).    % Processor-specific semantics
-define(STT_HIPROC,     15).    % Processor-specific semantics

%% This macro disassembles and assembles a symbol's visibility into
%% the st_other field.  The STV_ defines specify the actual visibility.

-define(ELF_ST_VISIBILITY(V),   ((V) band 16#3)).
%% The remaining bits in the st_other field are not currently used.
%% They should be set to zero.

-define(ELF36_ST_VISIBILITY(V), ?ELF_ST_VISIBILITY((V))).

%% The following constants control how a symbol may be accessed once it has
%% become part of an executable or shared library.

-define(STV_DEFAULT,    0).     % Visibility is specified by binding type
-define(STV_INTERNAL,   1).     % OS specific version of STV_HIDDEN
-define(STV_HIDDEN,     2).     % Can only be seen inside current component
-define(STV_PROTECTED,  3).     % Treat as STB_LOCAL inside current component

%% Relocation Entries
-record(elf36_Rel,
        { r_offset      :: elf36_Addr() % Location at which to apply the action
        , r_info        :: elf36_Word() % index and type of relocation
        }).

-record(elf36_Rela,
        { r_offset      :: elf36_Addr()  % Location at which to apply the action
        , r_info        :: elf36_Word()  % index and type of relocation
        , r_addend      :: elf36_Sword() % Constant addend used to compute value
        }).

-define(ELF36_RELA_SIZEOF, (3 * 4)).

%% Relocation info handling macros.

-define(ELF36_R_SYM(I),         ((I) bsr 8)).
-define(ELF36_R_TYPE(I),        ((I) band 16#ff)).
%% TODO: GNU binutils casts (S) below to unsigned
-define(ELF36_R_INFO(S,T),      (((S) bsl 8) + ((T) band 16#ff))).

%% Processor-specific relocation types.

-define(R_PDP10_NONE,           0).     % no reloc
-define(R_PDP10_IFIW,           1).     % local address to global word (IFIW)
-define(R_PDP10_EFIW,           2).     % global word (EFIW)
-define(R_PDP10_LOCAL_W,        3).     % local word
-define(R_PDP10_LOCAL_B,        4).     % local byte pointer to 9-bit byte
-define(R_PDP10_LOCAL_H,        5).     % local byte pointer to 18-bit halfword
-define(R_PDP10_GLOBAL_B,       6).     % global byte pointer to 9-bit byte
-define(R_PDP10_GLOBAL_H,       7).     % global byte pointer to 18-bit halfword
-define(R_PDP10_LITERAL_W,      8).     % word-sized literal
-define(R_PDP10_LITERAL_H,      9).     % halfword-sized literal
-define(R_PDP10_LITERAL_B,      10).    % byte-sized literal
-define(R_PDP10_max,            10).    % last entry

%% Note segments

-record(elf36_Note,
        { n_namesz      :: elf36_Word() % Size of entry's owner string
        , n_descsz      :: elf36_Word() % Size of the note descriptor
        , n_type        :: elf36_Word() % Interpretation of the descriptor
        , n_name        :: [elf36_Uchar()] % Start of the name+desc data
        }).

%% Values of note segment descriptor types for core files.

-define(NT_PRSTATUS,            1).             % Contains copy of prstatus struct
-define(NT_FPREGSET,            2).             % Contains copy of fpregset struct
-define(NT_PRPSINFO,            3).             % Contains copy of prpsinfo struct
-define(NT_TASKSTRUCT,          4).             % Contains copy of task struct
-define(NT_AUXV,                6).             % Contains copy of Elfxx_auxv_t
-define(NT_PRXFPREG,            16#46e62b7f).   % Contains a user_xfpregs_struct;
                                                %   note name must be "LINUX".
-define(NT_PPC_VMX,             16#100).        % PowerPC Altivec/VMX registers
                                                %   note name must be "LINUX".
-define(NT_PPC_VSX,             16#102).        % PowerPC VSX registers
                                                %   note name must be "LINUX".
-define(NT_PPC_TAR,             16#103).        % PowerPC Target Address Register
                                                %   note name must be "LINUX".
-define(NT_PPC_PPR,             16#104).        % PowerPC Program Priority Register
                                                %   note name must be "LINUX".
-define(NT_PPC_DSCR,            16#105).        % PowerPC Data Stream Control Register
                                                %   note name must be "LINUX".
-define(NT_PPC_EBB,             16#106).        % PowerPC Event Based Branch Registers
                                                %   note name must be "LINUX".
-define(NT_PPC_PMU,             16#107).        % PowerPC Performance Monitor Registers
                                                %   note name must be "LINUX".
-define(NT_PPC_TM_CGPR,         16#108).        % PowerPC TM checkpointed GPR Registers
                                                %   note name must be "LINUX".
-define(NT_PPC_TM_CFPR,         16#109).        % PowerPC TM checkpointed FPR Registers
                                                %   note name must be "LINUX".
-define(NT_PPC_TM_CVMX,         16#10a).        % PowerPC TM checkpointed VMX Registers
                                                %   note name must be "LINUX".
-define(NT_PPC_TM_CVSX,         16#10b).        % PowerPC TM checkpointed VSX Registers
                                                %   note name must be "LINUX".
-define(NT_PPC_TM_SPR,          16#10c).        % PowerPC TM Special Purpose Registers
                                                %   note name must be "LINUX".
-define(NT_PPC_TM_CTAR,         16#10d).        % PowerPC TM checkpointed TAR
                                                %   note name must be "LINUX".
-define(NT_PPC_TM_CPPR,         16#10e).        % PowerPC TM checkpointed PPR
                                                %   note name must be "LINUX".
-define(NT_PPC_TM_CDSCR,        16#10f).        % PowerPC TM checkpointed Data SCR
                                                %   note name must be "LINUX".
-define(NT_386_TLS,             16#200).        % x86 TLS information
                                                %   note name must be "LINUX".
-define(NT_386_IOPERM,          16#201).        % x86 io permissions
                                                %   note name must be "LINUX".
-define(NT_X86_XSTATE,          16#202).        % x86 XSAVE extended state
                                                %   note name must be "LINUX".
-define(NT_X86_CET,             16#203).        % x86 CET state.
                                                %   note name must be "LINUX".
-define(NT_X86_SHSTK,           16#204).        % x86 SHSTK state.
                                                % This replaces NT_X86_CET (16#203).
                                                %   note name must be "LINUX".
-define(NT_S390_HIGH_GPRS,      16#300).        % S/390 upper halves of GPRs
                                                %   note name must be "LINUX".
-define(NT_S390_TIMER,          16#301).        % S390 timer
                                                %   note name must be "LINUX".
-define(NT_S390_TODCMP,         16#302).        % S390 TOD clock comparator
                                                %   note name must be "LINUX".
-define(NT_S390_TODPREG,        16#303).        % S390 TOD programmable register
                                                %   note name must be "LINUX".
-define(NT_S390_CTRS,           16#304).        % S390 control registers
                                                %   note name must be "LINUX".
-define(NT_S390_PREFIX,         16#305).        % S390 prefix register
                                                %   note name must be "LINUX".
-define(NT_S390_LAST_BREAK,     16#306).        % S390 breaking event address
                                                %   note name must be "LINUX".
-define(NT_S390_SYSTEM_CALL,    16#307).        % S390 system call restart data
                                                %   note name must be "LINUX".
-define(NT_S390_TDB,            16#308).        % S390 transaction diagnostic block
                                                %   note name must be "LINUX".
-define(NT_S390_VXRS_LOW,       16#309).        % S390 vector registers 0-15 upper half
                                                %   note name must be "LINUX".
-define(NT_S390_VXRS_HIGH,      16#30a).        % S390 vector registers 16-31
                                                %   note name must be "LINUX".
-define(NT_S390_GS_CB,          16#30b).        % s390 guarded storage registers
                                                %   note name must be "LINUX".
-define(NT_S390_GS_BC,          16#30c).        % s390 guarded storage broadcast control block
                                                %   note name must be "LINUX".
-define(NT_ARM_VFP,             16#400).        % ARM VFP registers
%% The following definitions should really use NT_AARCH_..., but defined
%% this way for compatibility with Linux.
-define(NT_ARM_TLS,             16#401).        % AArch TLS registers
                                                %   note name must be "LINUX".
-define(NT_ARM_HW_BREAK,        16#402).        % AArch hardware breakpoint registers
                                                %   note name must be "LINUX".
-define(NT_ARM_HW_WATCH,        16#403).        % AArch hardware watchpoint registers
                                                %   note name must be "LINUX".
-define(NT_ARM_SYSTEM_CALL,     16#404).        % AArch ARM system call number
                                                %   note name must be "LINUX".
-define(NT_ARM_SVE,             16#405).        % AArch SVE registers.
                                                %   note name must be "LINUX".
-define(NT_ARM_PAC_MASK,        16#406).        % AArch pointer authentication code masks
                                                %   note name must be "LINUX".
-define(NT_ARM_PACA_KEYS,       16#407).        % ARM pointer authentication address
                                                % keys
                                                %   note name must be "LINUX".
-define(NT_ARM_PACG_KEYS,       16#408).        % ARM pointer authentication generic
                                                % keys
                                                %   note name must be "LINUX".
-define(NT_ARM_TAGGED_ADDR_CTRL,16#409).        % AArch64 tagged address control (prctl())
                                                %   note name must be "LINUX".
-define(NT_ARM_PAC_ENABLED_KEYS,16#40a).        % AArch64 pointer authentication
                                                % enabled keys (prctl())
                                                %   note name must be "LINUX".
-define(NT_ARM_SSVE,            16#40b).        % AArch64 SME streaming SVE registers.
                                                %   Note: name must be "LINUX".
-define(NT_ARM_ZA,              16#40c).        % AArch64 SME ZA register.
                                                %   Note: name must be "LINUX".
-define(NT_ARM_ZT,              16#40d).        % AArch64 SME2 ZT registers.
                                                %   Note: name must be "LINUX".
-define(NT_ARC_V2,              16#600).        % ARC HS accumulator/extra registers.
                                                %   note name must be "LINUX".
-define(NT_LARCH_CPUCFG,        16#a00).        % LoongArch CPU config registers
                                                %   note name must be "LINUX".
-define(NT_LARCH_CSR,           16#a01).        % LoongArch Control State Registers
                                                %   note name must be "LINUX".
-define(NT_LARCH_LSX,           16#a02).        % LoongArch SIMD eXtension registers
                                                %   note name must be "LINUX".
-define(NT_LARCH_LASX,          16#a03).        % LoongArch Advanced SIMD eXtension registers
                                                %   note name must be "LINUX".
-define(NT_LARCH_LBT,           16#a04).        % LoongArch Binary Translation registers
                                                %   note name must be "CORE".
-define(NT_LOONGARCH_HW_BREAK,  16#a05).        % LoongArch hardware breakpoint registers
                                                %   note name must be "LINUX".
-define(NT_LOONGARCH_HW_WATCH,  16#a06).        % LoongArch hardware watchpoint registers
                                                %   note name must be "LINUX".
-define(NT_RISCV_CSR,           16#900).        % RISC-V Control and Status Registers
                                                %   note name must be "LINUX".
-define(NT_SIGINFO,             16#53494749).   % Fields of siginfo_t.
-define(NT_FILE,                16#46494c45).   % Description of mapped files.

%% The range 16#ff000000 to 16#ffffffff is set aside for notes that don't
%% originate from any particular operating system.
-define(NT_GDB_TDESC,           16#ff000000).   % Contains copy of GDB's target description XML.

%% Note segments for core files on dir-style procfs systems.

-define(NT_PSTATUS,             10).    % Has a struct pstatus
-define(NT_FPREGS,              12).    % Has a struct fpregset
-define(NT_PSINFO,              13).    % Has a struct psinfo
-define(NT_LWPSTATUS,           16).    % Has a struct lwpstatus_t
-define(NT_LWPSINFO,            17).    % Has a struct lwpsinfo_t
-define(NT_WIN32PSTATUS,        18).    % Has a struct win32_pstatus

%% Note segment for SystemTap probes.
-define(NT_STAPSDT,             3).

%% Note segments for core files on FreeBSD systems.  Note name is
%% "FreeBSD".

-define(NT_FREEBSD_THRMISC,             7).      % Thread miscellaneous info.
-define(NT_FREEBSD_PROCSTAT_PROC,       8).      % Procstat proc data.
-define(NT_FREEBSD_PROCSTAT_FILES,      9).      % Procstat files data.
-define(NT_FREEBSD_PROCSTAT_VMMAP,      10).     % Procstat vmmap data.
-define(NT_FREEBSD_PROCSTAT_GROUPS,     11).     % Procstat groups data.
-define(NT_FREEBSD_PROCSTAT_UMASK,      12).     % Procstat umask data.
-define(NT_FREEBSD_PROCSTAT_RLIMIT,     13).     % Procstat rlimit data.
-define(NT_FREEBSD_PROCSTAT_OSREL,      14).     % Procstat osreldate data.
-define(NT_FREEBSD_PROCSTAT_PSSTRINGS,  15).     % Procstat ps_strings data.
-define(NT_FREEBSD_PROCSTAT_AUXV,       16).     % Procstat auxv data.
-define(NT_FREEBSD_PTLWPINFO,           17).     % Thread ptrace miscellaneous info.
-define(NT_FREEBSD_X86_SEGBASES,        16#200). % x86 segment base registers

%% Note segments for core files on NetBSD systems.  Note name
%% must start with "NetBSD-CORE".

-define(NT_NETBSDCORE_PROCINFO,         1).     % Has a struct procinfo
-define(NT_NETBSDCORE_AUXV,             2).     % Has auxv data
-define(NT_NETBSDCORE_LWPSTATUS,        24).    % Has LWPSTATUS data
-define(NT_NETBSDCORE_FIRSTMACH,        32).    % start of machdep note types


%% Note segments for core files on OpenBSD systems.  Note name is
%% "OpenBSD".

-define(NT_OPENBSD_PROCINFO,    10).
-define(NT_OPENBSD_AUXV,        11).
-define(NT_OPENBSD_REGS,        20).
-define(NT_OPENBSD_FPREGS,      21).
-define(NT_OPENBSD_XFPREGS,     22).
-define(NT_OPENBSD_WCOOKIE,     23).

%% Note segments for core files on QNX systems.  Note name
%% must start with "QNX".
-define(QNT_DEBUG_FULLPATH,     1).
-define(QNT_DEBUG_RELOC,        2).
-define(QNT_STACK,              3).
-define(QNT_GENERATOR,          4).
-define(QNT_DEFAULT_LIB,        5).
-define(QNT_CORE_SYSINFO,       6).
-define(QNT_CORE_INFO,          7).
-define(QNT_CORE_STATUS,        8).
-define(QNT_CORE_GREG,          9).
-define(QNT_CORE_FPREG,         10).
-define(QNT_LINK_MAP,           11).

%% Note segments for core files on Solaris systems.  Note name
%% must start with "CORE".
-define(SOLARIS_NT_PRSTATUS,     1).
-define(SOLARIS_NT_PRFPREG,      2).
-define(SOLARIS_NT_PRPSINFO,     3).
-define(SOLARIS_NT_PRXREG,       4).
-define(SOLARIS_NT_PLATFORM,     5).
-define(SOLARIS_NT_AUXV,         6).
-define(SOLARIS_NT_GWINDOWS,     7).
-define(SOLARIS_NT_ASRS,         8).
-define(SOLARIS_NT_LDT,          9).
-define(SOLARIS_NT_PSTATUS,     10).
-define(SOLARIS_NT_PSINFO,      13).
-define(SOLARIS_NT_PRCRED,      14).
-define(SOLARIS_NT_UTSNAME,     15).
-define(SOLARIS_NT_LWPSTATUS,   16).
-define(SOLARIS_NT_LWPSINFO,    17).
-define(SOLARIS_NT_PRPRIV,      18).
-define(SOLARIS_NT_PRPRIVINFO,  19).
-define(SOLARIS_NT_CONTENT,     20).
-define(SOLARIS_NT_ZONENAME,    21).
-define(SOLARIS_NT_PRCPUXREG,   22).

%% Note segments for core files on SPU systems.  Note name
%% must start with "SPU/".

-define(NT_SPU,                 1).

%% Values of note segment descriptor types for object files.

-define(NT_VERSION,             1).     % Contains a version string.
-define(NT_ARCH,                2).     % Contains an architecture string.
-define(NT_GO_BUILDID,          4).     % Contains GO buildid data.

%% Values for notes in non-core files using name "GNU".

-define(NT_GNU_ABI_TAG,         1).
-define(NT_GNU_HWCAP,           2).     % Used by ld.so and kernel vDSO.
-define(NT_GNU_BUILD_ID,        3).     % Generated by ld --build-id.
-define(NT_GNU_GOLD_VERSION,    4).     % Generated by gold.
-define(NT_GNU_PROPERTY_TYPE_0, 5).     % Generated by gcc.

-define(NT_GNU_BUILD_ATTRIBUTE_OPEN,    16#100).
-define(NT_GNU_BUILD_ATTRIBUTE_FUNC,    16#101).

-define(GNU_BUILD_ATTRIBUTE_TYPE_NUMERIC,       $*).
-define(GNU_BUILD_ATTRIBUTE_TYPE_STRING,        $$).
-define(GNU_BUILD_ATTRIBUTE_TYPE_BOOL_TRUE,     $+).
-define(GNU_BUILD_ATTRIBUTE_TYPE_BOOL_FALSE,    $!).

-define(GNU_BUILD_ATTRIBUTE_VERSION,    1).
-define(GNU_BUILD_ATTRIBUTE_STACK_PROT, 2).
-define(GNU_BUILD_ATTRIBUTE_RELRO,      3).
-define(GNU_BUILD_ATTRIBUTE_STACK_SIZE, 4).
-define(GNU_BUILD_ATTRIBUTE_TOOL,       5).
-define(GNU_BUILD_ATTRIBUTE_ABI,        6).
-define(GNU_BUILD_ATTRIBUTE_PIC,        7).
-define(GNU_BUILD_ATTRIBUTE_SHORT_ENUM, 8).

-define(NOTE_GNU_PROPERTY_SECTION_NAME, ".note.gnu.property").
-define(GNU_BUILD_ATTRS_SECTION_NAME,   ".gnu.build.attributes").

%% Values used in GNU .note.gnu.property notes (NT_GNU_PROPERTY_TYPE_0).
-define(GNU_PROPERTY_STACK_SIZE,                1).
-define(GNU_PROPERTY_NO_COPY_ON_PROTECTED,      2).
-define(GNU_PROPERTY_MEMORY_SEAL,               3).

%% A 4-byte unsigned integer property: A bit is set if it is set in all
%% relocatable inputs.
-define(GNU_PROPERTY_UINT32_AND_LO,     16#b0000000).
-define(GNU_PROPERTY_UINT32_AND_HI,     16#b0007fff).

%% A 4-byte unsigned integer property: A bit is set if it is set in any
%% relocatable inputs.
-define(GNU_PROPERTY_UINT32_OR_LO,      16#b0008000).
-define(GNU_PROPERTY_UINT32_OR_HI,      16#b000ffff).

%% The needed properties by the object file.
-define(GNU_PROPERTY_1_NEEDED,          ?GNU_PROPERTY_UINT32_OR_LO).

%% Set if the object file requires canonical function pointers and
%% cannot be used with copy relocation.
-define(GNU_PROPERTY_1_NEEDED_INDIRECT_EXTERN_ACCESS,   (1 bsl 0)).

%% Processor-specific semantics, lo
-define(GNU_PROPERTY_LOPROC,    16#c0000000).
%% Processor-specific semantics, hi
-define(GNU_PROPERTY_HIPROC,    16#dfffffff).
%% Application-specific semantics, lo
-define(GNU_PROPERTY_LOUSER,    16#e0000000).
%% Application-specific semantics, hi
-define(GNU_PROPERTY_HIUSER,    16#ffffffff).

-define(GNU_PROPERTY_X86_COMPAT_ISA_1_USED,     16#c0000000).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_NEEDED,   16#c0000001).

-define(GNU_PROPERTY_X86_COMPAT_ISA_1_486,      (1 bsl 0)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_586,      (1 bsl 1)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_686,      (1 bsl 2)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_SSE,      (1 bsl 3)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_SSE2,     (1 bsl 4)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_SSE3,     (1 bsl 5)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_SSSE3,    (1 bsl 6)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_SSE4_1,   (1 bsl 7)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_SSE4_2,   (1 bsl 8)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_AVX,      (1 bsl 9)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_AVX2,     (1 bsl 10)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_AVX512F,  (1 bsl 11)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_AVX512CD, (1 bsl 12)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_AVX512ER, (1 bsl 13)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_AVX512PF, (1 bsl 14)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_AVX512VL, (1 bsl 15)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_AVX512DQ, (1 bsl 16)).
-define(GNU_PROPERTY_X86_COMPAT_ISA_1_AVX512BW, (1 bsl 17)).

%% A 4-byte unsigned integer property: A bit is set if it is set in all
%% relocatable inputs.
-define(GNU_PROPERTY_X86_UINT32_AND_LO,         16#c0000002).
-define(GNU_PROPERTY_X86_UINT32_AND_HI,         16#c0007fff).

%% A 4-byte unsigned integer property: A bit is set if it is set in any
%% relocatable inputs.
-define(GNU_PROPERTY_X86_UINT32_OR_LO,          16#c0008000).
-define(GNU_PROPERTY_X86_UINT32_OR_HI,          16#c000ffff).

%% A 4-byte unsigned integer property: A bit is set if it is set in any
%% relocatable inputs and the property is present in all relocatable
%% inputs.
-define(GNU_PROPERTY_X86_UINT32_OR_AND_LO,      16#c0010000).
-define(GNU_PROPERTY_X86_UINT32_OR_AND_HI,      16#c0017fff).

-define(GNU_PROPERTY_X86_FEATURE_1_AND, (?GNU_PROPERTY_X86_UINT32_AND_LO + 0)).

-define(GNU_PROPERTY_X86_ISA_1_NEEDED, (?GNU_PROPERTY_X86_UINT32_OR_LO + 2)).
-define(GNU_PROPERTY_X86_FEATURE_2_NEEDED, (?GNU_PROPERTY_X86_UINT32_OR_LO + 1)).

-define(GNU_PROPERTY_X86_ISA_1_USED, (?GNU_PROPERTY_X86_UINT32_OR_AND_LO + 2)).
-define(GNU_PROPERTY_X86_FEATURE_2_USED, (?GNU_PROPERTY_X86_UINT32_OR_AND_LO + 1)).

%% GNU_PROPERTY_X86_ISA_1_BASELINE: CMOV, CX8 (cmpxchg8b), FPU (fld),
%% MMX, OSFXSR (fxsave), SCE (syscall), SSE and SSE2.
-define(GNU_PROPERTY_X86_ISA_1_BASELINE,        (1 bsl 0)).
%% GNU_PROPERTY_X86_ISA_1_V2: GNU_PROPERTY_X86_ISA_1_BASELINE,
%% CMPXCHG16B (cmpxchg16b), LAHF-SAHF (lahf), POPCNT (popcnt), SSE3,
%% SSSE3, SSE4.1 and SSE4.2.
-define(GNU_PROPERTY_X86_ISA_1_V2,              (1 bsl 1)).
%% GNU_PROPERTY_X86_ISA_1_V3: GNU_PROPERTY_X86_ISA_1_V2, AVX, AVX2, BMI1,
%% BMI2, F16C, FMA, LZCNT, MOVBE, XSAVE.
-define(GNU_PROPERTY_X86_ISA_1_V3,              (1 bsl 2)).
%% GNU_PROPERTY_X86_ISA_1_V4: GNU_PROPERTY_X86_ISA_1_V3, AVX512F,
%% AVX512BW, AVX512CD, AVX512DQ and AVX512VL.
-define(GNU_PROPERTY_X86_ISA_1_V4,              (1 bsl 3)).

-define(GNU_PROPERTY_X86_FEATURE_1_IBT,         (1 bsl 0)).
-define(GNU_PROPERTY_X86_FEATURE_1_SHSTK,       (1 bsl 1)).
-define(GNU_PROPERTY_X86_FEATURE_1_LAM_U48,     (1 bsl 2)).
-define(GNU_PROPERTY_X86_FEATURE_1_LAM_U57,     (1 bsl 3)).

-define(GNU_PROPERTY_X86_FEATURE_2_X86,         (1 bsl 0)).
-define(GNU_PROPERTY_X86_FEATURE_2_X87,         (1 bsl 1)).
-define(GNU_PROPERTY_X86_FEATURE_2_MMX,         (1 bsl 2)).
-define(GNU_PROPERTY_X86_FEATURE_2_XMM,         (1 bsl 3)).
-define(GNU_PROPERTY_X86_FEATURE_2_YMM,         (1 bsl 4)).
-define(GNU_PROPERTY_X86_FEATURE_2_ZMM,         (1 bsl 5)).
-define(GNU_PROPERTY_X86_FEATURE_2_FXSR,        (1 bsl 6)).
-define(GNU_PROPERTY_X86_FEATURE_2_XSAVE,       (1 bsl 7)).
-define(GNU_PROPERTY_X86_FEATURE_2_XSAVEOPT,    (1 bsl 8)).
-define(GNU_PROPERTY_X86_FEATURE_2_XSAVEC,      (1 bsl 9)).
-define(GNU_PROPERTY_X86_FEATURE_2_TMM,         (1 bsl 10)).
-define(GNU_PROPERTY_X86_FEATURE_2_MASK,        (1 bsl 11)).

-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_NEEDED, (?GNU_PROPERTY_X86_UINT32_OR_LO + 0)).

-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_USED,   (?GNU_PROPERTY_X86_UINT32_OR_AND_LO + 0)).

-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_CMOV,           (1 bsl 0)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_SSE,            (1 bsl 1)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_SSE2,           (1 bsl 2)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_SSE3,           (1 bsl 3)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_SSSE3,          (1 bsl 4)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_SSE4_1,         (1 bsl 5)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_SSE4_2,         (1 bsl 6)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX,            (1 bsl 7)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX2,           (1 bsl 8)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_FMA,            (1 bsl 9)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512F,        (1 bsl 10)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512CD,       (1 bsl 11)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512ER,       (1 bsl 12)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512PF,       (1 bsl 13)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512VL,       (1 bsl 14)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512DQ,       (1 bsl 15)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512BW,       (1 bsl 16)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512_4FMAPS,  (1 bsl 17)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512_4VNNIW,  (1 bsl 18)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512_BITALG,  (1 bsl 19)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512_IFMA,    (1 bsl 20)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512_VBMI,    (1 bsl 21)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512_VBMI2,   (1 bsl 22)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512_VNNI,    (1 bsl 23)).
-define(GNU_PROPERTY_X86_COMPAT_2_ISA_1_AVX512_BF16,    (1 bsl 24)).

%% AArch64 specific GNU PROPERTY.
-define(GNU_PROPERTY_AARCH64_FEATURE_1_AND,     16#c0000000).

-define(GNU_PROPERTY_AARCH64_FEATURE_1_BTI,     (1 bsl 0)).
-define(GNU_PROPERTY_AARCH64_FEATURE_1_PAC,     (1 bsl 1)).
-define(GNU_PROPERTY_AARCH64_FEATURE_1_GCS,     (1 bsl 2)).

%% Values used in GNU .note.ABI-tag notes (NT_GNU_ABI_TAG).
-define(GNU_ABI_TAG_LINUX,      0).
-define(GNU_ABI_TAG_HURD,       1).
-define(GNU_ABI_TAG_SOLARIS,    2).
-define(GNU_ABI_TAG_FREEBSD,    3).
-define(GNU_ABI_TAG_NETBSD,     4).
-define(GNU_ABI_TAG_SYLLABLE,   5).
-define(GNU_ABI_TAG_NACL,       6).

%% Values for NetBSD .note.netbsd.ident notes.  Note name is "NetBSD".

-define(NT_NETBSD_IDENT,        1).
-define(NT_NETBSD_MARCH,        5).

%% Values for NetBSD .note.netbsd.ident notes.  Note name is "PaX".
-define(NT_NETBSD_PAX,          3).
-define(NT_NETBSD_PAX_MPROTECT,         16#01). % Force enable Mprotect.
-define(NT_NETBSD_PAX_NOMPROTECT,       16#02). % Force disable Mprotect.
-define(NT_NETBSD_PAX_GUARD,            16#04). % Force enable Segvguard.
-define(NT_NETBSD_PAX_NOGUARD,          16#08). % Force disable Segvguard.
-define(NT_NETBSD_PAX_ASLR,             16#10). % Force enable ASLR.
-define(NT_NETBSD_PAX_NOASLR,           16#20). % Force disable ASLR.

%% Values for OpenBSD .note.openbsd.ident notes.  Note name is "OpenBSD".

-define(NT_OPENBSD_IDENT,       1).

%% Values for FreeBSD .note.ABI-tag notes.  Note name is "FreeBSD".

-define(NT_FREEBSD_ABI_TAG,     1).

%% Values for FDO .note.package notes as defined on https://systemd.io/COREDUMP_PACKAGE_METADATA/
-define(FDO_PACKAGING_METADATA, 16#cafe1a7e).

%% Values for FDO .note.dlopen notes as defined on https://systemd.io/ELF_DLOPEN_METADATA/
-define(FDO_DLOPEN_METADATA, 16#407c0c0a).

%% Program header

-record(elf36_Phdr,
        { p_type        :: elf36_Word() % Identifies program segment type
        , p_offset      :: elf36_Off()  % Segment file offset
        , p_vaddr       :: elf36_Addr() % Segment virtual address
        , p_paddr       :: elf36_Addr() % Segment physical address
        , p_filesz      :: elf36_Word() % Segment size in file
        , p_memsz       :: elf36_Word() % Segment size in memory
        , p_flags       :: elf36_Word() % Segment flags
        , p_align       :: elf36_Word() % Segment alignment, file & memory
        }).

-define(ELF36_PHDR_SIZEOF, (8 * 4)).

%% Values for program header, p_type field.

-define(PT_NULL,        0).             % Program header table entry unused
-define(PT_LOAD,        1).             % Loadable program segment
-define(PT_DYNAMIC,     2).             % Dynamic linking information
-define(PT_INTERP,      3).             % Program interpreter
-define(PT_NOTE,        4).             % Auxiliary information
-define(PT_SHLIB,       5).             % Reserved, unspecified semantics
-define(PT_PHDR,        6).             % Entry for header table itself
-define(PT_TLS,         7).             % Thread local storage segment
-define(PT_NUM,         8).             % Number of defined types.

-define(PT_LOOS,        16#60000000).   % OS-specific
-define(PT_HIOS,        16#6fffffff).   % OS-specific

-define(PT_SUNW_UNWIND,         (?PT_LOOS + 16#464e550)).
-define(PT_GNU_EH_FRAME,        (?PT_LOOS + 16#474e550)).       % Frame unwind information
-define(PT_SUNW_EH_FRAME,       ?PT_GNU_EH_FRAME).              % Solaris uses the same value
-define(PT_GNU_STACK,           (?PT_LOOS + 16#474e551)).       % Stack flags
-define(PT_GNU_RELRO,           (?PT_LOOS + 16#474e552)).       % Read-only after relocation
-define(PT_GNU_PROPERTY,        (?PT_LOOS + 16#474e553)).       % GNU property
-define(PT_GNU_SFRAME,          (?PT_LOOS + 16#474e554)).       % SFrame stack trace information

%% OpenBSD segment types.
-define(PT_OPENBSD_MUTABLE,     (?PT_LOOS + 16#5a3dbe5)).       % Like bss, but not immutable.
-define(PT_OPENBSD_RANDOMIZE,   (?PT_LOOS + 16#5a3dbe6)).       % Fill with random data.
-define(PT_OPENBSD_WXNEEDED,    (?PT_LOOS + 16#5a3dbe7)).       % Program does W^X violations.
-define(PT_OPENBSD_NOBTCFI,     (?PT_LOOS + 16#5a3dbe8)).       % No branch target CFI.
-define(PT_OPENBSD_SYSCALLS,    (?PT_LOOS + 16#5a3dbe9)).       % System call sites.
-define(PT_OPENBSD_BOOTDATA,    (?PT_LOOS + 16#5a41be6)).       % Section for boot arguments.

%% Solaris segment types.
-define(PT_SUNWBSS,             (?PT_LOOS + 16#ffffffa)).       % Sun Specific segment.
-define(PT_SUNWSTACK,           (?PT_LOOS + 16#ffffffb)).       % Stack segment.
-define(PT_SUNWDTRACE,          (?PT_LOOS + 16#ffffffc)).
-define(PT_SUNWCAP,             (?PT_LOOS + 16#ffffffd)).

%% Mbind segments
-define(PT_GNU_MBIND_NUM,       4096).
-define(PT_GNU_MBIND_LO,        (?PT_LOOS + 16#474e555)).
-define(PT_GNU_MBIND_HI,        (?PT_GNU_MBIND_LO + ?PT_GNU_MBIND_NUM - 1)).

-define(PT_LOPROC,      16#70000000).   % Processor-specific
-define(PT_HIPROC,      16#7FFFFFFF).   % Processor-specific

%% Program segment permissions, in program header p_flags field.

-define(PF_X,           (1 bsl 0)).     % Segment is executable
-define(PF_W,           (1 bsl 1)).     % Segment is writable
-define(PF_R,           (1 bsl 2)).     % Segment is readable
%%-define(PF_MASKOS,    16#0F000000).   % OS-specific reserved bits
-define(PF_MASKOS,      16#0FF00000).   % New value, Oct 4, 1999 Draft
-define(PF_MASKPROC,    16#F0000000).   % Processor-specific reserved bits

%% dynamic section structure

-record(elf36_Dyn,
        { d_tag         :: elf36_Sword()        % entry tag value
        , d_valOrPtr    :: elf36_Word()         % val (Addr) or ptr (Addr)
        }).

%% Dynamic section tags.

-define(DT_NULL,                0).
-define(DT_NEEDED,              1).
-define(DT_PLTRELSZ,            2).
-define(DT_PLTGOT,              3).
-define(DT_HASH,                4).
-define(DT_STRTAB,              5).
-define(DT_SYMTAB,              6).
-define(DT_RELA,                7).
-define(DT_RELASZ,              8).
-define(DT_RELAENT,             9).
-define(DT_STRSZ,               10).
-define(DT_SYMENT,              11).
-define(DT_INIT,                12).
-define(DT_FINI,                13).
-define(DT_SONAME,              14).
-define(DT_RPATH,               15).
-define(DT_SYMBOLIC,            16).
-define(DT_REL,                 17).
-define(DT_RELSZ,               18).
-define(DT_RELENT,              19).
-define(DT_PLTREL,              20).
-define(DT_DEBUG,               21).
-define(DT_TEXTREL,             22).
-define(DT_JMPREL,              23).
-define(DT_BIND_NOW,            24).
-define(DT_INIT_ARRAY,          25).
-define(DT_FINI_ARRAY,          26).
-define(DT_INIT_ARRAYSZ,        27).
-define(DT_FINI_ARRAYSZ,        28).
-define(DT_RUNPATH,             29).
-define(DT_FLAGS,               30).

%% Values in the range [DT_ENCODING, DT_LOOS) use d_un.d_ptr if the
%% value is even, d_un.d_val if odd.
-define(DT_ENCODING,            32).
-define(DT_PREINIT_ARRAY,       32).
-define(DT_PREINIT_ARRAYSZ,     33).
-define(DT_SYMTAB_SHNDX,        34).
-define(DT_RELRSZ,              35).
-define(DT_RELR,                36).
-define(DT_RELRENT,             37).

%% Note, the Oct 4, 1999 draft of the ELF ABI changed the values
%% for DT_LOOS and DT_HIOS.  Some implementations however, use
%% values outside of the new range (see below).
-define(OLD_DT_LOOS,    16#60000000).
-define(DT_LOOS,        16#6000000d).
-define(DT_HIOS,        16#6ffff000).
-define(OLD_DT_HIOS,    16#6fffffff).

-define(DT_LOPROC,      16#70000000).
-define(DT_HIPROC,      16#7fffffff).

%% The next 2 dynamic tag ranges, integer value range (DT_VALRNGLO to
%% DT_VALRNGHI) and virtual address range (DT_ADDRRNGLO to DT_ADDRRNGHI),
%% are used on Solaris.  We support them everywhere.  Note these values
%% lie outside of the (new) range for OS specific values.  This is a
%% deliberate special case and we maintain it for backwards compatability.

-define(DT_VALRNGLO,    16#6ffffd00).
-define(DT_GNU_FLAGS_1, 16#6ffffdf4).
-define(DT_GNU_PRELINKED,16#6ffffdf5).
-define(DT_GNU_CONFLICTSZ,16#6ffffdf6).
-define(DT_GNU_LIBLISTSZ,16#6ffffdf7).
-define(DT_CHECKSUM,    16#6ffffdf8).
-define(DT_PLTPADSZ,    16#6ffffdf9).
-define(DT_MOVEENT,     16#6ffffdfa).
-define(DT_MOVESZ,      16#6ffffdfb).
-define(DT_FEATURE,     16#6ffffdfc).
-define(DT_POSFLAG_1,   16#6ffffdfd).
-define(DT_SYMINSZ,     16#6ffffdfe).
-define(DT_SYMINENT,    16#6ffffdff).
-define(DT_VALRNGHI,    16#6ffffdff).

-define(DT_ADDRRNGLO,   16#6ffffe00).
-define(DT_GNU_HASH,    16#6ffffef5).
-define(DT_TLSDESC_PLT, 16#6ffffef6).
-define(DT_TLSDESC_GOT, 16#6ffffef7).
-define(DT_GNU_CONFLICT,16#6ffffef8).
-define(DT_GNU_LIBLIST, 16#6ffffef9).
-define(DT_CONFIG,      16#6ffffefa).
-define(DT_DEPAUDIT,    16#6ffffefb).
-define(DT_AUDIT,       16#6ffffefc).
-define(DT_PLTPAD,      16#6ffffefd).
-define(DT_MOVETAB,     16#6ffffefe).
-define(DT_SYMINFO,     16#6ffffeff).
-define(DT_ADDRRNGHI,   16#6ffffeff).

-define(DT_RELACOUNT,   16#6ffffff9).
-define(DT_RELCOUNT,    16#6ffffffa).
-define(DT_FLAGS_1,     16#6ffffffb).
-define(DT_VERDEF,      16#6ffffffc).
-define(DT_VERDEFNUM,   16#6ffffffd).
-define(DT_VERNEED,     16#6ffffffe).
-define(DT_VERNEEDNUM,  16#6fffffff).

%% This tag is a GNU extension to the Solaris version scheme.
-define(DT_VERSYM,      16#6ffffff0).

%% These section tags are used on Solaris.  We support them
%% everywhere, and hope they do not conflict.

-define(DT_AUXILIARY,   16#7ffffffd).
-define(DT_USED ,       16#7ffffffe).
-define(DT_FILTER,      16#7fffffff).


%% Values used in DT_FEATURE .dynamic entry.
-define(DTF_1_PARINIT,  16#00000001).
%% From
%%
%%   http://docs.sun.com:80/ab2/coll.45.13/LLM/@Ab2PageView/21165?Ab2Lang=C&Ab2Enc=iso-8859-1
%%
%%   DTF_1_CONFEXP is the same as DTF_1_PARINIT. It is a typo. The value
%%   defined here is the same as the one in <sys/link.h> on Solaris 8.
-define(DTF_1_CONFEXP,  16#00000002).

%% Flag values used in the DT_POSFLAG_1 .dynamic entry.
-define(DF_P1_LAZYLOAD, 16#00000001).
-define(DF_P1_GROUPPERM,16#00000002).

%% Flag value in the DT_GNU_FLAGS_1 /dynamic entry.
-define(DF_GNU_1_UNIQUE,16#00000001).

%% Flag value in in the DT_FLAGS_1 .dynamic entry.
-define(DF_1_NOW,       16#00000001).
-define(DF_1_GLOBAL,    16#00000002).
-define(DF_1_GROUP,     16#00000004).
-define(DF_1_NODELETE,  16#00000008).
-define(DF_1_LOADFLTR,  16#00000010).
-define(DF_1_INITFIRST, 16#00000020).
-define(DF_1_NOOPEN,    16#00000040).
-define(DF_1_ORIGIN,    16#00000080).
-define(DF_1_DIRECT,    16#00000100).
-define(DF_1_TRANS,     16#00000200).
-define(DF_1_INTERPOSE, 16#00000400).
-define(DF_1_NODEFLIB,  16#00000800).
-define(DF_1_NODUMP,    16#00001000).
-define(DF_1_CONLFAT,   16#00002000).
-define(DF_1_ENDFILTEE, 16#00004000).
-define(DF_1_DISPRELDNE,16#00008000).
-define(DF_1_DISPRELPND,16#00010000).
-define(DF_1_NODIRECT,  16#00020000).
-define(DF_1_IGNMULDEF, 16#00040000).
-define(DF_1_NOKSYMS,   16#00080000).
-define(DF_1_NOHDR,     16#00100000).
-define(DF_1_EDITED,    16#00200000).
-define(DF_1_NORELOC,   16#00400000).
-define(DF_1_SYMINTPOSE,16#00800000).
-define(DF_1_GLOBAUDIT, 16#01000000).
-define(DF_1_SINGLETON, 16#02000000).
-define(DF_1_STUB,      16#04000000).
-define(DF_1_PIE,       16#08000000).
-define(DF_1_KMOD,      16#10000000).
-define(DF_1_WEAKFILTER,16#20000000).
-define(DF_1_NOCOMMON,  16#40000000).

%% Flag values for the DT_FLAGS entry.
-define(DF_ORIGIN,      (1 bsl 0)).
-define(DF_SYMBOLIC,    (1 bsl 1)).
-define(DF_TEXTREL,     (1 bsl 2)).
-define(DF_BIND_NOW,    (1 bsl 3)).
-define(DF_STATIC_TLS,  (1 bsl 4)).

%% This structure appears in a SHT_GNU_verdef section.

-record(elf36_Verdef,
        { vd_version    :: elf36_Half()
        , vd_flags      :: elf36_Half()
        , vd_ndx        :: elf36_Half()
        , vd_cnt        :: elf36_Half()
        , vd_hash       :: elf36_Word()
        , vd_aud        :: elf36_Word()
        , vd_next       :: elf36_Word()
        }).

%% These constants are used for the version number of a Elf36_Verdef
%% structure.

-define(VER_DEF_NONE,           0).
-define(VER_DEF_CURRENT,        1).

%% These constants appear in the vd_flags field of a Elf36_Verdef
%% structure.
%%
%% Cf. the Solaris Linker and Libraries Guide, Ch. 7, Object File Format,
%% Versioning Sections, for a description:
%%
%% http://docs.sun.com/app/docs/doc/819-0690/chapter6-93046?l=en&a=view

-define(VER_FLG_BASE,           16#1).
-define(VER_FLG_WEAK,           16#2).
-define(VER_FLG_INFO,           16#4).

%% This structure appears in a SHT_GNU_verdef section.

-record(elf36_Verdaux,
        { vda_name      :: elf36_Word()
        , vda_next      :: elf36_Word()
        }).

%% This structure appears in a SHT_GNU_verneed section.

-record(elf36_Verneed,
        { vn_version    :: elf36_Half()
        , vn_cnt        :: elf36_Half()
        , vn_file       :: elf36_Word()
        , vn_aux        :: elf36_Word()
        , vn_next       :: elf36_Word()
        }).

%% These constants are used for the version number of a Elf36_Verneed
%% structure.

-define(VER_NEED_NONE,          0).
-define(VER_NEED_CURRENT,       1).

%% This structure appears in a SHT_GNU_verneed section.

-record(elf36_Vernaux,
        { vna_hash      :: elf36_Word()
        , vna_flags     :: elf36_Half()
        , vna_other     :: elf36_Half()
        , vna_name      :: elf36_Word()
        , vna_next      :: elf36_Word()
        }).

%% This structure appears in a SHT_GNU_versym section.  This is not a
%% standard ELF structure; ELF just uses Elf36_Half.

-record(elf36_Versym,
        { vs_vers       :: elf36_Half()
        }).

%% These special constants can be found in an Elf36_Versym field.

-define(VER_NDX_LOCAL,          0).
-define(VER_NDX_GLOBAL,         1).

%% This flag appears in a Versym structure.  It means that the symbol
%% is hidden, and is only visible with an explicit version number.
%% This is a GNU extension.

-define(VERSYM_HIDDEN,          16#8000).

%% This is the mask for the rest of the Versym information.

-define(VERSYM_VERSION,         16#7fff).

%% This is a special token which appears as part of a symbol name.  It
%% indicates that the rest of the name is actually the name of a
%% version node, and is not part of the actual name.  This is a GNU
%% extension.  For example, the symbol name `stat@ver2' is taken to
%% mean the symbol `stat' in version `ver2'.

-define(ELF_VER_CHR,    $@).

%% Structure for syminfo section.
-record(elf36_Syminfo,
        { si_boundto    :: elf36_Half()
        , si_flags      :: elf36_Half()
        }).

%% Possible values for si_boundto.

-define(SYMINFO_BT_SELF,        16#ffff).       % Symbol bound to self
-define(SYMINFO_BT_PARENT,      16#fffe).       % Symbol bound to parent
-define(SYMINFO_BT_LOWRESERVE,  16#ff00).       % Beginning of reserved entries

%% Possible bitmasks for si_flags.

-define(SYMINFO_FLG_DIRECT,     16#0001).       % Direct bound symbol
-define(SYMINFO_FLG_PASSTHRU,   16#0002).       % Pass-thru symbol for translator
-define(SYMINFO_FLG_COPY,       16#0004).       % Symbol is a copy-reloc
-define(SYMINFO_FLG_LAZYLOAD,   16#0008).       % Symbol bound to object to be lazy loaded

%% Syminfo version values.

-define(SYMINFO_NONE,           0).
-define(SYMINFO_CURRENT,        1).
-define(SYMINFO_NUM,            2).

%% This structure appears on the stack and in NT_AUXV core file notes.
-record(elf36_Auxv,
        { a_type        :: elf36_Word()
        , a_val         :: elf36_Addr()
        }).

%% Auxv a_type values.

-define(AT_NULL,        0).             % End of vector
-define(AT_IGNORE,      1).             % Entry should be ignored
-define(AT_EXECFD,      2).             % File descriptor of program
-define(AT_PHDR,        3).             % Program headers for program
-define(AT_PHENT,       4).             % Size of program header entry
-define(AT_PHNUM,       5).             % Number of program headers
-define(AT_PAGESZ,      6).             % System page size
-define(AT_BASE,        7).             % Base address of interpreter
-define(AT_FLAGS,       8).             % Flags
-define(AT_ENTRY,       9).             % Entry point of program
-define(AT_NOTELF,      10).            % Program is not ELF
-define(AT_UID,         11).            % Real uid
-define(AT_EUID,        12).            % Effective uid
-define(AT_GID,         13).            % Real gid
-define(AT_EGID,        14).            % Effective gid
-define(AT_CLKTCK,      17).            % Frequency of times()
-define(AT_PLATFORM,    15).            % String identifying platform.
-define(AT_HWCAP,       16).            % Machine dependent hints about
                                        % processor capabilities.
-define(AT_FPUCW,       18).            % Used FPU control word.
-define(AT_DCACHEBSIZE, 19).            % Data cache block size.
-define(AT_ICACHEBSIZE, 20).            % Instruction cache block size.
-define(AT_UCACHEBSIZE, 21).            % Unified cache block size.
-define(AT_IGNOREPPC,   22).            % Entry should be ignored
-define(AT_SECURE,      23).            % Boolean, was exec setuid-like?
-define(AT_BASE_PLATFORM, 24).          % String identifying real platform,
                                        % may differ from AT_PLATFORM.
-define(AT_RANDOM,      25).            % Address of 16 random bytes.
-define(AT_HWCAP2,      26).            % Extension of AT_HWCAP.
-define(AT_RSEQ_FEATURE_SIZE, 27).      % rseq supported feature size
-define(AT_RSEQ_ALIGN,  28).            % rseq allocation alignment
-define(AT_EXECFN,      31).            % Filename of executable.
%% Pointer to the global system page used for system calls and other
%% nice things.
-define(AT_SYSINFO,     32).
-define(AT_SYSINFO_EHDR,33).            % Pointer to ELF header of system-supplied DSO.

%% More complete cache descriptions than AT_[DIU]CACHEBSIZE.  If the
%% value is -1, then the cache doesn't exist.  Otherwise:
%%
%% bit 0-3:  Cache set-associativity; 0 means fully associative.
%% bit 4-7:  Log2 of cacheline size.
%% bit 8-31:  Size of the entire cache >> 8.

-define(AT_L1I_CACHESHAPE,      34).
-define(AT_L1D_CACHESHAPE,      35).
-define(AT_L2_CACHESHAPE,       36).
-define(AT_L3_CACHESHAPE,       37).

%% Shapes of the caches, with more room to describe them.
%% *GEOMETRY are comprised of cache line size in bytes in the bottom 16 bits
%% and the cache associativity in the next 16 bits.
-define(AT_L1I_CACHESIZE,       40).
-define(AT_L1I_CACHEGEOMETRY,   41).
-define(AT_L1D_CACHESIZE,       42).
-define(AT_L1D_CACHEGEOMETRY,   43).
-define(AT_L2_CACHESIZE,        44).
-define(AT_L2_CACHEGEOMETRY,    45).
-define(AT_L3_CACHESIZE,        46).
-define(AT_L3_CACHEGEOMETRY,    47).

-define(AT_MINSIGSTKSZ,         51). % Stack needed for signal delivery (AArch64).

-define(AT_FREEBSD_EXECPATH,    15).    % Path to the executable.
-define(AT_FREEBSD_CANARY,      16).    % Canary for SSP.
-define(AT_FREEBSD_CANARYLEN,   17).    % Length of the canary.
-define(AT_FREEBSD_OSRELDATE,   18).    % OSRELDATE.
-define(AT_FREEBSD_NCPUS,       19).    % Number of CPUs.
-define(AT_FREEBSD_PAGESIZES,   20).    % Pagesizes.
-define(AT_FREEBSD_PAGESIZESLEN,21).    % Number of pagesizes.
-define(AT_FREEBSD_TIMEKEEP,    22).    % Pointer to timehands.
-define(AT_FREEBSD_STACKPROT,   23).    % Initial stack protection.
-define(AT_FREEBSD_EHDRFLAGS,   24).    % e_flags field from ELF header.
-define(AT_FREEBSD_HWCAP,       25).    % CPU feature flags.
-define(AT_FREEBSD_HWCAP2,      26).    % CPU feature flags 2.
-define(AT_FREEBSD_BSDFLAGS,    27).    % ELF BSD Flags.
-define(AT_FREEBSD_ARGC,        28).    % Argument count.
-define(AT_FREEBSD_ARGV,        29).    % Argument vector.
-define(AT_FREEBSD_ENVC,        30).    % Environment count.
-define(AT_FREEBSD_ENVV,        31).    % Environment vvector.
-define(AT_FREEBSD_PS_STRINGS,  32).    % struct ps_strings.
-define(AT_FREEBSD_FXRNG,       33).    % Pointer to root RNG seed version.
-define(AT_FREEBSD_KPRELOAD,    34).    % Base of vdso.
-define(AT_FREEBSD_USRSTACKBASE,35).    % Top of user stack.
-define(AT_FREEBSD_USRSTACKLIM, 36).    % Grow limit of user stack.

-define(AT_SUN_UID,             2000).  % Effective user ID.
-define(AT_SUN_RUID,            2001).  % Real user ID.
-define(AT_SUN_GID,             2002).  % Effective group ID.
-define(AT_SUN_RGID,            2003).  % Real group ID.
-define(AT_SUN_LDELF,           2004).  % Dynamic linker's ELF header.
-define(AT_SUN_LDSHDR,          2005).  % Dynamic linker's section headers.
-define(AT_SUN_LDNAME,          2006).  % String giving name of dynamic linker.
-define(AT_SUN_LPAGESZ,         2007).  % Large pagesize.
-define(AT_SUN_PLATFORM,        2008).  % Platform name string.
-define(AT_SUN_CAP_HW1,         2009).  % Machine dependent hints about
                                        % processor capabilities.
-ifndef(AT_SUN_HWCAP).
-define(AT_SUN_HWCAP, ?AT_SUN_CAP_HW1). % For backward compat only.
-endif.
-define(AT_SUN_IFLUSH,          2010).  % Should flush icache?
-define(AT_SUN_CPU,             2011).  % CPU name string.
-define(AT_SUN_EMUL_ENTRY,      2012).  % COFF entry point address.
-define(AT_SUN_EMUL_EXECFD,     2013).  % COFF executable file descriptor.
-define(AT_SUN_EXECNAME,        2014).  % Canonicalized file name given to execve.
-define(AT_SUN_MMU,             2015).  % String for name of MMU module.
-define(AT_SUN_LDDATA,          2016).  % Dynamic linker's data segment address.
-define(AT_SUN_AUXFLAGS,        2017).  % AF_SUN_ flags passed from the kernel.
-define(AT_SUN_EMULATOR,        2018).  % Name of emulation binary for runtime
                                        % linker.
-define(AT_SUN_BRANDNAME,       2019).  % Name of brand library.
-define(AT_SUN_BRAND_AUX1,      2020).  % Aux vectors for brand modules.
-define(AT_SUN_BRAND_AUX2,      2021).
-define(AT_SUN_BRAND_AUX3,      2022).
-define(AT_SUN_CAP_HW2,         2023).  % Extension of AT_SUN_CAP_HW1.

-endif. % PDP10_ELF36_HRL
