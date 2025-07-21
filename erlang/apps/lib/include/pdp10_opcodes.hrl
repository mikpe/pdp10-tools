%%% -*- erlang-indent-level: 2 -*-
%%%
%%% pdp10_opcodes.hrl
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

-ifndef(PDP10_OPCODES_HRL).
-define(PDP10_OPCODES_HRL, 1).

%% Data Representation
%% ===================
%%
%%
%%            11111111112222222222333333
%%  012345678901234567890123456789012345
%% +------------------------------------+
%% |                                    |
%% +------------------------------------+
%%
%% The basic storage unit is a 36-bit wide word.  Its bits are numbered 0
%% to 35, in left-to-right order, with bit 0 being the most significant
%% and bit 35 the least significant.
%%
%% The architecture supports sub-word storage units via special instructions
%% and specially formatted "byte" pointers, where a byte may be from 0 to 36
%% bits wide.  Incrementing a byte pointer moves it right over the word towards
%% its less significant bits, indicating a big-endian byte order.
%%
%% A 72-bit long integer consists of two adjacent words, with the most
%% significant bits in the first word (lower address) and the least significant
%% bits in the second word (higher address), indicating a big-endian word order.

%% Instruction Representation
%% ==========================
%%
%% Basic instructions are stored in 36-bit words with the following format:
%%
%%             111 1 1111 112222222222333333
%%  012345678 9012 3 4567 890123456789012345
%% +---------+----+-+----+------------------+
%% |  opcode |  A |I|  X |         Y        |
%% +---------+----+-+----+------------------+
%%    9 bits    4  1   4        18 bits
%%
%% A 9-bit opcode is stored in the high 9 bits.
%% A is a 4-bit field specifying the accumulator (a register).
%% I is a 1-bit field specifying indirect addressing.
%% X is a 4-bit field specifying the index register.
%% Y is an 18-bit field specifying an address or offset.
%%
%% E, the effective addreess, is computed from I, X, and Y.
%%
%% In some instructions A contains further opcode bits.
%%
%% In some instructions A is unused and should be zero.
%%
%% In some instructions A must be non-zero.
%%
%% Instructions that not compute an effective address E
%% should have I, X, and Y set to zero.
%%
%% IO instructions have a slightly different format:
%%
%%              111 1 1111 112222222222333333
%%  012 3456789 012 3 4567 890123456789012345
%% +---+-------+---+-+----+------------------+
%% |op1| device|op2|I|  X |         Y        |
%% +---+-------+---+-+----+------------------+
%%   3   7 bits   3 1   4       18 bits
%%
%% The op1 field is all-bits-one (7), the device field addresses the selected
%% device, and the op2 field specifies the operation.  Both devices internal to
%% the processor and devices attached via external buses can be accessed.
%%
%% Some non-IO instructions also have a 7 in their high three bits.
%%
%% Extended instructions consist of two separate instruction words:
%%
%% A:
%%             111 1 1111 112222222222333333
%%  012345678 9012 3 4567 890123456789012345
%% +---------+----+-+----+------------------+
%% |    0123 |  A |I|  X |         Y        |
%% +---------+----+-+----+------------------+
%%    9 bits    4  1   4        18 bits
%%
%% E0:
%%             111 1 1111 112222222222333333
%%  012345678 9012 3 4567 890123456789012345
%% +---------+----+-+----+------------------+
%% | xopcode |0000|I|  X |         Y        |
%% +---------+----+-+----+------------------+
%%    9 bits    4  1   4        18 bits
%%
%% The first word is stored at address A in the instruction stream, in the basic
%% format with opcode 0123.  The second word is stored at the effective address
%% E0 specified by the the first word.  Its accumulator field is unused and must
%% be zero for compatibility with future extensions.

%% Known PDP10 CPU models, each represented by a distinct bit value.
%%
%% These are combined with bit-wise 'and', 'or', and 'not' operations
%% to form sets of CPU models, used to check if a given mnemonic or
%% opcode is available for a selected set of CPUs.

%% DEC processors.

-define(PDP6,           (1 bsl  0)).    % PDP-6 Type 166 Arithmetic Processor
-define(PDP10_KA10,     (1 bsl  1)).    % KA10
-define(PDP10_KA10_ITS, (1 bsl  2)).    % KA10, ITS microcode
-define(PDP10_KI10,     (1 bsl  3)).    % KI10
-define(PDP10_KL10,     (1 bsl  4)).    % KL10 (early, non-extended)
-define(PDP10_KL10_ITS, (1 bsl  5)).    % KL10, ITS microcode
-define(PDP10_KL10_271, (1 bsl  6)).    % KL10B, microcode >= 271, extended
-define(PDP10_KS10,     (1 bsl  7)).    % KS10
-define(PDP10_KS10_ITS, (1 bsl  8)).    % KS10, ITS microcode

%% XKL Processors.

-define(PDP10_XKL1,     (1 bsl  9)).    % XKL-1 / TOAD-1

-define(PDP10_LAST, ?PDP10_XKL1).

%% Others, not yet supported:
%%
%% DEC KC10 (Jupiter, KL10B successor with full extended addressing, not built)
%% DEC KD10 (Minnow, KS10 successor with full extended addressing, not built)
%%
%% XKL-2 (XKL-1 successor, no documentation available)
%%
%% System Concepts SC-20, SC-25, SC-30M, SC-40 (KC10-like)
%%
%% Foonly F-1, F-2, F-3, F-4 (KI10/KL10-hybrid)
%%
%% Xerox PARC MAXC (KI10-like?)

%% Convenience constants for combinations of CPU models.

-define(PDP10_ALL, (?PDP10_LAST bor (?PDP10_LAST - 1))).

-define(PDP10_KL10_271up, (?PDP10_KL10_271 bor ?PDP10_XKL1)).
-define(PDP10_KL10any, (?PDP10_KL10 bor ?PDP10_KL10_ITS bor ?PDP10_KL10_271up)).
-define(PDP10_KL10up, (?PDP10_KL10any bor ?PDP10_KS10)).

-define(PDP10_KI10_to_KL10, (?PDP10_KI10 bor ?PDP10_KL10any)).
-define(PDP10_KI10up, (?PDP10_KI10 bor ?PDP10_KL10up)).

-define(PDP10_ITS, (?PDP10_KA10_ITS bor ?PDP10_KL10_ITS bor ?PDP10_KS10_ITS)).

-define(PDP10_KA10any, (?PDP10_KA10 bor ?PDP10_KA10_ITS)).
-define(PDP10_KA10up, (?PDP10_KA10any bor ?PDP10_KI10up)).
-define(PDP10_KA10up_not_ITS, (?PDP10_KA10up band bnot ?PDP10_ITS)).
-define(PDP10_KA10_to_KI10, (?PDP10_KA10 bor ?PDP10_KI10)). % FIXME: should that be KA10any?
-define(PDP10_KA10_to_KL10, (?PDP10_KA10_to_KI10 bor ?PDP10_KL10any)).

-define(PDP10_not_KS10_or_XKL1, (?PDP10_ALL band bnot (?PDP10_KS10 bor ?PDP10_XKL1))). % FIXME: should that be KS10any?

-define(PDP6_to_KI10, (?PDP6 bor ?PDP10_KA10_to_KI10)).

-type pdp10_cpu_models() :: 1..?PDP10_ALL.

%% Device fields in IO instructions.

-type pdp10_cpu_device() :: 0..127.

%% Each instruction belongs to exactly one of these primary categories,
%% which determine how the high 13 bits are to be interpreted.

-define(PDP10_INSN_BASIC, 'PDP10_INSN_BASIC').
-define(PDP10_INSN_A_OPCODE, 'PDP10_INSN_A_OPCODE').
-define(PDP10_INSN_A_NONZERO, 'PDP10_INSN_A_NONZERO').
-define(PDP10_INSN_IO, 'PDP10_INSN_IO').

-type pdp10_insn_format() :: ?PDP10_INSN_BASIC
                           | ?PDP10_INSN_A_OPCODE
                           | ?PDP10_INSN_A_NONZERO
                           | ?PDP10_INSN_IO.

%% Instruction descriptors.

-record(pdp10_insn_desc,
        { name :: string()
          %% The high13 field is 13 bits, formatted as:
          %% <9 bit opcode><0000>       BASIC, A_NONZERO
          %% <9 + 4 bit opcode>         A_OPCODE
          %% <111><0000000><3 bit op>   IO
          %%
          %% An extended instruction uses the BASIC format with opcode 0123 for
          %% the first word, and the A_OPCODE(00) EXTENDED format for the second word.
        , high13 :: 0..((1 bsl 13) - 1)
        , format :: pdp10_insn_format()
        , a_unused = false :: boolean()
        , e_unused = false :: boolean()
        , extended = false :: boolean()
        , models :: pdp10_cpu_models()
        , section0 = undefined :: undefined | true | false
        , priority = undefined :: undefined | non_neg_integer()
        }).

-endif. % PDP10_STDINT_HRL
