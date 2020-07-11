%%% -*- erlang-indent-level: 2 -*-
%%%
%%% simulator for pdp10-elf
%%% Copyright (C) 2018-2020  Mikael Pettersson
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
%%%=============================================================================
%%%
%%% PDP10 Core Definitions
%%%
%%% Words are 36 bits wide.  The bits are numbered 0-35, left to right (most
%%% significant to least significant).  The left (more significant) half word
%%% is bits 0-17, and the right (less significant) half word is bits 18-36.
%%% Numbers use twos-complement representation, with bit 0 as the sign.
%%%
%%% A double word is two adjacent words treated as a single 72-bit entity,
%%% where the word with the lower address is the more significant.
%%%
%%% Bits are numbered in decimal, most other numbers are in octal, in particular
%%% accumulator numbers, addresses, and instruction codes are in octal.
%%%
%%% A virtual address on a fully extended processor is 30 bits, composed of a 12
%%% bit section number (upper 12 bits) and an 18-bit section offset (lower 18
%%% bits).  Paging hardware divides each section into 512 pages of 512 words.
%%% The address bits are numbered according to the right-justified position of
%%% an address in a word.  Thus bits 6-17 contain the section number, and bits
%%% 18-35 the section offset.
%%%
%%% Single-section processors (PDP6, KA10, KI10, non-extended KL10, KS10) have
%%% no section number in their virtual addresses, while the extended KL10 (aka
%%% KL10B) only has 5 bit section numbers (bits 13-17).
%%%
%%% The program counter register, PC, contains a virtual address.  Incrementing
%%% PC increments its segment offset without propagating any carry into the
%%% section number.
%%%
%%% Instructions are words.  In the basic instruction format, bits 0-8 specify
%%% the operation, and bits 9-12 (A) address an accumulator.  The rest of the
%%% word specifies how to calculate the effective address (E).  Bit 13 (I)
%%% specifies the type of addressing (indirect or not), bits 14-17 (X) specify
%%% an index register, and bits 18-35 (Y) specify a memory location.  Some
%%% instructions use bits 9-12 to address flags or as an extension to the
%%% instruction code.
%%%
%%% If an instruction does not use some part of the instruction word, that part
%%% is reserved and MUST BE ZERO, unless otherwise specified.

-ifndef(_SIM_CORE_HRL_).
-define(_SIM_CORE_HRL_, true).

-define(UINT_T(N), 0..((1 bsl (N)) - 1)).

-type uint12_t() :: ?UINT_T(12).
-type uint13_t() :: ?UINT_T(13).
-type uint18_t() :: ?UINT_T(18).
-type uint36_t() :: ?UINT_T(36).

-type word() :: uint36_t().

%%% Program Flags
%%% The Overflow, Carry 1/2, Floating Overflow, Floating Underflow, and No Divide
%%% flags are sticky: hardware sets them, but software must explicitly clear them.
%%% Note: architecturally these occupy bits 0-12 (PDP10 bit numbering) of a
%%% 36-bit word, but here we express them as occupying bits 12-0 (normal bit
%%% numbering) of a 13-bit word.  The bit numbers in the definitions are from
%%% the XKL-1 specification.

-define(PDP10_PF_NO_DIVIDE,               (12-12)).
-define(PDP10_PF_FLOATING_UNDERFLOW,      (12-11)).
-define(PDP10_PF_TRAP_1,                  (12-10)). % n/a on KA10 exec
-define(PDP10_PF_TRAP_2,                  (12-9)).  % n/a on KA10 exec
-define(PDP10_PF_ADDRESS_FAILURE_INHIBIT, (12-8)).  % n/a on KS10 and KA10 exec
-define(PDP10_PF_PUBLIC,                  (12-7)).  % n/a on XKL-1, KS10, and KA10 exec
-define(PDP10_PF_USER_IN_OUT,             (12-6)).  % user mode; n/a on XKL-1 and KS10
-define(PDP10_PF_PREVIOUS_CONTEXT_USER,   (12-6)).  % exec mode on KL10, KI10, KS10, and XKL-1
-define(PDP10_PF_USER,                    (12-5)).
-define(PDP10_PF_FIRST_PART_DONE,         (12-4)).
-define( PDP6_PF_BIS,                     (12-4)).  % PDP6 only
-define(PDP10_PF_FLOATING_OVERFLOW,       (12-3)).
-define( PDP6_PF_PC_CHANGE,               (12-3)).  % PDP6 only
-define(PDP10_PF_CARRY_1,                 (12-2)).
-define(PDP10_PF_CARRY_0,                 (12-1)).
-define(PDP10_PF_OVERFLOW,                (12-0)).  % user mode
-define(PDP10_PF_PREVIOUS_CONTEXT_PUBLIC, (12-0)).  % exec mode on KL10, KI10

%% Effective Address: section and offset for a 30-bit address, and a local/global flag.
-record(ea, {section :: uint12_t(), offset :: uint18_t(), islocal :: boolean()}).

%% Execution Core
-record(core,
        { %% user-mode visible context
          pc_section :: uint12_t()        % PC register, high 12 bits
        , pc_offset  :: uint18_t()        % PC register, low 18 bits
        , acs        :: tuple()           % array of 16 36-bit words
        , flags      :: uint13_t()        % status and condition bits
        %% TODO: add kernel-mode handling:
        %% - correctly handle being in kernel mode
        %% - ACBS: array of 8 AC blocks, user ACS is ACBS[CAB]
        %% - CAB: Current AC Block index, 3 bits
        %% - PCS: Previous Context Section, 12 bits
        %% - PCU: Previous Context User, 1 bit
        %% - PAB: Previous AC Block, 3 bit
        }).

-define(AC_SP, 8#17).

-endif. % _SIM_CORE_HRL_
