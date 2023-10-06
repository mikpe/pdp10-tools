%%% -*- erlang-indent-level: 2 -*-
%%%
%%% translation unit declarations for pdp10-elf as.
%%% Copyright (C) 2013-2023  Mikael Pettersson
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

-ifndef(TUNIT_HRL).
-define(TUNIT_HRL, 1).

%% An expression occurring in a statement. (TODO: extend)

-type operand() :: string()                     % label or symbol, may be "."
                 | {non_neg_integer(), $b | $f} % local label, eliminated before assembly
                 | integer().
-record(expr,
        { operand1 :: operand() | false
        , operator :: '+' | '-'
        , operand2 :: operand()
        , modifier :: false | w | b | h
        }).
-type expr() :: #expr{}.

%% A directive, label, or instruction is parsed to a statement, which is
%% either interpreted immediately or appended to the representation of the
%% current section.

%% .asci{i,z} [string (, string)*]
-record(s_dot_ascii, {z :: boolean(), strings :: [string()]}).

%% .byte [expr (, expr)*]
-record(s_dot_byte, {exprs :: [expr()]}).

%% .data [nr]
-record(s_dot_data, {nr :: non_neg_integer()}).

%% .file "foo.c"
-record(s_dot_file, {string :: string()}).

%% .globl foo
-record(s_dot_globl, {name :: string()}).

%% .ident "..."
-record(s_dot_ident, {string :: string()}).

%% .long [expr (, expr)*]
-record(s_dot_long, {exprs :: [expr()]}).

%% .popsection
-record(s_dot_popsection, {}).

%% .previous
-record(s_dot_previous, {}).

%% .section name, "flags", @type, ...
%% .pushsection name, [, nr], "flags", @type, ...
%% TODO: add support for G and ? flags and ,<group>,<linkage>
-record(s_dot_section,
        { name :: string()
        %% nr is false for .section and a subsection number for .pushsection
        , nr :: false | non_neg_integer()
        %% the following are as per the Elf36 Shdr spec
        , sh_type :: non_neg_integer()
        , sh_flags :: non_neg_integer()
        , sh_entsize :: non_neg_integer()
        }).

%% .short [expr (, expr)*]
-record(s_dot_short, {exprs :: [expr()]}).

%% .size foo,.-foo (TODO: extend)
-record(s_dot_size, {name :: string()}).

%% .subsection nr
-record(s_dot_subsection, {nr :: non_neg_integer()}).

%% .text [nr]
-record(s_dot_text, {nr :: non_neg_integer()}).

%% .type foo,@function (TODO: extend)
-record(s_dot_type, {name :: string()}).

%% .2byte [expr (, expr)*]
-record(s_dot_2byte, {exprs :: [expr()]}).

%% .4byte [expr (, expr)*]
-record(s_dot_4byte, {exprs :: [expr()]}).

%% foo: 1:
-record(s_label, {name :: string()}).
-record(s_local_label, {number :: non_neg_integer()}).

%% opcode accumulator,@address(index)
-record(s_insn,
        { high13        :: 0..((1 bsl 13) - 1)
        , at            :: boolean()
        , address       :: expr()
        , index         :: 0..((1 bsl 4) - 1)
        }).

-type stmt() :: #s_dot_ascii{}
              | #s_dot_byte{}
              | #s_dot_data{}
              | #s_dot_file{}
              | #s_dot_globl{}
              | #s_dot_ident{}
              | #s_dot_long{}
              | #s_dot_popsection{}
              | #s_dot_previous{}
              | #s_dot_section{}
              | #s_dot_short{}
              | #s_dot_size{}
              | #s_dot_subsection{}
              | #s_dot_text{}
              | #s_dot_type{}
              | #s_dot_2byte{}
              | #s_dot_4byte{}
              | #s_label{}
              | #s_local_label{}
              | #s_insn{}
              .

%% After the assembly phase sections contain raw image data.
%% Image data is reduced to sequences of "target bytes".
%% We do not use Erlang binaries here to avoid complicating
%% mapping from octet-based hosts to nonet-based targets.

-type tbyte() :: 0..511. % may contain octets or nonets
-type image() :: tbyte() | [image()].

%% Relocations.

-record(rela, { offset  :: non_neg_integer()
              , type    :: byte() % ?R_PDP10_*
              , symbol  :: string()
              , addend  :: integer()
              }).

%% Sections accumulate code or data, and define symbols.

-record(section,
        { name          :: string()
        , data          :: {stmts, [stmt()]} % before assembly, in reverse
                         | {image, image()}  % after assembly
                         | {relocs, string(), [#rela{}]}  % after assembly
        , dot           :: non_neg_integer()
        , shndx         :: non_neg_integer() % assigned during output
        %% FIXME: should contain an #elf36_Shdr{} here instead
        , sh_name       :: non_neg_integer() % assigned during output
        , sh_type       :: non_neg_integer()
        , sh_offset     :: non_neg_integer() % assigned during output
        , sh_flags      :: non_neg_integer()
        , sh_link       :: non_neg_integer() % assigned during output
        , sh_info       :: non_neg_integer() % assigned during output
        , sh_addralign  :: non_neg_integer()
        , sh_entsize    :: non_neg_integer()
        }).

%% Symbol values.

-record(symbol,
        { name          :: string()
        , section       :: false | abs | string()
        %% FIXME: should contain an #elf36_Sym{} here instead
        , st_value      :: false | non_neg_integer()
        , st_size       :: false | non_neg_integer()
        , st_info       :: byte()
        , st_name       :: non_neg_integer() % assigned during output
        , st_shndx      :: non_neg_integer() % assigned during output
        }).

%% The translation unit object is the top-level container for the
%% representation of the sections, other information collected from
%% the input, and information synthesized during assembly.

-record(tunit,
        { sections      :: #{string() => #section{}}
        , cursect       :: string()
        , symbols       :: #{string() => #symbol{}}
        , local_labels  :: #{non_neg_integer() => pos_integer()}
        }).

-endif. % TUNIT_HRL
