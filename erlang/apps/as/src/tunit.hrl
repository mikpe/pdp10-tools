%%% -*- erlang-indent-level: 2 -*-
%%%
%%% translation unit declarations for pdp10-elf as.
%%% Copyright (C) 2013-2019  Mikael Pettersson
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

-record(e_integer, {value :: integer()}).
-record(e_local_label, {number :: non_neg_integer(), direction :: $b | $f}).
-record(e_symbol, {name :: string()}).

-type expr() :: #e_integer{}
              | #e_local_label{}
              | #e_symbol{}
              .

%% A directive, label, or instruction is parsed to a statement, which is
%% either interpreted immediately or appended to the representation of the
%% current section.

%% .file "foo.c"
-record(s_dot_file, {string :: string()}).

%% .globl foo
-record(s_dot_globl, {name :: string()}).

%% .ident "..."
-record(s_dot_ident, {string :: string()}).

%% .popsection
-record(s_dot_popsection, {}).

%% .previous
-record(s_dot_previous, {}).

%% .pushsection name [, nr] (TODO: extend)
-record(s_dot_pushsection, {name :: string(), nr :: non_neg_integer()}).

%% .size foo,.-foo (TODO: extend)
-record(s_dot_size, {name :: string()}).

%% .subsection nr
-record(s_dot_subsection, {nr :: non_neg_integer()}).

%% .text [nr]
-record(s_dot_text, {nr :: non_neg_integer()}).

%% .type foo,@function (TODO: extend)
-record(s_dot_type, {name :: string()}).

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

-type stmt() :: #s_dot_file{}
              | #s_dot_globl{}
              | #s_dot_ident{}
              | #s_dot_popsection{}
              | #s_dot_previous{}
              | #s_dot_pushsection{}
              | #s_dot_size{}
              | #s_dot_subsection{}
              | #s_dot_text{}
              | #s_dot_type{}
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

%% Sections accumulate code or data, and define symbols.

-record(section,
        { name          :: string()
        , data          :: {stmts, [stmt()]} % before assembly, in reverse
                         | {image, image()}  % after assembly
        , dot           :: non_neg_integer()
        , shndx         :: non_neg_integer() % assigned during output
        %% FIXME: should contain an #elf36_Shdr{} here instead
        , sh_name       :: non_neg_integer() % assigned during output
        , sh_type       :: non_neg_integer()
        , sh_offset     :: non_neg_integer() % assigned during output
        , sh_flags      :: non_neg_integer()
        , sh_link       :: non_neg_integer() % assigned during output
        , sh_addralign  :: non_neg_integer()
        , sh_entsize    :: non_neg_integer()
        }).

%% Symbol values.

-record(symbol,
        { name          :: string()
        , section       :: false | string() % false if UNDEF or ABS
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
