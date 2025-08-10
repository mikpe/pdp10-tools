%%% -*- erlang-indent-level: 2 -*-
%%%
%%% internal declarations for pdp10-elf ld.
%%% Copyright (C) 2020-2025  Mikael Pettersson
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

-ifndef(LD_INTERNAL_HRL).
-define(LD_INTERNAL_HRL, 1).

-include_lib("lib/include/pdp10_elf36.hrl").

-type ifile() :: string() % explicit input .o-file
               | { Archive :: string() % archive name
                 , Name :: string() % member name
                 , Offset :: non_neg_integer() % member data (not header) at this offset
                 , Size :: non_neg_integer() % member data size
                 }.

-record(input,
        { file    :: ifile()
        , shtab   :: [#elf_Shdr{}]
        , symtab  :: [#elf_Sym{}]
        , stshndx :: non_neg_integer()
        }).

-record(sectfrag,
        { file   :: ifile()
        , shdr   :: #elf_Shdr{}
        , shndx  :: non_neg_integer()
        , relocs :: #elf_Shdr{} | false
        , offset :: non_neg_integer() % within enclosing #section{}
        }).

-record(section,
        { shdr  :: #elf_Shdr{}
        , frags :: [#sectfrag{}]
        }).

-record(segment,
        { phdr     :: #elf_Phdr{}
        , sections :: [#section{}]
        }).

%% global symbol table
-type global() :: #{Symbol :: string() => Value :: non_neg_integer()}.

%% local symbol table within a given file (if Value is string(), consult global table)
-type local() :: #{SymNdx :: non_neg_integer() => Value :: non_neg_integer() | string()}.

%% map from input files to their local symbol tables
-type filemap() :: #{File :: ifile() => Local :: local()}.

-endif. % LD_INTERNAL_HRL
