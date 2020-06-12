%%% -*- erlang-indent-level: 2 -*-
%%%
%%% simulator for pdp10-elf
%%% Copyright (C) 2020  Mikael Pettersson
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
%%% This simulates the address space of a user-mode process.
%%% Each page consists of 512 36-bit words.
%%% Mapping a page to a What =:= 'core' creates zero-initialized memory.
%%% Mapping a page to a What =/= 'core' causes accesses to fault with
%%% the page's {Prot, What} as Reason.  This can be used for demand-paging.
%%% Accessing an unmapped page faults with 'false' as Reason.
%%% Accessing a core-mapped page with incompatible protection faults with
%%% {Prot, What} as Reason.
%%%
%%% Internally, an address space is recorded in an ETS table, with:
%%% * {Address, Word} records for written memory cells, and
%%% * {-(PFN + 1), Prot, What} records for page table entries.
%%%
%%% Limitation: A page cannot be mapped at two or more virtual addresses at the
%%% same time, or be shared between two different address spaces (processes).

-module(sim_mem).

-export([ new/0
        , delete/1
        , mmap/4
        , mprotect/2
        , mquery/2
        , read_word/2
        , write_word/3
        ]).

-type address() :: non_neg_integer().
-type mem() :: ets:tid().
-type pfn() :: non_neg_integer().
-type prot() :: 0 | 2 | 4 | 6. % R(4) bor W(2)
-type what() :: 'core' | any().
-type word() :: non_neg_integer().

-export_type([ address/0
             , mem/0
             , pfn/0
             , prot/0
             , what/0
             , word/0
             ]).

-spec new() -> mem().
new() ->
  ets:new(undefined, [public]).

-spec delete(mem()) -> ok.
delete(MEM) ->
  ets:delete(MEM),
  ok.

-spec mmap(mem(), pfn(), prot(), what()) -> false | {prot(), what()}.
mmap(MEM, PFN, Prot, What) ->
  PageMapKey = -(PFN + 1),
  Old =
    case ets:lookup(MEM, PageMapKey) of
      [] -> false;
      [{_Key, OldProt, NonCore}] -> {OldProt, NonCore};
      [{_Key, OldProt, core}] ->
        erase_page(MEM, PFN),
        {OldProt, core}
    end,
  ets:insert(MEM, {PageMapKey, Prot, What}),
  Old.

erase_page(MEM, PFN) ->
  erase_page(MEM, PFN bsr 9, 0).

erase_page(_MEM, _Base, 512) -> ok;
erase_page(MEM, Base, Offset) ->
  ets:delete(MEM, Base + Offset),
  erase_page(MEM, Base, Offset + 1).

-spec mprotect(mem(), pfn()) -> ok | error.
mprotect(MEM, PFN) -> % so far only used for making pages read-only
  PageMapKey = -(PFN + 1),
  case ets:lookup(MEM, PageMapKey) of
    [] -> error;
    [{_Key, Prot, What}] ->
      ets:insert(MEM, {PageMapKey, Prot band bnot 2, What}),
      ok
  end.

-spec mquery(mem(), pfn()) -> false | {prot(), what()}.
mquery(MEM, PFN) ->
  PageMapKey = -(PFN + 1),
  case ets:lookup(MEM, PageMapKey) of
    [] -> false;
    [{_Key, Prot, What}] -> {Prot, What}
  end.

-spec read_word(mem(), address()) -> {ok, word()} | {error, {prot(), what()} | false}.
read_word(MEM, Address) ->
  PFN = Address bsr 9,
  PageMapKey = -(PFN + 1),
  case ets:lookup(MEM, PageMapKey) of
    [{_Key, Prot, core}] when (Prot band 4) =/= 0 ->
      case ets:lookup(MEM, Address) of
        [{_Address, Word}] -> {ok, Word};
        [] -> {ok, _Word = 0}
      end;
    [{_Key, Prot, What}] -> {error, {Prot, What}};
    [] -> {error, false}
  end.

-spec write_word(mem(), address(), word()) -> ok | {error, {prot(), what()} | false}.
write_word(MEM, Address, Word) ->
  PFN = Address bsr 9,
  PageMapKey = -(PFN + 1),
  case ets:lookup(MEM, PageMapKey) of
    [{_Key, Prot, core}] when (Prot band 2) =/= 0 ->
      ets:insert(MEM, {Address, Word}),
      ok;
    [{_Key, Prot, What}] -> {error, {Prot, What}};
    [] -> {error, false}
  end.
