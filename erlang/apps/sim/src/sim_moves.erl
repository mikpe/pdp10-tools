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
%%% 2.1 Full-Word Data Transmission

-module(sim_moves).

-export([ handle_EXCH/4
        , handle_MOVE/4
        , handle_MOVEI/4
        , handle_MOVEM/4
        , handle_MOVES/4
        , handle_MOVS/4
        , handle_MOVSI/4
        ]).

-include("sim_core.hrl").

%% 2.1.1 Exchange Instruction ==================================================

%% EXCH - Exchange Instruction

-spec handle_EXCH(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_EXCH(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      handle_EXCH(Core, Mem, AC, EA, CE, CA);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_EXCH(Core1, Mem1, IR, EA) end)
  end.

handle_EXCH(Core, Mem, AC, EA, CE, CA) ->
  case sim_core:cset(Core, Mem, EA, CA) of
    {ok, Core1} -> sim_core:next_pc(sim_core:set_ac(Core1, AC, CE), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> handle_EXCH(Core1, Mem1, AC, EA, CE, CA) end)
  end.

%% 2.1.2 Move Instruction Class ================================================

%% MOVE - Move

-spec handle_MOVE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      sim_core:next_pc(sim_core:set_ac(Core, AC, CE), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_MOVE(Core1, Mem1, IR, EA) end)
  end.

-spec handle_MOVEI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVEI(Core, Mem, IR, #ea{offset = E}) ->
  AC = IR band 8#17,
  sim_core:next_pc(sim_core:set_ac(Core, AC, E), Mem).

-spec handle_MOVEM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVEM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  handle_MOVEM_1(Core, Mem, CA, EA).

handle_MOVEM_1(Core, Mem, Word, EA) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} -> sim_core:next_pc(Core1, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> handle_MOVEM_1(Core1, Mem1, Word, EA) end)
  end.

-spec handle_MOVES(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVES(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      handle_MOVES(Core, Mem, AC, EA, CE);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_MOVES(Core1, Mem1, IR, EA) end)
  end.

handle_MOVES(Core, Mem, AC, EA, Word) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} -> sim_core:next_pc(set_non_zero_ac(Core1, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> handle_MOVES(Core1, Mem1, AC, EA, Word) end)
  end.

%% MOVS - Move Swapped

-spec handle_MOVS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVS(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      Swapped = swap_halves(CE),
      AC = IR band 8#17,
      sim_core:next_pc(sim_core:set_ac(Core, AC, Swapped), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_MOVS(Core1, Mem1, IR, EA) end)
  end.

-spec handle_MOVSI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVSI(Core, Mem, IR, #ea{offset = E}) ->
  AC = IR band 8#17,
  sim_core:next_pc(sim_core:set_ac(Core, AC, E bsl 18), Mem).

%% Miscellaneous ===============================================================

ea_address(#ea{section = Section, offset = Offset}) ->
  (Section bsl 18) bor Offset.

set_non_zero_ac(Core, _AC = 0, _Word) -> Core;
set_non_zero_ac(Core, AC, Word) -> sim_core:set_ac(Core, AC, Word).

swap_halves(Word) ->
  Low18Mask = ((1 bsl 18) - 1),
  ((Word band Low18Mask) bsl 18) bor ((Word bsr 18) band Low18Mask).
