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
%%% 2.8 Half-Word Data Transmission

-module(sim_halfword).

-export([ handle_HLL/4
        , handle_HLLE/4
        , handle_HLLEM/4
        , handle_HLLES/4
        , handle_HLLI/4
        , handle_HLLM/4
        , handle_HLLS/4
        , handle_HLLO/4
        , handle_HLLOI/4
        , handle_HLLOM/4
        , handle_HLLOS/4
        , handle_HLLZ/4
        , handle_HLLZM/4
        , handle_HLLZS/4
        , handle_HRL/4
        , handle_HRLI/4
        , handle_HRLM/4
        , handle_HRLS/4
        , handle_HRLZ/4
        , handle_HRLZM/4
        , handle_HRLZS/4
        ]).

-include("sim_core.hrl").

%% 2.8 Half-Word Data Transmission =============================================

%% HLL - Half Word Left to Left

-spec handle_HLL(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLL(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = set_left(CA, get_left(CE)),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_HLLI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLI(Core, Mem, IR, #ea{section = Section0, offset = Offset, islocal = IsLocal}) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  %% Behave as XHLLI (2.8.1) in non-zero sections.
  Section1 =
    if IsLocal, Offset =< 8#17, Section0 > 1 -> 1; % change local AC address to global one
       true -> Section0
    end,
  Word = set_left(CA, Section1),
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_HLLM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = set_left(CE, get_left(CA)),
      handle_writeback(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_HLLS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLS(Core, Mem, IR, EA) ->
  %% "If A is zero, HLLS is a no-op; otherwise, it is equivalent to MOVE."
  %% This implies that redundant memory accesses should not be performed.
  AC = IR band 8#17,
  case AC of
    0 -> sim_core:next_pc(Core, Mem);
    _ ->
      case sim_core:c(Core, Mem, EA) of
        {ok, CE} ->
          sim_core:next_pc(sim_core:set_ac(Core, AC, CE), Mem);
        {error, Reason} ->
          sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                              fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
      end
  end.

%% HLLZ - Half Word Left to Left, Zeros

-spec handle_HLLZ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLZ(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = set_left_zeros(get_left(CE)),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_HLLZM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLZM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = set_left_zeros(get_left(CA)),
  handle_writeback(Core, Mem, EA, Word).

-spec handle_HLLZS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLZS(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = set_left_zeros(get_left(CE)),
      handle_writeback(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% HLLO - Half Word Left to Left, Ones

-spec handle_HLLO(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLO(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = set_left_ones(get_left(CE)),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_HLLOI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLOI(Core, Mem, IR, _EA) ->
  AC = IR band 8#17,
  Word = (1 bsl 18) - 1,
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_HLLOM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLOM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = set_left_ones(get_left(CA)),
  handle_writeback(Core, Mem, EA, Word).

-spec handle_HLLOS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLOS(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = set_left_ones(get_left(CE)),
      handle_writeback(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% HLLE - Half Word Left to Left, Extend

-spec handle_HLLE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = set_left_extend(get_left(CE)),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_HLLEM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLEM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = set_left_extend(get_left(CA)),
  handle_writeback(Core, Mem, EA, Word).

-spec handle_HLLES(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HLLES(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = set_left_extend(get_left(CE)),
      handle_writeback(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% HRL - Half Word Right to Left

-spec handle_HRL(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HRL(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = set_left(CA, get_right(CE)),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_HRLI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HRLI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = set_left(CA, EA#ea.offset),
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_HRLM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HRLM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = set_left(CE, get_right(CA)),
      handle_writeback(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_HRLS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HRLS(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = set_left(CE, get_right(CE)),
      handle_writeback(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% HRLZ - Half Word Right to Left, Zeros

-spec handle_HRLZ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HRLZ(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = set_left_zeros(get_right(CE)),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_HRLZM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HRLZM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = set_left_zeros(get_right(CA)),
  handle_writeback(Core, Mem, EA, Word).

-spec handle_HRLZS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_HRLZS(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = set_left_zeros(get_right(CE)),
      handle_writeback(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% Miscellaneous ===============================================================

handle_writeback(Core, Mem, EA, Word) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} -> sim_core:next_pc(Core1, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, EA, Word) end)
  end.

handle_writeback(Core, Mem, AC, EA, Word) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} -> sim_core:next_pc(set_non_zero_ac(Core1, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, AC, EA, Word) end)
  end.

set_non_zero_ac(Core, _AC = 0, _Word) -> Core;
set_non_zero_ac(Core, AC, Word) -> sim_core:set_ac(Core, AC, Word).

ea_address(#ea{section = Section, offset = Offset}) ->
  (Section bsl 18) bor Offset.

get_left(Word) -> Word bsr 18.

get_right(Word) -> Word band ((1 bsl 18) - 1).

set_left(Word, Left) -> get_right(Word) bor (Left bsl 18).

set_left_extend(Left) ->
  Right = (0 - ((Left bsr 17) band 1)) band ((1 bsl 18) - 1),
  (Left bsl 18) bor Right.

set_left_ones(Left) -> (Left bsl 18) bor ((1 bsl 18) - 1).

set_left_zeros(Left) -> Left bsl 18.
