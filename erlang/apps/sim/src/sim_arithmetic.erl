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
%%% 2.6 Arithmetic Testing

-module(sim_arithmetic).

-export([ handle_AOBJN/4
        , handle_AOBJP/4
        , handle_CAIE/4
        , handle_CAIG/4
        , handle_CAIGE/4
        , handle_CAIL/4
        , handle_CAILE/4
        , handle_CAIN/4
        , handle_CAM/4
        , handle_CAMA/4
        , handle_CAME/4
        , handle_CAMG/4
        , handle_CAMGE/4
        , handle_CAML/4
        , handle_CAMLE/4
        , handle_CAMN/4
        ]).

-include("sim_core.hrl").

%% 2.6.1 Add One to Both Halves of AC and Jump =================================

%% AOBJP - Add One to Both Halves of AC and Jump if Positive

-spec handle_AOBJP(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOBJP(Core, Mem, IR, EA) ->
  %% Note: KA10 incorrectly does a 36-bit add with 8#000001000001 here instead
  %% of adjusting the two halves separately.
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Left = CA bsr 18,
  Right = CA band ((1 bsl 18) - 1),
  Left1 = (Left + 1) band ((1 bsl 18) - 1),
  Right1 = (Right + 1) band ((1 bsl 18) - 1),
  CA1 = (Left1 bsl 18) bor Right1,
  Core1 = sim_core:set_ac(Core, AC, CA1),
  case Left1 band (1 bsl 17) of
    0 -> jump(Core1, Mem, EA);
    _ -> sim_core:next_pc(Core1, Mem)
  end.

%% AOBJN - Add One to Both Halves of AC and Jump if Negative

-spec handle_AOBJN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOBJN(Core, Mem, IR, EA) ->
  %% Note: KA10 incorrectly does a 36-bit add with 8#000001000001 here instead
  %% of adjusting the two halves separately.
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Left = CA bsr 18,
  Right = CA band ((1 bsl 18) - 1),
  Left1 = (Left + 1) band ((1 bsl 18) - 1),
  Right1 = (Right + 1) band ((1 bsl 18) - 1),
  CA1 = (Left1 bsl 18) bor Right1,
  Core1 = sim_core:set_ac(Core, AC, CA1),
  case Left1 band (1 bsl 17) of
    0 -> sim_core:next_pc(Core1, Mem);
    _ -> jump(Core1, Mem, EA)
  end.

%% 2.6.2 Comparisons, Skips, and Jumps =========================================

%% CAI - Compare AC Immediate and Skip if Condition Satisfied

-spec handle_CAIL(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAIL(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  skip_if_L(Core, Mem, sext36(CA), EA#ea.offset).

-spec handle_CAIE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAIE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  skip_if_E(Core, Mem, CA, EA#ea.offset).

-spec handle_CAILE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAILE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  skip_if_LE(Core, Mem, sext36(CA), EA#ea.offset).

-spec handle_CAIGE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAIGE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  skip_if_LE(Core, Mem, EA#ea.offset, sext36(CA)). % AC >= 0,E -> 0,E =< AC

-spec handle_CAIN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAIN(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  skip_if_N(Core, Mem, CA, EA#ea.offset).

-spec handle_CAIG(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAIG(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  skip_if_L(Core, Mem, EA#ea.offset, sext36(CA)). % AC > 0,E -> 0,E < AC

%% CAM - Compare AC with Memory and Skip if Condition Satisfied

-spec handle_CAM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAM(Core, Mem, IR, EA) ->
  %% this reads memory, but is otherwise a no-op
  case sim_core:c(Core, Mem, EA) of
    {ok, _CE} -> sim_core:next_pc(Core, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_CAML(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAML(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      skip_if_L(Core, Mem, sext36(CA), sext36(CE));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_CAME(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAME(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      skip_if_E(Core, Mem, CA, CE);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_CAMLE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAMLE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      skip_if_LE(Core, Mem, sext36(CA), sext36(CE));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_CAMA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAMA(Core, Mem, IR, EA) ->
  %% this reads memory and skips, but has no other side-effect
  case sim_core:c(Core, Mem, EA) of
    {ok, _CE} -> sim_core:skip(Core, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_CAMGE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAMGE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      skip_if_LE(Core, Mem, sext36(CE), sext36(CA)); % AC >= E -> E =< AC
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_CAMN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAMN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      skip_if_N(Core, Mem, CA, CE);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_CAMG(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_CAMG(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      skip_if_L(Core, Mem, sext36(CE), sext36(CA)); % AC > E -> E < AC
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% Miscellaneous ===============================================================

jump(Core, Mem, EA) ->
  sim_core:run(Core#core{pc_offset = EA#ea.offset}, Mem).

skip_if_E(Core, Mem, X, Y) ->
  case X =:= Y of
    true -> sim_core:skip(Core, Mem);
    false -> sim_core:next_pc(Core, Mem)
  end.

skip_if_L(Core, Mem, X, Y) ->
  case X < Y of
    true -> sim_core:skip(Core, Mem);
    false -> sim_core:next_pc(Core, Mem)
  end.

skip_if_LE(Core, Mem, X, Y) ->
  case X =< Y of
    true -> sim_core:skip(Core, Mem);
    false -> sim_core:next_pc(Core, Mem)
  end.

skip_if_N(Core, Mem, X, Y) ->
  case X =/= Y of
    true -> sim_core:skip(Core, Mem);
    false -> sim_core:next_pc(Core, Mem)
  end.

%% Sign-extend a uint36_t() to the full width of its representation type.
-spec sext36(uint36_t()) -> integer().
sext36(X) ->
  UInt36Sbit = 1 bsl (36 - 1),
  UInt36Max = (1 bsl 36) - 1,
  ((X band UInt36Max) bxor UInt36Sbit) - UInt36Sbit.

ea_address(#ea{section = Section, offset = Offset}) ->
  (Section bsl 18) bor Offset.
