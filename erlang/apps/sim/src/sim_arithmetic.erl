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
        , handle_AOJ/4
        , handle_AOJA/4
        , handle_AOJE/4
        , handle_AOJG/4
        , handle_AOJGE/4
        , handle_AOJL/4
        , handle_AOJLE/4
        , handle_AOJN/4
        , handle_AOS/4
        , handle_AOSA/4
        , handle_AOSE/4
        , handle_AOSG/4
        , handle_AOSGE/4
        , handle_AOSL/4
        , handle_AOSLE/4
        , handle_AOSN/4
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
        , handle_JUMPA/4
        , handle_JUMPE/4
        , handle_JUMPG/4
        , handle_JUMPGE/4
        , handle_JUMPL/4
        , handle_JUMPLE/4
        , handle_JUMPN/4
        , handle_SKIP/4
        , handle_SKIPA/4
        , handle_SKIPE/4
        , handle_SKIPG/4
        , handle_SKIPGE/4
        , handle_SKIPL/4
        , handle_SKIPLE/4
        , handle_SKIPN/4
        , handle_SOJ/4
        , handle_SOJA/4
        , handle_SOJE/4
        , handle_SOJG/4
        , handle_SOJGE/4
        , handle_SOJL/4
        , handle_SOJLE/4
        , handle_SOJN/4
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

%% JUMP - Jump if AC Condition Satisfied

-spec handle_JUMPL(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_JUMPL(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  jump_if_L(Core, Mem, CA, EA).

-spec handle_JUMPE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_JUMPE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  jump_if_E(Core, Mem, CA, EA).

-spec handle_JUMPLE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_JUMPLE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  jump_if_LE(Core, Mem, CA, EA).

-spec handle_JUMPA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_JUMPA(Core, Mem, _IR, EA) ->
  jump(Core, Mem, EA).

-spec handle_JUMPGE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_JUMPGE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  jump_if_GE(Core, Mem, CA, EA).

-spec handle_JUMPN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_JUMPN(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  jump_if_N(Core, Mem, CA, EA).

-spec handle_JUMPG(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_JUMPG(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  jump_if_G(Core, Mem, CA, EA).

%% SKIP - Skip if Memory Condition Satisfied

-spec handle_SKIP(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SKIP(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      sim_core:next_pc(set_non_zero_ac(Core, IR, CE), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SKIPL(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SKIPL(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      skip_if_L(set_non_zero_ac(Core, IR, CE), Mem, sext36(CE), 0);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SKIPE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SKIPE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      skip_if_E(set_non_zero_ac(Core, IR, CE), Mem, CE, 0);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SKIPLE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SKIPLE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      skip_if_LE(set_non_zero_ac(Core, IR, CE), Mem, sext36(CE), 0);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SKIPA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SKIPA(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      sim_core:skip(set_non_zero_ac(Core, IR, CE), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SKIPGE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SKIPGE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      skip_if_LE(set_non_zero_ac(Core, IR, CE), Mem, 0, sext36(CE)); % C(E) >= 0 -> 0 =< C(E)
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SKIPN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SKIPN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      skip_if_N(set_non_zero_ac(Core, IR, CE), Mem, CE, 0);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SKIPG(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SKIPG(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      skip_if_L(set_non_zero_ac(Core, IR, CE), Mem, 0, sext36(CE)); % C(E) > 0 -> 0 < C(E)
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% AOJ - Add One to AC and Jump if Condition Satisfied

-spec handle_AOJ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOJ(Core, Mem, IR, _EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = add1(CA),
  sim_core:next_pc(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem).

-spec handle_AOJL(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOJL(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = add1(CA),
  jump_if_L(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

-spec handle_AOJE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOJE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = add1(CA),
  jump_if_E(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

-spec handle_AOJLE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOJLE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = add1(CA),
  jump_if_LE(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

-spec handle_AOJA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOJA(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = add1(CA),
  jump(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, EA).

-spec handle_AOJGE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOJGE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = add1(CA),
  jump_if_GE(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

-spec handle_AOJN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOJN(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = add1(CA),
  jump_if_N(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

-spec handle_AOJG(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOJG(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = add1(CA),
  jump_if_G(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

%% AOS - Add One to Memory and Skip if Condition Satisfied

-spec handle_AOS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOS(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      {Word, Flags} = add1(CE),
      handle_AOS(Core, Mem, IR, EA, Word, Flags);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_AOS(Core, Mem, IR, EA, Word, Flags) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} ->
      Core2 = set_non_zero_ac(Core1, IR, Word),
      Core3 = sim_core:set_flags(Core2, Flags),
      sim_core:next_pc(Core3, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA, Word, Flags) end)
  end.

-spec handle_AOSL(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOSL(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      {Word, Flags} = add1(CE),
      handle_AOSL(Core, Mem, IR, EA, Word, Flags);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_AOSL(Core, Mem, IR, EA, Word, Flags) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} ->
      Core2 = set_non_zero_ac(Core1, IR, Word),
      Core3 = sim_core:set_flags(Core2, Flags),
      skip_if_L(Core3, Mem, sext36(Word), 0);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA, Word, Flags) end)
  end.

-spec handle_AOSE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOSE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      {Word, Flags} = add1(CE),
      handle_AOSE(Core, Mem, IR, EA, Word, Flags);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_AOSE(Core, Mem, IR, EA, Word, Flags) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} ->
      Core2 = set_non_zero_ac(Core1, IR, Word),
      Core3 = sim_core:set_flags(Core2, Flags),
      skip_if_E(Core3, Mem, Word, 0);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA, Word, Flags) end)
  end.

-spec handle_AOSLE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOSLE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      {Word, Flags} = add1(CE),
      handle_AOSLE(Core, Mem, IR, EA, Word, Flags);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_AOSLE(Core, Mem, IR, EA, Word, Flags) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} ->
      Core2 = set_non_zero_ac(Core1, IR, Word),
      Core3 = sim_core:set_flags(Core2, Flags),
      skip_if_LE(Core3, Mem, sext36(Word), 0);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA, Word, Flags) end)
  end.

-spec handle_AOSA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOSA(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      {Word, Flags} = add1(CE),
      handle_AOSA(Core, Mem, IR, EA, Word, Flags);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_AOSA(Core, Mem, IR, EA, Word, Flags) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} ->
      Core2 = set_non_zero_ac(Core1, IR, Word),
      Core3 = sim_core:set_flags(Core2, Flags),
      sim_core:skip(Core3, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA, Word, Flags) end)
  end.

-spec handle_AOSGE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOSGE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      {Word, Flags} = add1(CE),
      handle_AOSGE(Core, Mem, IR, EA, Word, Flags);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_AOSGE(Core, Mem, IR, EA, Word, Flags) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} ->
      Core2 = set_non_zero_ac(Core1, IR, Word),
      Core3 = sim_core:set_flags(Core2, Flags),
      skip_if_LE(Core3, Mem, 0, sext36(Word)); % Word >= 0 -> 0 =< Word
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA, Word, Flags) end)
  end.

-spec handle_AOSN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOSN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      {Word, Flags} = add1(CE),
      handle_AOSN(Core, Mem, IR, EA, Word, Flags);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_AOSN(Core, Mem, IR, EA, Word, Flags) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} ->
      Core2 = set_non_zero_ac(Core1, IR, Word),
      Core3 = sim_core:set_flags(Core2, Flags),
      skip_if_N(Core3, Mem, Word, 0);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA, Word, Flags) end)
  end.

-spec handle_AOSG(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AOSG(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      {Word, Flags} = add1(CE),
      handle_AOSG(Core, Mem, IR, EA, Word, Flags);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_AOSG(Core, Mem, IR, EA, Word, Flags) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} ->
      Core2 = set_non_zero_ac(Core1, IR, Word),
      Core3 = sim_core:set_flags(Core2, Flags),
      skip_if_L(Core3, Mem, 0, sext36(Word)); % Word > 0 -> 0 < Word
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA, Word, Flags) end)
  end.

%% SOJ - Subtract One from AC and Jump if Condition Satisfied

-spec handle_SOJ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SOJ(Core, Mem, IR, _EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = sub1(CA),
  sim_core:next_pc(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem).

-spec handle_SOJL(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SOJL(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = sub1(CA),
  jump_if_L(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

-spec handle_SOJE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SOJE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = sub1(CA),
  jump_if_E(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

-spec handle_SOJLE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SOJLE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = sub1(CA),
  jump_if_LE(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

-spec handle_SOJA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SOJA(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = sub1(CA),
  jump(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, EA).

-spec handle_SOJGE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SOJGE(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = sub1(CA),
  jump_if_GE(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

-spec handle_SOJN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SOJN(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = sub1(CA),
  jump_if_N(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

-spec handle_SOJG(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SOJG(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = sub1(CA),
  jump_if_G(sim_core:set_flags(sim_core:set_ac(Core, AC, Word), Flags), Mem, Word, EA).

%% Miscellaneous ===============================================================

jump_if_E(Core, Mem, X, EA) ->
  case X =:= 0 of
    true -> jump(Core, Mem, EA);
    false -> sim_core:next_pc(Core, Mem)
  end.

jump_if_G(Core, Mem, X, EA) ->
  case sext36(X) > 0 of
    true -> jump(Core, Mem, EA);
    false -> sim_core:next_pc(Core, Mem)
  end.

jump_if_GE(Core, Mem, X, EA) ->
  case sext36(X) >= 0 of
    true -> jump(Core, Mem, EA);
    false -> sim_core:next_pc(Core, Mem)
  end.

jump_if_L(Core, Mem, X, EA) ->
  case sext36(X) < 0 of
    true -> jump(Core, Mem, EA);
    false -> sim_core:next_pc(Core, Mem)
  end.

jump_if_LE(Core, Mem, X, EA) ->
  case sext36(X) =< 0 of
    true -> jump(Core, Mem, EA);
    false -> sim_core:next_pc(Core, Mem)
  end.

jump_if_N(Core, Mem, X, EA) ->
  case X =/= 0 of
    true -> jump(Core, Mem, EA);
    false -> sim_core:next_pc(Core, Mem)
  end.

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

add1(Word) ->
  Add1 = (Word + 1) band ((1 bsl 36) - 1),
  Flags =
    case Add1 of
      8#400000000000 -> % (2^35 - 1) + 1
        (1 bsl ?PDP10_PF_TRAP_1) bor
        (1 bsl ?PDP10_PF_OVERFLOW) bor
        (1 bsl ?PDP10_PF_CARRY_1);
      0 -> % -1 + 1
        (1 bsl ?PDP10_PF_CARRY_0) bor (1 bsl ?PDP10_PF_CARRY_1);
      _ -> 0
    end,
  {Add1, Flags}.

sub1(Word) ->
  Sub1 = (Word - 1) band ((1 bsl 36) - 1),
  Flags =
    case Sub1 of
      8#377777777777 -> % -2^35 - 1
        (1 bsl ?PDP10_PF_TRAP_1) bor
        (1 bsl ?PDP10_PF_OVERFLOW) bor
        (1 bsl ?PDP10_PF_CARRY_0);
      8#777777777777 -> % 0 - 1
        0;
      _ -> % "any other non-zero number" - 1
        (1 bsl ?PDP10_PF_CARRY_0) bor (1 bsl ?PDP10_PF_CARRY_1)
    end,
  {Sub1, Flags}.

%% Sign-extend a uint36_t() to the full width of its representation type.
-spec sext36(uint36_t()) -> integer().
sext36(X) ->
  UInt36Sbit = 1 bsl (36 - 1),
  UInt36Max = (1 bsl 36) - 1,
  ((X band UInt36Max) bxor UInt36Sbit) - UInt36Sbit.

set_non_zero_ac(Core, IR, Word) ->
  case IR band 8#17 of
    0 -> Core;
    AC -> sim_core:set_ac(Core, AC, Word)
  end.

ea_address(#ea{section = Section, offset = Offset}) ->
  (Section bsl 18) bor Offset.
