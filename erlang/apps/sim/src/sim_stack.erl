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
%%% 2.10 Stack Operations

-module(sim_stack).

-export([ handle_ADJSP/4
        , handle_POP/4
        , handle_POPJ/4
        , handle_PUSH/4
        , handle_PUSHJ/4
        ]).

-include("sim_core.hrl").

%% 2.10 Stack Operations =======================================================

%% PUSH - Push

-spec handle_PUSH(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_PUSH(Core, Mem, IR, SrcEA) ->
  case sim_core:c(Core, Mem, SrcEA) of
    {ok, Word} ->
      AC = IR band 8#17,
      SP0 = sim_core:get_ac(Core, AC),
      {SP1, DstEA, Flags} = inc_sp(Core, SP0),
      handle_writeback(Core, Mem, AC, DstEA, SP1, Flags, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(SrcEA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, SrcEA) end)
  end.

%% POP - Pop

-spec handle_POP(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_POP(Core, Mem, IR, DstEA) ->
  AC = IR band 8#17,
  SP0 = sim_core:get_ac(Core, AC),
  {SP1, SrcEA, Flags} = dec_sp(Core, SP0),
  handle_POP(Core, Mem, AC, DstEA, SP1, Flags, SrcEA).

handle_POP(Core, Mem, AC, DstEA, SP, Flags, SrcEA) ->
  case sim_core:c(Core, Mem, SrcEA) of
    {ok, Word} -> handle_writeback(Core, Mem, AC, DstEA, SP, Flags, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(SrcEA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, AC, DstEA, SP, Flags, SrcEA) end)
  end.

%% PUSHJ - Push and Jump

-spec handle_PUSHJ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_PUSHJ(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  SP0 = sim_core:get_ac(Core, AC),
  {SP1, DstEA, Flags} = inc_sp(Core, SP0),
  PCWordRight = (Core#core.pc_offset + 1) band ((1 bsl 18) - 1),
  PCWordLeft =
    case Core#core.pc_section of
      0 -> Core#core.flags bsl 5;
      PCSection -> PCSection
    end,
  PCWord = (PCWordLeft bsl 18) bor PCWordRight,
  handle_PUSHJ(Core, Mem, AC, EA, SP1, DstEA, Flags, PCWord).

handle_PUSHJ(Core, Mem, AC, EA, SP, DstEA, Flags, PCWord) ->
  case sim_core:cset(Core, Mem, DstEA, PCWord) of
    {ok, Core1} ->
      handle_jump(Core1, Mem, AC, SP, Flags, EA#ea.section, EA#ea.offset);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(DstEA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, AC, EA, SP, DstEA, Flags, PCWord) end)
  end.

%% POPJ - Pop and Jump

-spec handle_POPJ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_POPJ(Core, Mem, IR, _EA) ->
  AC = IR band 8#17,
  SP0 = sim_core:get_ac(Core, AC),
  {SP1, SrcEA, Flags} = dec_sp(Core, SP0),
  handle_POPJ(Core, Mem, AC, SP1, Flags, SrcEA).

handle_POPJ(Core, Mem, AC, SP, Flags, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, PCWord} ->
       PCOffset = PCWord band ((1 bsl 18) - 1),
       PCSection =
         case Core#core.pc_section of
           0 -> 0;
           _ -> (PCWord bsr 18) band ((1 bsl 12) - 1)
         end,
       handle_jump(Core, Mem, AC, SP, Flags, PCSection, PCOffset);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, AC, SP, Flags, EA) end)
  end.

%% ADJSP - Adjust Stack Pointer

-spec handle_ADJSP(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ADJSP(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  SP0 = sim_core:get_ac(Core, AC),
  Offset = sext18(EA#ea.offset),
  {SP1, Flags} = adj_sp(Core, SP0, Offset),
  sim_core:next_pc(sim_core:set_flags(sim_core:set_ac(Core, AC, SP1), Flags), Mem).

%% Miscellaneous ===============================================================

handle_writeback(Core, Mem, AC, EA, SP, Flags, Word) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} ->
      Core2 = sim_core:set_ac(Core1, AC, SP),
      Core3 = sim_core:set_flags(Core2, Flags),
      sim_core:next_pc(Core3, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, AC, EA, SP, Flags, Word) end)
  end.

handle_jump(Core0, Mem, AC, SP, Flags, PCSection, PCOffset) ->
  Core1 = sim_core:set_ac(Core0, AC, SP),
  Core2 = sim_core:set_flags(Core1, Flags),
  Core = Core2#core{pc_section = PCSection, pc_offset = PCOffset},
  sim_core:run(Core, Mem).

adj_sp(Core, SP, Offset) ->
  case is_local_sp(Core, SP) of
    true -> local_adj_sp(SP, Offset);
    false -> global_adj_sp(SP, Offset)
  end.

dec_sp(Core, SP) ->
  case is_local_sp(Core, SP) of
    true -> local_dec_sp(Core, SP);
    false -> global_dec_sp(SP)
  end.

inc_sp(Core, SP) ->
  case is_local_sp(Core, SP) of
    true -> local_inc_sp(Core, SP);
    false -> global_inc_sp(SP)
  end.

is_local_sp(Core, SP) ->
  case Core#core.pc_section of
    0 -> true;
    _ ->
      Left = SP bsr 18,
      case Left band (1 bsl (18 - 1)) of
        0 -> (Left band 8#007777) =:= 0;
        _ -> true % negative
      end
  end.

local_adj_sp(SP0, Offset) ->
  Left0 = sext18(SP0 bsr 18),
  Right0 = SP0 band ((1 bsl 18) - 1),
  Left1 = Left0 + Offset,
  Right1 = (Right0 + Offset) band ((1 bsl 18) - 1),
  Trap =
    case Offset < 0 of
      true -> Left0 >= 0 andalso Left1 < 0;
      false -> Left0 < 0 andalso Left1 >= 0
    end,
  Flags = case Trap of true -> 1 bsl ?PDP10_PF_TRAP_2; false -> 0 end,
  SP1 = ((Left1 band ((1 bsl 18) - 1)) bsl 18) bor Right1,
  {SP1, Flags}.

local_dec_sp(Core, SP0) ->
  %% Note: KA10 incorrectly does a 36-bit sub with 8#000001000001 here instead
  %% of adjusting the two halves separately.
  Left0 = SP0 bsr 18,
  Right0 = SP0 band ((1 bsl 18) - 1),
  Left1 = (Left0 - 1) band ((1 bsl 18) - 1),
  Right1 = (Right0 - 1) band ((1 bsl 18) - 1),
  SP1 = (Left1 bsl 18) bor Right1,
  %% the resulting SrcEA is pre-dec
  EA = #ea{section = Core#core.pc_section, offset = Right0, islocal = true},
  %% trap is pre-dec count (left) is zero
  Flags =
    case Left0 =:= 0 of
      true -> 1 bsl ?PDP10_PF_TRAP_2;
      false -> 0
    end,
  {SP1, EA, Flags}.

local_inc_sp(Core, SP0) ->
  %% Note: KA10 incorrectly does a 36-bit add with 8#000001000001 here instead
  %% of adjusting the two halves separately.
  Left0 = SP0 bsr 18,
  Right0 = SP0 band ((1 bsl 18) - 1),
  Left1 = (Left0 + 1) band ((1 bsl 18) - 1),
  Right1 = (Right0 + 1) band ((1 bsl 18) - 1),
  SP1 = (Left1 bsl 18) bor Right1,
  %% the resulting DstEA is post-inc
  EA = #ea{section = Core#core.pc_section, offset = Right1, islocal = true},
  %% trap if post-inc count (left) is zero
  Flags =
    case Left1 =:= 0 of
      true -> 1 bsl ?PDP10_PF_TRAP_2;
      false -> 0
    end,
  {SP1, EA, Flags}.

global_adj_sp(SP0, Offset) ->
  E0 = SP0 band ((1 bsl 30) - 1),
  E1 = (E0 + Offset) band ((1 bsl 30) - 1),
  SP1 = (SP0 band (((1 bsl 6) - 1) bsl 30)) bor E1,
  {SP1, _Flags = 0}.

global_dec_sp(SP0) ->
  E0 = SP0 band ((1 bsl 30) - 1),
  E1 = (E0 - 1) band ((1 bsl 30) - 1),
  SP1 = (SP0 band (((1 bsl 6) - 1) bsl 30)) bor E1,
  %% the resulting SrcEA is pre-dec
  EA = #ea{section = E0 bsr 18, offset = E0 band ((1 bsl 18) - 1), islocal = false},
  {SP1, EA, _Flags = 0}.

global_inc_sp(SP0) ->
  E0 = SP0 band ((1 bsl 30) - 1),
  E1 = (E0 + 1) band ((1 bsl 30) - 1),
  SP1 = (SP0 band (((1 bsl 6) - 1) bsl 30)) bor E1,
  %% the resulting DstEA is post-inc
  EA = #ea{section = E1 bsr 18, offset = E1 band ((1 bsl 18) - 1), islocal = false},
  {SP1, EA, _Flags = 0}.

sext18(Half) ->
  UInt18Sbit = 1 bsl (18 - 1),
  (Half bxor UInt18Sbit) - UInt18Sbit.

ea_address(#ea{section = Section, offset = Offset}) ->
  (Section bsl 18) bor Offset.
