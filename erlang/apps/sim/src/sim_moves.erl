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

-export([ handle_BLT/4
        , handle_EXCH/4
        , handle_DMOVE/4
        , handle_DMOVEM/4
        , handle_DMOVN/4
        , handle_DMOVNM/4
        , handle_MOVE/4
        , handle_MOVEI/4
        , handle_MOVEM/4
        , handle_MOVES/4
        , handle_MOVM/4
        , handle_MOVMM/4
        , handle_MOVMS/4
        , handle_MOVN/4
        , handle_MOVNI/4
        , handle_MOVNM/4
        , handle_MOVNS/4
        , handle_MOVS/4
        , handle_MOVSI/4
        , handle_MOVSM/4
        , handle_MOVSS/4
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
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_EXCH(Core, Mem, AC, EA, CE, CA) ->
  case sim_core:cset(Core, Mem, EA, CA) of
    {ok, Core1} -> sim_core:next_pc(sim_core:set_ac(Core1, AC, CE), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, AC, EA, CE, CA) end)
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
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
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
  handle_writeback(Core, Mem, EA, CA).

-spec handle_MOVES(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVES(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      handle_MOVES(Core, Mem, AC, EA, CE);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_MOVES(Core, Mem, AC, EA, Word) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} -> sim_core:next_pc(set_non_zero_ac(Core1, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, AC, EA, Word) end)
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
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_MOVSI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVSI(Core, Mem, IR, #ea{offset = E}) ->
  AC = IR band 8#17,
  sim_core:next_pc(sim_core:set_ac(Core, AC, E bsl 18), Mem).

-spec handle_MOVSM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVSM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  handle_writeback(Core, Mem, EA, swap_halves(CA)).

-spec handle_MOVSS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVSS(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      handle_MOVES(Core, Mem, AC, EA, swap_halves(CE));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% MOVN - Move Negative

-spec handle_MOVN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      {Negative, Flags} = negate(CE),
      AC = IR band 8#17,
      sim_core:next_pc(sim_core:set_ac(sim_core:set_flags(Core, Flags), AC, Negative), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_MOVNI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVNI(Core, Mem, IR, #ea{offset = E}) ->
  AC = IR band 8#17,
  sim_core:next_pc(sim_core:set_ac(Core, AC, (-E) band ((1 bsl 36) - 1)), Mem).

-spec handle_MOVNM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVNM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Negated, Flags} = negate(CA),
  handle_MOVNM(Core, Mem, Negated, Flags, EA).

handle_MOVNM(Core, Mem, Word, Flags, EA) ->
  %% "2.9.6.1 Overflow Trapping in the KL10, KS10, and KI10 Processors" and
  %% "2.9.6.2 Overflow Trapping in the XKL-1 Processor" both state that if
  %% an instruction causes both overflow and a page failure, the page failure
  %% is handled first, and the overflow trap is handled after the instruction
  %% has restarted and completed.
  %% Therefore complete the store before updating flags and proceeding.
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} -> sim_core:next_pc(sim_core:set_flags(Core1, Flags), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, Word, Flags, EA) end)
  end.

-spec handle_MOVNS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVNS(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      {Negated, Flags} = negate(CE),
      handle_MOVNS(Core, Mem, AC, EA, Negated, Flags);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_MOVNS(Core, Mem, AC, EA, Word, Flags) ->
  %% See handle_MOVNM/5 for ordering requirements.
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} ->
      Core2 = set_non_zero_ac(Core1, AC, Word),
      Core3 = sim_core:set_flags(Core2, Flags),
      sim_core:next_pc(Core3, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, AC, EA, Word, Flags) end)
  end.

%% MOVM - Move Magnitude

-spec handle_MOVM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      {Magnitude, Flags} = magnitude(CE),
      AC = IR band 8#17,
      sim_core:next_pc(sim_core:set_ac(sim_core:set_flags(Core, Flags), AC, Magnitude), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_MOVMM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVMM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Magnitude, Flags} = magnitude(CA),
  handle_MOVNM(Core, Mem, Magnitude, Flags, EA).

-spec handle_MOVMS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_MOVMS(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      {Magnitude, Flags} = magnitude(CE),
      handle_MOVNS(Core, Mem, AC, EA, Magnitude, Flags);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% 2.1.4 Double Move Instructions ==============================================

%% DMOVE - Double Move

-spec handle_DMOVE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_DMOVE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Word0} ->
      handle_DMOVE(Core, Mem, IR, ea_plus_1(EA), Word0);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_DMOVE(Core, Mem, IR, EA, Word0) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Word1} ->
      AC = IR band 8#17,
      Core1 = sim_core:set_ac(Core, AC, Word0),
      Core2 = sim_core:set_ac(Core1, ac_plus_1(AC), Word1),
      sim_core:next_pc(Core2, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA, Word0) end)
  end.

%% DMOVEM - Double Move to Memory

-spec handle_DMOVEM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_DMOVEM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  Word0 = sim_core:get_ac(Core, AC),
  Word1 = sim_core:get_ac(Core, ac_plus_1(AC)),
  handle_DMOVEM(Core, Mem, Word0, Word1, EA).

handle_DMOVEM(Core, Mem, Word0, Word1, EA) ->
  case sim_core:cset(Core, Mem, EA, Word0) of
    {ok, Core1} -> handle_writeback(Core1, Mem, ea_plus_1(EA), Word1);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, Word0, Word1, EA) end)
  end.

%% DMOVN - Double Move Negative

-spec handle_DMOVN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_DMOVN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Word0} ->
      handle_DMOVN(Core, Mem, IR, ea_plus_1(EA), Word0);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

handle_DMOVN(Core, Mem, IR, EA, Word0) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Word1} ->
      {Negative0, Negative1, Flags} = dnegate(Word0, Word1),
      AC = IR band 8#17,
      Core1 = sim_core:set_ac(Core, AC, Negative0),
      Core2 = sim_core:set_ac(Core1, ac_plus_1(AC), Negative1),
      Core3 = sim_core:set_flags(Core2, Flags),
      sim_core:next_pc(Core3, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA, Word0) end)
  end.

%% DMOVNM - Double Move Negative to Memory

-spec handle_DMOVNM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_DMOVNM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  Word0 = sim_core:get_ac(Core, AC),
  Word1 = sim_core:get_ac(Core, ac_plus_1(AC)),
  {Negative0, Negative1, Flags} = dnegate(Word0, Word1),
  handle_DMOVNM(Core, Mem, Negative0, Negative1, Flags, EA).

handle_DMOVNM(Core, Mem, Word0, Word1, Flags, EA) ->
  case sim_core:cset(Core, Mem, EA, Word0) of
    {ok, Core1} -> handle_MOVNM(Core1, Mem, Word1, Flags, ea_plus_1(EA));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, Word0, Word1, Flags, EA) end)
  end.

%% 2.1.5 Block Transfers =======================================================

%% BLT - Block Transfer

-spec handle_BLT(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_BLT(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  SrcOffset = CA bsr 18,
  DstOffset = CA band ((1 bsl 18) - 1),
  handle_BLT(Core, Mem, AC, EA, SrcOffset, DstOffset).

handle_BLT(Core, Mem, AC, EA, SrcOffset, DstOffset) ->
  SrcEA = EA#ea{offset = SrcOffset},
  case sim_core:c(Core, Mem, SrcEA) of
    {ok, Word} ->
      DstEA = EA#ea{offset = DstOffset},
      case sim_core:cset(Core, Mem, DstEA, Word) of
        {ok, Core1} ->
          SrcOffset1 = (SrcOffset + 1) band ((1 bsl 18) - 1),
          DstOffset1 = (DstOffset + 1) band ((1 bsl 18) - 1),
          case DstOffset >= EA#ea.offset of
            true ->
              Core2 =
                case ea_is_ac(DstEA, AC) of
                  true -> Core1;
                  false -> handle_BLT_flush(Core, AC, SrcOffset1, DstOffset1)
                end,
              sim_core:next_pc(Core2, Mem);
            false -> handle_BLT(Core1, Mem, AC, EA, SrcOffset1, DstOffset1)
          end;
        {error, Reason} ->
          handle_BLT_fault(Core, Mem, DstEA, write, Reason, AC, SrcOffset, DstOffset)
      end;
    {error, Reason} ->
      handle_BLT_fault(Core, Mem, SrcEA, read, Reason, AC, SrcOffset, DstOffset)
  end.

handle_BLT_fault(Core, Mem, EA, Op, Reason, AC, SrcOffset, DstOffset) ->
  %% Following the "Caution" section in the documentation for BLT, this
  %% flushes the internal source and destination offsets to AC, and then
  %% arranges to resume the BLT at the instruction fetch stage instead of
  %% from an internal stage.
  Core1 = handle_BLT_flush(Core, AC, SrcOffset, DstOffset),
  sim_core:page_fault(Core1, Mem, EA, Op, Reason,
                      fun sim_core:run/2).

handle_BLT_flush(Core, AC, SrcOffset, DstOffset) ->
  sim_core:set_ac(Core, AC, (SrcOffset bsl 18) bor DstOffset).

%% Miscellaneous ===============================================================

handle_writeback(Core, Mem, EA, Word) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} -> sim_core:next_pc(Core1, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, write, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, EA, Word) end)
  end.

ac_plus_1(AC) ->
  (AC + 1) band 8#17.

ea_is_ac(#ea{section = Section, offset = Offset, islocal = IsLocal}, AC) ->
  (Offset =:= AC) andalso (IsLocal orelse Section =< 1).

ea_plus_1(#ea{offset = Offset, islocal = true} = EA) ->
  EA#ea{offset = (Offset + 1) band ((1 bsl 18) - 1)};
ea_plus_1(#ea{section = Section, offset = ((1 bsl 18) - 1)} = EA) ->
  EA#ea{section = (Section + 1) band ((1 bsl 12) - 1), offset = 0};
ea_plus_1(#ea{offset = Offset} = EA) ->
  EA#ea{offset = Offset + 1}.

magnitude(Word) ->
  case Word band (1 bsl 35) of
    0 -> % non-negative
      {_Magnitude = Word, _Flags = 0};
    _ -> % negative
      case (-Word) band ((1 bsl 36) - 1) of
        Word -> % magnitude of -2^35
          Flags = (1 bsl ?PDP10_PF_TRAP_1) bor
                  (1 bsl ?PDP10_PF_OVERFLOW) bor
                  (1 bsl ?PDP10_PF_CARRY_1),
          {_Magnitude = Word, Flags};
        Negated ->
          {_Magnitude = Negated, _Flags = 0}
      end
  end.

negate(Word) ->
  case (-Word) band ((1 bsl 36) - 1) of
    0 -> % negating 0
      Flags = (1 bsl ?PDP10_PF_CARRY_1) bor (1 bsl ?PDP10_PF_CARRY_0),
      {0, Flags};
    Word -> % negating -2^35
      Flags = (1 bsl ?PDP10_PF_TRAP_1) bor
              (1 bsl ?PDP10_PF_OVERFLOW) bor
              (1 bsl ?PDP10_PF_CARRY_1),
      {Word, Flags};
    Negated ->
      {Negated, _Flags = 0}
  end.

dnegate(Word0, Word1) ->
  DWord = (Word0 bsl 35) bor (Word1 band ((1 bsl 35) - 1)),
  Negated = (-DWord) band ((1 bsl 71) - 1),
  Flags =
    case Negated of
      0 -> % negating 0
        (1 bsl ?PDP10_PF_CARRY_1) bor (1 bsl ?PDP10_PF_CARRY_0);
      DWord -> % negating -2^70
        (1 bsl ?PDP10_PF_TRAP_1) bor
        (1 bsl ?PDP10_PF_OVERFLOW) bor
        (1 bsl ?PDP10_PF_CARRY_1);
      _ ->
        0
    end,
  Negated1 = Negated band ((1 bsl 35) - 1),
  Negated0 = Negated bsr 35,
  {Negated0, Negated1, Flags}.

set_non_zero_ac(Core, _AC = 0, _Word) -> Core;
set_non_zero_ac(Core, AC, Word) -> sim_core:set_ac(Core, AC, Word).

swap_halves(Word) ->
  Low18Mask = ((1 bsl 18) - 1),
  ((Word band Low18Mask) bsl 18) bor ((Word bsr 18) band Low18Mask).
