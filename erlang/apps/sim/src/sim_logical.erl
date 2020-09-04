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
%%% 2.7 Logical Testing and Modification

-module(sim_logical).

-export([ handle_TDC/4
        , handle_TDCE/4
        , handle_TDCA/4
        , handle_TDCN/4
        , handle_TDN/4
        , handle_TDNA/4
        , handle_TDNE/4
        , handle_TDNN/4
        , handle_TDO/4
        , handle_TDOA/4
        , handle_TDOE/4
        , handle_TDON/4
        , handle_TDZ/4
        , handle_TDZA/4
        , handle_TDZE/4
        , handle_TDZN/4
        , handle_TLC/4
        , handle_TLCA/4
        , handle_TLCE/4
        , handle_TLCN/4
        , handle_TLNE/4
        , handle_TLNN/4
        , handle_TLO/4
        , handle_TLOA/4
        , handle_TLOE/4
        , handle_TLON/4
        , handle_TLZ/4
        , handle_TLZA/4
        , handle_TLZE/4
        , handle_TLZN/4
        , handle_TRC/4
        , handle_TRCA/4
        , handle_TRCE/4
        , handle_TRCN/4
        , handle_TRNE/4
        , handle_TRNN/4
        , handle_TRO/4
        , handle_TROA/4
        , handle_TROE/4
        , handle_TRON/4
        , handle_TRZ/4
        , handle_TRZA/4
        , handle_TRZE/4
        , handle_TRZN/4
        , handle_TSC/4
        , handle_TSCA/4
        , handle_TSCE/4
        , handle_TSCN/4
        , handle_TSN/4
        , handle_TSNA/4
        , handle_TSNE/4
        , handle_TSNN/4
        , handle_TSO/4
        , handle_TSOA/4
        , handle_TSOE/4
        , handle_TSON/4
        , handle_TSZ/4
        , handle_TSZA/4
        , handle_TSZE/4
        , handle_TSZN/4
        ]).

-include("sim_core.hrl").

%% 2.7 Logical Testing and Modification ========================================

%% TRN - Test Right, No Modification, and Skip if Condition Satisfied

-spec handle_TRNE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRNE(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxNE(Core, Mem, IR, Mask).

-spec handle_TRNN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRNN(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxNN(Core, Mem, IR, Mask).

%% TRZ - Test Right, Zeros, and Skip if Condition Satisfied

-spec handle_TRZ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRZ(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxZ(Core, Mem, IR, Mask).

-spec handle_TRZE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRZE(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxZE(Core, Mem, IR, Mask).

-spec handle_TRZA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRZA(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxZA(Core, Mem, IR, Mask).

-spec handle_TRZN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRZN(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxZN(Core, Mem, IR, Mask).

%% TRC - Test Right, Complement, and Skip if Condition Satisfied

-spec handle_TRC(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRC(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxC(Core, Mem, IR, Mask).

-spec handle_TRCE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRCE(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxCE(Core, Mem, IR, Mask).

-spec handle_TRCA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRCA(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxCA(Core, Mem, IR, Mask).

-spec handle_TRCN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRCN(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxCN(Core, Mem, IR, Mask).

%% TRO - Test Right, Ones, and Skip if Condition Satisfied

-spec handle_TRO(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRO(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxO(Core, Mem, IR, Mask).

-spec handle_TROE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TROE(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxOE(Core, Mem, IR, Mask).

-spec handle_TROA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TROA(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxOA(Core, Mem, IR, Mask).

-spec handle_TRON(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TRON(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset,
  handle_TxON(Core, Mem, IR, Mask).

%% TLN - Test Left, No Modification, and Skip if Condition Satisfied

-spec handle_TLNE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLNE(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxNE(Core, Mem, IR, Mask).

-spec handle_TLNN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLNN(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxNN(Core, Mem, IR, Mask).

%% TLZ - Test Left, Zeros, and Skip if Condition Satisfied

-spec handle_TLZ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLZ(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxZ(Core, Mem, IR, Mask).

-spec handle_TLZE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLZE(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxZE(Core, Mem, IR, Mask).

-spec handle_TLZA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLZA(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxZA(Core, Mem, IR, Mask).

-spec handle_TLZN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLZN(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxZN(Core, Mem, IR, Mask).

%% TLC - Test Left, Complement, and Skip if Condition Satisfied

-spec handle_TLC(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLC(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxC(Core, Mem, IR, Mask).

-spec handle_TLCE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLCE(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxCE(Core, Mem, IR, Mask).

-spec handle_TLCA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLCA(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxCA(Core, Mem, IR, Mask).

-spec handle_TLCN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLCN(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxCN(Core, Mem, IR, Mask).

%% TLO - Test Left, Ones, and Skip if Condition Satisfied

-spec handle_TLO(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLO(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxO(Core, Mem, IR, Mask).

-spec handle_TLOE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLOE(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxOE(Core, Mem, IR, Mask).

-spec handle_TLOA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLOA(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxOA(Core, Mem, IR, Mask).

-spec handle_TLON(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TLON(Core, Mem, IR, EA) ->
  Mask = EA#ea.offset bsl 18,
  handle_TxON(Core, Mem, IR, Mask).

%% TDN - Test Direct, No Modification, and Skip if Condition Satisfied

-spec handle_TDN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDN(Core, Mem, IR, EA) ->
  %% this reads memory, but is otherwise a no-op
  case sim_core:c(Core, Mem, EA) of
    {ok, _Mask} -> sim_core:next_pc(Core, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDNE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDNE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxNE(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDNA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDNA(Core, Mem, IR, EA) ->
  %% this reads memory and skips, but has no other side-effect
  case sim_core:c(Core, Mem, EA) of
    {ok, _Mask} -> sim_core:skip(Core, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDNN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDNN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxNN(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% TDZ - Test Direct, Zeros, and Skip if Condition Satisfied

-spec handle_TDZ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDZ(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxZ(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDZE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDZE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxZE(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDZA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDZA(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxZA(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDZN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDZN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxZN(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% TDC - Test Direct, Complement, and Skip if Condition Satisfied

-spec handle_TDC(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDC(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxC(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDCE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDCE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxCE(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDCA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDCA(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxCA(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDCN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDCN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxCN(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% TDO - Test Direct, Ones, and Skip if Condition Satisfied

-spec handle_TDO(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDO(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxO(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDOE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDOE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxOE(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDOA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDOA(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxOA(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TDON(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TDON(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxON(Core, Mem, IR, Mask);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% TSN - Test Swapped, No Modification, and Skip if Condition Satisfied

-spec handle_TSN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSN(Core, Mem, IR, EA) ->
  %% this reads memory, but is otherwise a no-op
  case sim_core:c(Core, Mem, EA) of
    {ok, _Mask} -> sim_core:next_pc(Core, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSNE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSNE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxNE(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSNA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSNA(Core, Mem, IR, EA) ->
  %% this reads memory and skips, but has no other side-effect
  case sim_core:c(Core, Mem, EA) of
    {ok, _Mask} -> sim_core:skip(Core, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSNN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSNN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxNN(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% TSZ - Test Swapped, Zeros, and Skip if Condition Satisfied

-spec handle_TSZ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSZ(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxZ(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSZE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSZE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxZE(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSZA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSZA(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxZA(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSZN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSZN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxZN(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% TSC - Test Swapped, Complement, and Skip if Condition Satisfied

-spec handle_TSC(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSC(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxC(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSCE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSCE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxCE(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSCA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSCA(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxCA(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSCN(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSCN(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxCN(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% TSO - Test Swapped, Ones, and Skip if Condition Satisfied

-spec handle_TSO(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSO(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxO(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSOE(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSOE(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxOE(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSOA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSOA(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxOA(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

-spec handle_TSON(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_TSON(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, Mask} -> handle_TxON(Core, Mem, IR, swap_halves(Mask));
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% Miscellaneous ===============================================================

handle_TxC(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Core1 = sim_core:set_ac(Core, AC, CA bxor Mask),
  sim_core:next_pc(Core1, Mem).

handle_TxCA(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Core1 = sim_core:set_ac(Core, AC, CA bxor Mask),
  sim_core:skip(Core1, Mem).

handle_TxCE(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Core1 = sim_core:set_ac(Core, AC, CA bxor Mask),
  case CA band Mask of
    0 -> sim_core:skip(Core1, Mem);
    _ -> sim_core:next_pc(Core1, Mem)
  end.

handle_TxCN(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Core1 = sim_core:set_ac(Core, AC, CA bxor Mask),
  case CA band Mask of
    0 -> sim_core:next_pc(Core1, Mem);
    _ -> sim_core:skip(Core1, Mem)
  end.

handle_TxNE(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  case CA band Mask of
    0 -> sim_core:skip(Core, Mem);
    _ -> sim_core:next_pc(Core, Mem)
  end.

handle_TxNN(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  case CA band Mask of
    0 -> sim_core:next_pc(Core, Mem);
    _ -> sim_core:skip(Core, Mem)
  end.

handle_TxO(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Core1 = sim_core:set_ac(Core, AC, CA bor Mask),
  sim_core:next_pc(Core1, Mem).

handle_TxOA(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Core1 = sim_core:set_ac(Core, AC, CA bor Mask),
  sim_core:skip(Core1, Mem).

handle_TxOE(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Core1 = sim_core:set_ac(Core, AC, CA bor Mask),
  case CA band Mask of
    0 -> sim_core:skip(Core1, Mem);
    _ -> sim_core:next_pc(Core1, Mem)
  end.

handle_TxON(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Core1 = sim_core:set_ac(Core, AC, CA bor Mask),
  case CA band Mask of
    0 -> sim_core:next_pc(Core1, Mem);
    _ -> sim_core:skip(Core1, Mem)
  end.

handle_TxZ(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  sim_core:next_pc(sim_core:set_ac(Core, AC, CA band bnot Mask), Mem).

handle_TxZA(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  sim_core:skip(sim_core:set_ac(Core, AC, CA band bnot Mask), Mem).

handle_TxZE(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  case CA band Mask of
    0 -> sim_core:skip(Core, Mem); % masked bits already zero
    _ -> sim_core:next_pc(sim_core:set_ac(Core, AC, CA band bnot Mask), Mem)
  end.

handle_TxZN(Core, Mem, IR, Mask) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  case CA band Mask of
    0 -> sim_core:next_pc(Core, Mem); % masked bits already zero
    _ -> sim_core:skip(sim_core:set_ac(Core, AC, CA band bnot Mask), Mem)
  end.

swap_halves(Word) ->
  Low18Mask = ((1 bsl 18) - 1),
  ((Word band Low18Mask) bsl 18) bor ((Word bsr 18) band Low18Mask).
