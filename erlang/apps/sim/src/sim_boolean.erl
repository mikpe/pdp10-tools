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
%%% 2.4 Boolean Functions

-module(sim_boolean).

-export([ handle_AND/4
        , handle_ANDB/4
        , handle_ANDCA/4
        , handle_ANDCAB/4
        , handle_ANDCAI/4
        , handle_ANDCAM/4
        , handle_ANDCB/4
        , handle_ANDCBB/4
        , handle_ANDCBI/4
        , handle_ANDCBM/4
        , handle_ANDCM/4
        , handle_ANDCMB/4
        , handle_ANDCMI/4
        , handle_ANDCMM/4
        , handle_ANDI/4
        , handle_ANDM/4
        , handle_EQV/4
        , handle_EQVB/4
        , handle_EQVI/4
        , handle_EQVM/4
        , handle_IOR/4
        , handle_IORB/4
        , handle_IORI/4
        , handle_IORM/4
        , handle_ORCA/4
        , handle_ORCAB/4
        , handle_ORCAI/4
        , handle_ORCAM/4
        , handle_ORCM/4
        , handle_ORCMI/4
        , handle_SETCA/4
        , handle_SETCAB/4
        , handle_SETCAM/4
        , handle_SETCM/4
        , handle_SETCMB/4
        , handle_SETCMI/4
        , handle_SETCMM/4
        , handle_SETMB/4
        , handle_SETMI/4
        , handle_SETMM/4
        , handle_SETZ/4
        , handle_SETZB/4
        , handle_SETZM/4
        , handle_XOR/4
        , handle_XORB/4
        , handle_XORI/4
        , handle_XORM/4
        ]).

-include("sim_core.hrl").

%% 2.4 Boolean Functions =======================================================

%% SETZ - Set to Zeros

-spec handle_SETZ(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETZ(Core, Mem, IR, _EA) ->
  AC = IR band 8#17,
  sim_core:next_pc(sim_core:set_ac(Core, AC, 0), Mem).

-spec handle_SETZM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETZM(Core, Mem, IR, EA) ->
  case sim_core:cset(Core, Mem, EA, 0) of
    {ok, Core1} -> sim_core:next_pc(Core1, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> handle_SETZM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SETZB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETZB(Core, Mem, IR, EA) ->
  case sim_core:cset(Core, Mem, EA, 0) of
    {ok, Core1} ->
      AC = IR band 8#17,
      sim_core:next_pc(sim_core:set_ac(Core1, AC, 0), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> handle_SETZB(Core1, Mem1, IR, EA) end)
  end.

%% AND - And with AC

-spec handle_AND(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_AND(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE band CA,
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_AND(Core1, Mem1, IR, EA) end)
  end.

-spec handle_ANDI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = CA band EA#ea.offset,
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_ANDM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE band CA,
      handle_ANDM_1(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDM(Core1, Mem1, IR, EA) end)
  end.

handle_ANDM_1(Core, Mem, EA, Word) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} -> sim_core:next_pc(Core1, Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> handle_ANDM_1(Core1, Mem1, EA, Word) end)
  end.

-spec handle_ANDB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDB(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE band CA,
      handle_ANDB(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDB(Core1, Mem1, IR, EA) end)
  end.

handle_ANDB(Core, Mem, AC, EA, Word) ->
  case sim_core:cset(Core, Mem, EA, Word) of
    {ok, Core1} -> sim_core:next_pc(sim_core:set_ac(Core1, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), write, Reason,
                          fun(Core1, Mem1) -> handle_ANDB(Core1, Mem1, AC, EA, Word) end)
  end.

%% ANDCA - And with Complement of AC

-spec handle_ANDCA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCA(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE band bnot CA,
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDCA(Core1, Mem1, IR, EA) end)
  end.

-spec handle_ANDCAI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCAI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = EA#ea.offset band bnot CA,
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_ANDCAM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCAM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE band bnot CA,
      handle_ANDM_1(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDCAM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_ANDCAB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCAB(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE band bnot CA,
      handle_ANDB(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDCAB(Core1, Mem1, IR, EA) end)
  end.

%% SETM - Set to Memory

-spec handle_SETMI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETMI(Core, Mem, IR, #ea{section = Section0, offset = Offset, islocal = IsLocal}) ->
  AC = IR band 8#17,
  %% Behave as MOVEI in section 0, and as XMOVEI (2.1.3) in non-zero sections.
  Section1 =
    if IsLocal, Offset =< 8#17, Section0 > 1 -> 1; % change local AC address to global one
       true -> Section0
    end,
  Word = (Section1 bsl 18) bor Offset,
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_SETMM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETMM(Core, Mem, IR, EA) ->
  %% SETMM only checks that E is readable and writable.
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      handle_ANDM_1(Core, Mem, EA, CE);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_SETMM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SETMB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETMB(Core, Mem, IR, EA) ->
  %% Like MOVES, but writes also to AC 0.
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      handle_ANDB(Core, Mem, AC, EA, CE);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_SETMB(Core1, Mem1, IR, EA) end)
  end.

%% ANDCM - And Complement of Memory with AC

-spec handle_ANDCM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CA band bnot CE,
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDCM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_ANDCMI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCMI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = CA band bnot EA#ea.offset,
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_ANDCMM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCMM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CA band bnot CE,
      handle_ANDM_1(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDCMM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_ANDCMB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCMB(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CA band bnot CE,
      handle_ANDB(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDCMB(Core1, Mem1, IR, EA) end)
  end.

%% XOR - Exclusive Or with AC

-spec handle_XOR(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_XOR(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE bxor CA,
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_XOR(Core1, Mem1, IR, EA) end)
  end.

-spec handle_XORI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_XORI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = CA bxor EA#ea.offset,
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_XORM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_XORM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE bxor CA,
      handle_ANDM_1(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_XORM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_XORB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_XORB(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE bxor CA,
      handle_ANDB(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_XORB(Core1, Mem1, IR, EA) end)
  end.

%% IOR - Inclusive Or with AC

-spec handle_IOR(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_IOR(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE bor CA,
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_IOR(Core1, Mem1, IR, EA) end)
  end.

-spec handle_IORI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_IORI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = CA bor EA#ea.offset,
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_IORM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_IORM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE bor CA,
      handle_ANDM_1(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_IORM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_IORB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_IORB(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = CE bor CA,
      handle_ANDB(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_IORB(Core1, Mem1, IR, EA) end)
  end.

%% ANDCB - And Complements of Both

-spec handle_ANDCB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCB(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = ((bnot CE) band (bnot CA)) band ((1 bsl 36) - 1),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDCA(Core1, Mem1, IR, EA) end)
  end.

-spec handle_ANDCBI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCBI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = ((bnot CA) band (bnot EA#ea.offset)) band ((1 bsl 36) - 1),
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_ANDCBM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCBM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = ((bnot CE) band (bnot CA)) band ((1 bsl 36) - 1),
      handle_ANDM_1(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDCBM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_ANDCBB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ANDCBB(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = ((bnot CE) band (bnot CA)) band ((1 bsl 36) - 1),
      handle_ANDB(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ANDCBB(Core1, Mem1, IR, EA) end)
  end.

%% EQV - Equivalence with AC

-spec handle_EQV(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_EQV(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = (bnot (CE bxor CA)) band ((1 bsl 36) - 1),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_EQV(Core1, Mem1, IR, EA) end)
  end.

-spec handle_EQVI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_EQVI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = (bnot (CA bxor EA#ea.offset)) band ((1 bsl 36) - 1),
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_EQVM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_EQVM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = (bnot (CE bxor CA)) band ((1 bsl 36) - 1),
      handle_ANDM_1(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_EQVM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_EQVB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_EQVB(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = (bnot (CE bxor CA)) band ((1 bsl 36) - 1),
      handle_ANDB(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_EQVB(Core1, Mem1, IR, EA) end)
  end.

%% SETCA - Set to Complement of AC

-spec handle_SETCA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETCA(Core, Mem, IR, _EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = (bnot CA) band ((1 bsl 36) - 1),
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_SETCAM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETCAM(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = (bnot CA) band ((1 bsl 36) - 1),
  handle_ANDM_1(Core, Mem, EA, Word).

-spec handle_SETCAB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETCAB(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = (bnot CA) band ((1 bsl 36) - 1),
  handle_ANDB(Core, Mem, AC, EA, Word).

%% ORCA - Inclusive Or with Complement of AC

-spec handle_ORCA(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ORCA(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = (CE bor bnot CA) band ((1 bsl 36) - 1),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ORCA(Core1, Mem1, IR, EA) end)
  end.

-spec handle_ORCAI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ORCAI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = (EA#ea.offset bor bnot CA) band ((1 bsl 36) - 1),
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_ORCAM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ORCAM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = (CE bor bnot CA) band ((1 bsl 36) - 1),
      handle_ANDM_1(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ORCAM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_ORCAB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ORCAB(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = (CE bor bnot CA) band ((1 bsl 36) - 1),
      handle_ANDB(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ORCAB(Core1, Mem1, IR, EA) end)
  end.

%% SETCM - Set to Complement of Memory

-spec handle_SETCM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETCM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = (bnot CE) band ((1 bsl 36) - 1),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_SETCM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SETCMI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETCMI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  Word = (bnot EA#ea.offset) band ((1 bsl 36) - 1),
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

-spec handle_SETCMM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETCMM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      Word = (bnot CE) band ((1 bsl 36) - 1),
      handle_ANDM_1(Core, Mem, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_SETCMM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_SETCMB(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_SETCMB(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      Word = (bnot CE) band ((1 bsl 36) - 1),
      handle_ANDB(Core, Mem, AC, EA, Word);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_SETCMB(Core1, Mem1, IR, EA) end)
  end.

%% ORCM - Inclusive Or Complement or Memory with AC

-spec handle_ORCM(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ORCM(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, CE} ->
      AC = IR band 8#17,
      CA = sim_core:get_ac(Core, AC),
      Word = (CA bor bnot CE) band ((1 bsl 36) - 1),
      sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem);
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, ea_address(EA), read, Reason,
                          fun(Core1, Mem1) -> handle_ORCM(Core1, Mem1, IR, EA) end)
  end.

-spec handle_ORCMI(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ORCMI(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = (CA bor bnot EA#ea.offset) band ((1 bsl 36) - 1),
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

%% Miscellaneous ===============================================================

ea_address(#ea{section = Section, offset = Offset}) ->
  (Section bsl 18) bor Offset.
