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
        , handle_ANDI/4
        , handle_ANDM/4
        , handle_SETZ/4
        , handle_SETZB/4
        , handle_SETZM/4
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

%% Miscellaneous ===============================================================

ea_address(#ea{section = Section, offset = Offset}) ->
  (Section bsl 18) bor Offset.
