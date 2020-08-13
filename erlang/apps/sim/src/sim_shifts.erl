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
%%% 2.5 Shift and Rotate

-module(sim_shifts).

-export([ handle_LSH/4
        , handle_LSHC/4
        ]).

-include("sim_core.hrl").

%% 2.5 Shift and Rotate ========================================================

%% LSH - Logical Shift

-spec handle_LSH(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_LSH(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = lsh(CA, EA#ea.offset),
  set_ac_next_pc(Core, Mem, AC, Word).

%% LSHC - Logical Shift Combined

-spec handle_LSHC(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_LSHC(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA0 = sim_core:get_ac(Core, AC),
  CA1 = sim_core:get_ac(Core, (AC + 1) band 8#17),
  {Word0, Word1} = lshc(CA0, CA1, EA#ea.offset),
  set_acs_next_pc(Core, Mem, AC, Word0, Word1).

%% Miscellaneous ===============================================================

lsh(CA, Offset) ->
  case Offset band (1 bsl 17) of
    0 -> % left shift
      Count = Offset band ((1 bsl 8) - 1),
      if Count >= 36 -> 0;
         true -> (CA band ((1 bsl (36 - Count)) - 1)) bsl Count
      end;
    _ -> % right shift
      Count = (-Offset) band ((1 bsl 8) - 1),
      if Count >= 36 -> 0;
         true -> CA bsr Count
      end
  end.

lshc(CA0, CA1, Offset) ->
  case Offset band (1 bsl 17) of
    0 -> % left shift
      Count = Offset band ((1 bsl 8) - 1),
      if Count >= 72 ->
           {_Word0 = 0, _Word1 = 0};
         Count >= 36 ->
           Count1 = Count - 36,
           Word0 = (CA1 band ((1 bsl (36 - Count1)) - 1)) bsl Count1,
           {Word0, _Word1 = 0};
         true ->
           Word0 = ((CA0 band ((1 bsl (36 - Count)) - 1)) bsl Count) bor (CA1 bsr (36 - Count)),
           Word1 = ((CA1 band ((1 bsl (36 - Count)) - 1)) bsl Count),
           {Word0, Word1}
      end;
    _ -> % right shift
      Count = (-Offset) band ((1 bsl 8) - 1),
      if Count >= 72 ->
           {_Word0 = 0, _Word1 = 0};
         Count >= 36 ->
           Word1 = CA0 bsr (Count - 36),
           {_Word0 = 0, Word1};
         true ->
           Word0 = CA0 bsr Count,
           Word1 = ((CA0 band ((1 bsl Count) - 1)) bsl (36 - Count)) bor (CA1 bsr Count),
           {Word0, Word1}
      end
  end.

set_acs_next_pc(Core, Mem, AC, Word0, Word1) ->
  set_ac_next_pc(sim_core:set_ac(Core, AC, Word0), Mem, (AC + 1) band 8#17, Word1).

set_ac_next_pc(Core, Mem, AC, Word) ->
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).
