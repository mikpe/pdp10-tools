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

%% Miscellaneous ===============================================================

jump(Core, Mem, EA) ->
  sim_core:run(Core#core{pc_offset = EA#ea.offset}, Mem).
