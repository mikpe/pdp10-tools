%%% -*- erlang-indent-level: 2 -*-
%%%
%%% simulator for pdp10-elf
%%% Copyright (C) 2020-2021  Mikael Pettersson
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
%%% 2.11 Byte Manipulation

-module(sim_bytepointers).

-export([ handle_IBP/4
        ]).

-include("sim_core.hrl").

%% 2.11 Byte Manipulation ======================================================

%% IBP   - Increment Byte Pointer (A is zero)
%% ADJBP - Adjust Byte Pointer (A is non-zero)

-spec handle_IBP(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_IBP(Core, Mem, IR, EA) ->
  case sim_core:c(Core, Mem, EA) of
    {ok, BP} ->
      case IR band 8#17 of
        0 -> ibp(Core, Mem, EA, BP);
        AC -> adjbp(Core, Mem, AC, EA, BP)
      end;
    {error, Reason} ->
      sim_core:page_fault(Core, Mem, EA, read, Reason,
                          fun(Core1, Mem1) -> ?FUNCTION_NAME(Core1, Mem1, IR, EA) end)
  end.

%% Miscellaneous ===============================================================

ibp(Core, Mem, EA, BP) ->
  XXX.
