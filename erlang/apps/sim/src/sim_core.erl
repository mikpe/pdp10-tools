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
%%% Runs a loaded pdp10-elf user-mode program.

-module(sim_core).

-export([ run/6
        , format_error/1
        ]).

-include("sim_core.hrl").

%% Run a program ===============================================================

-spec run(Mem :: sim_mem:mem(),
          PC :: word(),
          SP :: word(),
          Argc :: word(),
          Argv :: word(),
          Envp :: word()) -> ok | {error, {module(), term()}}.
run(_Mem, _PC, _SP, _Argc, _Argv, _Envp) -> ok. % FIXME

%% Error Formatting ============================================================

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    _  -> [] % FIXME
  end.
