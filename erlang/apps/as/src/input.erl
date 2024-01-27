%%% -*- erlang-indent-level: 2 -*-
%%%
%%% input processing phase for pdp10-elf as
%%% Copyright (C) 2013-2024  Mikael Pettersson
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

-module(input).

-export([ files/1
        ]).

-include("tunit.hrl").

-spec files([string()]) -> {ok, #tunit{}} | {error, {module(), term()}}.
files(Files) ->
  case input_pass1:pass1(fixup_files(Files)) of
    {ok, SectionsMap} -> input_pass2:pass2(SectionsMap);
    {error, _Reason} = Error -> Error
  end.

%% Internal --------------------------------------------------------------------

fixup_files([]) -> ["--"]; % alias for stdin
fixup_files(Files) -> Files.
