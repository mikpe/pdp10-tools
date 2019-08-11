%%% -*- erlang-indent-level: 2 -*-
%%%
%%% translation unit handling for pdp10-elf as.
%%% Copyright (C) 2013-2019  Mikael Pettersson
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

-module(tunit).

-export([ new/0
        , get_section/2
        , put_section/2
        , get_symbol/2
        , put_symbol/2
        ]).

-include("tunit.hrl").

%% API -------------------------------------------------------------------------

new() ->
  #tunit{ sections = #{}
        , cursect = false
        , symbols = #{}
        }.

get_section(#tunit{sections = Sections}, Name) ->
  maps:get(Name, Sections, false).

put_section(Tunit = #tunit{sections = Sections}, Section) ->
  Tunit#tunit{sections = maps:put(Section#section.name, Section, Sections)}.

get_symbol(#tunit{symbols = Symbols}, Name) ->
  maps:get(Name, Symbols, false).

put_symbol(Tunit = #tunit{symbols = Symbols}, Symbol) ->
  Tunit#tunit{symbols = maps:put(Symbol#symbol.name, Symbol, Symbols)}.
