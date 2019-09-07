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
        , get_local_label/2
        , put_local_label/3
        ]).

-include("tunit.hrl").

%% API -------------------------------------------------------------------------

-spec new() -> #tunit{}.
new() ->
  #tunit{ sections = #{}
        , cursect = false
        , symbols = #{}
        , local_labels = #{}
        }.

-spec get_section(#tunit{}, string()) -> #section{} | false.
get_section(#tunit{sections = Sections}, Name) ->
  maps:get(Name, Sections, false).

-spec put_section(#tunit{}, #section{}) -> #tunit{}.
put_section(Tunit = #tunit{sections = Sections}, Section) ->
  Tunit#tunit{sections = maps:put(Section#section.name, Section, Sections)}.

-spec get_symbol(#tunit{}, string()) -> #symbol{} | false.
get_symbol(#tunit{symbols = Symbols}, Name) ->
  maps:get(Name, Symbols, false).

-spec put_symbol(#tunit{}, #symbol{}) -> #tunit{}.
put_symbol(Tunit = #tunit{symbols = Symbols}, Symbol) ->
  Tunit#tunit{symbols = maps:put(Symbol#symbol.name, Symbol, Symbols)}.

-spec get_local_label(#tunit{}, non_neg_integer()) -> pos_integer() | false.
get_local_label(#tunit{local_labels = LocalLabels}, Number) ->
  maps:get(Number, LocalLabels, false).

-spec put_local_label(#tunit{}, non_neg_integer(), pos_integer()) -> #tunit{}.
put_local_label(Tunit = #tunit{local_labels = LocalLabels}, Number, Serial) ->
  Tunit#tunit{local_labels = maps:put(Number, Serial, LocalLabels)}.
