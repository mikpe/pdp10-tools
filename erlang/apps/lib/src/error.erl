%%% -*- erlang-indent-level: 2 -*-
%%%
%%% error message formatting
%%% Copyright (C) 2019  Mikael Pettersson
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
%%%-----------------------------------------------------------------------------
%%%
%%% Format error terms for output in end-user visible diagnostics.
%%%
%%% Our standard representation of error terms is as {Module, Reason} 2-tuples,
%%% where Module:format_error(Reason) returns a textual representation of Reason.
%%% These error terms are typically returned as {error, {Module, Reason}} values
%%% or thrown as error exceptions.
%%%
%%% This code will _not_ attempt to load Module.

-module(error).
-export([format/1]).

-spec format(term()) -> io_lib:chars().
format({Module, Reason} = Error) when is_atom(Module) ->
  case erlang:function_exported(Module, format_error, 1) of
    true ->
      try Module:format_error(Reason)
      catch _:_ -> default_format(Error)
      end;
    false -> default_format(Error)
  end;
format(Error) -> default_format(Error).

default_format(Error) ->
  io_lib:format("~p", [Error]).
