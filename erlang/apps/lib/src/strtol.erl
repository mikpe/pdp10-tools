%%% -*- erlang-indent-level: 2 -*-
%%%
%%% strtol() for Erlang
%%% Copyright (C) 2018-2023  Mikael Pettersson
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Provides C strto[u]l(3) like functionality in Erlang.  Erlang's built-in
%%% support (erlang:list_to_integer/[12], string:to_integer/1, and
%%% io_lib:fread/[23]) is too Erlang-centric to be a drop-in replacement.
%%% Since Erlang has bignums, this code performs no overflow checks.

-module(strtol).
-export([parse/2, format_error/1]).

-type base() :: 0 | 2..36.

-spec parse(string(), base())
      -> {ok, {integer(), string()}} | {error, {module(), term()}}.
parse(String, Base) ->
  scan_spaces(String, Base).

scan_spaces(String, Base) ->
  case String of
    [C | Rest] ->
      case isspace(C) of
        true -> scan_spaces(Rest, Base);
        false -> scan_sign(C, Rest, Base)
      end;
    [] -> no_digits()
  end.

scan_sign(C, Rest, Base) ->
  case C of
    $- -> scan_base(Rest, Base, _Minus = true);
    $+ -> scan_base(Rest, Base, _Minus = false);
    _  -> scan_base([C | Rest], Base, _Minus = false)
  end.

scan_base(String, Base, Minus) ->
  case Base of
    0 ->
      case String of
        [$0, C | Rest] when C =:= $x; C =:= $X ->
          scan_digits(Rest, _Base = 16, Minus);
        [$0 | Rest] ->
          scan_digits(Rest, _Base = 8, Minus);
        _ ->
          scan_digits(String, _Base = 10, Minus)
      end;
    16 ->
      case String of
        [$0, C | Rest] when C =:= $x; C =:= $X ->
          scan_digits(Rest, Base, Minus);
        _ ->
          scan_digits(String, Base, Minus)
      end;
    _ when Base >= 2, Base =< 36 ->
      scan_digits(String, Base, Minus);
    _ ->
      {error, {?MODULE, {invalid_base, Base}}}
  end.

scan_digits(String, Base, Minus) ->
  case String of
    [C | Rest] ->
      D = digit_value(C),
      if D < Base -> scan_digits(Rest, D, Base, Minus);
         true -> no_digits()
      end;
    [] -> no_digits()
  end.

scan_digits(String, Value, Base, Minus) ->
  case String of
    [C | Rest] ->
      D = digit_value(C),
      if D < Base -> scan_digits(Rest, Value * Base + D, Base, Minus);
         true -> return(String, Value, Minus)
      end;
    [] ->
      return([], Value, Minus)
  end.

return(Rest, Value0, Minus) ->
  Value = if Minus -> -Value0; true -> Value0 end,
  {ok, {Value, Rest}}.

no_digits() ->
  mkerror(no_digits).

isspace(C) ->
  case C of
    $\s -> true;
    $\f -> true;
    $\n -> true;
    $\r -> true;
    $\t -> true;
    $\v -> true;
    _   -> false
  end.

digit_value(C) ->
  if C >= $0, C =< $9 -> C - $0;
     C >= $A, C =< $Z -> C - $A + 10;
     C >= $a, C =< $z -> C - $a + 10;
     true -> 255 % out-of-band value >= any valid base
  end.

%% Error Formatting ------------------------------------------------------------

mkerror(Tag) ->
  {error, {?MODULE, Tag}}.

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    no_digits -> "no valid digits found";
    {invalid_base, Base} -> io_lib:format("invalid base: ~p", [Base]);
    _ -> io_lib:format("~p", [Reason])
  end.
