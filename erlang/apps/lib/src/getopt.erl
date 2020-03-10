%%% -*- erlang-indent-level: 2 -*-
%%%
%%% getopt for Erlang programs
%%% Copyright (C) 2018-2020  Mikael Pettersson
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

-module(getopt).
-export([parse/3, format_error/1]).

-define(no, no).
-define(required, required).
-define(optional, optional).
-define(ambiguous, ambiguous).
-define(invalid, invalid).
-define(error, error).

-type longopt_spec() :: {Name :: string(),
                         HasArg :: ?no | ?required | ?optional,
                         Val :: term()}.

-type longopt() :: term() | {term(), string()}.

-spec parse([string()], string(), [longopt_spec()])
        -> {ok, {[longopt()], [string()]}}
         | {error, {module(), term()}}.
parse(Argv, OptString, LongOpts) ->
  parse_argv(Argv, OptString, LongOpts, [], []).

parse_argv([], _OptString, _LongOpts, RevOpts, RevArgv) ->
  finish(RevOpts, RevArgv, []);
parse_argv(["--" | Argv], _OptString, _LongOpts, RevOpts, RevArgv) ->
  finish(RevOpts, RevArgv, Argv);
parse_argv([Arg = "-" | Argv], OptString, LongOpts, RevOpts, RevArgv) ->
  case OptString of
    [$+ | _] -> finish(RevOpts, [Arg | RevArgv], Argv);
    [$- | _] -> parse_argv(Argv, OptString, LongOpts, [{1, Arg} | RevOpts], RevArgv);
    _ -> parse_argv(Argv, OptString, LongOpts, RevOpts, [Arg | RevArgv])
  end;
parse_argv([[$-, $- | Long] | Argv], OptString, LongOpts, RevOpts, RevArgv) ->
  parse_long(Long, Argv, OptString, LongOpts, RevOpts, RevArgv);
parse_argv([[$- | Element] | Argv], OptString, LongOpts, RevOpts, RevArgv) ->
  parse_element(Element, Argv, OptString, LongOpts, RevOpts, RevArgv);
parse_argv([Arg | Argv], OptString, LongOpts, RevOpts, RevArgv) ->
  case OptString of
    [$+ | _] -> finish(RevOpts, [Arg | RevArgv], Argv);
    [$- | _] -> parse_argv(Argv, OptString, LongOpts, [{1, Arg} | RevOpts], RevArgv);
    _ -> parse_argv(Argv, OptString, LongOpts, RevOpts, [Arg | RevArgv])
  end.

finish(RevOpts, RevArgv, RestArgv) ->
  {ok, {lists:reverse(RevOpts), lists:reverse(RevArgv, RestArgv)}}.

%% Short Options ---------------------------------------------------------------

parse_element([], Argv, OptString, LongOpts, RevOpts, RevArgv) ->
  parse_argv(Argv, OptString, LongOpts, RevOpts, RevArgv);
parse_element([OptCh | Element], Argv, OptString, LongOpts, RevOpts, RevArgv) ->
  case optch_argument(OptCh, OptString) of
    ?no ->
      parse_element(Element, Argv, OptString, LongOpts, [OptCh | RevOpts], RevArgv);
    ?required ->
      case {Element, Argv} of
        {[_|_], _} ->
          parse_argv(Argv, OptString, LongOpts, [{OptCh, Element} | RevOpts], RevArgv);
        {[], [Arg = [Ch | _] | Argv2]} when Ch =/= $- ->
          parse_argv(Argv2, OptString, LongOpts, [{OptCh, Arg} | RevOpts], RevArgv);
        {_, _} ->
          mkerror(missing_argument, OptCh)
      end;
    ?optional ->
      case Element of
        [_|_] ->
          parse_argv(Argv, OptString, LongOpts, [{OptCh, Element} | RevOpts], RevArgv);
        [] ->
          parse_argv(Argv, OptString, LongOpts, [OptCh | RevOpts], RevArgv)
      end;
    ?invalid ->
      mkerror(invalid_option, OptCh);
    ?error ->
      mkerror(invalid_optstring, OptString)
  end.

optch_argument(OptCh, [$+ | OptString]) -> optch_argument2(OptCh, OptString);
optch_argument(OptCh, [$- | OptString]) -> optch_argument2(OptCh, OptString);
optch_argument(OptCh, OptString) -> optch_argument2(OptCh, OptString).

optch_argument2(_OptCh, []) -> ?invalid;
optch_argument2(_OptCh, [$: | _]) -> ?error;
optch_argument2(OptCh, [OptCh, $:, $: | _]) -> ?optional;
optch_argument2(OptCh, [OptCh, $: | _]) -> ?required;
optch_argument2(OptCh, [OptCh | _]) -> ?no;
optch_argument2(OptCh, [_OptCh2, $:, $: | OptString]) -> optch_argument2(OptCh, OptString);
optch_argument2(OptCh, [_OptCh2, $: | OptString]) -> optch_argument2(OptCh, OptString);
optch_argument2(OptCh, [_OptCh2 | OptString]) -> optch_argument2(OptCh, OptString).

%% Long Options ----------------------------------------------------------------

parse_long(Long, Argv, OptString, LongOpts, RevOpts, RevArgv) ->
  [Prefix | MaybeArg] = string:split(Long, "="),
  case find_longopt(Prefix, LongOpts) of
    {_Name, HasArg, Val} ->
      case {HasArg, MaybeArg, Argv} of
        {?no, [], _} ->
          parse_argv(Argv, OptString, LongOpts, [Val | RevOpts], RevArgv);
        {?no, _, _} ->
          mkerror(invalid_argument_long, Prefix);
        {?required, [Arg], _} ->
          parse_argv(Argv, OptString, LongOpts, [{Val, Arg} | RevOpts], RevArgv);
        {?required, [], [Arg = [Ch | _] | Argv2]} when Ch =/= $- ->
          parse_argv(Argv2, OptString, LongOpts, [{Val, Arg} | RevOpts], RevArgv);
        {?required, [], _} ->
          mkerror(missing_argument_long, Prefix);
        {?optional, [Arg], _} ->
          parse_argv(Argv, OptString, LongOpts, [{Val, Arg} | RevOpts], RevArgv);
        {?optional, [], _} ->
          parse_argv(Argv, OptString, LongOpts, [Val | RevOpts], RevArgv);
        {_, _, _} ->
          erlang:error(badarg)
      end;
    ?invalid ->
      mkerror(invalid_option_long, Prefix);
    ?ambiguous ->
      mkerror(ambiguous_option, Prefix)
  end.

find_longopt(Prefix, LongOpts) ->
  find_longopt(Prefix, LongOpts, ?invalid).

find_longopt(_Prefix, [], Candidate) -> Candidate;
find_longopt(Prefix, [Option = {Name, _HasArg, _Val} | LongOpts], Candidate) ->
  case string:prefix(Name, Prefix) of
    nomatch -> find_longopt(Prefix, LongOpts, Candidate);
    [] -> Option;
    _ when Candidate =:= ?invalid -> find_longopt(Prefix, LongOpts, Option);
    _ -> ?ambiguous
  end;
find_longopt(_Prefix, _LongOpts, _Candidate) -> erlang:error(badarg).

%% Error Formatting ------------------------------------------------------------

mkerror(Tag, Data) ->
  {error, {?MODULE, {Tag, Data}}}.

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {missing_argument, OptCh} ->
      io_lib:format("missing argument to -~c", [OptCh]);
    {invalid_option, OptCh} ->
      io_lib:format("invalid option -~c", [OptCh]);
    {invalid_optstring, OptString} ->
      io_lib:format("invalid optstring ~p", [OptString]);
    {invalid_argument_long, Prefix} ->
      io_lib:format("invalid argument to --~s", [Prefix]);
    {missing_argument_long, Prefix} ->
      io_lib:format("missing argument to --~s", [Prefix]);
    {invalid_option_long, Prefix} ->
      io_lib:format("invalid option --~s", [Prefix]);
    {ambiguous_option, Prefix} ->
      io_lib:format("ambiguous option --~s", [Prefix]);
    _ ->
      io_lib:format("~p", [Reason])
  end.
