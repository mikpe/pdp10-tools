%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Copyright (C) 2018  Mikael Pettersson
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% getopt for Erlang programs

-module(getopt).
-export([parse/2, parse/3]).

-type option() :: char() | {char(), string()}.

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

-spec parse([string()], string()) -> {ok, {[option()], [string()]}} | {error, any()}.
parse(Argv, OptString) ->
  parse(Argv, OptString, []).

-spec parse([string()], string(), [longopt_spec()])
        -> {ok, {[longopt()], [string()]}}
         | {error, any()}.
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
          {error, io_lib:format("missing argment to -~c", [OptCh])}
      end;
    ?optional ->
      case Element of
        [_|_] ->
          parse_argv(Argv, OptString, LongOpts, [{OptCh, Element} | RevOpts], RevArgv);
        [] ->
          parse_argv(Argv, OptString, LongOpts, [OptCh | RevOpts], RevArgv)
      end;
    ?invalid ->
      {error, io_lib:format("invalid option -~c", [OptCh])};
    ?error ->
      {error, io_lib:format("invalid optstring ~p", [OptString])}
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
          {error, io_lib:format("invalid argument to --~s", [Prefix])};
        {?required, [Arg], _} ->
          parse_argv(Argv, OptString, LongOpts, [{Val, Arg} | RevOpts], RevArgv);
        {?required, [], [Arg = [Ch | _] | Argv2]} when Ch =/= $- ->
          parse_argv(Argv2, OptString, LongOpts, [{Val, Arg} | RevOpts], RevArgv);
        {?required, [], _} ->
          {error, io_lib:format("missing argument to --~s", [Prefix])};
        {?optional, [Arg], _} ->
          parse_argv(Argv, OptString, LongOpts, [{Val, Arg} | RevOpts], RevArgv);
        {?optional, [], _} ->
          parse_argv(Argv, OptString, LongOpts, [Val | RevOpts], RevArgv);
        {_, _, _} ->
          {error, io_lib:format("invalid longopts ~p", [LongOpts])}
      end;
    ?invalid ->
      {error, io_lib:format("invalid option --~s", [Prefix])};
    ?ambiguous ->
      {error, io_lib:format("ambiguous option --~s", [Prefix])};
    ?error ->
      {error, io_lib:format("invalid longopts ~p", [LongOpts])}
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
find_longopt(_Prefix, _LongOpts, _Candidate) -> ?error.
