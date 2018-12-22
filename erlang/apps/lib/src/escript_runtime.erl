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
%%% Runtime support for stand-alone programs started as escripts.

-module(escript_runtime).

-export([ errmsg/2
        , fatal/2
        , fmterr/2
        , progname/0
        , start/2
        ]).

%% TODO: drop these macros when dropping support for OTP < OTP-21
-ifdef(OTP_RELEASE). % >= OTP-21
-define(EXN_WITH_STACKTRACE(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACKTRACE(Stacktrace), ok).
-else. % < OTP-21
-define(EXN_WITH_STACKTRACE(Class, Reason, Stacktrace), Class:Reason).
-define(GET_STACKTRACE(Stacktrace), Stacktrace = erlang:get_stacktrace()).
-endif.

%% escript logs unhandled exceptions to standard output, but we want them to
%% end up on standard error
start(Main, Argv) ->
  try
    Main(Argv)
  catch ?EXN_WITH_STACKTRACE(Class, Reason, Stacktrace) ->
    ?GET_STACKTRACE(Stacktrace),
    fmterr("~s: Unhandled exception ~p:~p\n~p\n", [progname(), Class, Reason, Stacktrace]),
    halt(1)
  end.

fatal(Fmt, Args) ->
  errmsg(Fmt, Args),
  halt(1).

errmsg(Fmt, Args) ->
  fmterr("~s: Error: " ++ Fmt, [progname() | Args]).

fmterr(Fmt, Args) ->
  io:format(standard_error, Fmt, Args).

progname() ->
  filename:basename(escript:script_name()).
