%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Runtime support for stand-alone programs started as escripts.
%%% Copyright (C) 2018  Mikael Pettersson
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
