%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'as' clone for pdp10-elf
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

-module(as).
-export([main/1]).

%% Command-line interface ======================================================

main(Argv) ->
  escript_runtime:start(fun main_/1, Argv).

main_(Argv) ->
  case getopt:parse(Argv, "vo:",
                    [ {"version", no, version}
                    ]) of
    {ok, {Options, Files}} ->
      OutFile = scan_options(Options),
      case as(Files, OutFile) of
        ok -> halt(0);
        {error, Reason} -> escript_runtime:fatal("~s\n", [error:format(Reason)])
      end;
    {error, Reason} ->
      escript_runtime:errmsg("~s\n", [error:format(Reason)]),
      usage()
  end.

scan_options(Options) ->
  lists:foldl(fun scan_option/2, "a.out", Options).

scan_option($v, OutFile) ->
  version(),
  OutFile;
scan_option(version, _OutFile) ->
  version(),
  halt(0);
scan_option({$o, OutFile}, _OutFile) ->
  OutFile.

usage() ->
  escript_runtime:fmterr(
    "Usage: ~s [-v] [-o objfile] [files..]\n",
    [escript_runtime:progname()]),
  halt(1).

version() ->
  io:format(standard_io, "pdp10-tools as version 0.2\n", []).

%% As ==========================================================================

as(Files, OutFile) ->
  case input:files(Files) of
    {ok, Tunit0} ->
      case assemble:tunit(Tunit0) of
        {ok, Tunit} -> output:tunit(Tunit, OutFile);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.
