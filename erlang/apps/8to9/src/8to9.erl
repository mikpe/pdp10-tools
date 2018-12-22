%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Copyright (C) 2013-2018  Mikael Pettersson
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
%%% Convert octet files to nonet files.

-module('8to9').
-export([main/1]).

-record(args, {infile, outfile}).

main(Argv) ->
  escript_runtime:start(fun main_/1, Argv).

main_(Argv) ->
  case getopt:parse(Argv, "Vi:o:",
                    [ {"version", 'no', $V}
                    , {"infile", 'required', $i}
                    , {"outfile", 'required', $o}
                    ]) of
    {ok, {Options, []}} ->
      Args = scan_options(Options, #args{}),
      OutFile = get_outfile(Args),
      InFile = get_infile(Args),
      copy(InFile, OutFile),
      pdp10_stdio:fclose(OutFile);
    {ok, {_Options, _}} ->
      escript_runtime:errmsg("non-option parameters\n", []),
      usage();
    {error, ErrMsg} ->
      escript_runtime:errmsg("~s\n", [ErrMsg]),
      usage()
  end.

usage() ->
  escript_runtime:fmterr("Usage: ~s [-V] [-i INFILE] [-o OUTFILE]\n", [escript_runtime:progname()]),
  halt(1).

scan_options([$V | _Options], _Args) ->
  io:format(standard_io, "pdp10-tools 8to9 version 0.1\n", []),
  halt(0);
scan_options([{$i, Path} | Options], Args) ->
  scan_options(Options, Args#args{infile = Path});
scan_options([{$o, Path} | Options], Args) ->
  scan_options(Options, Args#args{outfile = Path});
scan_options([], Args) -> Args.

copy(InFile, OutFile) ->
  case file:read(InFile, 1) of
    eof ->
      ok;
    {ok, [Octet]} ->
      case pdp10_stdio:fputc(Octet, OutFile) of
        ok ->
          copy(InFile, OutFile);
        {error, _Reason} = Error ->
          escript_runtime:fatal("writing output: ~p\n", [Error])
      end;
    {error, _Reason} = Error ->
      escript_runtime:fatal("reading input: ~p\n", [Error])
  end.

get_outfile(#args{outfile = undefined}) ->
  {ok, OutFile} = pdp10_stdio:stdout(),
  OutFile;
get_outfile(#args{outfile = Path}) ->
  case pdp10_stdio:fopen(Path, [raw, write, delayed_write]) of
    {ok, OutFile} -> OutFile;
    {error, _Reason} = Error ->
      escript_runtime:fatal("opening ~s: ~p\n", [Path, Error])
  end.

get_infile(#args{infile = undefined}) ->
  standard_io;
get_infile(#args{infile = Path}) ->
  case file:open(Path, [raw, read, read_ahead]) of
    {ok, InFile} -> InFile;
    {error, _Reason} = Error ->
      escript_runtime:fatal("opening ~s: ~p\n", [Path, Error])
  end.
