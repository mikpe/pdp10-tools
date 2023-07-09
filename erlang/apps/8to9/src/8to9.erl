%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 8to9 -- convert octet files to nonet files
%%% Copyright (C) 2013-2023  Mikael Pettersson
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

-module('8to9').
-export([main/1]).

-record(args, {infile, outfile}).

-spec main([string()]) -> no_return().
main(Argv) ->
  case my_getopt:parse(Argv, "Vi:o:",
                       [ {"version", 'no', $V}
                       , {"infile", 'required', $i}
                       , {"outfile", 'required', $o}
                       ]) of
    {ok, {Options, []}} ->
      Args = scan_options(Options, #args{}),
      OutFile = get_outfile(Args),
      InFile = get_infile(Args),
      copy(InFile, OutFile),
      pdp10_stdio:fclose(OutFile),
      halt(0);
    {ok, {_Options, [X | _]}} ->
      escript_runtime:errmsg("non-option parameter: ~s\n", [X]),
      usage();
    {error, Reason} ->
      escript_runtime:errmsg("~s\n", [error:format(Reason)]),
      usage()
  end.

usage() ->
  escript_runtime:fmterr("Usage: ~s [-V] [-i INFILE] [-o OUTFILE]\n",
                         [escript_runtime:progname()]),
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
        {error, Reason} ->
          escript_runtime:fatal("writing output: ~s\n", [error:format(Reason)])
      end;
    {error, Reason} ->
      escript_runtime:fatal("reading input: ~s\n", [error:format(Reason)])
  end.

get_outfile(#args{outfile = undefined}) ->
  {ok, OutFile} = pdp10_stdio:stdout(),
  OutFile;
get_outfile(#args{outfile = Path}) ->
  case pdp10_stdio:fopen(Path, [raw, write, delayed_write]) of
    {ok, OutFile} -> OutFile;
    {error, Reason} ->
      escript_runtime:fatal("opening ~s: ~s\n", [Path, error:format(Reason)])
  end.

get_infile(#args{infile = undefined}) ->
  standard_io;
get_infile(#args{infile = Path}) ->
  case file:open(Path, [raw, read, read_ahead]) of
    {ok, InFile} -> InFile;
    {error, Reason} ->
      escript_runtime:fatal("opening ~s: ~s\n",
                            [Path, error:format({file, Reason})])
  end.
