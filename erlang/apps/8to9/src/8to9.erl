%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 8to9 -- converts octet files to nonet files and back
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

-record(args, {infile, outfile, reverse}).

-spec main([string()]) -> no_return().
main(Argv) ->
  case my_getopt:parse(Argv, "Vi:o:r",
                       [ {"version", 'no', $V}
                       , {"infile", 'required', $i}
                       , {"outfile", 'required', $o}
                       , {"reverse", 'no', $r}
                       ]) of
    {ok, {Options, []}} ->
      Args = scan_options(Options, #args{}),
      copy(Args),
      halt(0);
    {ok, {_Options, [X | _]}} ->
      escript_runtime:errmsg("non-option parameter: ~s\n", [X]),
      usage();
    {error, Reason} ->
      escript_runtime:errmsg("~s\n", [error:format(Reason)]),
      usage()
  end.

usage() ->
  escript_runtime:fmterr("Usage: ~s [-V] [-i INFILE] [-o OUTFILE] [-r]\n",
                         [escript_runtime:progname()]),
  halt(1).

scan_options([$V | _Options], _Args) ->
  io:format(standard_io, "pdp10-tools 8to9 version 0.2\n", []),
  halt(0);
scan_options([{$i, Path} | Options], Args) ->
  scan_options(Options, Args#args{infile = Path});
scan_options([{$o, Path} | Options], Args) ->
  scan_options(Options, Args#args{outfile = Path});
scan_options([$r | Options], Args) ->
  scan_options(Options, Args#args{reverse = true});
scan_options([], Args) -> Args.

%% copy between octet and nonet files ==========================================

copy(#args{infile = Input, outfile = Output, reverse = Reverse}) ->
  {FopenInput, Fgetc, FopenOutput, Fputc, Fclose} =
    case Reverse of
      true ->
        { fun nonet_fopen_input/1
        , fun nonet_fgetc/1
        , fun octet_fopen_output/1
        , fun octet_fputc/2
        , fun octet_fclose/1
        };
      undefined ->
        { fun octet_fopen_input/1
        , fun octet_fgetc/1
        , fun nonet_fopen_output/1
        , fun nonet_fputc/2
        , fun nonet_fclose/1
        }
    end,
  InFile = copy_open(FopenInput, Input, "input"),
  OutFile = copy_open(FopenOutput, Output, "output"),
  copy(InFile, Fgetc, OutFile, Fputc),
  Fclose(OutFile).

copy_open(Fopen, What, Mode) ->
  case Fopen(What) of
    {ok, File} -> File;
    {error, Reason} ->
      escript_runtime:fatal("opening %s for ~s: ~s\n",
                            [What, Mode, error:format(Reason)])
  end.

copy(InFile, Fgetc, OutFile, Fputc) ->
  case Fgetc(InFile) of
    eof ->
      ok;
    {ok, Octet} ->
      case Fputc(Octet, OutFile) of
        ok ->
          copy(InFile, Fgetc, OutFile, Fputc);
        {error, Reason} ->
          escript_runtime:fatal("writing output: ~s\n", [error:format(Reason)])
      end;
    {error, Reason} ->
      escript_runtime:fatal("reading input: ~s\n", [error:format(Reason)])
  end.

%% nonet files =================================================================

nonet_fopen_input(undefined) -> pdp10_stdio:stdin();
nonet_fopen_input(Path) -> pdp10_stdio:fopen(Path, [raw, read, read_ahead]).

nonet_fopen_output(undefined) -> pdp10_stdio:stdout();
nonet_fopen_output(Path) -> pdp10_stdio:fopen(Path, [raw, write, delayed_write]).

nonet_fclose(FP) ->
  pdp10_stdio:fclose(FP).

nonet_fgetc(InFile) ->
  case pdp10_stdio:fgetc(InFile) of
    {ok, Nonet} = Result ->
      %% When reading from a nonet file with the intention of writing to an
      %% octet file, the data must fit in octets.
      case Nonet < 256 of
        true -> Result;
        false -> {error, {non_octet_data, Nonet}}
      end;
    Other -> Other % eof or {error, _}
  end.

nonet_fputc(Octet, OutFile) ->
  pdp10_stdio:fputc(Octet, OutFile).

%% octet files =================================================================

octet_fopen_input(undefined) -> {ok, standard_io};
octet_fopen_input(Path) ->
  case file:open(Path, [raw, read, read_ahead]) of
    {ok, _FD} = Result -> Result;
    {error, Reason} -> {error, {file, Reason}}
  end.

octet_fopen_output(undefined) -> {ok, standard_io};
octet_fopen_output(Path) ->
  case file:open(Path, [raw, write, delayed_write]) of
    {ok, _FD} = Result -> Result;
    {error, Reason} -> {error, {file, Reason}}
  end.

octet_fclose(standard_io) -> ok;
octet_fclose(FD) ->
  case file:close(FD) of
    ok -> ok;
    {error, Reason} -> {error, {file, Reason}}
  end.

octet_fgetc(InFile) ->
  case file:read(InFile, 1) of
    {ok, [Octet]} -> {ok, Octet};
    eof -> eof;
    {error, Reason} -> {error, {file, Reason}}
  end.

octet_fputc(Octet, OutFile) ->
  case file:write(OutFile, [Octet]) of
    ok -> ok;
    {error, Reason} -> {error, {file, Reason}}
  end.
