%%% -*- erlang-indent-level: 2 -*-
%%%
%%% stdio for I/O with 8-bit bytes
%%% Copyright (C) 2025  Mikael Pettersson
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
%%% Wrapper around file providing a stdio like API.

-module(stdio8).

-export([ fclose/1
        , fgetc/1
        , fopen/2
        , fputc/2
        , fputs/2
        , fread/2
        , fseek/2
        , ftell/1
        , stdin/0
        , stdout/0
        , format_error/1
        ]).

-type file() :: file:io_device() | io:device().
-type location() :: file:location().

-export_type([ file/0
             , location/0
             ]).

%% API -------------------------------------------------------------------------

-spec fclose(file()) -> ok | {error, {module(), term()}}.
fclose(IoDev) ->
  case file:close(IoDev) of
    ok -> ok;
    {error, Reason} -> {error, {file, Reason}}
  end.

-spec fgetc(file()) -> {ok, byte()} | eof | {error, {module(), term()}}.
fgetc(IoDev) ->
  case file:read(IoDev, 1) of
    {ok, [Octet]} -> {ok, Octet};
    eof -> eof;
    {error, Reason} -> {error, {file, Reason}}
  end.

-spec fopen(file:name_all(), [file:mode()])
       -> {ok, file()} | {error, {module(), term()}}.
fopen(Path, Modes) ->
  case file:open(Path, Modes) of
    {ok, _IoDev} = Result -> Result;
    {error, Reason} -> {error, {file, Reason}}
  end.

-spec fputc(byte(), file()) -> ok | {error, {module(), term()}}.
fputc(Octet, IoDev) ->
  case file:write(IoDev, [Octet]) of
    ok -> ok;
    {error, Reason} -> {error, {file, Reason}}
  end.

-spec fputs([byte()], file()) -> ok | {error, {module(), term()}}.
fputs(Octets, IoDev) ->
  case file:write(IoDev, Octets) of
    ok -> ok;
    {error, Reason} -> {error, {file, Reason}}
  end.

-spec fread(non_neg_integer(), file())
        -> {ok, [byte()]} | eof | {error, {module(), term()}}.
fread(NrOctets, IoDev) ->
  case file:read(IoDev, NrOctets) of
    {ok, Octets} = Result ->
      case length(Octets) =:= NrOctets of
        true -> Result;
        false -> {error, {?MODULE, premature_eof}}
      end;
    eof -> eof;
    {error, Reason} -> {error, {file, Reason}}
  end.

-spec fseek(file(), location()) -> ok | {error, {module(), term()}}.
fseek(IoDev, Location) ->
  case file:position(IoDev, Location) of
    {ok, _Pos} -> ok;
    {error, Reason} -> {error, {file, Reason}}
  end.

-spec ftell(file()) -> non_neg_integer().
ftell(IoDev) ->
  {ok, Position} = file:position(IoDev, {cur, 0}),
  Position.

-spec stdin() -> {ok, file()}.
stdin() ->
  {ok, standard_io}.

-spec stdout() -> {ok, file()}.
stdout() ->
  {ok, standard_io}.

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    premature_eof ->
      "premature EOF";
    _ ->
      io_lib:format("~p", [Reason])
  end.
