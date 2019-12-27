%%% -*- erlang-indent-level: 2 -*-
%%%
%%% scanner state manager for pdp10-elf as
%%% Copyright (C) 2019  Mikael Pettersson
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

-module(scan_state).

%% API
-export([ % file I/O wrappers
          fclose/1
        , fgetc/1
        , fopen/1
        , stdin/0
        , ungetc/2
          % meta-data accessors
        , location/1
        , format_error/1
        ]).

%% The scanner state records the I/O handle, implements a one-character
%% pushback buffer, and maintains the current line number.
%% TODO: maintain column number too?
-record(state,
        { filename      :: string()
        , iodev         :: file:fd() | standard_io
        , ungetc        :: [] | byte()
        , linenr        :: pos_integer()
        }).

-type scan_state() :: {scan_state, reference()}.
-type location() :: {Filename :: string(), LineNr :: pos_integer()}.

-export_type([scan_state/0, location/0]).

%% API -------------------------------------------------------------------------

-spec fclose(scan_state()) -> ok.
fclose(Handle) ->
  State = #state{} = get(Handle),
  case State#state.iodev of
    standard_io -> ok;
    IoDev -> file:close(IoDev)
  end,
  erase(Handle),
  ok.

-spec fgetc(scan_state()) -> {ok, byte()} | eof | {error, {module(), term()}}.
fgetc(Handle) ->
  State = #state{} = get(Handle),
  case State#state.ungetc of
    [] ->
      case file:read(State#state.iodev, 1) of
        {ok, [Byte]} ->
          case Byte of
            $\n ->
              put(Handle, State#state{linenr = State#state.linenr + 1}),
              {ok, $\n};
            _ ->
              {ok, Byte}
          end;
        eof ->
          eof;
        {error, Reason} ->
          {error, {file, Reason}}
      end;
    Ch ->
      put(Handle, State#state{ungetc = []}),
      {ok, Ch}
  end.

-spec fopen(string()) -> {ok, scan_state()} | {error, {module(), term()}}.
fopen(Filename) ->
  case file:open(Filename, [raw, read, read_ahead]) of
    {ok, IoDev} -> do_fopen(Filename, IoDev);
    {error, Reason} -> {error, {file, Reason}}
  end.

-spec stdin() -> {ok, scan_state()}.
stdin() ->
  do_fopen(_Filename = "<stdin>", _IoDev = standard_io).

do_fopen(Filename, IoDev) ->
  State = #state{ filename = Filename
                , iodev = IoDev
                , ungetc = []
                , linenr = 1
                },
  Handle = {scan_state, make_ref()},
  put(Handle, State),
  {ok, Handle}.

-spec ungetc(byte(), scan_state()) -> ok | {error, {module(), term()}}.
ungetc(Ch, Handle) ->
  State = #state{} = get(Handle),
  case State#state.ungetc of
    [] ->
      put(Handle, State#state{ungetc = Ch}),
      ok;
    _ ->
      {error, {?MODULE, ungetc}}
  end.

-spec location(scan_state()) -> {ok, location()}.
location(Handle) ->
  State = #state{} = get(Handle),
  Location = {State#state.filename, State#state.linenr},
  {ok, Location}.

-spec format_error(term()) -> io_lib:chars().
format_error(ungetc) -> "internal error: invalid ungetc".
