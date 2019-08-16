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
-behaviour(gen_server).

%% API
-export([ % file I/O wrappers
          fclose/1
        , fgetc/1
        , fopen/1
        , stdin/0
        , ungetc/2
          % meta-data accessors
        , filename/1
        , linenr/1
        , format_error/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
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

-type scan_state() :: pid().

-export_type([scan_state/0]).

%% API -------------------------------------------------------------------------

-spec fclose(scan_state()) -> ok | {error, {module(), term()}}.
fclose(Pid) ->
  gen_server:call(Pid, fclose, infinity).

-spec fgetc(scan_state()) -> {ok, byte()} | eof | {error, {module(), term()}}.
fgetc(Pid) ->
  gen_server:call(Pid, fgetc, infinity).

-spec fopen(string()) -> {ok, scan_state()} | {error, {module(), term()}}.
fopen(File) ->
  do_fopen(File).

-spec stdin() -> {ok, scan_state()}.
stdin() ->
  do_fopen(stdin).

do_fopen(File) ->
  gen_server:start(?MODULE, File, []).

-spec ungetc(byte(), scan_state()) -> ok | {error, {module(), term()}}.
ungetc(Ch, Pid) ->
  gen_server:call(Pid, {ungetc, Ch}, infinity).

-spec filename(scan_state()) -> {ok, string()}.
filename(Pid) ->
  gen_server:call(Pid, filename, infinity).

-spec linenr(scan_state()) -> {ok, pos_integer()}.
linenr(Pid) ->
  gen_server:call(Pid, linenr, infinity).

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {bad_request, Req} ->
      io_lib:format("internal error: bad request: ~p", [Req]);
    ungetc ->
      "internal error: invalid ungetc";
    _ ->
      io_lib:format("~p", [Reason])
  end.

%% gen_server callbacks --------------------------------------------------------

init(stdin) ->
  do_init("<stdin>", standard_io);
init(File) ->
  case file:open(File, [raw, read, read_ahead]) of
    {ok, IoDev} -> do_init(File, IoDev);
    {error, Reason} -> {stop, {file, Reason}}
  end.

do_init(FileName, IoDev) ->
  {ok, #state{ filename = FileName
             , iodev = IoDev
             , ungetc = []
             , linenr = 1
             }}.

handle_call(Req, _From, State) ->
  case Req of
    fclose ->
      handle_fclose(State);
    fgetc ->
      handle_fgetc(State);
    {ungetc, Ch} ->
      handle_ungetc(State, Ch);
    filename ->
      {reply, {ok, State#state.filename}, State};
    linenr ->
      {reply, {ok, State#state.linenr}, State};
    _ ->
      {reply, {error, {?MODULE, {bad_request, Req}}}, State}
  end.

handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  do_fclose(State).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% fclose ----------------------------------------------------------------------

handle_fclose(State) ->
  {stop, normal, ok, State}.

do_fclose(State) ->
  case State#state.iodev of
    standard_io -> ok;
    IoDev -> file:close(IoDev)
  end.

%% fgetc -----------------------------------------------------------------------

handle_fgetc(State) ->
  case State#state.ungetc of
    [] ->
      {Result, NewState} =
        case file:read(State#state.iodev, 1) of
          {ok, [Byte]} ->
            {{ok, Byte},
             case Byte of
               $\n -> State#state{linenr = State#state.linenr + 1};
               _ -> State
             end};
          eof -> {eof, State};
          {error, Reason} -> {{error, {file, Reason}}, State}
        end,
      {reply, Result, NewState};
    Ch ->
      {reply, {ok, Ch}, State#state{ungetc = []}}
  end.

%% ungetc ----------------------------------------------------------------------

handle_ungetc(State, Ch) ->
  case State#state.ungetc of
    [] -> {reply, ok, State#state{ungetc = Ch}};
    _ -> {reply, {error, {?MODULE, ungetc}}, State}
  end.
