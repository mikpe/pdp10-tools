%%% -*- erlang-indent-level: 2 -*-
%%%
%%% stdio clone for I/O with 9-bit bytes
%%% Copyright (C) 2013-2020  Mikael Pettersson
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
%%% Provide stdio-like interface for I/O to and from files with 9-bit logical
%%% bytes (nonets), represented by files with 8-bit physical bytes (octets).
%%%
%%% Theory of operation:
%%%
%%% - The state of a nonet file is composed of: a file handle for an underlying
%%%   octet file, the current read/write position in the nonet file, a 16-bit
%%%   shift register buffering partial octets (writes) or nonets (reads), a
%%%   counter indicating the number of bits in the shift register (which may
%%%   be negative after a seek), and a flag indicating if the last operation
%%%   was a read, write, or seek.
%%%
%%% - Write streams: fputc() adds 9 bits to shiftreg and 9 to shiftreg_nr_bits,
%%%   then each complete group of 8 bits in shiftreg is shifted out and written
%%%   to the octet file.  Between fputc() calls shiftreg contains between 0 and
%%%   7 bits, inclusive; during an fputc() it may contain up to 7+9 == 16 bits.
%%%
%%% - Read streams: fgetc() reads an octet from the octet file and adds 8 bits
%%%   to shiftreg and 8 to shiftreg_nr_bits, this is repeated if needed to make
%%%   shiftreg contains at least 9 bits.  Then 9 bits are shifted out from
%%%   shiftreg and returned.  Between fgetc() calls shiftreg contains between 0
%%%   and 7 bits, inclusive; during an fgetc() it may contain up to 8+8 == 16
%%%   bits.
%%%
%%% - An fseek() repositions the octet file to the closest octet boundary at or
%%%   before the requested nonet boundary, and sets shiftreg_nr_bits to the bit
%%%   difference, as a number between 0 and -7, inclusive.  A subsequent fgetc()
%%%   or fputc() detects this and reinitializes shiftreg as appropriate for that
%%%   I/O direction.
%%%
%%% - Explicit fflush() calls are not supported as they require seekable files,
%%%   and we want to support non-seekable files (e.g., stdin and stdout).
%%%   Therefore we do not enforce the C requirements of calling fflush() or
%%%   fseek() before switching from output to input, or calling fseek() before
%%%   switching from input to output (unless the last input encountered EOF).
%%%   Our implementation detects and handles direction changes automatically.
%%%
%%% - stdin and stdout are non-seekable even when bound to regular files, this
%%%   is a limitation of the Erlang standard_io implementation.

-module(pdp10_stdio).
-behaviour(gen_server).

%% API
-export([ fopen/2
        , fclose/1
        , fgetc/1
        , fread/3
        , fputc/2
        , fputs/2
        , fseek/2
        , ftell/1
        , stdin/0
        , stdout/0
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

-record(file, {pid :: pid()}).

-record(state,
        { iodev            :: file:fd() | standard_io
        , nonet_pos        :: non_neg_integer()
        , shiftreg         :: 0..65535 % 16 bits unsigned
        , shiftreg_nr_bits :: -7..16
        , read             :: boolean()
        , write            :: boolean()
        , iodir            :: read | write | seek
        }).

-type nonet() :: 0..511.

%% API -------------------------------------------------------------------------

-spec fopen(file:name_all(), [file:mode()])
       -> {ok, #file{}} | {error, {module(), term()}}.
fopen(Path, Modes) ->
  do_open({fopen, Path, Modes}).

do_open(What) ->
  case gen_server:start(?MODULE, What, []) of
    {ok, Pid} -> {ok, #file{pid = Pid}};
    {error, {shutdown, Reason}} -> {error, Reason};
    {error, _Reason} = Error -> Error
  end.

-spec fclose(#file{}) -> ok | {error, {module(), term()}}.
fclose(#file{pid = Pid}) ->
  gen_server:call(Pid, fclose, infinity).

-spec fgetc(#file{}) -> {ok, nonet()} | eof | {error, {module(), term()}}.
fgetc(#file{pid = Pid}) ->
  gen_server:call(Pid, fgetc, infinity).

-spec fread(non_neg_integer(), non_neg_integer(), #file{})
        -> {ok, [nonet()]} | eof | {error, {module(), term()}}.
fread(Size, NMemb, #file{pid = Pid}) ->
  gen_server:call(Pid, {fread, Size, NMemb}, infinity).

-spec fputc(nonet(), #file{}) -> ok | {error, {module(), term()}}.
fputc(Nonet, #file{pid = Pid}) ->
  gen_server:call(Pid, {fputc, Nonet}, infinity).

-spec fputs([nonet()], #file{}) -> ok | {error, {module(), term()}}.
fputs(Nonets, #file{pid = Pid}) ->
  gen_server:call(Pid, {fputs, Nonets}, infinity).

-spec fseek(#file{}, file:location()) -> ok | {error, {module(), term()}}.
fseek(#file{pid = Pid}, Location) ->
  gen_server:call(Pid, {fseek, Location}, infinity).

-spec ftell(#file{}) -> non_neg_integer().
ftell(#file{pid = Pid}) ->
  gen_server:call(Pid, ftell, infinity).

-spec stdin() -> {ok, #file{}}.
stdin() ->
  do_open(stdin).

-spec stdout() -> {ok, #file{}}.
stdout() ->
  do_open(stdout).

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {bad_request, Req} ->
      io_lib:format("bad request ~p", [Req]);
    no_io_direction ->
      "no I/O direction";
    {bad_mode, Mode} ->
      io_lib:format("bad mode ~p", [Mode]);
    {bad_fread, Size, NMemb} ->
      io_lib:format("bad fread size ~p nmemb ~p", [Size, NMemb]);
    eof ->
      "end-of-file during fread";
    write_only ->
      "read from write-only file";
    {bad_whence, Whence} ->
      io_lib:format("bad whence ~p", [Whence]);
    _ ->
      io_lib:format("~p", [Reason])
  end.

%% gen_server callbacks --------------------------------------------------------

init({fopen, Path, Modes}) ->
  do_init(handle_fopen(Path, Modes));
init(stdin) ->
  do_init(handle_stdin());
init(stdout) ->
  do_init(handle_stdout()).

do_init({ok, {IoDev, Read, Write}}) ->
  {ok, #state{ iodev = IoDev
             , nonet_pos = 0
             , shiftreg = 0
             , shiftreg_nr_bits = 0
             , read = Read
             , write = Write
             , iodir = seek
             }};
do_init({error, Reason}) ->
  %% The {shutdown, ...} wrapper prevents an unwanted crash report.
  {stop, {shutdown, Reason}}.

handle_call(Req, _From, State) ->
  case Req of
    fclose ->
      handle_fclose(State);
    fgetc ->
      handle_fgetc(State);
    {fread, Size, NMemb} ->
      handle_fread(Size, NMemb, State);
    {fputc, Nonet} ->
      handle_fputc(Nonet, State);
    {fputs, Nonets} ->
      handle_fputs(Nonets, State);
    {fseek, Location} ->
      handle_fseek(State, Location);
    ftell ->
      handle_ftell(State);
    _ ->
      {reply, mkerror({bad_request, Req}), State}
  end.

handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State = []) -> ok;
terminate(_Reason, State) ->
  handle_fclose(State).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% fopen -----------------------------------------------------------------------

handle_fopen(Path, Modes) ->
  case iodir(Modes) of
    {ok, {Read, Write}} ->
      case file_open(Path, fopen_modes(Modes)) of
        {ok, IoDev} -> {ok, {IoDev, Read, Write}};
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

%% add [raw, delayed_write, read_ahead] but only if they are absent
fopen_modes(Modes0) ->
  lists:foldl(fun({IsPresent, Mode}, Modes) ->
                case IsPresent(Mode, Modes) of
                  true  -> Modes;
                  false -> [Mode | Modes]
                end
              end,
              Modes0,
              [ {fun lists:member/2, raw}
              , {fun proplists:is_defined/2, delayed_write}
              , {fun proplists:is_defined/2, read_ahead}
              ]).

iodir(Modes) -> iodir(Modes, false, false).

iodir([], false, false) -> mkerror(no_io_direction);
iodir([], Read, Write) -> {ok, {Read, Write}};
iodir([Mode | Modes], Read0, Write0) ->
  case mode_iodir(Mode) of
    error -> mkerror({bad_mode, Mode});
    {Read1, Write1} -> iodir(Modes, Read0 or Read1, Write0 or Write1)
  end.

mode_iodir(append) -> error; % NYI
mode_iodir(binary) -> error; % we want lists of octets
mode_iodir({encoding, _}) -> error; % we use raw
mode_iodir(read) -> {true, false};
mode_iodir(write) -> {false, true};
mode_iodir(exclusive) -> {false, true};
mode_iodir(_) -> {false, false}.

%% stdin -----------------------------------------------------------------------

handle_stdin() ->
  {ok, {_IoDev = standard_io, _Read = true, _Write = false}}.

%% stdout ----------------------------------------------------------------------

handle_stdout() ->
  {ok, {_IoDev = standard_io, _Read = false, _Write = true}}.

%% fclose ----------------------------------------------------------------------

handle_fclose(State) ->
  Result1 = flush_buffered_write(State),
  Result2 =
    case State#state.iodev of
      standard_io -> ok;
      IoDev -> file_close(IoDev)
    end,
  Result =
    case Result1 of
      ok -> Result2;
      {error, _Reason} -> Result1
    end,
  {stop, normal, Result, []}.

%% fgetc -----------------------------------------------------------------------

handle_fgetc(State0) ->
  case prepare_to_read(State0) of
    {ok, State1} ->
      {Result, State} = fgetc_nonet(State1),
      {reply, Result, State};
    {error, _Reason} = Error -> {reply, Error, State0}
  end.

fgetc_nonet(State) ->
  %% There are four cases to consider here:
  %%
  %% * shiftreg_nr_bits >= 9
  %%   There is a complete nonet in the buffer.
  %%   We'll take a nonet from the buffer without reading any octets.
  %%
  %% * 1 <= shiftreg_nr_bits <= 8
  %%   There is a partial nonet in the buffer.
  %%   We'll read one octet, then take a nonet from the buffer.
  %%
  %% * shiftreg_nr_bits == 0
  %%   We're at a 72-bit boundary, with an empty buffer.
  %%   We'll read two octets, then take a nonet from the buffer.
  %%
  %% * -7 <= shiftreg_nr_bits <= -1
  %%   A seek placed octet_pos 1 to 7 bits before nonet_pos.
  %%   We'll read two octets, discard the first -shiftreg_nr_bits,
  %%   then take a nonet from the buffer.
  ShiftregNrBits = State#state.shiftreg_nr_bits,
  if ShiftregNrBits >= 9 -> fgetc_shiftreg(State);
     ShiftregNrBits > 0 -> fgetc_refill(State, false);
     true -> fgetc_refill(State, true)
  end.

fgetc_refill(State0, AnotherP) ->
  case fgetc_octet(State0) of
    {ok, State} ->
      case AnotherP of
        false -> fgetc_shiftreg(State);
        true -> fgetc_refill(State, false)
      end;
    eof ->
      %% An EOF during read permits the next operation to be a write, without
      %% an intervening fflush() or fseek().  We should reposition octet_pos
      %% before nonet_pos to allow this, but that doesn't work for non-seekable
      %% input files.  Instead this is handled by prepare_to_write() which
      %% does that fseek() when changing I/O direction from reading to writing.
      {eof, State0};
    {error, _Reason} = Error -> {Error, State0}
  end.

fgetc_shiftreg(State) ->
  #state{ shiftreg = Shiftreg
        , shiftreg_nr_bits = ShiftregNrBits
        , nonet_pos = NonetPos
        } = State,
  Nonet = (Shiftreg bsr (ShiftregNrBits - 9)) band 16#1ff,
  {{ok, Nonet}, State#state{ shiftreg_nr_bits = ShiftregNrBits - 9
                           , nonet_pos = NonetPos + 1 }}.

fgetc_octet(State) ->
  #state{ iodev = IoDev
        , shiftreg = Shiftreg
        , shiftreg_nr_bits = ShiftregNrBits
        } = State,
  case file_read1(IoDev) of
    {ok, Octet} ->
      {ok, State#state{ shiftreg = ((Shiftreg band 16#ff) bsl 8) bor (Octet band 16#ff)
                      , shiftreg_nr_bits = ShiftregNrBits + 8 }};
    eof -> eof;
    {error, _Reason} = Error -> Error
  end.

%% fread -----------------------------------------------------------------------

handle_fread(Size, NMemb, State0) ->
  case prepare_to_read(State0) of
    {ok, State} ->
      case freadwrite_params_ok(Size, NMemb) of
        false -> {reply, mkerror({bad_fread, Size, NMemb}), State};
        true -> fread_loop(Size * NMemb, [], State)
      end;
    {error, _Reason} = Error -> {reply, Error, State0}
  end.

fread_loop(0, Acc, State) -> {reply, {ok, lists:reverse(Acc)}, State};
fread_loop(N, Acc, State0) ->
  case fgetc_nonet(State0) of
    {{ok, Nonet}, State} -> fread_loop(N - 1, [Nonet | Acc], State);
    {eof, State} when Acc =:= [] -> {reply, eof, State};
    {eof, State} -> {reply, mkerror(eof), State};
    {{error, _Reason} = Error, State} -> {reply, Error, State}
  end.

%% On an octet-based host, in-core data structures representing nonet-based
%% target data will actually contain oversize octet-based host data with
%% padding.  For example, 9, 18, and 36-bit target integers are typically
%% stored in 16, 32, and 64-bit host integers, respectively.
%%
%% This means that I/O of aggregate structures must be avoided, and instead
%% be performed on each primitive data field individually, using explicit
%% marshalling code for multi-nonet primitive data types.
%%
%% To detect mistakes in I/O, fread and fwrite only accept strings (size == 1)
%% and single marshalled primitive data values (nmemb == 1, size == 1, 2, or 4).

freadwrite_params_ok(_Size = 0, _NMemb    ) -> true;
freadwrite_params_ok(_Size,     _NMemb = 0) -> true;
freadwrite_params_ok(_Size = 1, _NMemb    ) -> true;
freadwrite_params_ok(_Size = 2, _NMemb = 1) -> true;
freadwrite_params_ok(_Size = 4, _NMemb = 1) -> true;
freadwrite_params_ok(_Size,     _NMemb    ) -> false.

%% prepare_to_read -------------------------------------------------------------

prepare_to_read(State0) ->
  case State0 of
    #state{iodir = read} -> {ok, State0};
    #state{iodir = seek, read = true} -> {ok, State0#state{iodir = read}};
    #state{iodir = write, read = true} ->
      case do_fseek(State0, 0, cur) of
        {ok, State} -> {ok, State#state{iodir = read}};
        {error, _Reason} = Error -> Error
      end;
    #state{} -> mkerror(write_only)
  end.

%% fputc -----------------------------------------------------------------------

handle_fputc(Nonet, State0) ->
  case prepare_to_write(State0) of
    {ok, State1} ->
      {Result, State} = fputc_nonet(Nonet, State1),
      {reply, Result, State};
    {{error, _Reason} = Error, State1} -> {reply, Error, State1}
  end.

fputc_nonet(Nonet, State0) ->
  #state{ shiftreg = Shiftreg0
        , shiftreg_nr_bits = ShiftregNrBits0
        } = State0,
  ShiftregNrBits1 = ShiftregNrBits0 + 9,
  State1 = State0#state{ shiftreg = ((Shiftreg0 band 16#7f) bsl 9) bor (Nonet band 16#1ff)
                       , shiftreg_nr_bits = ShiftregNrBits1
                       },
  case fputc_octet(State1) of
    {ok, State2} ->
      case if ShiftregNrBits1 =:= 16 -> fputc_octet(State2);
              true -> {ok, State2}
           end of
        {ok, State} -> {ok, State#state{nonet_pos = State#state.nonet_pos + 1}};
        {error, _Reason} = Error -> {Error, State2}
      end;
    {error, _Reason} = Error -> {Error, State1}
  end.

fputc_octet(State) ->
  #state{ iodev = IoDev
        , shiftreg = Shiftreg
        , shiftreg_nr_bits = ShiftregNrBits
        } = State,
  Octet = (Shiftreg bsr (ShiftregNrBits - 8)) band 16#ff,
  case file_write1(IoDev, Octet) of
    ok -> {ok, State#state{shiftreg_nr_bits = ShiftregNrBits - 8}};
    {error, _Reason} = Error -> Error
  end.

%% fputs -----------------------------------------------------------------------

handle_fputs(Nonets, State0) ->
  case prepare_to_write(State0) of
    {ok, State} -> fputs_loop(Nonets, State);
    {{error, _Reason} = Error, State} -> {reply, Error, State}
  end.

fputs_loop([], State) -> {reply, ok, State};
fputs_loop([Nonet | Nonets], State0) ->
  {Result, State} = fputc_nonet(Nonet, State0),
  case Result of
    ok -> fputs_loop(Nonets, State);
    {error, _Reason} = Error -> {reply, Error, State}
  end.

%% prepare_to_write ------------------------------------------------------------

prepare_to_write(State0) ->
  case State0 of
    #state{iodir = write} -> {ok, State0};
    #state{iodir = seek, write = true} -> {ok, State0#state{iodir = write}};
    #state{iodir = read, write = true} ->
      case do_fseek(State0, 0, cur) of
        {ok, State} -> reload_shiftreg(State);
        {error, _Reason} = Error -> {Error, State0}
      end;
    #state{} -> {{error, read_only}, State0}
  end.

reload_shiftreg(State = #state{shiftreg_nr_bits = ShiftregNrBits0}) ->
  if ShiftregNrBits0 < 0 ->
       %%
       %% -7 <= shiftreg_nr_bits <= -1.
       %% fseek placed octet_pos 1 to 7 bits before nonet_pos.
       %% We will peek at the octet at octet_pos, and preload shiftreg with
       %% the -shiftreg_nr_bits high bits from the octet.
       %%
       %% read the next octet, which we will partially overwrite
       case peek_next_octet(State) of
         {ok, Octet} ->
           ShiftregNrBits = -ShiftregNrBits0,
           Shiftreg = (Octet band 16#ff) bsr (8 - ShiftregNrBits),
           {ok, State#state{ shiftreg_nr_bits = ShiftregNrBits
                           , shiftreg = Shiftreg
                           , iodir = write }};
         {error, _Reason} = Error -> {Error, State}
       end;
     true -> {ok, State#state{iodir = write}}
  end.


peek_next_octet(#state{iodev = IoDev}) ->
  %% read the next octet which we will partially overwrite
  case file_read1(IoDev) of
    {ok, Octet} ->
      %% Rewind to correct position and direction for subsequent write.
      case file_position(IoDev, {cur, -1}) of
        {ok, _Position} -> {ok, Octet};
        {error, _Reason} = Error -> Error
      end;
    eof ->
      %% Note: in C we'd fseek(..., 0, SEEK_CUR) here (I/O direction change)
      {ok, 16#00};
    {error, _Reason} = Error -> Error
  end.

%% fseek -----------------------------------------------------------------------

handle_fseek(State0, Location) ->
  {Whence, Offset} = normalize_location(Location),
  case do_fseek(State0, Offset, Whence) of
    {ok, State} -> {reply, ok, State};
    {error, _Reason} = Error -> {reply, Error, State0}
  end.

do_fseek(State, Offset, Whence) ->
  case flush_buffered_write(State) of
    {error, _Reason} = Error -> Error;
    ok ->
      case start_pos(State, Whence) of
        {error, _Reason} = Error -> Error;
        {ok, StartPos} ->
          NonetPos = StartPos + Offset,
          %%
          %% Compute 'octet_pos = (nonet_pos * 9) / 8;' without
          %% overflowing the intermediate term.
          %%
          %% Let nonet_pos = C * 8 + D, where C = nonet_pos / 8 and
          %% D = nonet_pos % 8.
          %%
          %% (nonet_pos * 9) / 8
          %% == ((C * 8 + D) * 9) / 8
          %% == (C * 8 * 9 + D * 9) / 8
          %% == C * 9 + (D * 9) / 8
          %% == (nonet_pos / 8) * 9 + ((nonet_pos % 8) * 9) / 8
          %%
          %% (The above for algorithmic reference, as overflow can
          %% happen in C but not in Erlang.)
          %%
          OctetPos = (NonetPos div 8) * 9 + ((NonetPos rem 8) * 9) div 8,
          case file_position(State#state.iodev, {bof, OctetPos}) of
            {error, _Reason} = Error -> Error;
            {ok, _Position} ->
              %%
              %% Now octet_pos will be from 0 to 7 bits before nonet_pos.
              %% Depending on whether the next I/O is a read or a write,
              %% different actions need to be taken.  Set shiftreg_nr_bits
              %% to the negation of the number of "slack" bits to signal
              %% this case.
              %%
              {ok, State#state{ nonet_pos = NonetPos
                              , shiftreg = 0
                              , shiftreg_nr_bits = -(NonetPos rem 8)
                              , iodir = seek }}
          end
      end
  end.

start_pos(State, Whence) ->
  case Whence of
    bof -> {ok, 0};
    cur -> {ok, State#state.nonet_pos};
    eof ->
      case file_position(State#state.iodev, eof) of
        {error, _Reason} = Error -> Error;
        {ok, OctetPos} ->
          %%
          %% Compute 'nonet_pos = (octet_pos * 8) / 9;' without
          %% overflowing the intermediate term.
          %%
          %% Let octet_pos = A * 9 + B, where A = octet_pos / 9 and
          %% B = octet_pos % 9.
          %%
          %% (octet_pos * 8) / 9
          %% == ((A * 9 + B) * 8) / 9
          %% == (A * 9 * 8 + B * 8) / 9
          %% == A * 8 + (B * 8) / 9
          %% == (octet_pos / 9) * 8 + ((octet_pos % 9) * 8) / 9
          %%
          %% (The above for algorithmic reference, as overflow can
          %% happen in C but not in Erlang.)
          %%
          NonetPos = (OctetPos div 9) * 8 + ((OctetPos rem 9) * 8) div 9,
          {ok, NonetPos}
      end;
    _ -> mkerror({bad_whence, Whence})
  end.

normalize_location(Location) ->
  case Location of
    bof -> {bof, 0};
    cur -> {cur, 0};
    eof -> {eof, 0};
    _ when is_integer(Location) -> {bof, Location};
    _ -> Location
  end.

%% ftell -----------------------------------------------------------------------

handle_ftell(State) ->
  {reply, State#state.nonet_pos, State}.

%% flush -----------------------------------------------------------------------

%% Explicit flushing does not work for non-seekable output files, as the flush
%% may leave the file position one octet beyond the indended one, with no means
%% of rewinding.  Therefore:
%%
%% - as part of fclose() and fseek() calls we flush buffered output _without_
%%   rewinding the file position
%% - when changing I/O direction we issue an fseek(..., 0, SEEK_CUR) which
%%   flushes buffered output and then rewinds the file position
%% - explicit fflush() or fseek() calls are not required to change I/O direction
%% - explicit fflush() calls are not supported
flush_buffered_write(State) ->
  case State#state.iodir of
    write ->
      ShiftregNrBits = State#state.shiftreg_nr_bits,
      if ShiftregNrBits > 0 -> % there is unwritten output
           case peek_last_octet(State) of
             {ok, Octet0} ->
               OctetNrBits = 8 - ShiftregNrBits,
               Shiftreg = State#state.shiftreg,
               Octet1 = Octet0 band ((1 bsl OctetNrBits) - 1),
               Octet = Octet1 bor ((Shiftreg bsl OctetNrBits) band 16#ff),
               file_write1(State#state.iodev, Octet);
             {error, _Reason} = Error -> Error
           end;
         true -> ok
      end;
    _ -> ok
  end.

peek_last_octet(#state{read = false}) ->
  %% write-only file, truncated or new
  {ok, 16#00};
peek_last_octet(#state{iodev = IoDev}) ->
  %% read the next octet which we will partially overwrite
  %% Note: in C we'd fseek(..., 0, SEEK_CUR) here (I/O direction change)
  case file_read1(IoDev) of
    {ok, Octet} ->
      case file_position(IoDev, {cur, -1}) of
        {ok, _Position} -> {ok, Octet};
        {error, _Reason} = Error -> Error
      end;
    eof -> {ok, 16#00};
    {error, _Reason} = Error -> Error
  end.

%% File Operations -------------------------------------------------------------

file_close(IoDev) ->
  case file:close(IoDev) of
    ok -> ok;
    {error, Reason} -> mkfileerror(Reason)
  end.

file_open(Path, Modes) ->
  case file:open(Path, Modes) of
    {ok, _IoDev} = Result -> Result;
    {error, Reason} -> mkfileerror(Reason)
  end.

file_position(IoDev, Pos) ->
  case file:position(IoDev, Pos) of
    {ok, _NewPos} = Result -> Result;
    {error, Reason} -> mkfileerror(Reason)
  end.

file_read1(IoDev) ->
  case file:read(IoDev, 1) of
    {ok, [Octet]} -> {ok, Octet};
    eof -> eof;
    {error, Reason} -> mkfileerror(Reason)
  end.

file_write1(IoDev, Octet) ->
  case file:write(IoDev, [Octet]) of
    ok -> ok;
    {error, Reason} -> mkfileerror(Reason)
  end.

%% Error Formatting ------------------------------------------------------------

mkerror(Reason) ->
  {error, {?MODULE, Reason}}.

mkfileerror(Reason) ->
  {error, {file, Reason}}.
