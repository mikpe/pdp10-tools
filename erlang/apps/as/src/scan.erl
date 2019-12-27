%%% -*- erlang-indent-level: 2 -*-
%%%
%%% scanner for pdp10-elf as
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

-module(scan).

-export([ token/1
        , format_error/1
        , stdin/0
        , fopen/1
        , fclose/1
        ]).

-include("token.hrl").

-type scan_state() :: {scan_state, reference()}.
-type location() :: {Filename :: string(), LineNr :: pos_integer()}.

-export_type([scan_state/0, location/0]).

%% Scan State ------------------------------------------------------------------

%% The scanner state records the I/O handle, implements a one-character
%% pushback buffer, and maintains the current line number.
%% TODO: maintain column number too?
-record(scan_state,
        { filename      :: string()
        , iodev         :: file:fd() | standard_io
        , ungetc        :: [] | byte()
        , linenr        :: pos_integer()
        }).

-spec fclose(scan_state()) -> ok.
fclose(Handle) ->
  ScanState = #scan_state{} = get(Handle),
  case ScanState#scan_state.iodev of
    standard_io -> ok;
    IoDev -> file:close(IoDev)
  end,
  erase(Handle),
  ok.

-spec fgetc(scan_state()) -> {ok, byte()} | eof | {error, {module(), term()}}.
fgetc(Handle) ->
  ScanState = #scan_state{} = get(Handle),
  case ScanState#scan_state.ungetc of
    [] ->
      case file:read(ScanState#scan_state.iodev, 1) of
        {ok, [Byte]} ->
          case Byte of
            $\n ->
              put(Handle, ScanState#scan_state{linenr = ScanState#scan_state.linenr + 1}),
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
      put(Handle, ScanState#scan_state{ungetc = []}),
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
  ScanState = #scan_state{ filename = Filename
                         , iodev = IoDev
                         , ungetc = []
                         , linenr = 1
                         },
  Handle = {scan_state, make_ref()},
  put(Handle, ScanState),
  {ok, Handle}.

-spec ungetc(byte(), scan_state()) -> ok | {error, {module(), term()}}.
ungetc(Ch, Handle) ->
  ScanState = #scan_state{} = get(Handle),
  case ScanState#scan_state.ungetc of
    [] ->
      put(Handle, ScanState#scan_state{ungetc = Ch}),
      ok;
    _ ->
      {error, {?MODULE, ungetc}}
  end.

-spec location(scan_state()) -> {ok, location()}.
location(Handle) ->
  ScanState = #scan_state{} = get(Handle),
  Location = {ScanState#scan_state.filename, ScanState#scan_state.linenr},
  {ok, Location}.

%% Scanner ---------------------------------------------------------------------

-spec token(scan_state())
      -> {ok, {location(), token()}} | {error, {module(), term()}}.
token(ScanState) ->
  %% TODO: optimize
  {ok, Location} = location(ScanState),
  case fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> {ok, {Location, ?T_EOF}};
    {ok, Ch} ->
      case Ch of
        $\s -> token(ScanState);
        $\t -> token(ScanState);
        $\r -> token(ScanState);
        $\f -> token(ScanState);
        $\n -> {ok, {Location, ?T_NEWLINE}};
        $#  -> do_line_comment(ScanState);
        $@  -> {ok, {Location, ?T_AT}};
        $:  -> {ok, {Location, ?T_COLON}};
        $;  -> {ok, {Location, ?T_NEWLINE}};
        $,  -> {ok, {Location, ?T_COMMA}};
        $(  -> {ok, {Location, ?T_LPAREN}};
        $)  -> {ok, {Location, ?T_RPAREN}};
        $/  -> do_slash(ScanState);
        $\" -> do_string(ScanState, Location, []);
        $-  -> {ok, {Location, ?T_MINUS}};
        _   ->
          if $0 =< Ch, Ch =< $9 -> do_number(ScanState, Location, Ch);
             ($A =< Ch andalso Ch =< $Z) orelse
             ($a =< Ch andalso Ch =< $z) orelse
             Ch =:= $. orelse
             Ch =:= $$ orelse
             Ch =:= $_ -> do_symbol(ScanState, Location, [Ch]);
             true -> badchar(ScanState, Ch, "")
           end
      end
  end.

%% Scan after seeing '#'.
do_line_comment(ScanState) ->
  case fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> badchar(ScanState, eof, "in line comment");
    {ok, $\n} -> {ok, {location(ScanState), ?T_NEWLINE}};
    {ok, _Ch} -> do_line_comment(ScanState)
  end.

%% Scan after seeing '/'.
do_slash(ScanState) ->
  case fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    {ok, $*} -> do_c_comment(ScanState, false);
    {ok, Ch} ->
      ungetc(Ch, ScanState),
      badchar(ScanState, Ch, "after /"); % TODO: NYI: T_DIV
    eof ->
      badchar(ScanState, eof, "after /")
  end.

%% Scan after seeing '/* ...'.
do_c_comment(ScanState, PrevWasStar) ->
  case fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> badchar(ScanState, eof, "in /*...*/ comment");
    {ok, $*} -> do_c_comment(ScanState, true);
    {ok, $/} when PrevWasStar -> token(ScanState);
    {ok, _Ch} -> do_c_comment(ScanState, false)
  end.

%% Scan after seeing '"'.
do_string(ScanState, Location, Chars) ->
  case fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> badchar(ScanState, eof, "in string literal");
    {ok, $\n} -> badchar(ScanState, $\n, "in string literal");
    {ok, $\"} -> {ok, {Location, {?T_STRING, lists:reverse(Chars)}}};
    {ok, $\\} ->
      case do_escape(ScanState) of
        {error, _Reason} = Error -> Error;
        {ok, Ch} -> do_string(ScanState, Location, [Ch | Chars])
      end;
    {ok, Ch} -> do_string(ScanState, Location, [Ch | Chars])
  end.

%% Scan after seeing '\' in a string literal.
do_escape(ScanState) ->
  case fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> badchar(ScanState, eof, "in \\ character escape");
    {ok, Ch} ->
      case Ch of
        $n  -> {ok, $\n};
        $t  -> {ok, $\t};
        $f  -> {ok, $\f};
        $r  -> {ok, $\r};
        $b  -> {ok, $\b};
        $\\ -> {ok, $\\};
        $\' -> {ok, $\'};
        $\" -> {ok, $\"};
        _   ->
          if $0 =< Ch, Ch =< $7 -> do_octal_escape(ScanState, Ch - $0, 2);
             true -> badchar(ScanState, Ch, "in \\ character escape")
          end
      end
  end.

do_octal_escape(_ScanState, Val, 0) -> {ok, Val};
do_octal_escape(ScanState, Val, N) ->
  case fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> badchar(ScanState, eof, "in \\ character escape");
    {ok, Ch} ->
      if $0 =< Ch, Ch =< $t -> do_octal_escape(ScanState, Val * 8 + (Ch - $0), N - 1);
         true ->
           case ungetc(Ch, ScanState) of
             {error, _Reason} = Error -> Error;
             ok -> {ok, Val}
           end
       end
  end.

do_symbol(ScanState, Location, Chars) ->
  case fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> do_symbol(Location, lists:reverse(Chars));
    {ok, Ch} ->
      if ($A =< Ch andalso Ch =< $Z) orelse
         ($a =< Ch andalso Ch =< $z) orelse
         ($0 =< Ch andalso Ch =< $9) orelse
         Ch =:= $. orelse
         Ch =:= $$ orelse
         Ch =:= $_ -> do_symbol(ScanState, Location, [Ch | Chars]);
         true ->
           case ungetc(Ch, ScanState) of
             {error, _Reason} = Error -> Error;
             ok -> do_symbol(Location, lists:reverse(Chars))
           end
      end
  end.

do_symbol(Location, Chars) ->
  case Chars of
    [$.] -> {ok, {Location, ?T_DOT}};
    [$. | _] -> {ok, {Location, token:from_symbol(Chars)}};
    _ -> {ok, {Location, {?T_SYMBOL, Chars}}}
  end.

do_number(ScanState, Location, Dig0) ->
  case Dig0 of
    $0 ->
      case fgetc(ScanState) of
        {error, _Reason} = Error -> Error;
        eof -> {ok, {Location, {?T_UINTEGER, Dig0 - $0}}};
        {ok, Ch} ->
          if Ch =:= $x; Ch =:= $X ->
               %% must have hex digit after 0x
               case fgetc(ScanState) of
                 {error, _Reason} = Error -> Error;
                 eof -> badchar(ScanState, eof, "after 0x in number");
                 {ok, Ch} ->
                   case chval(Ch) of
                     ChVal when ChVal < 16 ->
                       do_number(ScanState, Location, _Base = 16, ChVal);
                     _Val -> badchar(ScanState, Ch, "after 0x in number")
                   end
               end;
             true ->
               case ungetc(Ch, ScanState) of
                 {error, _Reason} = Error -> Error;
                 ok -> do_number(ScanState, Location, _Base = 8, _Val = 0)
               end
          end
      end;
    _ -> do_number(ScanState, Location, _Base = 10, _Val = Dig0 - $0)
  end.

do_number(ScanState, Location, Base, Val) ->
  case fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> {ok, {Location, {?T_UINTEGER, Val}}};
    {ok, Ch} ->
      case chval(Ch) of
        ChVal when ChVal < Base ->
          do_number(ScanState, Location, Base, Val * Base + ChVal);
        _ChVal when Base =< 10 andalso (Ch =:= $b orelse Ch =:= $f) ->
          {ok, {Location, {?T_LOCAL_LABEL, Val, Ch}}};
        _ChVal ->
          case ungetc(Ch, ScanState) of
            {error, _Reason} = Error -> Error;
            ok -> {ok, {Location, {?T_UINTEGER, Val}}}
          end
      end
  end.

chval(Ch) ->
  if $0 =< Ch, Ch =< $9 -> Ch - $0;
     $A =< Ch, Ch =< $F -> Ch - ($A - 10);
     $a =< Ch, Ch =< $f -> Ch - ($a - 10);
     true -> 255 % signals invalid by being >= any valid base
  end.

badchar(ScanState, Ch, Context) ->
  {ok, {FileName, LineNr}} = location(ScanState),
  {error, {?MODULE, {FileName, LineNr, Ch, Context}}}.

-spec format_error(term()) -> io_lib:chars().
format_error(ungetc) -> "internal error: invalid ungetc";
format_error({FileName, LineNr, Ch, Context}) ->
  io_lib:format("~s line ~p: invalid character '~s' ~s",
                [FileName, LineNr, char_to_string(Ch), Context]).

char_to_string(eof) -> "<EOF>";
char_to_string(Ch) when $\s =< Ch, Ch =< $~ -> [$', Ch, $'];
char_to_string(Ch) ->
  [$', $\\, $0 + ((Ch bsr 6) band 3), $0 + ((Ch bsr 3) band 7), $0 + (Ch band 7), $'].
