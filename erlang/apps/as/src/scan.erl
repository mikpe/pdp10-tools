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
        ]).

-include("token.hrl").

token(ScanState) ->
  case scan_state:fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> {ok, ?T_EOF};
    {ok, Ch} ->
      case Ch of
        $\s -> token(ScanState);
        $\t -> token(ScanState);
        $\r -> token(ScanState);
        $\f -> token(ScanState);
        $\n -> {ok, ?T_NEWLINE};
        $#  -> do_line_comment(ScanState);
        $@  -> {ok, ?T_AT};
        $:  -> {ok, ?T_COLON};
        $,  -> {ok, ?T_COMMA};
        $(  -> {ok, ?T_LPAREN};
        $)  -> {ok, ?T_RPAREN};
        $/  -> do_slash(ScanState);
        $\" -> do_string(ScanState, []);
        $-  -> {ok, ?T_MINUS};
        _   ->
          if $0 =< Ch, Ch =< $9 -> do_number(ScanState, Ch);
             ($A =< Ch andalso Ch =< $Z) orelse
             ($a =< Ch andalso Ch =< $z) orelse
             Ch =:= $. orelse
             Ch =:= $$ orelse
             Ch =:= $_ -> do_symbol(ScanState, [Ch]);
             true -> badchar(ScanState, Ch, "")
           end
      end
  end.

%% Scan after seeing '#'.
do_line_comment(ScanState) ->
  case scan_state:fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> badchar(ScanState, eof, "in line comment");
    {ok, $\n} -> {ok, ?T_NEWLINE};
    {ok, _Ch} -> do_line_comment(ScanState)
  end.

%% Scan after seeing '/'.
do_slash(ScanState) ->
  case scan_state:fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    {ok, $*} -> do_c_comment(ScanState, false);
    {ok, Ch} ->
      scan_state:ungetc(Ch, ScanState),
      badchar(ScanState, Ch, "after /"); % TODO: NYI: T_DIV
    eof ->
      badchar(ScanState, eof, "after /")
  end.

%% Scan after seeing '/* ...'.
do_c_comment(ScanState, PrevWasStar) ->
  case scan_state:fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> badchar(ScanState, eof, "in /*...*/ comment");
    {ok, $*} -> do_c_comment(ScanState, true);
    {ok, $/} when PrevWasStar -> token(ScanState);
    {ok, _Ch} -> do_c_comment(ScanState, false)
  end.

%% Scan after seeing '"'.
do_string(ScanState, Chars) ->
  case scan_state:fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> badchar(ScanState, eof, "in string literal");
    {ok, $\n} -> badchar(ScanState, $\n, "in string literal");
    {ok, $\"} -> {ok, {?T_STRING, lists:reverse(Chars)}};
    {ok, $\\} ->
      case do_escape(ScanState) of
        {error, _Reason} = Error -> Error;
        {ok, Ch} -> do_string(ScanState, [Ch | Chars])
      end;
    {ok, Ch} -> do_string(ScanState, [Ch | Chars])
  end.

%% Scan after seeing '\' in a string literal.
do_escape(ScanState) ->
  case scan_state:fgetc(ScanState) of
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
  case scan_state:fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> badchar(ScanState, eof, "in \\ character escape");
    {ok, Ch} ->
      if $0 =< Ch, Ch =< $t -> do_octal_escape(ScanState, Val * 8 + (Ch - $0), N - 1);
         true ->
           case scan_state:ungetc(Ch, ScanState) of
             {error, _Reason} = Error -> Error;
             ok -> {ok, Val}
           end
       end
  end.

do_symbol(ScanState, Chars) ->
  case scan_state:fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> do_symbol(lists:reverse(Chars));
    {ok, Ch} ->
      if ($A =< Ch andalso Ch =< $Z) orelse
         ($a =< Ch andalso Ch =< $z) orelse
         ($0 =< Ch andalso Ch =< $9) orelse
         Ch =:= $. orelse
         Ch =:= $$ orelse
         Ch =:= $_ -> do_symbol(ScanState, [Ch | Chars]);
         true ->
           case scan_state:ungetc(Ch, ScanState) of
             {error, _Reason} = Error -> Error;
             ok -> do_symbol(lists:reverse(Chars))
           end
      end
  end.

do_symbol(Chars) ->
  case Chars of
    [$.] -> {ok, ?T_DOT};
    [$. | _] -> {ok, token:from_symbol(Chars)};
    _ -> {ok, {?T_SYMBOL, Chars}}
  end.

do_number(ScanState, Dig0) ->
  case Dig0 of
    $0 ->
      case scan_state:fgetc(ScanState) of
        {error, _Reason} = Error -> Error;
        eof -> {ok, {?T_UINTEGER, Dig0 - $0}};
        {ok, Ch} ->
          if Ch =:= $x; Ch =:= $X ->
               %% must have hex digit after 0x
               case scan_state:fgetc(ScanState) of
                 {error, _Reason} = Error -> Error;
                 eof -> badchar(ScanState, eof, "after 0x in number");
                 {ok, Ch} ->
                   case chval(Ch) of
                     ChVal when ChVal < 16 ->
                       do_number(ScanState, _Base = 16, ChVal);
                     _Val -> badchar(ScanState, Ch, "after 0x in number")
                   end
               end;
             true ->
               case scan_state:ungetc(Ch, ScanState) of
                 {error, _Reason} = Error -> Error;
                 ok -> do_number(ScanState, _Base = 8, _Val = 0)
               end
          end
      end;
    _ -> do_number(ScanState, _Base = 10, _Val = Dig0 - $0)
  end.

do_number(ScanState, Base, Val) ->
  case scan_state:fgetc(ScanState) of
    {error, _Reason} = Error -> Error;
    eof -> {ok, {?T_UINTEGER, Val}};
    {ok, Ch} ->
      case chval(Ch) of
        ChVal when ChVal < Base ->
          do_number(ScanState, Base, Val * Base + ChVal);
        _ChVal ->
          %% TODO: check for <decimal>[bf] which is a local label reference
          case scan_state:ungetc(Ch, ScanState) of
            {error, _Reason} = Error -> Error;
            ok -> {ok, {?T_UINTEGER, Val}}
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
  ChStr = char_to_string(Ch),
  {ok, FileName} = scan_state:filename(ScanState),
  {ok, LineNr} = scan_state:linenr(ScanState),
  {error, lists:flatten(io_lib:format("~s line ~p: invalid character '~s' ~s",
                                      [FileName, LineNr, ChStr, Context]))}.

char_to_string(eof) -> "<EOF>";
char_to_string(Ch) when $\s =< Ch, Ch =< $~ -> [$', Ch, $'];
char_to_string(Ch) ->
  [$', $\\, $0 + ((Ch bsr 6) band 3), $0 + ((Ch bsr 3) band 7), $0 + (Ch band 7), $'].
