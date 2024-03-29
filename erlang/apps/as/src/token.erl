%%% -*- erlang-indent-level: 2 -*-
%%%
%%% token handling for pdp10-elf as
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

-module(token).

-export([ from_symbol/1
        , format/1
        ]).

-include("token.hrl").

-spec from_symbol(string()) -> token().
from_symbol(Name) ->
  case Name of
    ".ascii"            -> ?T_DOT_ASCII;
    ".asciz"            -> ?T_DOT_ASCIZ;
    ".byte"             -> ?T_DOT_BYTE;
    ".data"             -> ?T_DOT_DATA;
    ".file"             -> ?T_DOT_FILE;
    ".globl"            -> ?T_DOT_GLOBL;
    ".hword"            -> ?T_DOT_HWORD;
    ".ident"            -> ?T_DOT_IDENT;
    ".long"             -> ?T_DOT_LONG;
    ".popsection"       -> ?T_DOT_POPSECTION;
    ".previous"         -> ?T_DOT_PREVIOUS;
    ".pushsection"      -> ?T_DOT_PUSHSECTION;
    ".section"          -> ?T_DOT_SECTION;
    ".short"            -> ?T_DOT_SHORT;
    ".size"             -> ?T_DOT_SIZE;
    ".string"           -> ?T_DOT_STRING;
    ".subsection"       -> ?T_DOT_SUBSECTION;
    ".text"             -> ?T_DOT_TEXT;
    ".type"             -> ?T_DOT_TYPE;
    ".word"             -> ?T_DOT_WORD;
    ".2byte"            -> ?T_DOT_2BYTE;
    ".4byte"            -> ?T_DOT_4BYTE;
    _                   -> {?T_SYMBOL, Name}
  end.

-spec format(token()) -> io_lib:chars().
format(Token) ->
  case Token of
    ?T_DOT_ASCII        -> ".ascii";
    ?T_DOT_ASCIZ        -> ".asciz";
    ?T_DOT_BYTE         -> ".byte";
    ?T_DOT_DATA         -> ".data";
    ?T_DOT_FILE         -> ".file";
    ?T_DOT_GLOBL        -> ".globl";
    ?T_DOT_HWORD        -> ".hword";
    ?T_DOT_IDENT        -> ".ident";
    ?T_DOT_LONG         -> ".long";
    ?T_DOT_POPSECTION   -> ".popsection";
    ?T_DOT_PREVIOUS     -> ".previous";
    ?T_DOT_PUSHSECTION  -> ".pushsection";
    ?T_DOT_SECTION      -> ".section";
    ?T_DOT_SHORT        -> ".short";
    ?T_DOT_SIZE         -> ".size";
    ?T_DOT_STRING       -> ".string";
    ?T_DOT_SUBSECTION   -> ".subsection";
    ?T_DOT_TEXT         -> ".text";
    ?T_DOT_TYPE         -> ".type";
    ?T_DOT_WORD         -> ".word";
    ?T_DOT_2BYTE        -> ".2byte";
    ?T_DOT_4BYTE        -> ".4byte";
    {?T_SYMBOL, Name}   -> io_lib:format("symbol:~s", [Name]);
    {?T_LOCAL_LABEL, Number, Direction} ->
      io_lib:format("label: ~.10b~c", [Number, Direction]);
    {?T_STRING, Str}    -> io_lib:format("string:~p", [Str]); % FIXME: quoting
    {?T_UINTEGER, Int}  -> io_lib:format("uinteger:~p", [Int]);
    ?T_AT               -> "@";
    ?T_COLON            -> ":";
    ?T_COMMA            -> ",";
    ?T_DOT              -> ".";
    ?T_LPAREN           -> "(";
    ?T_MINUS            -> "-";
    ?T_PLUS             -> "+";
    ?T_RPAREN           -> ")";
    ?T_NEWLINE          -> "<newline>";
    ?T_EOF              -> "<eof>"
  end.
