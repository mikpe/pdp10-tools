%%% -*- erlang-indent-level: 2 -*-
%%%
%%% token handling for pdp10-elf as
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

-module(token).

-export([ from_symbol/1
        , format/1
        ]).

-include("token.hrl").

-spec from_symbol(string()) -> token().
from_symbol(Name) ->
  case Name of
    ".file"             -> ?T_DOT_FILE;
    ".globl"            -> ?T_DOT_GLOBL;
    ".ident"            -> ?T_DOT_IDENT;
    ".size"             -> ?T_DOT_SIZE;
    ".text"             -> ?T_DOT_TEXT;
    ".type"             -> ?T_DOT_TYPE;
    _                   -> {?T_SYMBOL, Name}
  end.

-spec format(token()) -> io_lib:chars().
format(Token) ->
  case Token of
    ?T_DOT_FILE         -> ".file";
    ?T_DOT_GLOBL        -> ".globl";
    ?T_DOT_IDENT        -> ".ident";
    ?T_DOT_SIZE         -> ".size";
    ?T_DOT_TEXT         -> ".text";
    ?T_DOT_TYPE         -> ".type";
    {?T_SYMBOL, Name}   -> io_lib:format("symbol:~s", [Name]);
    {?T_UINTEGER, Int}  -> io_lib:format("uinteger:~p", [Int]);
    {?T_STRING, Str}    -> io_lib:format("string:~p", [Str]); % FIXME: quoting
    ?T_AT               -> "@";
    ?T_COLON            -> ":";
    ?T_COMMA            -> ",";
    ?T_DOT              -> ".";
    ?T_LPAREN           -> "(";
    ?T_MINUS            -> "-";
    ?T_RPAREN           -> ")";
    ?T_NEWLINE          -> "<newline>";
    ?T_EOF              -> "<eof>"
  end.
