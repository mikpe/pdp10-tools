%%% -*- erlang-indent-level: 2 -*-
%%%
%%% token definitions for pdp10-elf as
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

-ifndef(TOKEN_HRL).
-define(TOKEN_HRL, 1).

%% Manifest constants for tokens.  These are passed around as-is for
%% attribute-less tokens, or as {Token, Attribute} tuples for tokens
%% with attributes (ordinary symbols, literals, errors).

%% reserved symbols including directives
-define(T_DOT_FILE,     'T_DOT_FILE').  % .file
-define(T_DOT_GLOBL,    'T_DOT_GLOBL'). % .globl
-define(T_DOT_IDENT,    'T_DOT_IDENT'). % .ident
-define(T_DOT_SIZE,     'T_DOT_SIZE').  % .size
-define(T_DOT_TEXT,     'T_DOT_TEXT').  % .text
-define(T_DOT_TYPE,     'T_DOT_TYPE').  % .type

%% ordinary (non-reserved, non-special non-synthetic) symbols
-define(T_SYMBOL,       'T_SYMBOL').    % pushj, foo, .Lbar

%% literals
-define(T_LOCAL_LABEL,  'T_LOCAL_LABEL'). % 1f, 0b
-define(T_STRING,       'T_STRING').    % "foo"
-define(T_UINTEGER,     'T_UINTEGER').  % 017

%% special symbols including operators and separators
-define(T_AT,           'T_AT').        % @
-define(T_COLON,        'T_COLON').     % :
-define(T_COMMA,        'T_COMMA').     % ,
-define(T_DOT,          'T_DOT').       % .
-define(T_LPAREN,       'T_LPAREN').    % (
-define(T_MINUS,        'T_MINUS').     % -
-define(T_RPAREN,       'T_RPAREN').    % )

%% synthetic symbols
-define(T_NEWLINE,      'T_NEWLINE').   % <end-of-line>
-define(T_EOF,          'T_EOF').       % <end-of-file>

-type token() :: ?T_DOT_FILE
               | ?T_DOT_GLOBL
               | ?T_DOT_IDENT
               | ?T_DOT_SIZE
               | ?T_DOT_TEXT
               | ?T_DOT_TYPE
               | {?T_SYMBOL, string()}
               | {?T_LOCAL_LABEL, non_neg_integer(), $b | $f}
               | {?T_STRING, string()}
               | {?T_UINTEGER, non_neg_integer()}
               | ?T_AT
               | ?T_COLON
               | ?T_COMMA
               | ?T_DOT
               | ?T_LPAREN
               | ?T_MINUS
               | ?T_RPAREN
               | ?T_NEWLINE
               | ?T_EOF
               .

-endif. % TOKEN_HRL
