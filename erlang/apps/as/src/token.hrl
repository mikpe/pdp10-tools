%%% -*- erlang-indent-level: 2 -*-
%%%
%%% token definitions for pdp10-elf as
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

-ifndef(TOKEN_HRL).
-define(TOKEN_HRL, 1).

%% Manifest constants for tokens.  These are passed around as-is for
%% attribute-less tokens, or as {Token, Attribute} tuples for tokens
%% with attributes (ordinary symbols, literals, errors).

%% reserved symbols including directives
-define(T_DOT_ASCII,            'T_DOT_ASCII').         % .ascii
-define(T_DOT_ASCIZ,            'T_DOT_ASCIZ').         % .asciz
-define(T_DOT_BYTE,             'T_DOT_BYTE').          % .byte
-define(T_DOT_DATA,             'T_DOT_DATA').          % .data
-define(T_DOT_FILE,             'T_DOT_FILE').          % .file
-define(T_DOT_GLOBL,            'T_DOT_GLOBL').         % .globl
-define(T_DOT_HWORD,            'T_DOT_HWORD').         % .hword
-define(T_DOT_IDENT,            'T_DOT_IDENT').         % .ident
-define(T_DOT_LONG,             'T_DOT_LONG').          % .long
-define(T_DOT_POPSECTION,       'T_DOT_POPSECTION').    % .popsection
-define(T_DOT_PREVIOUS,         'T_DOT_PREVIOUS').      % .previous
-define(T_DOT_PUSHSECTION,      'T_DOT_PUSHSECTION').   % .pushsection
-define(T_DOT_SECTION,          'T_DOT_SECTION').       % .section
-define(T_DOT_SHORT,            'T_DOT_SHORT').         % .short
-define(T_DOT_SIZE,             'T_DOT_SIZE').          % .size
-define(T_DOT_SUBSECTION,       'T_DOT_SUBSECTION').    % .subsection
-define(T_DOT_TEXT,             'T_DOT_TEXT').          % .text
-define(T_DOT_TYPE,             'T_DOT_TYPE').          % .type
-define(T_DOT_WORD,             'T_DOT_WORD').          % .word

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
-define(T_PLUS,         'T_PLUS').      % +
-define(T_RPAREN,       'T_RPAREN').    % )

%% synthetic symbols
-define(T_NEWLINE,      'T_NEWLINE').   % <end-of-line>
-define(T_EOF,          'T_EOF').       % <end-of-file>

-type token() :: ?T_DOT_ASCII
               | ?T_DOT_ASCIZ
               | ?T_DOT_BYTE
               | ?T_DOT_DATA
               | ?T_DOT_FILE
               | ?T_DOT_GLOBL
               | ?T_DOT_HWORD
               | ?T_DOT_IDENT
               | ?T_DOT_LONG
               | ?T_DOT_POPSECTION
               | ?T_DOT_PREVIOUS
               | ?T_DOT_PUSHSECTION
               | ?T_DOT_SECTION
               | ?T_DOT_SHORT
               | ?T_DOT_SIZE
               | ?T_DOT_SUBSECTION
               | ?T_DOT_TEXT
               | ?T_DOT_TYPE
               | ?T_DOT_WORD
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
               | ?T_PLUS
               | ?T_RPAREN
               | ?T_NEWLINE
               | ?T_EOF
               .

-endif. % TOKEN_HRL
