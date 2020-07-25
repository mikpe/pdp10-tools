%%% -*- erlang-indent-level: 2 -*-
%%%
%%% simulator for pdp10-elf
%%% Copyright (C) 2020  Mikael Pettersson
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
%%%=============================================================================
%%%
%%% Test cases for 2.4 Boolean Functions

-module(sim_boolean_tests).

-include("../src/sim_core.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_FLAGS, (1 bsl ?PDP10_PF_USER)).

-define(LOW18(X), ((X) band ((1 bsl 18) - 1))).

-define(INSN(OP, AC, I, X, Y),
        (((OP) bsl (35 - 8)) bor
         ((AC) bsl (35 - 12)) bor
         ((I) bsl (35 - 13)) bor
         ((X) bsl (35 - 17)) bor
         ?LOW18(Y))).

-define(COMMA2(LEFT, RIGHT), ((?LOW18(LEFT) bsl 18) bor ?LOW18(RIGHT))). % LEFT,,RIGHT in MACRO-10

-define(INSN_INVALID, ?INSN(0, 0, 0, 0, 0)).

-define(OP_MOVEI, 8#201).
-define(OP_SETZ, 8#400).
-define(OP_SETZI, 8#401).
-define(OP_SETZM, 8#402).
-define(OP_SETZB, 8#403).
-define(OP_AND, 8#404).
-define(OP_ANDI, 8#405).
-define(OP_ANDM, 8#406).
-define(OP_ANDB, 8#407).
-define(OP_ANDCA, 8#410).
-define(OP_ANDCAI, 8#411).
-define(OP_ANDCAM, 8#412).
-define(OP_ANDCAB, 8#413).
-define(OP_SETM, 8#414).
-define(OP_SETMI, 8#415).
-define(OP_XMOVEI, ?OP_SETMI).
-define(OP_SETMM, 8#416).
-define(OP_SETMB, 8#417).
-define(OP_ANDCM, 8#420).
-define(OP_ANDCMI, 8#421).
-define(OP_ANDCMM, 8#422).
-define(OP_ANDCMB, 8#423).
-define(OP_SETA, 8#424).
-define(OP_SETAI, 8#425).
-define(OP_SETAM, 8#426).
-define(OP_SETAB, 8#426).
-define(OP_XOR, 8#430).
-define(OP_XORI, 8#431).
-define(OP_XORM, 8#432).
-define(OP_XORB, 8#433).
-define(OP_IOR, 8#434).
-define(OP_IORI, 8#435).
-define(OP_IORM, 8#436).
-define(OP_IORB, 8#437).
-define(OP_ANDCB, 8#440).
-define(OP_ANDCBI, 8#441).
-define(OP_ANDCBM, 8#442).
-define(OP_ANDCBB, 8#443).
-define(OP_EQV, 8#444).
-define(OP_EQVI, 8#445).
-define(OP_EQVM, 8#446).
-define(OP_EQVB, 8#447).
-define(OP_SETCA, 8#450).
-define(OP_SETCAI, 8#451).
-define(OP_SETCAM, 8#452).
-define(OP_SETCAB, 8#453).
-define(OP_ORCA, 8#454).
-define(OP_ORCAI, 8#455).
-define(OP_ORCAM, 8#456).
-define(OP_ORCAB, 8#457).
-define(OP_SETCM, 8#460).
-define(OP_SETCMI, 8#461).
-define(OP_SETCMM, 8#462).
-define(OP_SETCMB, 8#463).
-define(OP_ORCM, 8#464).
-define(OP_ORCMI, 8#465).
-define(OP_ORCMM, 8#466).
-define(OP_ORCMB, 8#467).
-define(OP_ORCB, 8#470).
-define(OP_ORCBI, 8#471).
-define(OP_ORCBM, 8#472).
-define(OP_ORCBB, 8#473).
-define(OP_SETO, 8#474).
-define(OP_SETOI, 8#475).
-define(OP_SETOM, 8#476).
-define(OP_SETOB, 8#477).

%% 2.4 Boolean Functions =======================================================

%% SETZ - Set to Zeros

setz_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)} % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_SETZ, 1, 0, 0, 0)}  % 1,,101/ SETZ 1,
    , {1, 8#102, ?INSN_INVALID}                % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 0} % AC1 = 0
         ]).

setzi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)} % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_SETZI, 1, 0, 0, 0)} % 1,,101/ SETZI 1,
    , {1, 8#102, ?INSN_INVALID}                % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 0} % AC1 = 0
         ]).

setzm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETZM, 0, 0, 0, 8#200)} % 1,,100/ SETZM 200
    , {1, 8#101, ?INSN_INVALID}                    % 1,,101/ <invalid>
    , {1, 8#200, ?COMMA2(-1, -1)}                  % 1,,200/ -1,,-1
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, 0} % C(1,,200) = 0
         ]).

setzb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_SETZB, 1, 0, 0, 8#200)} % 1,,101/ SETZB 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(-1, -1)}                  % 1,,200/ -1,,-1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, 0} % C(1,,200) = 0
         , {#ea{section = 1, offset = 1, islocal = false}, 0} % AC1 = 0
         ]).

%% AND - And with AC

and_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_AND, 1, 0, 0, 8#200)}      % 1,,101/ AND 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(-1, 8#333333)}               % 1,,200/ -1,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#303030} % AC1 = 0,,303030
         ]).

andi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDI, 1, 0, 0, 8#333333)}  % 1,,101/ ANDI 1,333333
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#303030} % AC1 = 0,,303030
         ]).

andm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDM, 1, 0, 0, 8#200)}     % 1,,101/ ANDM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(-1, 8#333333)}               % 1,,200/ -1,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, 8#303030} % C(1,,200) = 0,,303030
         ]).

andb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDB, 1, 0, 0, 8#200)}     % 1,,101/ ANDB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(-1, 8#333333)}               % 1,,200/ -1,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, 8#303030} % C(1,,200) = 0,,303030
         , {#ea{section = 1, offset = 1, islocal = false}, 8#303030} % AC1 = 0,,303030
         ]).

%% ANDCA - And with Complement of AC

andca_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDCA, 1, 0, 0, 8#200)}    % 1,,101/ ANDCA 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(-1, 8#333333)}               % 1,,200/ -1,,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#030303)} % AC1 = -1,,030303
         ]).

andcai_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)}  % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDCAI, 1, 0, 0, 8#333333)} % 1,,101/ ANDCAI 1,333333
    , {1, 8#102, ?INSN_INVALID}                        % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#030303} % AC1 = 0,,030303
         ]).

andcam_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDCAM, 1, 0, 0, 8#200)}   % 1,,101/ ANDCAM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(-1, 8#333333)}               % 1,,200/ -1,,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#030303)} % C(1,,200) = -1,,030303
         ]).

andcab_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDCAB, 1, 0, 0, 8#200)}   % 1,,101/ ANDCAB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(-1, 8#333333)}               % 1,,200/ -1,,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#030303)} % C(1,,200) = -1,,030303
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#030303)} % AC1 = -1,,030303
         ]).

%% SETM - Set to Memory

setm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETM, 1, 0, 0, 8#150)}   % 1,,100/ SETM 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, 8#42}                              % 1,,150/ 0,,42
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#42} % AC1 = 42
         ]).

setmi_test() ->
  %% In section 0, SETMI = MOVEI.
  Prog =
    [ {0, 8#100, ?INSN(?OP_SETMI, 1, 0, 0, 8#200)}  % 0,,100/ SETMI 1,200
    , {0, 8#101, ?INSN_INVALID}                     % 0,,101/ <invalid>
    ],
  expect(Prog, [], {0, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#200} % AC1 = 200
         ]).

xmovei_ac_test() ->
  %% In sections > 0, SETMI = XMOVEI.
  %% Check that a local AC address is converted to a global one.
  Prog =
    [ {2, 8#100, ?INSN(?OP_XMOVEI, 1, 0, 0, 6)}     % 2,,100/ XMOVEI 1,6
    , {2, 8#101, ?INSN_INVALID}                     % 2,,101/ <invalid>
    ],
  expect(Prog, [], {2, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 6)} % AC1 = 1,,6
         ]).

xmovei_section_zero_ac_test() ->
  %% In sections > 0, SETMI = XMOVEI.
  %% Check that a local AC address in section zero is not converted to a global one.
  %% See "Extended Addressing", Rev. 5, Jul. 1983, KC10 / Project Jupiter docs,
  %% section 8.10, second example.  The example is incorrect, in that its
  %% EA-calculation dereferences an uninitialized word.  The third example in
  %% section 7.0 shows the correct setup.
  Prog =
    [ {2, 8#100, ?INSN(?OP_XMOVEI, 1, 1, 0, 8#150)} % 2,,100/ XMOVEI 1,@150
    , {2, 8#101, ?INSN_INVALID}                     % 2,,101/ <invalid>
    , {2, 8#150, ?COMMA2(8#200000, 8#100)}          % 2,,150/ 200000,,100 ; indirect EFIW
    , {0, 8#100, ?COMMA2(0, 6)}                     % 0,,100/ 0,,6 ; IFIW
    ],
  expect(Prog, [], {2, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 0, offset = 1, islocal = false}, 6} % AC1 = 0,,6
         ]).

xmovei_non_ac_test() ->
  %% In sections > 0, SETMI = XMOVEI.
  %% Check that an EA not denoting a local AC address is loaded as-is.
  Prog =
    [ {2, 8#100, ?INSN(?OP_XMOVEI, 1, 0, 0, 8#42)}  % 2,,100/ XMOVEI 1,42
    , {2, 8#101, ?INSN_INVALID}                     % 2,,101/ <invalid>
    ],
  expect(Prog, [], {2, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(2, 8#42)} % AC1 = 2,,42
         ]).

setmm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}        % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_SETMM, 1, 0, 0, 8#200)}    % 1,,101/ SETMM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 2)}                       % 1,,200/ 1,,2
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 2)} % C(1,,200) = 1,,2
         , {#ea{section = 1, offset = 1, islocal = false}, 3}
         ]).

setmb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 8#27)}   % 1,,100/ MOVEI 0,27
    , {1, 8#101, ?INSN(?OP_SETMB, 0, 0, 0, 8#150)}  % 1,,101/ SETMB 0,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#42}                              % 1,,150/ 0,,42
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 8#42} % C(1,,150) = 42
         , {#ea{section = 1, offset = 0, islocal = false}, 8#42} % AC0 = 42
         ]).

%% ANDCM - And Complement of Memory with AC

andcm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#333333)} % 1,,100/ MOVEI 1,333333
    , {1, 8#101, ?INSN(?OP_ANDCM, 1, 0, 0, 8#200)}    % 1,,101/ ANDCM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#707070)}                % 1,,200/ 0,,707070
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(0, 8#030303)} % AC1 = 0,,030303
         ]).

andcmi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#333333)}  % 1,,100/ MOVEI 1,333333
    , {1, 8#101, ?INSN(?OP_ANDCMI, 1, 0, 0, 8#707070)} % 1,,101/ ANDCMI 1,707070
    , {1, 8#102, ?INSN_INVALID}                        % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#030303} % AC1 = 0,,030303
         ]).

andcmm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#333333)} % 1,,100/ MOVEI 1,333333
    , {1, 8#101, ?INSN(?OP_ANDCMM, 1, 0, 0, 8#200)}   % 1,,101/ ANDCMM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#707070)}                % 1,,200/ 0,,707070
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(0, 8#030303)} % C(1,,200) = 0,,030303
         ]).

andcmb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#333333)} % 1,,100/ MOVEI 1,333333
    , {1, 8#101, ?INSN(?OP_ANDCMB, 1, 0, 0, 8#200)}   % 1,,101/ ANDCMB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(-1, 8#707070)}               % 1,,200/ -1,,707070
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(0, 8#030303)} % C(1,,200) = 0,,030303
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(0, 8#030303)} % AC1 = 0,,030303
         ]).

seta_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETA, 0, 0, 0, 0)}         % 1,,100/ SETA 0,0
    , {1, 8#101, ?INSN_INVALID}                       % 1,,101/ <invalid>
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS, []).

setai_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETAI, 0, 0, 0, 0)}        % 1,,100/ SETAI 0,0
    , {1, 8#101, ?INSN_INVALID}                       % 1,,101/ <invalid>
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS, []).

setam_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#42)}   % 1,,100/ MOVEI 1,42
    , {1, 8#101, ?INSN(?OP_SETAM, 1, 0, 0, 8#150)}  % 1,,101/ SETAM 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#27}                              % 1,,150/ 0,,27
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 8#42} % C(1,,150) = 42
         ]).

setab_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#42)}   % 1,,100/ MOVEI 1,42
    , {1, 8#101, ?INSN(?OP_SETAB, 1, 0, 0, 8#150)}  % 1,,101/ SETAB 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#27}                              % 1,,150/ 0,,27
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 8#42} % C(1,,150) = 42
         ]).

%% XOR - Exclusive Or with AC

xor_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_XOR, 1, 0, 0, 8#200)}      % 1,,101/ XOR 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#333333)}                % 1,,200/ 0,,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#434343} % AC1 = 0,,434343
         ]).

xori_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_XORI, 1, 0, 0, 8#333333)}  % 1,,101/ XORI 1,333333
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#434343} % AC1 = 0,,434343
         ]).

xorm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_XORM, 1, 0, 0, 8#200)}     % 1,,101/ XORM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#333333)}                % 1,,200/ 0,,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, 8#434343} % C(1,,200) = 0,,434343
         ]).

xorb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_XORB, 1, 0, 0, 8#200)}     % 1,,101/ XORB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#333333)}                % 1,,200/ 0,,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, 8#434343} % C(1,,200) = 0,,434343
         , {#ea{section = 1, offset = 1, islocal = false}, 8#434343} % AC1 = 0,,434343
         ]).

%% IOR - Inclusive Or with AC

ior_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_IOR, 1, 0, 0, 8#200)}      % 1,,101/ IOR 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#070707)}                % 1,,200/ 0,,070707
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#777777} % AC1 = 0,,-1
         ]).

iori_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_IORI, 1, 0, 0, 8#070707)}  % 1,,101/ IORI 1,070707
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#777777} % AC1 = 0,,-1
         ]).

iorm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_IORM, 1, 0, 0, 8#200)}     % 1,,101/ IORM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#070707)}                % 1,,200/ 0,,070707
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, 8#777777} % C(1,,200) = 0,,-1
         ]).

iorb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_IORB, 1, 0, 0, 8#200)}     % 1,,101/ IORB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#070707)}                % 1,,200/ 0,,070707
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, 8#777777} % C(1,,200) = 0,,-1
         , {#ea{section = 1, offset = 1, islocal = false}, 8#777777} % AC1 = 0,,-1
         ]).

%% ANDCB - And Complements of Both

andcb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDCB, 1, 0, 0, 8#200)}    % 1,,101/ ANDCB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#070707)}                % 1,,200/ 0,,070707
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 0)} % AC1 = -1,,0
         ]).

andcbi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)}  % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDCBI, 1, 0, 0, 8#070707)} % 1,,101/ ANDCBI 1,070707
    , {1, 8#102, ?INSN_INVALID}                        % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 0)} % AC1 = -1,,0
         ]).

andcbm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDCBM, 1, 0, 0, 8#200)}   % 1,,101/ ANDCBM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#070707)}                % 1,,200/ 0,,070707
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 0)} % C(1,,200) = -1,,0
         ]).

andcbb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ANDCBB, 1, 0, 0, 8#200)}   % 1,,101/ ANDCBB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#070707)}                % 1,,200/ 0,,070707
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 0)} % C(1,,200) = -1,,0
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 0)} % AC1 = -1,,0
         ]).

%% EQV - Equivalence with AC

eqv_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_EQV, 1, 0, 0, 8#200)}      % 1,,101/ EQV 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#333333)}                % 1,,200/ 0,,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#343434)} % AC1 = -1,,343434
         ]).

eqvi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_EQVI, 1, 0, 0, 8#333333)}  % 1,,101/ EQVI 1,333333
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#343434)} % AC1 = -1,,343434
         ]).

eqvm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_EQVM, 1, 0, 0, 8#200)}     % 1,,101/ EQVM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#333333)}                % 1,,200/ 0,,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#343434)} % C(1,,200) = -1,,343434
         ]).

eqvb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_EQVB, 1, 0, 0, 8#200)}     % 1,,101/ EQVB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#333333)}                % 1,,200/ 0,,333333
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#343434)} % C(1,,200) = -1,,343434
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#343434)} % AC1 = -1,,343434
         ]).

%% SETCA - Set to Complement of AC

setca_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_SETCA, 1, 0, 0, 0)}        % 1,,101/ SETCA 1,
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#070707)} % AC1 = -1,,070707
         ]).

setcai_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_SETCAI, 1, 0, 0, 0)}       % 1,,101/ SETCAI 1,
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#070707)} % AC1 = -1,,070707
         ]).

setcam_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_SETCAM, 1, 0, 0, 8#200)}   % 1,,101/ SETCAM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, 0}                                   % 1,,200/ 0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#070707)} % C(1,,200) = -1,,070707
         ]).

setcab_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_SETCAB, 1, 0, 0, 8#200)}   % 1,,101/ SETCAB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, 0}                                   % 1,,200/ 0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#070707)} % C(1,,200) = -1,,070707
         , {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#070707)} % C(1,,200) = -1,,070707
         ]).

%% ORCA - Inclusive Or with Complement of AC

orca_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCA, 1, 0, 0, 8#200)}     % 1,,101/ ORCA 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#303030)}                % 1,,200/ 0,,303030
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#373737)} % AC1 = -1,,373737
         ]).

orcai_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)}  % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCAI, 1, 0, 0, 8#303030)}  % 1,,101/ ORCAI 1,303030
    , {1, 8#102, ?INSN_INVALID}                        % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#373737)} % AC1 = -1,,373737
         ]).

orcam_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCAM, 1, 0, 0, 8#200)}    % 1,,101/ ORCAM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#303030)}                % 1,,200/ 0,,303030
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#373737)} % C(1,,200) = -1,,373737
         ]).

orcab_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCAB, 1, 0, 0, 8#200)}    % 1,,101/ ORCAB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#303030)}                % 1,,200/ 0,,303030
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#373737)} % C(1,,200) = -1,,373737
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#373737)} % AC1 = -1,,373737
         ]).

%% SETCM - Set to Complement of Memory

setcm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETCM, 1, 0, 0, 8#200)}    % 1,,100/ SETCM 1,200
    , {1, 8#101, ?INSN_INVALID}                       % 1,,101/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#707070)}                % 1,,200/ 0,,707070
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#070707)} % AC1 = -1,,070707
         ]).

setcmi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETCMI, 1, 0, 0, 8#707070)} % 1,,100/ SETCMI 1,707070
    , {1, 8#101, ?INSN_INVALID}                        % 1,,101/ <invalid>
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#070707)} % AC1 = -1,,070707
         ]).

setcmm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETCMM, 1, 0, 0, 8#200)}   % 1,,100/ SETCMM 1,200
    , {1, 8#101, ?INSN_INVALID}                       % 1,,101/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#707070)}                % 1,,200/ 0,,707070
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#070707)} % C(1,,200) = -1,,070707
         ]).

setcmb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETCMB, 1, 0, 0, 8#200)}   % 1,,100/ SETCMB 1,200
    , {1, 8#101, ?INSN_INVALID}                       % 1,,101/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#707070)}                % 1,,200/ 0,,707070
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, 8#070707)} % C(1,,200) = -1,,070707
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 8#070707)} % AC1 = -1,,070707
         ]).

%% ORCM - Inclusive Or Complement or Memory with AC

orcm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCM, 1, 0, 0, 8#200)}     % 1,,101/ ORCM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#707070)}                % 1,,200/ 0,,707070
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

orcmi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)}  % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCMI, 1, 0, 0, 8#707070)}  % 1,,101/ ORCMI 1,707070
    , {1, 8#102, ?INSN_INVALID}                        % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

orcmm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCMM, 1, 0, 0, 8#200)}    % 1,,101/ ORCMM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#707070)}                % 1,,200/ 0,,707070
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, -1)} % C(1,,200) = -1,,-1
         ]).

orcmb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCMB, 1, 0, 0, 8#200)}    % 1,,101/ ORCMB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#707070)}                % 1,,200/ 0,,707070
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, -1)} % C(1,,200) = -1,,-1
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

%% ORCB - Inclusive Or Complements of Both

orcb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCB, 1, 0, 0, 8#200)}     % 1,,101/ ORCB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#070707)}                % 1,,200/ 0,,070707
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

orcbi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)}  % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCBI, 1, 0, 0, 8#070707)}  % 1,,101/ ORCBI 1,070707
    , {1, 8#102, ?INSN_INVALID}                        % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

orcbm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCBM, 1, 0, 0, 8#200)}    % 1,,101/ ORCBM 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#070707)}                % 1,,200/ 0,,070707
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, -1)} % C(1,,200) = -1,,-1
         ]).

orcbb_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707070)} % 1,,100/ MOVEI 1,707070
    , {1, 8#101, ?INSN(?OP_ORCBB, 1, 0, 0, 8#200)}    % 1,,101/ ORCBB 1,200
    , {1, 8#102, ?INSN_INVALID}                       % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 8#070707)}                % 1,,200/ 0,,070707
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, -1)} % C(1,,200) = -1,,-1
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

%% SETO - Set to Ones

seto_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETO, 1, 0, 0, 0)}  % 1,,100/ SETO 1,
    , {1, 8#101, ?INSN_INVALID}                % 1,,101/ <invalid>
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

setoi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETOI, 1, 0, 0, 0)} % 1,,100/ SETOI 1,
    , {1, 8#101, ?INSN_INVALID}                % 1,,101/ <invalid>
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

setom_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETOM, 0, 0, 0, 8#200)} % 1,,100/ SETOM 200
    , {1, 8#101, ?INSN_INVALID}                    % 1,,101/ <invalid>
    , {1, 8#200, 0}                                % 1,,200/ 0
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, -1)} % C(1,,200) = -1,,-1
         ]).

setob_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SETOB, 1, 0, 0, 8#200)} % 1,,100/ SETOB 1,200
    , {1, 8#101, ?INSN_INVALID}                    % 1,,101/ <invalid>
    , {1, 8#200, 0}                                % 1,,200/ 0
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, -1)} % C(1,,200) = -1,,-1
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

%% Common code to run short sequences ==========================================

expect(Prog, ACs, ExpectedPC, ExpectedFlags, ExpectedEs) ->
  {Core, Mem} = init(Prog, ACs),
  {Core1, Mem1, {error, {sim_core, {dispatch, PC, _IR, _EA}}}} = sim_core:run(Core, Mem),
  ActualPC = {PC bsr 18, PC band ((1 bsl 18) - 1)},
  ?assertEqual(ExpectedPC, ActualPC),
  ?assertEqual(ExpectedFlags, Core1#core.flags),
  lists:foreach(fun({EA, ExpectedE}) ->
                  {ok, ActualE} = sim_core:c(Core1, Mem1, EA),
                  ?assertEqual(ExpectedE, ActualE)
                end, ExpectedEs),
  sim_mem:delete(Mem).

init(Prog, ACs) ->
  {PCSection, PCOffset} = prog_pc(Prog),
  Mem = init_mem(Prog),
  Core = init_core(PCSection, PCOffset, ACs),
  {Core, Mem}.

prog_pc([{Section, Offset, _Word} | _Rest]) -> {Section, Offset}.

init_mem(Prog) -> init_mem(Prog, sim_mem:new()).

init_mem([], Mem) -> Mem;
init_mem([{Section, Offset, Word} | Rest], Mem) ->
  init_word(Section, Offset, Word, Mem),
  init_mem(Rest, Mem).

init_word(Section, Offset, Word, Mem) ->
  Address = (Section bsl 18) bor Offset,
  PFN = Address bsr 9,
  case sim_mem:mquery(Mem, PFN) of
    false -> sim_mem:mmap(Mem, PFN, 4+2, core);
    {_Prot, _What} -> ok
  end,
  ok = sim_mem:write_word(Mem, Address, Word).

init_core(PCSection, PCOffset, ACs) ->
  #core{ pc_section = PCSection
       , pc_offset = PCOffset
       , acs = init_acs(ACs, list_to_tuple(lists:duplicate(16, 0)))
       , flags = ?DEFAULT_FLAGS
       }.

init_acs([], ACS) -> ACS;
init_acs([{AC, Val} | Rest], ACS) -> init_acs(Rest, setelement(AC + 1, Val, ACS)).
