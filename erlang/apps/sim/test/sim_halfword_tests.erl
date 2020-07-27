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
%%% Test cases for 2.8 Half-Word Data Transmission

-module(sim_halfword_tests).

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
-define(OP_MOVSI, 8#205).
-define(OP_HLL,   8#500).
-define(OP_HLLI,  8#501).
-define(OP_XHLLI, ?OP_HLLI).
-define(OP_HLLM,  8#502).
-define(OP_HLLS,  8#503).
-define(OP_HRL,   8#504).
-define(OP_HRLI,  8#505).
-define(OP_HRLM,  8#506).
-define(OP_HRLS,  8#507).
-define(OP_HLLZ,  8#510).
-define(OP_HLLZI, 8#511).
-define(OP_HLLZM, 8#512).
-define(OP_HLLZS, 8#513).
-define(OP_HRLZ,  8#514).
-define(OP_HRLZI, 8#515).
-define(OP_HRLZM, 8#516).
-define(OP_HRLZS, 8#517).
-define(OP_HLLO,  8#520).
-define(OP_HLLOI, 8#521).
-define(OP_HLLOM, 8#522).
-define(OP_HLLOS, 8#523).
-define(OP_HLLE,  8#530).
-define(OP_HLLEI, 8#531).
-define(OP_HLLEM, 8#532).
-define(OP_HLLES, 8#533).

%% 2.8 Half-Word Data Transmission =============================================

%% HLL - Half Word Left to Left

hll_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}    % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLL, 1, 0, 0, 8#200)}  % 1,,101/ HLL 1,200
    , {1, 8#102, ?INSN_INVALID}                   % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 0)}                   % 1,,200/ 1,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 1)} % AC1 = 1,,1
         ]).

hlli_test() ->
  %% In section 0, HLLI clears AC left.
  Prog =
    [ {0, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 1)}    % 0,,100/ MOVSI 1,1
    , {0, 8#101, ?INSN(?OP_HLLI, 1, 0, 0, 0)}     % 0,,101/ HLLI 1,0
    , {0, 8#102, ?INSN_INVALID}                   % 0,,102/ <invalid>
    ],
  expect(Prog, [], {0, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 0} % AC1 = 0
         ]).

xhlli_ac_test() ->
  %% In non-zero sections, XHLLI loads the EA's section number into AC left.
  %% Check that a local AC address is converted to a global one.
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}    % 0,,100/ MOVEI 1,1
    , {2, 8#101, ?INSN(?OP_XHLLI, 1, 0, 0, 0)}    % 0,,101/ XHLLI 1,0
    , {2, 8#102, ?INSN_INVALID}                   % 0,,102/ <invalid>
    ],
  expect(Prog, [], {2, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 1)} % AC1 = 1,,1
         ]).

xhlli_non_ac_test() ->
  %% In non-zero sections, XHLLI loads the EA's section number into AC left.
  %% Check that the section of a non-AC EA is loaded as-is.
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}    % 0,,100/ MOVEI 1,1
    , {2, 8#101, ?INSN(?OP_XHLLI, 1, 0, 0, 8#20)} % 0,,101/ XHLLI 1,20
    , {2, 8#102, ?INSN_INVALID}                   % 0,,102/ <invalid>
    ],
  expect(Prog, [], {2, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(2, 1)} % AC1 = 2,,1
         ]).

hllm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 1)}    % 1,,100/ MOVSI 1,1
    , {1, 8#101, ?INSN(?OP_HLLM, 1, 0, 0, 8#200)} % 1,,101/ HLLM 1,200
    , {1, 8#102, ?INSN_INVALID}                   % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                   % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 1)} % C(1,,200) = 1,,1
         ]).

hlls_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}    % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLLS, 1, 0, 0, 8#200)} % 1,,101/ HLLS 1,200
    , {1, 8#102, ?INSN_INVALID}                   % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 0)}                   % 1,,200/ 1,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 0)} % AC1 = 1,,0
         ]).

hlls_no_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 1)}    % 1,,100/ MOVEI 0,1
    , {1, 8#101, ?INSN(?OP_HLLS, 0, 0, 0, 8#200)} % 1,,101/ HLLS 0,200
    , {1, 8#102, ?INSN_INVALID}                   % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 0)}                   % 1,,200/ 1,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 0, islocal = false}, ?COMMA2(0, 1)} % AC0 = 0,,1
         ]).

%% HLLZ - Half Word Left to Left, Zeros

hllz_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLLZ, 1, 0, 0, 8#200)}  % 1,,101/ HLLZ 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 0)}                    % 1,,200/ 1,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 0)} % AC1 = 1,,0
         ]).

hllzi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 0,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLLZI, 1, 0, 0, 0)}     % 0,,101/ HLLZI 1,0
    , {1, 8#102, ?INSN_INVALID}                    % 0,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 0} % AC1 = 0
         ]).

hllzm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 1)}     % 1,,100/ MOVSI 1,1
    , {1, 8#101, ?INSN(?OP_HLLZM, 1, 0, 0, 8#200)} % 1,,101/ HLLZM 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 0)} % C(1,,200) = 1,,0
         ]).

hllzs_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLLZS, 1, 0, 0, 8#200)} % 1,,101/ HLLZS 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 1)}                    % 1,,200/ 1,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 0)} % C(1,,200) = 1,,0
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 0)} % AC1 = 1,,0
         ]).

hllzs_no_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 1)}     % 1,,100/ MOVEI 0,1
    , {1, 8#101, ?INSN(?OP_HLLZS, 0, 0, 0, 8#200)} % 1,,101/ HLLZS 0,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 0)}                    % 1,,200/ 1,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 0)} % C(1,,200) = 1,,0
         , {#ea{section = 1, offset = 0, islocal = false}, ?COMMA2(0, 1)} % AC0 = 0,,1
         ]).

%% HLLO - Half Word Left to Left, Ones

hllo_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLLO, 1, 0, 0, 8#200)}  % 1,,101/ HLLO 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 0)}                    % 1,,200/ 1,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, -1)} % AC1 = 1,,-1
         ]).

hlloi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 0,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLLOI, 1, 0, 0, 0)}     % 0,,101/ HLLOI 1,0
    , {1, 8#102, ?INSN_INVALID}                    % 0,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(0, -1)} % AC1 = 0,,-1
         ]).

hllom_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 1)}     % 1,,100/ MOVSI 1,1
    , {1, 8#101, ?INSN(?OP_HLLOM, 1, 0, 0, 8#200)} % 1,,101/ HLLOM 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, -1)} % C(1,,200) = 1,,-1
         ]).

hllos_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLLOS, 1, 0, 0, 8#200)} % 1,,101/ HLLOS 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 1)}                    % 1,,200/ 1,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, -1)} % C(1,,200) = 1,,-1
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, -1)} % AC1 = 1,,-1
         ]).

hllos_no_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 1)}     % 1,,100/ MOVEI 0,1
    , {1, 8#101, ?INSN(?OP_HLLOS, 0, 0, 0, 8#200)} % 1,,101/ HLLOS 0,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 0)}                    % 1,,200/ 1,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, -1)} % C(1,,200) = 1,,-1
         , {#ea{section = 1, offset = 0, islocal = false}, ?COMMA2(0, 1)} % AC0 = 0,,1
         ]).

%% HLLE - Half Word Left to Left, Extend

hlle_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLLE, 1, 0, 0, 8#200)}  % 1,,101/ HLLE 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(-1, 0)}                   % 1,,200/ -1,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

hllei_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 0,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLLEI, 1, 0, 0, 0)}     % 0,,101/ HLLEI 1,0
    , {1, 8#102, ?INSN_INVALID}                    % 0,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 0} % AC1 = 0
         ]).

hllem_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 1)}     % 1,,100/ MOVSI 1,1
    , {1, 8#101, ?INSN(?OP_HLLEM, 1, 0, 0, 8#200)} % 1,,101/ HLLEM 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 0)} % C(1,,200) = 1,,0
         ]).

hlles_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HLLES, 1, 0, 0, 8#200)} % 1,,101/ HLLES 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(-1, 0)}                   % 1,,200/ -1,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(-1, -1)} % C(1,,200) = -1,,-1
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, -1)} % AC1 = -1,,-1
         ]).

hlles_no_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 1)}     % 1,,100/ MOVEI 0,1
    , {1, 8#101, ?INSN(?OP_HLLES, 0, 0, 0, 8#200)} % 1,,101/ HLLES 0,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(1, 1)}                    % 1,,200/ 1,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 0)} % C(1,,200) = 1,,0
         , {#ea{section = 1, offset = 0, islocal = false}, ?COMMA2(0, 1)} % AC0 = 0,,1
         ]).

%% HRL - Half Word Right to Left

hrl_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HRL, 1, 0, 0, 8#200)}   % 1,,101/ HRL 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 1)} % AC1 = 1,,1
         ]).

hrli_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 0,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HRLI, 1, 0, 0, 1)}      % 0,,101/ HRLI 1,1
    , {1, 8#102, ?INSN_INVALID}                    % 0,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 1)} % AC1 = 1,,1
         ]).

hrlm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HRLM, 1, 0, 0, 8#200)}  % 1,,101/ HRLM 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 1)} % C(1,200) = 1,,1
         ]).

hrls_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HRLS, 1, 0, 0, 8#200)}  % 1,,101/ HRLS 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 1)} % C(1,200) = 1,,1
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 1)} % AC1 = 1,,1
         ]).

hrls_no_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 1)}     % 1,,100/ MOVEI 0,1
    , {1, 8#101, ?INSN(?OP_HRLS, 0, 0, 0, 8#200)}  % 1,,101/ HRLS 0,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 1)} % C(1,200) = 1,,1
         , {#ea{section = 1, offset = 0, islocal = false}, ?COMMA2(0, 1)} % AC0 = 0,,1
         ]).

%% HRLZ - Half Word Right to Left, Zeros

hrlz_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HRLZ, 1, 0, 0, 8#200)}  % 1,,101/ HRLZ 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 0)} % AC1 = 1,,0
         ]).

hrlzi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HRLZI, 1, 0, 0, 1)}     % 1,,101/ HRLZI 1,1
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 0)} % AC1 = 1,,0
         ]).

hrlzm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HRLZM, 1, 0, 0, 8#200)} % 1,,101/ HRLZM 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 0)} % C(1,,200) = 1,,0
         ]).

hrlzs_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}     % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_HRLZS, 1, 0, 0, 8#200)} % 1,,101/ HRLZS 1,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 0)} % C(1,,200) = 1,,0
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(1, 0)} % AC1 = 1,,0
         ]).

hrlzs_no_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 1)}     % 1,,100/ MOVEI 0,1
    , {1, 8#101, ?INSN(?OP_HRLZS, 0, 0, 0, 8#200)} % 1,,101/ HRLZS 0,200
    , {1, 8#102, ?INSN_INVALID}                    % 1,,102/ <invalid>
    , {1, 8#200, ?COMMA2(0, 1)}                    % 1,,200/ 0,,1
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#200, islocal = false}, ?COMMA2(1, 0)} % C(1,,200) = 1,,0
         , {#ea{section = 1, offset = 0, islocal = false}, ?COMMA2(0, 1)} % AC0 = 0,,1
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
