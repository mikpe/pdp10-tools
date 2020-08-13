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
%%% Test cases for 2.7 Logical Testing and Modification

-module(sim_logical_tests).

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

-define(EA(S, O), #ea{section = S, offset = O, islocal = false}).
-define(AC(A), ?EA(1, A)).

-define(INSN_INVALID, ?INSN(0, 0, 0, 0, 0)).

-define(OP_MOVEI, 8#201).
-define(OP_MOVSI, 8#205).
-define(OP_TRN,   8#600).
-define(OP_TLN,   8#601).
-define(OP_TRNE,  8#602).
-define(OP_TLNE,  8#603).
-define(OP_TRNA,  8#604).
-define(OP_TLNA,  8#605).
-define(OP_TRNN,  8#606).
-define(OP_TLNN,  8#607).
-define(OP_TDN,   8#610).
-define(OP_TSN,   8#611).
-define(OP_TDNE,  8#612).
-define(OP_TSNE,  8#613).
-define(OP_TDNA,  8#614).
-define(OP_TSNA,  8#615).
-define(OP_TDNN,  8#616).
-define(OP_TSNN,  8#617).
-define(OP_TRZ,   8#620).
-define(OP_TLZ,   8#621).
-define(OP_TRZE,  8#622).
-define(OP_TLZE,  8#623).
-define(OP_TRZA,  8#624).
-define(OP_TLZA,  8#625).
-define(OP_TRZN,  8#626).
-define(OP_TLZN,  8#627).
-define(OP_TDZ,   8#630).
-define(OP_TSZ,   8#631).
-define(OP_TDZE,  8#632).
-define(OP_TSZE,  8#633).
-define(OP_TDZA,  8#634).
-define(OP_TSZA,  8#635).
-define(OP_TDZN,  8#636).
-define(OP_TSZN,  8#637).
-define(OP_TRC,   8#640).
-define(OP_TLC,   8#641).
-define(OP_TRCE,  8#642).
-define(OP_TLCE,  8#643).
-define(OP_TRCA,  8#644).
-define(OP_TLCA,  8#645).
-define(OP_TRCN,  8#646).
-define(OP_TLCN,  8#647).
-define(OP_TDC,   8#650).
-define(OP_TSC,   8#651).
-define(OP_TDCE,  8#652).
-define(OP_TSCE,  8#653).
-define(OP_TDCA,  8#654).
-define(OP_TSCA,  8#655).
-define(OP_TDCN,  8#656).
-define(OP_TSCN,  8#657).
-define(OP_TRO,   8#660).
-define(OP_TLO,   8#661).
-define(OP_TROE,  8#662).
-define(OP_TLOE,  8#663).
-define(OP_TROA,  8#664).
-define(OP_TLOA,  8#665).
-define(OP_TRON,  8#666).
-define(OP_TLON,  8#667).
-define(OP_TDO,   8#670).
-define(OP_TSO,   8#671).
-define(OP_TDOE,  8#672).
-define(OP_TSOE,  8#673).
-define(OP_TDOA,  8#674).
-define(OP_TSOA,  8#675).
-define(OP_TDON,  8#676).
-define(OP_TSON,  8#677).

%% 2.7 Logical Testing and Modification ========================================

%% TRN - Test Right, No Modification, and Skip if Condition Satisfied

trn_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRN, 1, 0, 0, 0)}        % 1,,101/ TRN 1,
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

trne_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRNE, 1, 0, 0, 8#070)}   % 1,,101/ TRNE 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRNE, 1, 0, 0, 8#707)}   % 1,,101/ TRNE 1,707
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

trna_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRNA, 1, 0, 0, 0)}       % 1,,101/ TRNA 1,
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

trnn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRNN, 1, 0, 0, 8#707)}   % 1,,101/ TRNN 1,707
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRNN, 1, 0, 0, 8#070)}   % 1,,101/ TRNN 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

%% TRZ - Test Right, Zeros, and Skip if Condition Satisfied

trz_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRZ, 1, 0, 0, 8#700)}    % 1,,101/ TRZ 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

trze_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRZE, 1, 0, 0, 8#070)}   % 1,,101/ TRZE 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRZE, 1, 0, 0, 8#700)}   % 1,,101/ TRZE 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

trza_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRZA, 1, 0, 0, 8#700)}   % 1,,101/ TRZA 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

trzn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRZN, 1, 0, 0, 8#700)}   % 1,,101/ TRZN 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRZN, 1, 0, 0, 8#070)}   % 1,,101/ TRZN 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

%% TRC - Test Right, Complement, and Skip if Condition Satisfied

trc_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRC, 1, 0, 0, 8#770)}    % 1,,101/ TRC 1,770
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#077}                          % AC1 = 0,,077
         ]).

trce_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRCE, 1, 0, 0, 8#070)}   % 1,,101/ TRCE 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRCE, 1, 0, 0, 8#700)}   % 1,,101/ TRCE 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

trca_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRCA, 1, 0, 0, 8#770)}   % 1,,101/ TRCA 1,770
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#077}                          % AC1 = 0,,077
         ]).

trcn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRCN, 1, 0, 0, 8#700)}   % 1,,101/ TRCN 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRCN, 1, 0, 0, 8#070)}   % 1,,101/ TRCN 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]).

%% TRO - Test Right, Ones, and Skip if Condition Satisfied

tro_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRO, 1, 0, 0, 8#070)}    % 1,,101/ TRO 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]).

troe_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TROE, 1, 0, 0, 8#070)}   % 1,,101/ TROE 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TROE, 1, 0, 0, 8#720)}   % 1,,101/ TROE 1,720
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]).

troa_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TROA, 1, 0, 0, 8#720)}   % 1,,101/ TROA 1,720
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]).

tron_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRON, 1, 0, 0, 8#720)}   % 1,,101/ TRON 1,720
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TRON, 1, 0, 0, 8#020)}   % 1,,101/ TRON 1,020
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]).

%% TLN - Test Left, No Modification, and Skip if Condition Satisfied

tln_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLN, 1, 0, 0, 0)}        % 1,,101/ TLN 1,
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), ?COMMA2(8#707, 0)}              % AC1 = 707,,0
         ]).

tlne_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLNE, 1, 0, 0, 8#070)}   % 1,,101/ TLNE 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), ?COMMA2(8#707, 0)}              % AC1 = 707,,0
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLNE, 1, 0, 0, 8#707)}   % 1,,101/ TLNE 1,707
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), ?COMMA2(8#707, 0)}              % AC1 = 707,,0
         ]).

tlna_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLNA, 1, 0, 0, 0)}       % 1,,101/ TLNA 1,
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), ?COMMA2(8#707, 0)}              % AC1 = 707,,0
         ]).

tlnn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLNN, 1, 0, 0, 8#707)}   % 1,,101/ TLNN 1,707
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), ?COMMA2(8#707, 0)}              % AC1 = 707,,0
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLNN, 1, 0, 0, 8#070)}   % 1,,101/ TLNN 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), ?COMMA2(8#707, 0)}              % AC1 = 707,,0
         ]).

%% TLZ - Test Left, Zeros, and Skip if Condition Satisfied

tlz_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLZ, 1, 0, 0, 8#700)}    % 1,,101/ TLZ 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), ?COMMA2(8#007, 0)}              % AC1 = 007,,0
         ]).

tlze_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLZE, 1, 0, 0, 8#070)}   % 1,,101/ TLZE 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), ?COMMA2(8#707, 0)}              % AC1 = 707,,0
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLZE, 1, 0, 0, 8#700)}   % 1,,101/ TLZE 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), ?COMMA2(8#007, 0)}              % AC1 = 007,,0
         ]).

tlza_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLZA, 1, 0, 0, 8#700)}   % 1,,101/ TLZA 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), ?COMMA2(8#007, 0)}              % AC1 = 007,,0
         ]).

tlzn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLZN, 1, 0, 0, 8#700)}   % 1,,101/ TLZN 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), ?COMMA2(8#007, 0)}              % AC1 = 007,,0
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLZN, 1, 0, 0, 8#070)}   % 1,,101/ TLZN 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), ?COMMA2(8#707, 0)}              % AC1 = 707,,0
         ]).

%% TLC - Test Left, Complement, and Skip if Condition Satisfied

tlc_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLC, 1, 0, 0, 8#770)}    % 1,,101/ TLC 1,770
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), ?COMMA2(8#077, 0)}              % AC1 = 077,,0
         ]).

tlce_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLCE, 1, 0, 0, 8#070)}   % 1,,101/ TLCE 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), ?COMMA2(8#777, 0)}              % AC1 = 777,,0
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLCE, 1, 0, 0, 8#700)}   % 1,,101/ TLCE 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), ?COMMA2(8#007, 0)}              % AC1 = 007,,0
         ]).

tlca_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLCA, 1, 0, 0, 8#770)}   % 1,,101/ TLCA 1,770
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), ?COMMA2(8#077, 0)}              % AC1 = 077,,0
         ]).

tlcn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLCN, 1, 0, 0, 8#700)}   % 1,,101/ TLCN 1,700
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), ?COMMA2(8#007, 0)}              % AC1 = 007,,0
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLCN, 1, 0, 0, 8#070)}   % 1,,101/ TLCN 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), ?COMMA2(8#777, 0)}              % AC1 = 777,,0
         ]).

%% TLO - Test Left, Ones, and Skip if Condition Satisfied

tlo_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLO, 1, 0, 0, 8#070)}    % 1,,101/ TLO 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), ?COMMA2(8#777, 0)}              % AC1 = 777,,0
         ]).

tloe_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLOE, 1, 0, 0, 8#070)}   % 1,,101/ TLOE 1,070
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), ?COMMA2(8#777, 0)}              % AC1 = 777,,0
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLOE, 1, 0, 0, 8#720)}   % 1,,101/ TLOE 1,720
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), ?COMMA2(8#727, 0)}              % AC1 = 727,,0
         ]).

tloa_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLOA, 1, 0, 0, 8#720)}   % 1,,101/ TLOA 1,720
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), ?COMMA2(8#727, 0)}              % AC1 = 727,,0
         ]).

tlon_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLON, 1, 0, 0, 8#720)}   % 1,,101/ TLON 1,720
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), ?COMMA2(8#727, 0)}              % AC1 = 727,,0
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, 8#707)}  % 1,,100/ MOVSI 1,707
    , {1, 8#101, ?INSN(?OP_TLON, 1, 0, 0, 8#020)}   % 1,,101/ TLON 1,020
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), ?COMMA2(8#727, 0)}              % AC1 = 727,,0
         ]).

%% TDN - Test Direct, No Modification, and Skip if Condition Satisfied

tdn_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDN, 1, 0, 0, 8#150)}    % 1,,101/ TDN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 0}                                 % 1,,150/ 0,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

tdne_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDNE, 1, 0, 0, 8#150)}   % 1,,101/ TDNE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#070}                             % 1,,150/ 0,,070
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDNE, 1, 0, 0, 8#150)}   % 1,,101/ TDNE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#707}                             % 1,,150/ 0,,707
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

tdna_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDNA, 1, 0, 0, 8#150)}   % 1,,101/ TDNA 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 0}                                 % 1,,150/ 0,,0
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

tdnn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDNN, 1, 0, 0, 8#150)}   % 1,,101/ TDNN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#707}                             % 1,,150/ 0,,707
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDNN, 1, 0, 0, 8#150)}   % 1,,101/ TDNN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#070}                             % 1,,150/ 0,,070
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

%% TDZ - Test Direct, Zeros, and Skip if Condition Satisfied

tdz_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDZ, 1, 0, 0, 8#150)}    % 1,,101/ TDZ 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#700}                             % 1,,150/ 0,,700
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

tdze_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDZE, 1, 0, 0, 8#150)}   % 1,,101/ TDZE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#070}                             % 1,,150/ 0,,070
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDZE, 1, 0, 0, 8#150)}   % 1,,101/ TDZE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#700}                             % 1,,150/ 0,,700
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

tdza_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDZA, 1, 0, 0, 8#150)}   % 1,,101/ TDZA 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#700}                             % 1,,150/ 0,,700
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

tdzn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDZN, 1, 0, 0, 8#150)}   % 1,,101/ TDZN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#700}                             % 1,,150/ 0,,700
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDZN, 1, 0, 0, 8#150)}   % 1,,101/ TDZN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#070}                             % 1,,150/ 0,,070
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

%% TDC - Test Direct, Complement, and Skip if Condition Satisfied

tdc_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDC, 1, 0, 0, 8#150)}    % 1,,101/ TDC 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#770}                             % 1,,150/ 0,,770
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#077}                          % AC1 = 0,,077
         ]).

tdce_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDCE, 1, 0, 0, 8#150)}   % 1,,101/ TDCE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#070}                             % 1,,150/ 0,,070
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDCE, 1, 0, 0, 8#150)}   % 1,,101/ TDCE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#700}                             % 1,,150/ 0,,700
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

tdca_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDCA, 1, 0, 0, 8#150)}   % 1,,101/ TDCA 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#770}                             % 1,,150/ 0,,770
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#077}                          % AC1 = 0,,077
         ]).

tdcn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDCN, 1, 0, 0, 8#150)}   % 1,,101/ TDCN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#700}                             % 1,,150/ 0,,700
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDCN, 1, 0, 0, 8#150)}   % 1,,101/ TDCN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#070}                             % 1,,150/ 0,,070
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]).

%% TDO - Test Direct, Ones, and Skip if Condition Satisfied

tdo_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDO, 1, 0, 0, 8#150)}    % 1,,101/ TDO 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#070}                             % 1,,150/ 0,,070
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]).

tdoe_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDOE, 1, 0, 0, 8#150)}   % 1,,101/ TDOE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#070}                             % 1,,150/ 0,,070
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDOE, 1, 0, 0, 8#150)}   % 1,,101/ TDOE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#720}                             % 1,,150/ 0,,720
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]).

tdoa_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDOA, 1, 0, 0, 8#150)}   % 1,,101/ TDOA 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#720}                             % 1,,150/ 0,,720
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]).

tdon_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDON, 1, 0, 0, 8#150)}   % 1,,101/ TDON 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#720}                             % 1,,150/ 0,,720
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TDON, 1, 0, 0, 8#150)}   % 1,,101/ TDON 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#020}                             % 1,,150/ 0,,020
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]).

%% TSN - Test Swapped, No Modification, and Skip if Condition Satisfied

tsn_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSN, 1, 0, 0, 8#150)}    % 1,,101/ TSN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 0}                                 % 1,,150/ 0,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

tsne_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSNE, 1, 0, 0, 8#150)}   % 1,,101/ TSNE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#070, 0)}                 % 1,,150/ 070,,0
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSNE, 1, 0, 0, 8#150)}   % 1,,101/ TSNE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#707, 0)}                 % 1,,150/ 707,,0
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

tsna_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSNA, 1, 0, 0, 8#150)}   % 1,,101/ TSNA 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 0}                                 % 1,,150/ 0,,0
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

tsnn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSNN, 1, 0, 0, 8#150)}   % 1,,101/ TSNN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#707, 0)}                 % 1,,150/ 707,,0
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSNN, 1, 0, 0, 8#150)}   % 1,,101/ TSNN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#070, 0)}                 % 1,,150/ 070,,0
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

%% TSZ - Test Swapped, Zeros, and Skip if Condition Satisfied

tsz_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSZ, 1, 0, 0, 8#150)}    % 1,,101/ TSZ 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, ?COMMA2(8#700, 0)}                 % 1,,150/ 700,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

tsze_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSZE, 1, 0, 0, 8#150)}   % 1,,101/ TSZE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#070, 0)}                 % 1,,150/ 070,,0
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSZE, 1, 0, 0, 8#150)}   % 1,,101/ TSZE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#700, 0)}                 % 1,,150/ 700,,0
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

tsza_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSZA, 1, 0, 0, 8#150)}   % 1,,101/ TSZA 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#700, 0)}                 % 1,,150/ 700,,0
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

tszn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSZN, 1, 0, 0, 8#150)}   % 1,,101/ TSZN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#700, 0)}                 % 1,,150/ 700,,0
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSZN, 1, 0, 0, 8#150)}   % 1,,101/ TSZN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#070, 0)}                 % 1,,150/ 070,,0
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#707}                          % AC1 = 0,,707
         ]).

%% TSC - Test Swapped, Complement, and Skip if Condition Satisfied

tsc_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSC, 1, 0, 0, 8#150)}    % 1,,101/ TSC 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, ?COMMA2(8#770, 0)}                 % 1,,150/ 770,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#077}                          % AC1 = 0,,077
         ]).

tsce_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSCE, 1, 0, 0, 8#150)}   % 1,,101/ TSCE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#070, 0)}                 % 1,,150/ 070,,0
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSCE, 1, 0, 0, 8#150)}   % 1,,101/ TSCE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#700, 0)}                 % 1,,150/ 700,,0
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]).

tsca_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSCA, 1, 0, 0, 8#150)}   % 1,,101/ TSCA 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#770, 0)}                 % 1,,150/ 770,,0
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#077}                          % AC1 = 0,,077
         ]).

tscn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSCN, 1, 0, 0, 8#150)}   % 1,,101/ TSCN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#700, 0)}                 % 1,,150/ 700,,0
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#007}                          % AC1 = 0,,007
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSCN, 1, 0, 0, 8#150)}   % 1,,101/ TSCN 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#070, 0)}                 % 1,,150/ 070,,0
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]).

%% TSO - Test Swapped, Ones, and Skip if Condition Satisfied

tso_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSO, 1, 0, 0, 8#150)}    % 1,,101/ TSO 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, ?COMMA2(8#070, 0)}                 % 1,,150/ 070,,0
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]).

tsoe_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSOE, 1, 0, 0, 8#150)}   % 1,,101/ TSOE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#070, 0)}                 % 1,,150/ 070,,0
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#777}                          % AC1 = 0,,777
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSOE, 1, 0, 0, 8#150)}   % 1,,101/ TSOE 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#720, 0)}                 % 1,,150/ 720,,0
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]).

tsoa_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSOA, 1, 0, 0, 8#150)}   % 1,,101/ TSOA 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#720, 0)}                 % 1,,150/ 720,,0
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,      % skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]).

tson_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSON, 1, 0, 0, 8#150)}   % 1,,101/ TSON 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#720, 0)}                 % 1,,150/ 720,,0
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#707)}  % 1,,100/ MOVEI 1,707
    , {1, 8#101, ?INSN(?OP_TSON, 1, 0, 0, 8#150)}   % 1,,101/ TSON 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, ?COMMA2(8#020, 0)}                 % 1,,150/ 020,,0
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no skip
         [ {?AC(1), 8#727}                          % AC1 = 0,,727
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
init_acs([{AC, Val} | Rest], ACS) -> init_acs(Rest, setelement(AC + 1, ACS, Val)).
