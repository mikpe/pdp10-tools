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
%%% Test cases for 2.1 Full-Word Data Transmission

-module(sim_moves_tests).

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

-define(OP_INVALID, 0).
-define(OP_DMOVE, 8#120).
-define(OP_DMOVN, 8#121).
-define(OP_DMOVEM, 8#124).
-define(OP_DMOVNM, 8#125).
-define(OP_MOVE, 8#200).
-define(OP_MOVEI, 8#201).
-define(OP_MOVEM, 8#202).
-define(OP_MOVES, 8#203).
-define(OP_MOVS, 8#204).
-define(OP_MOVSI, 8#205).
-define(OP_MOVSM, 8#206).
-define(OP_MOVSS, 8#207).
-define(OP_MOVN, 8#210).
-define(OP_MOVNI, 8#211).
-define(OP_MOVNM, 8#212).
-define(OP_MOVNS, 8#213).
-define(OP_MOVM, 8#214).
-define(OP_MOVMI, 8#215).
-define(OP_MOVMM, 8#216).
-define(OP_MOVMS, 8#217).
-define(OP_EXCH, 8#250).
-define(OP_BLT, 8#251).

%% 2.1.1 Exchange Instruction ==================================================

exch_ac_mem_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#27)}   % 1,,100/ MOVEI 1,27
    , {1, 8#101, ?INSN(?OP_EXCH, 1, 0, 0, 8#200)}   % 1,,101/ EXCH 1,200
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#200, 8#42}                              % 1,,200/ 0,,42
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#42}     % AC1 = 42
         , {#ea{section = 1, offset = 8#200, islocal = false}, 8#27} % C(1,,200) = 27
         ]).

exch_ac_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#27)}   % 1,,100/ MOVEI 1,27
    , {1, 8#101, ?INSN(?OP_MOVEI, 2, 0, 0, 8#42)}   % 1,,101/ MOVEI 2,42
    , {1, 8#102, ?INSN(?OP_EXCH, 1, 0, 0, 2)}       % 1,,102/ EXCH 1,2
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#42} % AC1 = 42
         , {#ea{section = 1, offset = 2, islocal = false}, 8#27} % AC2 = 27
         ]).

%% 2.1.2 Move Instruction Class ================================================

move_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVE, 1, 0, 0, 8#150)}   % 1,,100/ MOVE 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, 8#42}                              % 1,,150/ 0,,42
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#42} % AC1 = 42
         ]).

movei_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#200)}  % 1,,100/ MOVEI 1,200
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    ],
  %% Note that the EA in 1,,100 evaluates to 1,,200 local, but only the
  %% in-section offset is loaded into AC1.
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#200} % AC1 = 200
         ]).

movem_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#42)}   % 1,,100/ MOVEI 1,42
    , {1, 8#101, ?INSN(?OP_MOVEM, 1, 0, 0, 8#150)}  % 1,,101/ MOVEM 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#27}                              % 1,,150/ 0,,27
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 8#42} % C(1,,150) = 42
         ]).

moves_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#27)}   % 1,,100/ MOVEI 1,27
    , {1, 8#101, ?INSN(?OP_MOVES, 1, 0, 0, 8#150)}  % 1,,101/ MOVES 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#42}                              % 1,,150/ 0,,42
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 8#42} % C(1,,150) = 42
         , {#ea{section = 1, offset = 1, islocal = false}, 8#42} % AC1 = 42
         ]).

moves_noac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 8#27)}   % 1,,100/ MOVEI 0,27
    , {1, 8#101, ?INSN(?OP_MOVES, 0, 0, 0, 8#150)}  % 1,,101/ MOVES 0,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#42}                              % 1,,150/ 0,,42
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 8#42} % C(1,,150) = 42
         , {#ea{section = 1, offset = 0, islocal = false}, 8#27} % AC0 = 27
         ]).

movs_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVS, 1, 0, 0, 8#150)}   % 1,,100/ MOVS 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, ?COMMA2(8#27, 8#42)}               % 1,,150/ 27,,42
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(8#42, 8#27)} % AC1 = 42,,27
         ]).

movsi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, -1)}     % 1,,100/ MOVSI 1,-1
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(-1, 0)} % AC1 = -1,,0
         ]).

movsm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#42)}   % 1,,100/ MOVEI 1,42
    , {1, 8#101, ?INSN(?OP_MOVSM, 1, 0, 0, 8#150)}  % 1,,101/ MOVSM 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#27}                              % 1,,150/ 0,,27
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, ?COMMA2(8#42, 0)} % C(1,,150) = 42,,0
         ]).

movss_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 0)}      % 1,,100/ MOVEI 1,0
    , {1, 8#101, ?INSN(?OP_MOVSS, 1, 0, 0, 8#150)}  % 1,,101/ MOVSS 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, ?COMMA2(8#27, 8#42)}               % 1,,150/ 27,,42
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, ?COMMA2(8#42, 8#27)} % C(1,,150) = 42,,27
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(8#42, 8#27)} % AC1 = 42,,27
         ]).

movss_noac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 0)}      % 1,,100/ MOVEI 0,0
    , {1, 8#101, ?INSN(?OP_MOVSS, 0, 0, 0, 8#150)}  % 1,,101/ MOVSS 0,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, ?COMMA2(8#27, 8#42)}               % 1,,150/ 27,,42
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, ?COMMA2(8#42, 8#27)} % C(1,,150) = 42,,27
         , {#ea{section = 1, offset = 0, islocal = false}, 0} % AC0 = 0
         ]).

movn_no_flags_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVN, 1, 0, 0, 8#150)}   % 1,,100/ MOVN 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, 8#42}                              % 1,,150/ 0,,42
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, -8#42 band ((1 bsl 36) - 1)} % AC1 = -42
         ]).

movn_zero_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVN, 1, 0, 0, 8#150)}   % 1,,100/ MOVN 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, 0}                                 % 1,,150/ 0
    ],
  Flags = ?DEFAULT_FLAGS bor (1 bsl ?PDP10_PF_CARRY_1) bor (1 bsl ?PDP10_PF_CARRY_0),
  expect(Prog, [], {1, 8#101}, Flags,
         [ {#ea{section = 1, offset = 1, islocal = false}, 0} % AC1 = 0
         ]).

movn_minint_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVN, 1, 0, 0, 8#150)}   % 1,,100/ MOVN 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, 1 bsl 35}                          % 1,,150/ 400000,,0
    ],
  Flags = ?DEFAULT_FLAGS bor (1 bsl ?PDP10_PF_OVERFLOW) bor (1 bsl ?PDP10_PF_CARRY_1),
  expect(Prog, [], {1, 8#101}, Flags,
         [ {#ea{section = 1, offset = 1, islocal = false}, 1 bsl 35} % AC1 = 400000,,0
         ]).

movni_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 8#42)}   % 1,,100/ MOVNI 1,42
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, (-8#42) band ((1 bsl 36) - 1)} % AC1 = -42
         ]).

movnm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#42)}   % 1,,100/ MOVEI 1,42
    , {1, 8#101, ?INSN(?OP_MOVNM, 1, 0, 0, 8#150)}  % 1,,101/ MOVNM 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#27}                              % 1,,150/ 0,,27
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, (-8#42) band ((1 bsl 36) - 1)} % C(1,,150) = -42
         ]).

movns_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 0)}      % 1,,100/ MOVEI 1,0
    , {1, 8#101, ?INSN(?OP_MOVNS, 1, 0, 0, 8#150)}  % 1,,101/ MOVNS 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#42}                              % 1,,150/ 0,,42
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, (-8#42) band ((1 bsl 36) - 1)} % C(1,,150) = -42
         , {#ea{section = 1, offset = 1, islocal = false}, (-8#42) band ((1 bsl 36) - 1)} % AC1 = -42
         ]).

movns_noac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 0)}      % 1,,100/ MOVEI 0,0
    , {1, 8#101, ?INSN(?OP_MOVNS, 0, 0, 0, 8#150)}  % 1,,101/ MOVNS 0,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#42}                              % 1,,150/ 0,,42
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, (-8#42) band ((1 bsl 36) - 1)} % C(1,,150) = -42
         , {#ea{section = 1, offset = 0, islocal = false}, 0} % AC0 = 0
         ]).

movm_positive_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVM, 1, 0, 0, 8#150)}   % 1,,100/ MOVM 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, 8#42}                              % 1,,150/ 0,,42
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#42} % AC1 = 42
         ]).

movm_zero_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVM, 1, 0, 0, 8#150)}   % 1,,100/ MOVM 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, 0}                                 % 1,,150/ 0,,0
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 0} % AC1 = 0
         ]).

movm_negative_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVM, 1, 0, 0, 8#150)}   % 1,,100/ MOVM 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, (-8#42) band ((1 bsl 36) -1)}      % 1,,150/ -42
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#42} % AC1 = 42
         ]).

movm_minint_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVM, 1, 0, 0, 8#150)}   % 1,,100/ MOVM 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, 1 bsl 35}                          % 1,,150/ 400000,,0
    ],
  Flags = ?DEFAULT_FLAGS bor (1 bsl ?PDP10_PF_OVERFLOW) bor (1 bsl ?PDP10_PF_CARRY_1),
  expect(Prog, [], {1, 8#101}, Flags,
         [ {#ea{section = 1, offset = 1, islocal = false}, 1 bsl 35} % AC1 = 400000,,0
         ]).

movmi_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVMI, 1, 0, 0, 8#200)}  % 1,,100/ MOVMI 1,200
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    ],
  %% Note that the EA in 1,,100 evaluates to 1,,200 local, but only the
  %% in-section offset is loaded into AC1.
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 8#200} % AC1 = 200
         ]).

movmm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 8#42)}   % 1,,100/ MOVNI 1,42
    , {1, 8#101, ?INSN(?OP_MOVMM, 1, 0, 0, 8#150)}  % 1,,101/ MOVMM 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, 8#27}                              % 1,,150/ 0,,27
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 8#42} % C(1,,150) = 42
         ]).

movms_ac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 0)}      % 1,,100/ MOVEI 1,0
    , {1, 8#101, ?INSN(?OP_MOVMS, 1, 0, 0, 8#150)}  % 1,,101/ MOVMS 1,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, (-8#42) band ((1 bsl 36) - 1)}     % 1,,150/ -42
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 8#42} % C(1,,150) = 42
         , {#ea{section = 1, offset = 1, islocal = false}, 8#42} % AC1 = 42
         ]).

movms_noac_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 0)}      % 1,,100/ MOVEI 0,0
    , {1, 8#101, ?INSN(?OP_MOVMS, 0, 0, 0, 8#150)}  % 1,,101/ MOVMS 0,150
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#150, (-8#42) band ((1 bsl 36) - 1)}     % 1,,150/ -42
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 8#42} % C(1,,150) = 42
         , {#ea{section = 1, offset = 0, islocal = false}, 0} % AC0 = 0
         ]).

%% 2.1.4 Double Move Instructions ==============================================

dmove_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_DMOVE, 8#17, 0, 0, 8#150)} % 1,,100/ DMOVE 17,150
    , {1, 8#101, ?INSN_INVALID}                       % 1,,101/ <invalid>
    , {1, 8#150, 8#42}                                % 1,,150/ 0,,42
    , {1, 8#151, 8#27}                                % 1,,151/ 0,,27
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#17, islocal = false}, 8#42} % AC17 = 42
         , {#ea{section = 1, offset = 0, islocal = false}, 8#27} % AC0 = 27
         ]).

dmovem_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 8#42)}   % 1,,100/ MOVEI 1,42
    , {1, 8#101, ?INSN(?OP_MOVEI, 2, 0, 0, 8#27)}   % 1,,101/ MOVEI 2,27
    , {1, 8#102, ?INSN(?OP_DMOVEM, 1, 0, 0, 8#150)} % 1,,102/ MOVEM 1,150
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 0}                                 % 1,,150/ 0
    , {1, 8#151, 0}                                 % 1,,151/ 0
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 8#42} % C(1,,150) = 42
         , {#ea{section = 1, offset = 8#151, islocal = false}, 8#27} % C(1,,151) = 27
         ]).

dmovn_no_flags_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_DMOVN, 1, 0, 0, 8#150)}  % 1,,100/ DMOVN 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, ?COMMA2(-1, -1)}                   % 1,,150/ -1
    , {1, 8#151, ?COMMA2(-1, -1)}                   % 1,,151/ -1
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 1, islocal = false}, 0} % AC1 = 0
         , {#ea{section = 1, offset = 2, islocal = false}, 1} % AC2 = 1
         ]).

dmovn_zero_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, -1)}     % 1,,100/ MOVEI 1,-1
    , {1, 8#101, ?INSN(?OP_MOVEI, 2, 0, 0, -1)}     % 1,,101/ MOVEI 2,-1
    , {1, 8#102, ?INSN(?OP_DMOVN, 1, 0, 0, 8#150)}  % 1,,102/ DMOVN 1,150
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 0}                                 % 1,,150/ 0
    , {1, 8#151, 0}                                 % 1,,151/ 0
    ],
  Flags = ?DEFAULT_FLAGS bor (1 bsl ?PDP10_PF_CARRY_1) bor (1 bsl ?PDP10_PF_CARRY_0),
  expect(Prog, [], {1, 8#103}, Flags,
         [ {#ea{section = 1, offset = 1, islocal = false}, 0} % AC1 = 0
         , {#ea{section = 1, offset = 2, islocal = false}, 0} % AC2 = 0
         ]).

dmovn_minint_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_DMOVN, 1, 0, 0, 8#150)}  % 1,,100/ DMOVN 1,150
    , {1, 8#101, ?INSN_INVALID}                     % 1,,101/ <invalid>
    , {1, 8#150, 1 bsl 35}                          % 1,,150/ 400000,,0
    , {1, 8#151, 0}                                 % 1,,151/ 0
    ],
  Flags = ?DEFAULT_FLAGS bor (1 bsl ?PDP10_PF_OVERFLOW) bor (1 bsl ?PDP10_PF_CARRY_1),
  expect(Prog, [], {1, 8#101}, Flags,
         [ {#ea{section = 1, offset = 1, islocal = false}, 1 bsl 35} % AC1 = 400000,,0
         , {#ea{section = 1, offset = 2, islocal = false}, 0} % AC2 = 0
         ]).

dmovnm_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 1)}      % 1,,100/ MOVNI 1,1
    , {1, 8#101, ?INSN(?OP_MOVNI, 2, 0, 0, 1)}      % 1,,101/ MOVNI 2,1
    , {1, 8#102, ?INSN(?OP_DMOVNM, 1, 0, 0, 8#150)} % 1,,102/ DMOVNM 1,150
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    , {1, 8#150, 8#27}                              % 1,,150/ 0,,27
    , {1, 8#151, 8#27}                              % 1,,150/ 0,,27
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,
         [ {#ea{section = 1, offset = 8#150, islocal = false}, 0} % C(1,,150) = 0
         , {#ea{section = 1, offset = 8#151, islocal = false}, 1} % C(1,,151) = 1
         ]).

%% 2.1.5 Block Transfers =======================================================

blt_bzero_test() ->
  %% First example in 2.1.5: using BLT to clear a block of words.
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 0)}     % 1,,100/ MOVEI 1,0
    , {1, 8#101, ?INSN(?OP_MOVEM, 1, 0, 0, 8#200)} % 1,,101/ MOVEM 1,200 ; clear 200
    , {1, 8#102, ?INSN(?OP_MOVE, 1, 0, 0, 8#150)}  % 1,,102/ MOVE 1,150 ; AC1 = 200,,201
    , {1, 8#103, ?INSN(?OP_BLT, 1, 0, 0, 8#203)}   % 1,,103/ BLT 1,203 ; clear 201-203
    , {1, 8#104, ?INSN_INVALID}                    % 1,,104/ <invalid>
    , {1, 8#150, ?COMMA2(8#200, 8#201)}            % 1,,150/ 200,,201
    , {1, 8#200, 1}                                % 1,,200/ 1 ; first to be cleared
    , {1, 8#201, 1}                                % 1,,201/ 1
    , {1, 8#202, 1}                                % 1,,202/ 1
    , {1, 8#203, 1}                                % 1,,203/ 1 ; last to be cleared
    , {1, 8#204, 1}                                % 1,,204/ 1
    ],
  expect(Prog, [], {1, 8#104}, ?DEFAULT_FLAGS,
         [ %% check that the four words at 200-203 were cleared
           {#ea{section = 1, offset = 8#200, islocal = false}, 0} % C(1,,200) = 0
         , {#ea{section = 1, offset = 8#201, islocal = false}, 0} % C(1,,201) = 0
         , {#ea{section = 1, offset = 8#202, islocal = false}, 0} % C(1,,202) = 0
         , {#ea{section = 1, offset = 8#203, islocal = false}, 0} % C(1,,203) = 0
           %% check that the next word was not overwritten
         , {#ea{section = 1, offset = 8#204, islocal = false}, 1} % C(1,,204) = 1
           %% check that AC1 contains the last transferred word's offsets + 1
         , {#ea{section = 1, offset = 1, islocal = false}, ?COMMA2(8#203, 8#204)} % AC1 = 203,,204
         ]).

blt_load_acs_test() ->
  %% Second example in 2.1.5: using BLT to load ACs from memory.
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVSI, 3, 0, 0, 8#200)} % 2,,100/ MOVSI 3,200 ; AC3 = 200,,0
    , {2, 8#101, ?INSN(?OP_BLT, 3, 0, 0, 3)}       % 2,,101/ BLT 3,3 ; load ACs 0-3 from 200-203
    , {2, 8#102, ?INSN_INVALID}                    % 2,,102/ <invalid>
    , {2, 8#200, 1}                                % 2,,200/ 1
    , {2, 8#201, 2}                                % 2,,201/ 2
    , {2, 8#202, 3}                                % 2,,202/ 3
    , {2, 8#203, 4}                                % 2,,203/ 4
    ],
  expect(Prog, [], {2, 8#102}, ?DEFAULT_FLAGS,
         [ %% check that ACs 0-3 were loaded from 200-203
           {#ea{section = 1, offset = 1, islocal = false}, 2} % AC1 = 2
         , {#ea{section = 1, offset = 2, islocal = false}, 3} % AC2 = 3
           %% this also checks that if the AC parameter to BLT is the last
           %% location to be copied, it still has that value and not the
           %% last transferred word's offsets + 1 (204,,4 here)
         , {#ea{section = 1, offset = 3, islocal = false}, 4} % AC3 = 4
           %% check that the next AC was not overwritten
         , {#ea{section = 1, offset = 4, islocal = false}, 0} % AC4 = 0
         ]).

blt_store_acs_test() ->
  %% Third example in 2.1.5: using BLT to store ACs in memory.
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVEI, 4, 0, 0, 8#200)} % 2,,100/ MOVEI 4,200 ; AC4 = 0,,200
    , {2, 8#101, ?INSN(?OP_BLT, 4, 0, 0, 8#203)}   % 2,,101/ BLT 4,203 ; store ACs 0-3 in 200-203
    , {2, 8#102, ?INSN_INVALID}                    % 2,,102/ <invalid>
    , {2, 8#200, 1}                                % 2,,200/ 1
    , {2, 8#201, 1}                                % 2,,201/ 1
    , {2, 8#202, 1}                                % 2,,202/ 1
    , {2, 8#203, 1}                                % 2,,203/ 1
    , {2, 8#204, 8#42}                             % 2,,204/ 42
    ],
  expect(Prog, [], {2, 8#102}, ?DEFAULT_FLAGS,
         [ %% check that ACs 0-3 were stored to 200-203
           {#ea{section = 2, offset = 8#200, islocal = false}, 0} % C(2,,200) = 0
         , {#ea{section = 2, offset = 8#201, islocal = false}, 0} % C(2,,201) = 0
         , {#ea{section = 2, offset = 8#202, islocal = false}, 0} % C(2,,202) = 0
         , {#ea{section = 2, offset = 8#203, islocal = false}, 0} % C(2,,203) = 0
           %% check that the next word was not overwritten
         , {#ea{section = 2, offset = 8#204, islocal = false}, 8#42} % C(2,,204) = 42
           %% check that AC4 contains the last transferred word's offsets + 1
         , {#ea{section = 1, offset = 4, islocal = false}, ?COMMA2(4, 8#204)} % AC4 = 4,,204
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
