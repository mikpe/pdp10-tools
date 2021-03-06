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
%%% Test cases for 2.6 Arithmetic Testing

-module(sim_arithmetic_tests).

-include("../src/sim_core.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_FLAGS, (1 bsl ?PDP10_PF_USER)).
-define(CARRY_FLAGS, % for AOJ/AOS/SOJ/SOS when carry 0 and 1 get set
        (?DEFAULT_FLAGS bor
         (1 bsl ?PDP10_PF_CARRY_0) bor
         (1 bsl ?PDP10_PF_CARRY_1))).

-define(LOW18(X), ((X) band ((1 bsl 18) - 1))).
-define(LOW36(X), ((X) band ((1 bsl 36) - 1))).

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

-define(OP_MOVEI,  8#201).
-define(OP_MOVSI,  8#205).
-define(OP_MOVNI,  8#211).
-define(OP_AOBJP,  8#252).
-define(OP_AOBJN,  8#253).
-define(OP_CAI,    8#300).
-define(OP_CAIL,   8#301).
-define(OP_CAIE,   8#302).
-define(OP_CAILE,  8#303).
-define(OP_CAIA,   8#304).
-define(OP_CAIGE,  8#305).
-define(OP_CAIN,   8#306).
-define(OP_CAIG,   8#307).
-define(OP_CAM,    8#310).
-define(OP_CAML,   8#311).
-define(OP_CAME,   8#312).
-define(OP_CAMLE,  8#313).
-define(OP_CAMA,   8#314).
-define(OP_CAMGE,  8#315).
-define(OP_CAMN,   8#316).
-define(OP_CAMG,   8#317).
-define(OP_JUMP,   8#320).
-define(OP_JUMPL,  8#321).
-define(OP_JUMPE,  8#322).
-define(OP_JUMPLE, 8#323).
-define(OP_JUMPA,  8#324).
-define(OP_JUMPGE, 8#325).
-define(OP_JUMPN,  8#326).
-define(OP_JUMPG,  8#327).
-define(OP_SKIP,   8#330).
-define(OP_SKIPL,  8#331).
-define(OP_SKIPE,  8#332).
-define(OP_SKIPLE, 8#333).
-define(OP_SKIPA,  8#334).
-define(OP_SKIPGE, 8#335).
-define(OP_SKIPN,  8#336).
-define(OP_SKIPG,  8#337).
-define(OP_AOJ,    8#340).
-define(OP_AOJL,   8#341).
-define(OP_AOJE,   8#342).
-define(OP_AOJLE,  8#343).
-define(OP_AOJA,   8#344).
-define(OP_AOJGE,  8#345).
-define(OP_AOJN,   8#346).
-define(OP_AOJG,   8#347).
-define(OP_AOS,    8#350).
-define(OP_AOSL,   8#351).
-define(OP_AOSE,   8#352).
-define(OP_AOSLE,  8#353).
-define(OP_AOSA,   8#354).
-define(OP_AOSGE,  8#355).
-define(OP_AOSN,   8#356).
-define(OP_AOSG,   8#357).
-define(OP_SOJ,    8#360).
-define(OP_SOJL,   8#361).
-define(OP_SOJE,   8#362).
-define(OP_SOJLE,  8#363).
-define(OP_SOJA,   8#364).
-define(OP_SOJGE,  8#365).
-define(OP_SOJN,   8#366).
-define(OP_SOJG,   8#367).
-define(OP_SOS,    8#370).
-define(OP_SOSL,   8#371).
-define(OP_SOSE,   8#372).
-define(OP_SOSLE,  8#373).
-define(OP_SOSA,   8#374).
-define(OP_SOSGE,  8#375).
-define(OP_SOSN,   8#376).
-define(OP_SOSG,   8#377).

%% 2.6.1 Add One to Both Halves of AC and Jump =================================

%% AOBJP - Add One to Both Halves of AC and Jump if Positive

aobjp_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, -2)}     % 1,,100/ MOVSI 1,-2
    , {1, 8#101, ?INSN(?OP_AOBJP, 1, 0, 0, 8#103)}  % 1,,101/ AOBJP 1,103
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no jump
         [{?AC(1), ?COMMA2(-1, 1)}                  % AC1 = -1,,1
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, -1)}     % 1,,100/ MOVSI 1,-1
    , {1, 8#101, ?INSN(?OP_AOBJP, 1, 0, 0, 8#103)}  % 1,,101/ AOBJP 1,103
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#103}, ?DEFAULT_FLAGS,     % jump
         [{?AC(1), ?COMMA2(0, 1)}                   % AC1 = 0,,1
         ]).

%% AOBJN - Add One to Both Halves of AC and Jump if Negative

aobjn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, -2)}     % 1,,100/ MOVSI 1,-2
    , {1, 8#101, ?INSN(?OP_AOBJN, 1, 0, 0, 8#103)}  % 1,,101/ AOBJN 1,103
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,     % jump
         [{?AC(1), ?COMMA2(-1, 1)}                  % AC1 = -1,,1
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVSI, 1, 0, 0, -1)}     % 1,,100/ MOVSI 1,-1
    , {1, 8#101, ?INSN(?OP_AOBJN, 1, 0, 0, 8#103)}  % 1,,101/ AOBJN 1,103
    , {1, 8#102, ?INSN_INVALID}                     % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                     % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,     % no jump
         [{?AC(1), ?COMMA2(0, 1)}                   % AC1 = 0,,1
         ]).

%% 2.6.2 Comparisons, Skips, and Jumps =========================================

%% CAI - Compare AC Immediate and Skip if Condition Satisfied

cai_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_CAI, 0, 0, 0, 0)}         % 1,,100/ CAI
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS, []).  % no skip

cail_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_CAIL, 1, 0, 0, 2)}        % 1,,101/ CAIL 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAIL, 1, 0, 0, 2)}        % 1,,101/ CAIL 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

caie_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_CAIE, 1, 0, 0, 2)}        % 1,,101/ CAIE 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAIE, 1, 0, 0, 2)}        % 1,,101/ CAIE 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

caile_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_CAILE, 1, 0, 0, 2)}       % 1,,101/ CAILE 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_CAILE, 1, 0, 0, 2)}       % 1,,101/ CAILE 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAILE, 1, 0, 0, 2)}       % 1,,101/ CAILE 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

caia_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_CAIA, 0, 0, 0, 0)}        % 1,,100/ CAIA
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS, []).  % skip

caige_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAIGE, 1, 0, 0, 2)}       % 1,,101/ CAIGE 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_CAIGE, 1, 0, 0, 2)}       % 1,,101/ CAIGE 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_CAIGE, 1, 0, 0, 2)}       % 1,,101/ CAIGE 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

cain_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAIN, 1, 0, 0, 2)}        % 1,,101/ CAIN 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_CAIN, 1, 0, 0, 2)}        % 1,,101/ CAIN 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

caig_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAIG, 1, 0, 0, 2)}        % 1,,101/ CAIG 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_CAIG, 1, 0, 0, 2)}        % 1,,101/ CAIG 1,2
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

%% CAM - Compare AC with Memory and Skip if Condition Satisfied

cam_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_CAM, 0, 0, 0, 8#150)}     % 1,,100/ CAM 150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS, []).  % no skip

caml_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_CAML, 1, 0, 0, 8#150)}    % 1,,101/ CAML 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAML, 1, 0, 0, 8#150)}    % 1,,101/ CAML 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

came_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_CAME, 1, 0, 0, 8#150)}    % 1,,101/ CAME 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAME, 1, 0, 0, 8#150)}    % 1,,101/ CAME 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

camle_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_CAMLE, 1, 0, 0, 8#150)}   % 1,,101/ CAMLE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_CAMLE, 1, 0, 0, 8#150)}   % 1,,101/ CAMLE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog2, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAMLE, 1, 0, 0, 8#150)}   % 1,,101/ CAMLE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

cama_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_CAMA, 0, 0, 0, 8#150)}    % 1,,100/ CAMA 150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS, []).  % skip

camge_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAMGE, 1, 0, 0, 8#150)}   % 1,,101/ CAMGE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_CAMGE, 1, 0, 0, 8#150)}   % 1,,101/ CAMGE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog2, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_CAMGE, 1, 0, 0, 8#150)}   % 1,,101/ CAMGE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

camn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAMN, 1, 0, 0, 8#150)}    % 1,,101/ CAMN 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_CAMN, 1, 0, 0, 8#150)}    % 1,,101/ CAMN 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

camg_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_CAMG, 1, 0, 0, 8#150)}    % 1,,101/ CAMG 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_CAMG, 1, 0, 0, 8#150)}    % 1,,101/ CAMG 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no skip

%% JUMP - Jump if AC Condition Satisfied

jump_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_JUMP, 0, 0, 0, 0)}        % 1,,100/ JUMP
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    ],
  expect(Prog, [], {1, 8#101}, ?DEFAULT_FLAGS, []).  % no jump

jumpl_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_JUMPL, 1, 0, 0, 8#103)}   % 1,,101/ JUMPL 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % jump
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_JUMPL, 1, 0, 0, 8#103)}   % 1,,101/ JUMPL 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no jump

jumpe_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 0)}       % 1,,100/ MOVEI 1,0
    , {1, 8#101, ?INSN(?OP_JUMPE, 1, 0, 0, 8#103)}   % 1,,101/ JUMPE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % jump
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_JUMPE, 1, 0, 0, 8#103)}   % 1,,101/ JUMPE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no jump

jumple_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_JUMPLE, 1, 0, 0, 8#103)}  % 1,,101/ JUMPLE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % jump
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 0)}       % 1,,100/ MOVEI 1,0
    , {1, 8#101, ?INSN(?OP_JUMPLE, 1, 0, 0, 8#103)}  % 1,,101/ JUMPLE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % jump
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_JUMPLE, 1, 0, 0, 8#103)}  % 1,,101/ JUMPLE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no jump

jumpa_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_JUMPA, 0, 0, 0, 8#102)}   % 1,,100/ JUMPA 102
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS, []).  % jump

jumpge_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_JUMPGE, 1, 0, 0, 8#103)}  % 1,,101/ JUMPGE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % jump
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 0)}       % 1,,100/ MOVEI 1,0
    , {1, 8#101, ?INSN(?OP_JUMPGE, 1, 0, 0, 8#103)}  % 1,,101/ JUMPGE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % jump
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_JUMPGE, 1, 0, 0, 8#103)}  % 1,,101/ JUMPGE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no jump

jumpn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_JUMPN, 1, 0, 0, 8#103)}   % 1,,101/ JUMPN 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % jump
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 0)}       % 1,,100/ MOVEI 1,0
    , {1, 8#101, ?INSN(?OP_JUMPN, 1, 0, 0, 8#103)}   % 1,,101/ JUMPN 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no jump

jumpg_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 3)}       % 1,,100/ MOVEI 1,3
    , {1, 8#101, ?INSN(?OP_JUMPG, 1, 0, 0, 8#103)}   % 1,,101/ JUMPG 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS, []), % jump
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 3)}       % 1,,100/ MOVNI 1,3
    , {1, 8#101, ?INSN(?OP_JUMPG, 1, 0, 0, 8#103)}   % 1,,101/ JUMPG 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS, []). % no jump

%% SKIP - Skip if Memory Condition Satisfied

skip_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 8#42)}    % 1,,100/ MOVEI 0,42
    , {1, 8#101, ?INSN(?OP_SKIP, 0, 0, 0, 8#150)}    % 1,,101/ SKIP 150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 2
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,       % no skip
         [{?AC(0), 8#42}]).                          % AC(0) = 42

skipl_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_SKIPL, 1, 0, 0, 8#150)}   % 1,,100/ SKIPL 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog1, [], {1, 8#102}, ?DEFAULT_FLAGS,      % skip
         [{?AC(1), ?LOW36(-2)}]),                    % AC1 = -2
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_SKIPL, 1, 0, 0, 8#150)}   % 1,,100/ SKIPL 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 2
    ],
  expect(Prog2, [], {1, 8#101}, ?DEFAULT_FLAGS,      % no skip
         [{?AC(1), 2}]).                             % AC1 = 2

skipe_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_SKIPE, 1, 0, 0, 8#150)}   % 1,,100/ SKIPE 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 0}                                  % 1,,150/ 0
    ],
  expect(Prog1, [], {1, 8#102}, ?DEFAULT_FLAGS, []), % skip
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_SKIPE, 1, 0, 0, 8#150)}   % 1,,100/ SKIPE 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 0,,2
    ],
  expect(Prog2, [], {1, 8#101}, ?DEFAULT_FLAGS,      % no skip
        [{?AC(1), 2}]).                              % AC1 = 2

skiple_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_SKIPLE, 1, 0, 0, 8#150)}  % 1,,100/ SKIPLE 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog1, [], {1, 8#102}, ?DEFAULT_FLAGS,      %
         [{?AC(1), ?LOW36(-2)}]),                    % AC1 = -2
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_SKIPLE, 1, 0, 0, 8#150)}  % 1,,100/ SKIPLE 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 0}                                  % 1,,150/ 0
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,      % skip
         [{?AC(1), 0}]),                             % AC1 = 0
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_SKIPLE, 1, 0, 0, 8#150)}  % 1,,100/ SKIPLE 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 2
    ],
  expect(Prog3, [], {1, 8#101}, ?DEFAULT_FLAGS,      % no skip
         [{?AC(1), 2}]).                             % AC1 = 2

skipa_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_SKIPA, 1, 0, 0, 8#150)}   % 1,,100/ SKIPA 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 2
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,       % skip
         [{?AC(1), 2}]).                             % AC1 = 2

skipge_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_SKIPGE, 1, 0, 0, 8#150)}  % 1,,100/ SKIPGE 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 2
    ],
  expect(Prog1, [], {1, 8#102}, ?DEFAULT_FLAGS,      % skip
         [{?AC(1), 2}]),                             % AC1 = 2
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_SKIPGE, 1, 0, 0, 8#150)}  % 1,,100/ SKIPGE 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 0}                                  % 1,,150/ 0
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,      % skip
         [{?AC(1), 0}]),                             % AC1 = 0
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_SKIPGE, 1, 0, 0, 8#150)}  % 1,,100/ SKIPGE 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog3, [], {1, 8#101}, ?DEFAULT_FLAGS,      % no skip
         [{?AC(1), ?LOW36(-2)}]).                    % AC1 = -2

skipn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_SKIPN, 1, 0, 0, 8#150)}   % 1,,100/ SKIPN 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 2
    ],
  expect(Prog1, [], {1, 8#102}, ?DEFAULT_FLAGS,      % skip
         [{?AC(1), 2}]),                             % AC(1) = 2
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_SKIPN, 1, 0, 0, 8#150)}   % 1,,100/ SKIPN 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 0}                                  % 1,,150/ 0
    ],
  expect(Prog2, [], {1, 8#101}, ?DEFAULT_FLAGS,      % no skip
         [{?AC(1), 0}]).                             % AC(1) = 0

skipg_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_SKIPG, 1, 0, 0, 8#150)}   % 1,,100/ SKIPG 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, 2}                                  % 1,,150/ 2
    ],
  expect(Prog1, [], {1, 8#102}, ?DEFAULT_FLAGS,      % skip
         [{?AC(1), 2}]),                             % AC(1) = 2
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_SKIPG, 1, 0, 0, 8#150)}   % 1,,100/ SKIPG 1,150
    , {1, 8#101, ?INSN_INVALID}                      % 1,,101/ <invalid>
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog2, [], {1, 8#101}, ?DEFAULT_FLAGS,      % no skip
         [{?AC(1), ?LOW36(-2)}]).                    % AC(1) = -2

%% AOJ - Add One to AC and Jump if Condition Satisfied

aoj_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_AOJ, 1, 0, 0, 0)}         % 1,,101/ AOJ 1,
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,       % no jump
         [ {?AC(1), 3}]).                            % AC(1) = 3

aojl_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 2)}       % 1,,100/ MOVNI 1,2
    , {1, 8#101, ?INSN(?OP_AOJL, 1, 0, 0, 8#103)}    % 1,,101/ AOJL 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump
         [ {?AC(1), ?LOW36(-1)}]),                   % AC(1) = -1
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_AOJL, 1, 0, 0, 8#103)}    % 1,,101/ AOJL 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump
         [ {?AC(1), 3}]).                            % AC(1) = 3

aoje_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 1)}       % 1,,100/ MOVNI 1,1
    , {1, 8#101, ?INSN(?OP_AOJE, 1, 0, 0, 8#103)}    % 1,,101/ AOJE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?AC(1), 0}]),                            % AC(1) = 0
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_AOJE, 1, 0, 0, 8#103)}    % 1,,101/ AOJE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump
         [ {?AC(1), 3}]).                            % AC(1) = 3

aojle_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 2)}       % 1,,100/ MOVNI 1,2
    , {1, 8#101, ?INSN(?OP_AOJLE, 1, 0, 0, 8#103)}   % 1,,101/ AOJLE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump
         [ {?AC(1), ?LOW36(-1)}]),                   % AC(1) = -1
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 1)}       % 1,,100/ MOVNI 1,1
    , {1, 8#101, ?INSN(?OP_AOJLE, 1, 0, 0, 8#103)}   % 1,,101/ AOJLE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?AC(1), 0}]),                            % AC(1) = 0
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_AOJLE, 1, 0, 0, 8#103)}   % 1,,101/ AOJLE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump
         [ {?AC(1), 3}]).                            % AC(1) = 3

aoja_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_AOJA, 1, 0, 0, 8#103)}    % 1,,101/ AOJA 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,       % jump
         [ {?AC(1), 3}]).                            % AC(1) = 3

aojge_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_AOJGE, 1, 0, 0, 8#103)}   % 1,,101/ AOJGE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump
         [ {?AC(1), 3}]),                            % AC(1) = 3
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 1)}       % 1,,100/ MOVNI 1,1
    , {1, 8#101, ?INSN(?OP_AOJGE, 1, 0, 0, 8#103)}   % 1,,101/ AOJGE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?AC(1), 0}]),                            % AC(1) = 0
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 2)}       % 1,,100/ MOVNI 1,2
    , {1, 8#101, ?INSN(?OP_AOJGE, 1, 0, 0, 8#103)}   % 1,,101/ AOJGE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump
         [ {?AC(1), ?LOW36(-1)}]).                   % AC(1) = -1

aojn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_AOJN, 1, 0, 0, 8#103)}    % 1,,101/ AOJN 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump
         [ {?AC(1), 3}]),                            % AC(1) = 3
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 1)}       % 1,,100/ MOVNI 1,1
    , {1, 8#101, ?INSN(?OP_AOJN, 1, 0, 0, 8#103)}    % 1,,101/ AOJN 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?AC(1), 0}]).                            % AC(1) = 0

aojg_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_AOJG, 1, 0, 0, 8#103)}    % 1,,101/ AOJG 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump
         [ {?AC(1), 3}]),                            % AC(1) = 3
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 2)}       % 1,,100/ MOVNI 1,2
    , {1, 8#101, ?INSN(?OP_AOJG, 1, 0, 0, 8#103)}    % 1,,101/ AOJG 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump
         [ {?AC(1), ?LOW36(-1)}]).                   % AC(1) = -1

%% AOS - Add One to Memory and Skip if Condition Satisfied

aos_test() ->
  Prog =
    [ {1, 8#101, ?INSN(?OP_AOS, 1, 0, 0, 8#150)}     % 1,,101/ AOS 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog, [], {1, 8#102}, ?DEFAULT_FLAGS,       % no jump
         [ {?EA(1, 8#150), ?LOW36(3)}                % C(1,,150) = 3
         , {?AC(1), ?LOW36(3)}]).                    % AC(1) = 3

aosl_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_AOSL, 1, 0, 0, 8#150)}    % 1,,101/ AOSL 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump
         [ {?EA(1, 8#150), ?LOW36(-1)}               % C(1,,150) = -1
         , {?AC(1), ?LOW36(-1)}]),                   % AC(1) = -1
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_AOSL, 1, 0, 0, 8#150)}    % 1,,101/ AOSL 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump
         [ {?EA(1, 8#150), ?LOW36(3)}                % C(1,,150) = 3
         , {?AC(1), ?LOW36(3)}]).                    % AC(1) = 3

aose_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_AOSE, 1, 0, 0, 8#150)}    % 1,,101/ AOSE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-1)}                         % 1,,150/ -1
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(0)}                % C(1,,150) = 0
         , {?AC(1), ?LOW36(0)}]),                    % AC(1) = 0
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_AOSE, 1, 0, 0, 8#150)}    % 1,,101/ AOSE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump
         [ {?EA(1, 8#150), ?LOW36(3)}                % C(1,,150) = 3
         , {?AC(1), ?LOW36(3)}]).                    % AC(1) = 3

aosle_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_AOSLE, 1, 0, 0, 8#150)}   % 1,,101/ AOSLE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump
         [ {?EA(1, 8#150), ?LOW36(-1)}               % C(1,,150) = -1
         , {?AC(1), ?LOW36(-1)}]),                   % AC(1) = -1
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_AOSLE, 1, 0, 0, 8#150)}   % 1,,101/ AOSLE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-1)}                         % 1,,150/ -1
    ],
  expect(Prog2, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(0)}                % C(1,,150) = 0
         , {?AC(1), ?LOW36(0)}]),                    % AC(1) = 0
  Prog3 =
    [ {1, 8#101, ?INSN(?OP_AOSLE, 1, 0, 0, 8#150)}   % 1,,101/ AOSLE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump
         [ {?EA(1, 8#150), ?LOW36(3)}                % C(1,,150) = 3
         , {?AC(1), ?LOW36(3)}]).                    % AC(1) = 3

aosa_test() ->
  Prog =
    [ {1, 8#101, ?INSN(?OP_AOSA, 1, 0, 0, 8#150)}    % 1,,101/ AOSA 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog, [], {1, 8#103}, ?DEFAULT_FLAGS,       % jump
         [ {?EA(1, 8#150), ?LOW36(3)}                % C(1,,150) = 3
         , {?AC(1), ?LOW36(3)}]).                    % AC(1) = 3

aosge_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_AOSGE, 1, 0, 0, 8#150)}   % 1,,101/ AOSGE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump
         [ {?EA(1, 8#150), ?LOW36(3)}                % C(1,,150) = 3
         , {?AC(1), ?LOW36(3)}]),                    % AC(1) = 3
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_AOSGE, 1, 0, 0, 8#150)}   % 1,,101/ AOSGE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-1)}                         % 1,,150/ -1
    ],
  expect(Prog2, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(0)}                % C(1,,150) = 0
         , {?AC(1), ?LOW36(0)}]),                    % AC(1) = 0
  Prog3 =
    [ {1, 8#101, ?INSN(?OP_AOSGE, 1, 0, 0, 8#150)}   % 1,,101/ AOSGE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump
         [ {?EA(1, 8#150), ?LOW36(-1)}               % C(1,,150) = -1
         , {?AC(1), ?LOW36(-1)}]).                   % AC(1) = -1

aosn_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_AOSN, 1, 0, 0, 8#150)}    % 1,,101/ AOSN 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump
         [ {?EA(1, 8#150), ?LOW36(3)}                % C(1,,150) = 3
         , {?AC(1), ?LOW36(3)}]),                    % AC(1) = 3
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_AOSN, 1, 0, 0, 8#150)}    % 1,,101/ AOSN 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-1)}                         % 1,,150/ -1
    ],
  expect(Prog2, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(0)}                % C(1,,150) = 0
         , {?AC(1), ?LOW36(0)}]).                    % AC(1) = 0

aosg_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_AOSG, 1, 0, 0, 8#150)}    % 1,,101/ AOSG 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump
         [ {?EA(1, 8#150), ?LOW36(3)}                % C(1,,150) = 3
         , {?AC(1), ?LOW36(3)}]),                    % AC(1) = 3
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_AOSG, 1, 0, 0, 8#150)}    % 1,,101/ AOSG 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog2, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump
         [ {?EA(1, 8#150), ?LOW36(-1)}               % C(1,,150) = -1
         , {?AC(1), ?LOW36(-1)}]).                   % AC(1) = -1

%% SOJ - Subtract One from AC and Jump if Condition Satisfied

soj_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_SOJ, 1, 0, 0, 0)}         % 1,,101/ SOJ 1,
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    ],
  expect(Prog, [], {1, 8#102}, ?CARRY_FLAGS,         % no jump, carry 0 and 1
         [ {?AC(1), 1}]).                            % AC(1) = 1

sojl_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 2)}       % 1,,100/ MOVNI 1,2
    , {1, 8#101, ?INSN(?OP_SOJL, 1, 0, 0, 8#103)}    % 1,,101/ SOJL 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?AC(1), ?LOW36(-3)}]),                   % AC(1) = -3
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_SOJL, 1, 0, 0, 8#103)}    % 1,,101/ SOJL 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?AC(1), 1}]).                            % AC(1) = 1

soje_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}       % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_SOJE, 1, 0, 0, 8#103)}    % 1,,101/ SOJE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?AC(1), 0}]),                            % AC(1) = 0
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_SOJE, 1, 0, 0, 8#103)}    % 1,,101/ SOJE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?AC(1), 1}]).                            % AC(1) = 1

sojle_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 0)}       % 1,,100/ MOVEI 1,0
    , {1, 8#101, ?INSN(?OP_SOJLE, 1, 0, 0, 8#103)}   % 1,,101/ SOJLE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump, no carry flags
         [ {?AC(1), ?LOW36(-1)}]),                   % AC(1) = -1
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}       % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_SOJLE, 1, 0, 0, 8#103)}   % 1,,101/ SOJLE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?AC(1), 0}]),                            % AC(1) = 0
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_SOJLE, 1, 0, 0, 8#103)}   % 1,,101/ SOJLE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog3, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?AC(1), 1}]).                            % AC(1) = 1

soja_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_SOJA, 1, 0, 0, 8#103)}    % 1,,101/ SOJA 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog, [], {1, 8#103}, ?CARRY_FLAGS,         % jump, carry 0 and 1
         [ {?AC(1), 1}]).                            % AC(1) = 1

sojge_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_SOJGE, 1, 0, 0, 8#103)}   % 1,,101/ SOJGE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?AC(1), 1}]),                            % AC(1) = 1
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}       % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_SOJGE, 1, 0, 0, 8#103)}   % 1,,101/ SOJGE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?AC(1), 0}]),                            % AC(1) = 0
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 0)}       % 1,,100/ MOVEI 1,0
    , {1, 8#101, ?INSN(?OP_SOJGE, 1, 0, 0, 8#103)}   % 1,,101/ SOJGE 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump, no carry flags
         [ {?AC(1), ?LOW36(-1)}]).                   % AC(1) = -1

sojn_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_SOJN, 1, 0, 0, 8#103)}    % 1,,101/ SOJN 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?AC(1), 1}]),                            % AC(1) = 1
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 1)}       % 1,,100/ MOVEI 1,1
    , {1, 8#101, ?INSN(?OP_SOJN, 1, 0, 0, 8#103)}    % 1,,101/ SOJN 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?AC(1), 0}]).                            % AC(1) = 0

sojg_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_MOVEI, 1, 0, 0, 2)}       % 1,,100/ MOVEI 1,2
    , {1, 8#101, ?INSN(?OP_SOJG, 1, 0, 0, 8#103)}    % 1,,101/ SOJG 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?AC(1), 1}]),                            % AC(1) = 1
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_MOVNI, 1, 0, 0, 2)}       % 1,,100/ MOVNI 1,2
    , {1, 8#101, ?INSN(?OP_SOJG, 1, 0, 0, 8#103)}    % 1,,101/ SOJG 1,103
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    ],
  expect(Prog2, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?AC(1), ?LOW36(-3)}]).                   % AC(1) = -3

%% SOS - Subtract One from Memory and Skip if Condition Satisfied

sos_test() ->
  Prog =
    [ {1, 8#101, ?INSN(?OP_SOS, 1, 0, 0, 8#150)}     % 1,,101/ SOS 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog, [], {1, 8#102}, ?CARRY_FLAGS,         % no jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(1)}                % C(1,,150) = 1
         , {?AC(1), ?LOW36(1)}]).                    % AC(1) = 1

sosl_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_SOSL, 1, 0, 0, 8#150)}    % 1,,101/ SOSL 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(-3)}               % C(1,,150) = -3
         , {?AC(1), ?LOW36(-3)}]),                   % AC(1) = -3
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_SOSL, 1, 0, 0, 8#150)}    % 1,,101/ SOSL 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog2, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(1)}                % C(1,,150) = 1
         , {?AC(1), ?LOW36(1)}]).                    % AC(1) = 1

sose_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_SOSE, 1, 0, 0, 8#150)}    % 1,,101/ SOSE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(1)}                          % 1,,150/ 1
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(0)}                % C(1,,150) = 0
         , {?AC(1), ?LOW36(0)}]),                    % AC(1) = 0
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_SOSE, 1, 0, 0, 8#150)}    % 1,,101/ SOSE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog2, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(1)}                % C(1,,150) = 1
         , {?AC(1), ?LOW36(1)}]).                    % AC(1) = 1

sosle_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_SOSLE, 1, 0, 0, 8#150)}   % 1,,101/ SOSLE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(0)}                          % 1,,150/ 0
    ],
  expect(Prog1, [], {1, 8#103}, ?DEFAULT_FLAGS,      % jump, no carry flags
         [ {?EA(1, 8#150), ?LOW36(-1)}               % C(1,,150) = -1
         , {?AC(1), ?LOW36(-1)}]),                   % AC(1) = -1
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_SOSLE, 1, 0, 0, 8#150)}   % 1,,101/ SOSLE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(1)}                          % 1,,150/ 1
    ],
  expect(Prog2, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(0)}                % C(1,,150) = 0
         , {?AC(1), ?LOW36(0)}]),                    % AC(1) = 0
  Prog3 =
    [ {1, 8#101, ?INSN(?OP_SOSLE, 1, 0, 0, 8#150)}   % 1,,101/ SOSLE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog3, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(1)}                % C(1,,150) = 1
         , {?AC(1), ?LOW36(1)}]).                    % AC(1) = 1

sosa_test() ->
  Prog =
    [ {1, 8#101, ?INSN(?OP_SOSA, 1, 0, 0, 8#150)}    % 1,,101/ SOSA 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog, [], {1, 8#103}, ?CARRY_FLAGS,         % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(1)}                % C(1,,150) = 1
         , {?AC(1), ?LOW36(1)}]).                    % AC(1) = 1

sosge_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_SOSGE, 1, 0, 0, 8#150)}   % 1,,101/ SOSGE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(1)}                % C(1,,150) = 1
         , {?AC(1), ?LOW36(1)}]),                    % AC(1) = 1
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_SOSGE, 1, 0, 0, 8#150)}   % 1,,101/ SOSGE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(1)}                          % 1,,150/ 1
    ],
  expect(Prog2, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(0)}                % C(1,,150) = 0
         , {?AC(1), ?LOW36(0)}]),                    % AC(1) = 0
  Prog3 =
    [ {1, 8#101, ?INSN(?OP_SOSGE, 1, 0, 0, 8#150)}   % 1,,101/ SOSGE 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(0)}                          % 1,,150/ 0
    ],
  expect(Prog3, [], {1, 8#102}, ?DEFAULT_FLAGS,      % no jump, no carry flags
         [ {?EA(1, 8#150), ?LOW36(-1)}               % C(1,,150) = -1
         , {?AC(1), ?LOW36(-1)}]).                   % AC(1) = -1

sosn_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_SOSN, 1, 0, 0, 8#150)}    % 1,,101/ SOSN 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(1)}                % C(1,,150) = 1
         , {?AC(1), ?LOW36(1)}]),                    % AC(1) = 1
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_SOSN, 1, 0, 0, 8#150)}    % 1,,101/ SOSN 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(1)}                          % 1,,150/ 1
    ],
  expect(Prog2, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(0)}                % C(1,,150) = 0
         , {?AC(1), ?LOW36(0)}]).                    % AC(1) = 0

sosg_test() ->
  Prog1 =
    [ {1, 8#101, ?INSN(?OP_SOSG, 1, 0, 0, 8#150)}    % 1,,101/ SOSG 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(2)}                          % 1,,150/ 2
    ],
  expect(Prog1, [], {1, 8#103}, ?CARRY_FLAGS,        % jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(1)}                % C(1,,150) = 1
         , {?AC(1), ?LOW36(1)}]),                    % AC(1) = 1
  Prog2 =
    [ {1, 8#101, ?INSN(?OP_SOSG, 1, 0, 0, 8#150)}    % 1,,101/ SOSG 1,150
    , {1, 8#102, ?INSN_INVALID}                      % 1,,102/ <invalid>
    , {1, 8#103, ?INSN_INVALID}                      % 1,,103/ <invalid>
    , {1, 8#150, ?LOW36(-2)}                         % 1,,150/ -2
    ],
  expect(Prog2, [], {1, 8#102}, ?CARRY_FLAGS,        % no jump, carry 0 and 1
         [ {?EA(1, 8#150), ?LOW36(-3)}               % C(1,,150) = -3
         , {?AC(1), ?LOW36(-3)}]).                   % AC(1) = -3

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
