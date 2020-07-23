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
%%% Test cases for PDP10 Effective Address Calculation, taken from
%%% "Extended Addressing", Rev. 5, Jul. 1983, KC10 / Project Jupiter docs.

-module(sim_ea_tests).

-include("../src/sim_core.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LOW18(X), ((X) band ((1 bsl 18) - 1))).

-define(INSN(OP, AC, I, X, Y),
        (((OP) bsl (35 - 8)) bor
         ((AC) bsl (35 - 12)) bor
         ((I) bsl (35 - 13)) bor
         ((X) bsl (35 - 17)) bor
         ?LOW18(Y))).

-define(COMMA2(LEFT, RIGHT), ((?LOW18(LEFT) bsl 18) bor ?LOW18(RIGHT))). % LEFT,,RIGHT in MACRO-10

-define(OP_INVALID, 0).
-define(OP_MOVE, 8#200).
-define(OP_MOVEI, 8#201).

no_indexing_5_1_1_test() ->
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_INVALID, 1, 0, 0, 8#200)} % 1,,100/ MOVE 1,200
    ],
  expect(Prog1, [], {1, 8#100}, #ea{section = 1, offset = 8#200, islocal = true}),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 1,,100/ MOVE 1,@150
    , {1, 8#150, ?COMMA2(8#400000, 8#200)}           % 1,,150/ 400000,,200 ; IFIW
    ],
  expect(Prog2, [], {1, 8#100}, #ea{section = 1, offset = 8#200, islocal = true}),
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 1,,100/ MOVE 1,@150
    , {1, 8#150, ?COMMA2(8#1, 8#200)}                % 1,,150/ 1,,200 ; EFIW
    ],
  expect(Prog3, [], {1, 8#100}, #ea{section = 1, offset = 8#200, islocal = false}).

ifiw_with_local_index_5_1_2_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVE, 1, 0, 0, 8#150)}    % 1,,100/ MOVE 1,150
    , {1, 8#101, ?INSN(?OP_INVALID, 2, 1, 0, 8#151)} % 1,,101/ MOVE 2,@151
    , {1, 8#150, ?COMMA2(-1, 8#10)}                  % 1,,150/ -1,,10 ; local index
    , {1, 8#151, ?COMMA2(8#400001, 8#200)}           % 1,,151/ 400001,,200 ; IFIW
    ],
  expect(Prog, [], {1, 8#101}, #ea{section = 1, offset = 8#210, islocal = true}).

ifiw_with_global_index_5_1_3_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVE, 1, 0, 0, 8#150)}    % 1,,100/ MOVE 1,150
    , {1, 8#101, ?INSN(?OP_INVALID, 2, 0, 1, -2)}    % 1,,101/ MOVE 2,-2(1)
    , {1, 8#150, ?COMMA2(2, 8#10)}                   % 1,,150/ [2,,10] ; global index
    ],
  expect(Prog, [], {1, 8#101}, #ea{section = 2, offset = 6, islocal = false}).

efiw_with_global_index_5_1_4_test() ->
  Prog =
    [ {1, 8#100, ?INSN(?OP_MOVE, 1, 0, 0, 8#150)}    % 1,,100/ MOVE 1,150
    , {1, 8#101, ?INSN(?OP_INVALID, 2, 1, 0, 8#151)} % 1,,101/ MOVE 2,@151
    , {1, 8#150, ?COMMA2(2, 8#10)}                   % 1,,150/ 2,,10
    , {1, 8#151, ?COMMA2(8#010002, 8#200)}           % 1,,151/ 010002,,200 ; EFIW
    ],
  expect(Prog, [], {1, 8#101}, #ea{section = 4, offset = 8#210, islocal = false}).

simple_ea_calc_examples_5_3_test() ->
  Prog1 =
    [ {0, 8#200, ?INSN(?OP_INVALID, 1, 0, 0, 8#100)} % 0,,200/ MOVE 1,100
    ],
  expect(Prog1, [], {0, 8#200}, #ea{section = 0, offset = 8#100, islocal = true}),
  Prog2 =
    [ {1, 8#200, ?INSN(?OP_INVALID, 1, 1, 0, 8#300)} % 1,,200/ MOVE1,@300
    , {1, 8#300, ?COMMA2(8#400000, 8#100)}           % 1,,300/ 400000,,100
    ],
  expect(Prog2, [], {1, 8#200}, #ea{section = 1, offset = 8#100, islocal = true}),
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#300)} % 1,,100/ MOVE 1,@300
    , {1, 8#300, ?COMMA2(2, 8#200)}                  % 1,,300/ 2,,200
    ],
  expect(Prog3, [], {1, 8#100}, #ea{section = 2, offset = 8#200, islocal = false}).

ac_references_6_1_test() ->
  Prog1 =
    [ {2, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 2,,100/ MOVE 1,@150
    , {2, 8#150, ?COMMA2(8#400000, 5)}               % 2,,150/ 400000,,5
    ],
  expect(Prog1, [], {2, 8#100}, #ea{section = 2, offset = 5, islocal = true}),
  Prog2 =
    [ {2, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 2,,100/ MOVE 1,@150
    , {2, 8#150, ?COMMA2(2, 5)}                      % 2,,150/ 2,,5
    ],
  expect(Prog2, [], {2, 8#100}, #ea{section = 2, offset = 5, islocal = false}),
  Prog3 =
    [ {2, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 2,,100/ MOVE 1,@150
    , {2, 8#150, ?COMMA2(1, 5)}                      % 2,,150/ 1,,5
    ],
  expect(Prog3, [], {2, 8#100}, #ea{section = 1, offset = 5, islocal = false}).

incrementing_ea_6_2_test() ->
  Prog1 =
    [ {2, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 2,,100/ DMOVE 1,@150
    , {2, 8#150, ?COMMA2(8#400000, 8#777777)}        % 2,,150/ 400000,,777777
    ],
  expect(Prog1, [], {2, 8#100}, #ea{section = 2, offset = 8#777777, islocal = true}),
  Prog2 =
    [ {2, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 2,,100/ DMOVE 1,@150
    , {2, 8#150, ?COMMA2(2, 8#777777)}               % 2,,150/ 2,,777777
    ],
  expect(Prog2, [], {2, 8#100}, #ea{section = 2, offset = 8#777777, islocal = false}).

multi_section_ea_calcs_7_0_test() ->
  Prog1 =
    [ {3, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 3,,100/ MOVE 1,@150
    , {3, 8#150, ?COMMA2(8#200002, 8#100)}           % 3,,150/ 200002,,100
    , {2, 8#100, ?COMMA2(3, 8#200)}                  % 2,,100/ 3,,200
    ],
  expect(Prog1, [], {3, 8#100}, #ea{section = 3, offset = 8#200, islocal = false}),
  Prog2 =
    [ {3, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 3,,100/ MOVE 1,@150
    , {3, 8#150, ?COMMA2(8#200002, 8#100)}           % 3,,150/ 200002,,100
    , {2, 8#100, ?COMMA2(8#400000, 8#200)}           % 2,,100/ 400000,,200
    ],
  expect(Prog2, [], {3, 8#100}, #ea{section = 2, offset = 8#200, islocal = true}),
  Prog3 =
    [ {3, 8#077, ?INSN(?OP_MOVEI, 3, 0, 0, 1)}       % 3,,077/ MOVEI 3,1
    , {3, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 3,,100/ MOVE 1,@150
    , {3, 8#150, ?COMMA2(8#200000, 8#100)}           % 3,,150/ 200000,,100
    , {0, 8#100, ?COMMA2(3, 8#200)}                  % 0,,100/ 3,,200
    ],
  expect(Prog3, [], {3, 8#100}, #ea{section = 0, offset = 8#201, islocal = true}).

xmovei_and_xhlli_8_10_test() ->
 Prog1 =
    [ {2, 8#100, ?INSN(?OP_INVALID, 1, 0, 0, 6)}     % 2,,100/ XMOVEI 1,6
    ],
  expect(Prog1, [], {2, 8#100}, #ea{section = 2, offset = 6, islocal = true}),
  %% The second example in 8.10 is broken, in that the EA-calculcation follows
  %% an indirect EFIW into section zero expecting to find an IFIW at 0,,6.
  %% Compare this with the third example in 7.0 which also defines the IFIW
  %% that the indirect EFIW points to.
  Prog2 =
    [ {2, 8#100, ?INSN(?OP_INVALID, 1, 1, 0, 8#150)} % 2,,100/ XMOVEI 1,@150
    , {2, 8#150, ?COMMA2(8#200000, 8#100)}           % 2,,150/ 200000,,100 ; indirect EFIW
    , {0, 8#100, ?COMMA2(0, 6)}                      % 0,,100/ 0,,6 ; IFIW
    ],
  expect(Prog2, [], {2, 8#100}, #ea{section = 0, offset = 6, islocal = true}).

%% Remaining examples from the "Extended Addressing" document relate to
%% instructions using the EA not the initial EA calculation itself, and
%% they will be added as those instructions are implemented.

%% Common code to run short sequences and check final EA =======================

expect(Prog, ACs, ExpectedPC, ExpectedEA) ->
  {Core, Mem} = init(Prog, ACs),
  {_Core, _Mem, {error, {sim_core, {dispatch, PC, _IR, ActualEA}}}} = sim_core:run(Core, Mem),
  ActualPC = {PC bsr 18, PC band ((1 bsl 18) - 1)},
  ?assertEqual(ExpectedPC, ActualPC),
  ?assertEqual(ExpectedEA, ActualEA),
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
  Flags = (1 bsl ?PDP10_PF_USER),
  #core{ pc_section = PCSection
       , pc_offset = PCOffset
       , acs = init_acs(ACs, list_to_tuple(lists:duplicate(16, 0)))
       , flags = Flags
       }.

init_acs([], ACS) -> ACS;
init_acs([{AC, Val} | Rest], ACS) -> init_acs(Rest, setelement(AC + 1, Val, ACS)).
