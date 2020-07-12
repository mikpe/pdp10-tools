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
-define(OP_MOVE, 8#200).
-define(OP_MOVEI, 8#201).
-define(OP_EXCH, 8#250).

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
