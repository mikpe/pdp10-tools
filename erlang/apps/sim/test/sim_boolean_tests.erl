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
