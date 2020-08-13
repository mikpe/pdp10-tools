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
%%% Test cases for 2.10 Stack Operations

-module(sim_stack_tests).

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

-define(OP_INVALID, 0).
-define(OP_ADJSP, 8#105).
-define(OP_MOVE,  8#200).
-define(OP_MOVEI, 8#201).
-define(OP_PUSHJ, 8#260).
-define(OP_PUSH,  8#261).
-define(OP_POP,   8#262).
-define(OP_POPJ,  8#263).

%% 2.10 Stack Operations =======================================================

%% PUSH - Push

push_global_test() ->
  %% Check that a global SP can push into next section without clobbering AC0.
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 1)}      % 2,,100/ MOVEI 0,1
    , {2, 8#101, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,101/ MOVE 7,150
    , {2, 8#102, ?INSN(?OP_PUSH, 7, 0, 0, 8#151)}   % 2,,102/ PUSH 7,151
    , {2, 8#103, ?INSN_INVALID}                     % 2,,103/ <invalid>
    , {2, 8#150, ?COMMA2(2, -1)}                    % 2,,150/ 2,,-1
    , {2, 8#151, ?COMMA2(8#27, 8#42)}               % 2,,151/ 27,,42
    , {3, 0, 0}                                     % 3,,000/ 0
    ],
  expect(Prog, [], {2, 8#103}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(3, 0)}                  % AC7 = 3,,0
         , {?EA(3, 0), ?COMMA2(8#27, 8#42)}         % C(3,,0) = 27,,42
         , {?AC(0), 1}                              % AC0 = 1
         ]).

push_local_test() ->
  %% Check that a local SP wraps around and aliases AC0.
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_PUSH, 7, 0, 0, 8#151)}   % 2,,101/ PUSH 7,151
    , {2, 8#102, ?INSN_INVALID}                     % 2,,102/ <invalid>
    , {2, 8#150, ?COMMA2(-2, -1)}                   % 2,,150/ -2,,-1
    , {2, 8#151, ?COMMA2(8#27, 8#42)}               % 2,,151/ 27,,42
    ],
  expect(Prog, [], {2, 8#102}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(-1, 0)}                 % AC7 = -1,,0
         , {?AC(0), ?COMMA2(8#27, 8#42)}            % AC0 = 27,,42
         ]).

push_sec0_test() ->
  %% Check that section 0 always interprets SP as local.
  Prog =
    [ {0, 8#100, ?INSN(?OP_MOVEI, 0, 0, 0, 1)}      % 0,,100/ MOVEI 0,1
    , {0, 8#101, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 0,,101/ MOVE 7,150
    , {0, 8#102, ?INSN(?OP_PUSH, 7, 0, 0, 8#151)}   % 0,,102/ PUSH 7,151
    , {0, 8#103, ?INSN_INVALID}                     % 0,,103/ <invalid>
    , {0, 8#150, ?COMMA2(2, -1)}                    % 0,,150/ 2,,-1
    , {0, 8#151, ?COMMA2(8#27, 8#42)}               % 0,,151/ 27,,42
    ],
  expect(Prog, [], {0, 8#103}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(3, 0)}                  % AC7 = 3,,0
         , {?AC(0), ?COMMA2(8#27, 8#42)}            % AC0 = 27,,42
         ]).

%% POP - Pop

pop_global_test() ->
  %% Check that a global SP can pop into previous section.
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_POP, 7, 0, 0, 1)}        % 2,,101/ POP 7,1
    , {2, 8#102, ?INSN_INVALID}                     % 2,,102/ <invalid>
    , {2, 8#150, ?COMMA2(3, 0)}                     % 2,,150/ 3,,0
    , {3, 0, ?COMMA2(8#27, 8#42)}                   % 3,,000/ 27,,42
    ],
  expect(Prog, [], {2, 8#102}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(2, -1)}                 % AC7 = 2,,-1
         , {?AC(1), ?COMMA2(8#27, 8#42)}            % AC1 = 27,,42
         ]).

pop_local_test() ->
  %% Check that a local SP wraps around and aliases AC0.
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_MOVE, 0, 0, 0, 8#151)}   % 2,,101/ MOVE 0,151
    , {2, 8#102, ?INSN(?OP_POP, 7, 0, 0, 1)}        % 2,,102/ POP 7,1
    , {2, 8#103, ?INSN_INVALID}                     % 2,,103/ <invalid>
    , {2, 8#150, ?COMMA2(-1, 0)}                    % 2,,150/ -1,,0
    , {2, 8#151, ?COMMA2(8#27, 8#42)}               % 2,,151/ 27,,42
    ],
  expect(Prog, [], {2, 8#103}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(-2, -1)}                % AC7 = -2,,-1
         , {?AC(1), ?COMMA2(8#27, 8#42)}            % AC1 = 27,,42
         ]).

pop_sec0_test() ->
  %% Check that section 0 always interprets SP as local.
  Prog =
    [ {0, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 0,,100/ MOVE 7,150
    , {0, 8#101, ?INSN(?OP_MOVE, 0, 0, 0, 8#151)}   % 0,,101/ MOVE 0,151
    , {0, 8#102, ?INSN(?OP_POP, 7, 0, 0, 1)}        % 0,,102/ POP 7,1
    , {0, 8#103, ?INSN_INVALID}                     % 0,,103/ <invalid>
    , {0, 8#150, ?COMMA2(3, 0)}                     % 0,,150/ 3,,0
    , {0, 8#151, ?COMMA2(8#27, 8#42)}               % 0,,151/ 27,,42
    ],
  expect(Prog, [], {0, 8#103}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(2, -1)}                 % AC7 = 2,,-1
         , {?AC(1), ?COMMA2(8#27, 8#42)}            % AC1 = 27,,42
         ]).

%% PUSHJ - Push and Jump

pushj_global_test() ->
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_PUSHJ, 7, 0, 0, 8#110)}  % 2,,101/ PUSHJ 7,110
    , {2, 8#110, ?INSN_INVALID}                     % 2,,110/ <invalid>
    , {2, 8#150, ?COMMA2(2, 8#177)}                 % 2,,150/ 2,,177
    , {2, 8#200, 0}                                 % 2,,200/ 0
    ],
  expect(Prog, [], {2, 8#110}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(2, 8#200)}              % AC7 = 2,,200
         , {?EA(2, 8#200), ?COMMA2(2, 8#102)}       % C(2,,200) = 2,,102
         ]).

pushj_local_test() ->
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_PUSHJ, 7, 0, 0, 8#110)}  % 2,,101/ PUSHJ 7,110
    , {2, 8#110, ?INSN_INVALID}                     % 2,,110/ <invalid>
    , {2, 8#150, ?COMMA2(-2, 8#177)}                % 2,,150/ -2,,177
    , {2, 8#200, 0}                                 % 2,,200/ 0
    ],
  expect(Prog, [], {2, 8#110}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(-1, 8#200)}             % AC7 = -1,,200
         , {?EA(2, 8#200), ?COMMA2(2, 8#102)}       % C(2,,200) = 2,,102
         ]).

pushj_sec0_test() ->
  Prog =
    [ {0, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 0,,100/ MOVE 7,150
    , {0, 8#101, ?INSN(?OP_PUSHJ, 7, 0, 0, 8#110)}  % 0,,101/ PUSHJ 7,110
    , {0, 8#110, ?INSN_INVALID}                     % 0,,110/ <invalid>
    , {0, 8#150, ?COMMA2(2, 8#177)}                 % 0,,150/ 2,,177
    , {0, 8#200, 0}                                 % 0,,200/ 0
    ],
  expect(Prog, [], {0, 8#110}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(3, 8#200)}             % AC7 = 3,,200
         , {?EA(0, 8#200), ?COMMA2(?DEFAULT_FLAGS bsl 5, 8#102)} % C(0,,200) = <flags>00000,,102
         ]).

%% POPJ - Pop and Jump

popj_global_test() ->
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_POPJ, 7, 0, 0, 0)}       % 2,,101/ POPJ 7,
    , {2, 8#110, ?INSN_INVALID}                     % 2,,110/ <invalid>
    , {2, 8#150, ?COMMA2(2, 8#200)}                 % 2,,150/ 2,,200
    , {2, 8#200, ?COMMA2(2, 8#110)}                 % 2,,200/ 2,,110
    ],
  expect(Prog, [], {2, 8#110}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(2, 8#177)}              % AC7 = 2,,177
         ]).

popj_local_test() ->
  Prog =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_POPJ, 7, 0, 0, 0)}       % 2,,101/ POPJ 7,
    , {2, 8#110, ?INSN_INVALID}                     % 2,,110/ <invalid>
    , {2, 8#150, ?COMMA2(-1, 8#200)}                % 2,,150/ -1,,200
    , {2, 8#200, ?COMMA2(2, 8#110)}                 % 2,,200/ 2,,110
    ],
  expect(Prog, [], {2, 8#110}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(-2, 8#177)}             % AC7 = -2,,177
         ]).

popj_sec0_test() ->
  Prog =
    [ {0, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 0,,100/ MOVE 7,150
    , {0, 8#101, ?INSN(?OP_POPJ, 7, 0, 0, 0)}       % 0,,101/ POPJ 7,
    , {0, 8#110, ?INSN_INVALID}                     % 0,,110/ <invalid>
    , {0, 8#150, ?COMMA2(3, 8#200)}                 % 0,,150/ 3,,200
    , {0, 8#200, ?COMMA2(-1, 8#110)}                % 0,,200/ -1,,110
    ],
  expect(Prog, [], {0, 8#110}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(2, 8#177)}              % AC7 = 2,,177
         ]).

%% ADJSP - Adjust Stack Pointer

adjsp_global_test() ->
  Prog1 =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_ADJSP, 7, 0, 0, 2)}      % 2,,101/ ADJSP 7,2
    , {2, 8#102, ?INSN_INVALID}                     % 2,,102/ <invalid>
    , {2, 8#150, ?COMMA2(2, -1)}                    % 2,,150/ 2,,-1
    ],
  expect(Prog1, [], {2, 8#102}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(3, 1)}                  % AC7 = 3,,1
         ]),
  Prog2 =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_ADJSP, 7, 0, 0, -2)}     % 2,,101/ ADJSP 7,-2
    , {2, 8#102, ?INSN_INVALID}                     % 2,,102/ <invalid>
    , {2, 8#150, ?COMMA2(3, 1)}                     % 2,,150/ 3,,1
    ],
  expect(Prog2, [], {2, 8#102}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(2, -1)}                  % AC7 = 2,,-1
         ]).

adjsp_local_test() ->
  Prog1 =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_ADJSP, 7, 0, 0, 2)}      % 2,,101/ ADJSP 7,2
    , {2, 8#102, ?INSN_INVALID}                     % 2,,102/ <invalid>
    , {2, 8#150, ?COMMA2(-3, -1)}                   % 2,,150/ -3,,-1
    ],
  expect(Prog1, [], {2, 8#102}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(-1, 1)}                 % AC7 = -1,,1
         ]),
  Prog2 =
    [ {2, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 2,,100/ MOVE 7,150
    , {2, 8#101, ?INSN(?OP_ADJSP, 7, 0, 0, -2)}     % 2,,101/ ADJSP 7,-2
    , {2, 8#102, ?INSN_INVALID}                     % 2,,102/ <invalid>
    , {2, 8#150, ?COMMA2(-1, 1)}                    % 2,,150/ -1,,1
    ],
  expect(Prog2, [], {2, 8#102}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(-3, -1)}                % AC7 = -3,,-1
         ]).

adjsp_sec0_test() ->
  Prog1 =
    [ {0, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 0,,100/ MOVE 7,150
    , {0, 8#101, ?INSN(?OP_ADJSP, 7, 0, 0, 2)}      % 0,,101/ ADJSP 7,2
    , {0, 8#102, ?INSN_INVALID}                     % 0,,102/ <invalid>
    , {0, 8#150, ?COMMA2(0, -1)}                    % 0,,150/ 0,,-1
    ],
  expect(Prog1, [], {0, 8#102}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(2, 1)}                  % AC7 = 2,,1
         ]),
  Prog2 =
    [ {0, 8#100, ?INSN(?OP_MOVE, 7, 0, 0, 8#150)}   % 0,,100/ MOVE 7,150
    , {0, 8#101, ?INSN(?OP_ADJSP, 7, 0, 0, -2)}     % 0,,101/ ADJSP 7,-2
    , {0, 8#102, ?INSN_INVALID}                     % 0,,102/ <invalid>
    , {0, 8#150, ?COMMA2(2, 1)}                     % 0,,150/ 2,,1
    ],
  expect(Prog2, [], {0, 8#102}, ?DEFAULT_FLAGS,
         [ {?AC(7), ?COMMA2(0, -1)}                 % AC7 = 0,,-1
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
