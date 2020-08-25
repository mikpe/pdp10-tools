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
%%% Test cases for 2.5 Shift and Rotate

-module(sim_shifts_tests).

-include("../src/sim_core.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_FLAGS, (1 bsl ?PDP10_PF_USER)).

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

-define(OP_ASH,  8#240).
-define(OP_ROT,  8#241).
-define(OP_LSH,  8#242).
-define(OP_ASHC, 8#244).
-define(OP_ROTC, 8#245).
-define(OP_LSHC, 8#246).

%% 2.5 Shift and Rotate ========================================================

%% LSH - Logical Shift

lsh_test() ->
  ACS =
    [ {1, ?COMMA2(8#000111, 8#222333)}                  % AC1 = 000111222333
    ],
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_LSH, 1, 0, 0, 9)}            % 1,,100/ LSH 1,9
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog1, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#111222, 8#333000)}        % AC1 = 111222333000
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_LSH, 1, 0, 0, -9)}           % 1,,100/ LSH 1,-9
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog2, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#000000, 8#111222)}        % AC1 = 000000111222
         ]).

%% LSHC - Logical Shift Combined

lshc_test() ->
  ACS =
    [ {1, ?COMMA2(8#000111, 8#222333)}                  % AC1 = 000111222333
    , {2, ?COMMA2(8#444555, 8#666777)}                  % AC2 = 444555666777
    ],
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_LSHC, 1, 0, 0, 9)}           % 1,,100/ LSHC 1,9
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog1, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#111222, 8#333444)}        % AC1 = 111222333444
         , {?AC(2), ?COMMA2(8#555666, 8#777000)}        % AC2 = 555666777000
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_LSHC, 1, 0, 0, 45)}          % 1,,100/ LSHC 1,55
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog2, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#555666, 8#777000)}        % AC1 = 555666777000
         , {?AC(2), ?COMMA2(8#000000, 8#000000)}        % AC2 = 000000000000
         ]),
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_LSHC, 1, 0, 0, -9)}          % 1,,100/ LSHC 1,-9
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog3, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#000000, 8#111222)}        % AC1 = 000000111222
         , {?AC(2), ?COMMA2(8#333444, 8#555666)}        % AC2 = 333444555666
         ]),
  Prog4 =
    [ {1, 8#100, ?INSN(?OP_LSHC, 1, 0, 0, -45)}         % 1,,100/ LSHC 1,-55
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog4, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#000000, 8#000000)}        % AC1 = 000000000000
         , {?AC(2), ?COMMA2(8#000000, 8#111222)}        % AC2 = 000000111222
         ]).

%% ROT - Rotate

rot_test() ->
  ACS =
    [ {1, ?COMMA2(8#111222, 8#333444)}                  % AC1 = 111222333444
    ],
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_ROT, 1, 0, 0, 36+9)}         % 1,,100/ ROT 1,55
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog1, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#222333, 8#444111)}        % AC1 = 222333444111
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_ROT, 1, 0, 0, -(36+9))}      % 1,,100/ ROT 1,-55
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog2, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#444111, 8#222333)}        % AC1 = 444111222333
         ]).

%% ROTC - Rotate Combined

rotc_test() ->
  ACS =
    [ {1, ?COMMA2(8#000111, 8#222333)}                  % AC1 = 000111222333
    , {2, ?COMMA2(8#444555, 8#666777)}                  % AC2 = 444555666777
    ],
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_ROTC, 1, 0, 0, 18)}          % 1,,100/ ROTC 1,22
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog1, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#222333, 8#444555)}        % AC1 = 222333444555
         , {?AC(2), ?COMMA2(8#666777, 8#000111)}        % AC2 = 666777000111
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_ROTC, 1, 0, 0, 36+18)}       % 1,,100/ ROTC 1,66
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog2, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#666777, 8#000111)}        % AC1 = 666777000111
         , {?AC(2), ?COMMA2(8#222333, 8#444555)}        % AC2 = 222333444555
         ]),
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_ROTC, 1, 0, 0, -18)}         % 1,,100/ ROTC 1,-22
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog3, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#666777, 8#000111)}        % AC1 = 666777000111
         , {?AC(2), ?COMMA2(8#222333, 8#444555)}        % AC2 = 222333444555
         ]),
  Prog4 =
    [ {1, 8#100, ?INSN(?OP_ROTC, 1, 0, 0, -(36+18))}    % 1,,100/ ROTC 1,-66
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog4, ACS, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#222333, 8#444555)}        % AC1 = 222333444555
         , {?AC(2), ?COMMA2(8#666777, 8#000111)}        % AC2 = 666777000111
         ]).

%% ASH - Arithmetic Shift

ash_test() ->
  ACS1 =
    [ {1, ?COMMA2(8#000111, 8#222333)}                  % AC1 = 000111222333
    ],
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_ASH, 1, 0, 0, 6)}            % 1,,100/ ASH 1,6
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog1, ACS1, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#011122, 8#233300)}        % AC1 = 011122233300
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_ASH, 1, 0, 0, -9)}           % 1,,100/ ASH 1,-9
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog2, ACS1, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#000000, 8#111222)}        % AC1 = 000000111222
         ]),
  ACS2 =
    [ {1, ?COMMA2(8#777555, 8#333111)}                  % AC1 = 777555333111
    ],
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_ASH, 1, 0, 0, 6)}            % 1,,100/ ASH 1,6
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog3, ACS2, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#755533, 8#311100)}        % AC1 = 755533311100
         ]),
  Prog4 =
    [ {1, 8#100, ?INSN(?OP_ASH, 1, 0, 0, -9)}           % 1,,100/ ASH 1,-9
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
    ],
  expect(Prog4, ACS2, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#777777, 8#555333)}        % AC1 = 777777555333
         ]).

%% ASHC - Arithmetic Shift Combined

ashc_test() ->
  ACS1 =
    [ {1, ?COMMA2(8#000111, 8#222333)}                  % AC1 = 000111222333
    , {2, ?COMMA2(8#777666, 8#555444)}                  % AC2 = 777666555444
    ],
  Prog1 =
    [ {1, 8#100, ?INSN(?OP_ASHC, 1, 0, 0, 6)}           % 1,,100/ ASHC 1,6
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
   ],
  expect(Prog1, ACS1, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#011122, 8#233377)}        % AC1 = 011122233377
         , {?AC(2), ?COMMA2(8#366655, 8#544400)}        % AC2 = 366655544400
         ]),
  Prog2 =
    [ {1, 8#100, ?INSN(?OP_ASHC, 1, 0, 0, -9)}          % 1,,100/ ASHC 1,-9
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
   ],
  expect(Prog2, ACS1, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#000000, 8#111222)}        % AC1 = 000000111222
         , {?AC(2), ?COMMA2(8#155777, 8#666555)}        % AC2 = 155777666555
         ]),
  ACS2 =
    [ {1, ?COMMA2(8#000000, 8#000000)}                  % AC1 = 0
    , {2, ?COMMA2(8#000666, 8#555444)}                  % AC2 = 000666555444
    ],
  Prog3 =
    [ {1, 8#100, ?INSN(?OP_ASHC, 1, 0, 0, 35+6)}        % 1,,100/ ASHC 1,51
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
   ],
  expect(Prog3, ACS2, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#066655, 8#544400)}        % AC1 = 066655544400
         , {?AC(2), ?COMMA2(8#000000, 8#000000)}        % AC2 = 0
         ]),
  Prog4 =
    [ {1, 8#100, ?INSN(?OP_ASHC, 1, 0, 0, -(35+9))}     % 1,,100/ ASHC 1,54
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
   ],
  expect(Prog4, ACS1, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(8#000000, 8#000000)}        % AC1 = 0
         , {?AC(2), ?COMMA2(8#000000, 8#111222)}        % AC2 = 000000111222
         ]),
  ACS3 =
    [ {1, 0}                                            % AC1 = 0
    , {2, 0}                                            % AC2 = 0
    ],
  Prog5 =
    [ {1, 8#100, ?INSN(?OP_ASHC, 1, 0, 0, 70+6)}        % 1,,100/ ASHC 1,114
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
   ],
  expect(Prog5, ACS3, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), 0}                                  % AC1 = 0
         , {?AC(2), 0}                                  % AC2 = 0
         ]),
  ACS4 =
    [ {1, ?COMMA2(-1, -1)}                              % AC1 = -1
    , {2, ?COMMA2(-1, -1)}                              % AC2 = -1
    ],
  Prog6 =
    [ {1, 8#100, ?INSN(?OP_ASHC, 1, 0, 0, -(70+6))}     % 1,,100/ ASHC 1,-114
    , {1, 8#101, ?INSN_INVALID}                         % 1,,101/ <invalid>
   ],
  expect(Prog6, ACS4, {1, 8#101}, ?DEFAULT_FLAGS,
         [ {?AC(1), ?COMMA2(-1, -1)}                    % AC1 = -1
         , {?AC(2), ?COMMA2(-1, -1)}                    % AC2 = -1
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
