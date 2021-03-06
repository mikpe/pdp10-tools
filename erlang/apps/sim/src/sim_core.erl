%%% -*- erlang-indent-level: 2 -*-
%%%
%%% simulator for pdp10-elf
%%% Copyright (C) 2018-2020  Mikael Pettersson
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
%%% Runs a loaded pdp10-elf user-mode program.

-module(sim_core).

-export([ run/6
        , run/2
        , next_pc/2
        , skip/2
        , calculate_ea/5
        , page_fault/6
        , c/3
        , cset/4
        , get_ac/2
        , set_ac/3
        , set_flag/2
        , set_flags/2
        , format_error/1
        ]).

-include("sim_core.hrl").

%% Run a program ===============================================================

-spec run(Mem :: sim_mem:mem(),
          PC :: word(),
          SP :: word(),
          Argc :: word(),
          Argv :: word(),
          Envp :: word()) -> {ok, integer()} | {error, {module(), term()}}.
run(Mem, PC, SP, Argc, Argv, Envp) ->
  PCSection = (PC bsr 18) band ((1 bsl 12) - 1),
  PCOffset = PC band ((1 bsl 18) - 1),
  ACS0 = list_to_tuple(lists:duplicate(16, 0)),
  ACS1 = do_set_ac(ACS0, 1, Argc),
  ACS2 = do_set_ac(ACS1, 2, Argv),
  ACS3 = do_set_ac(ACS2, 3, Envp),
  ACS  = do_set_ac(ACS3, ?AC_SP, SP),
  Flags = (1 bsl ?PDP10_PF_USER),
  Core = #core{ pc_section = PCSection
              , pc_offset = PCOffset
              , acs = ACS
              , flags = Flags
              },
  {_Core, _Mem, Result} = run(Core, Mem),
  Result.

%% The instruction fetch, effective address calculation, and dispatch process,
%% is a tail-recursive state machine, in order to support correct handling of
%% faults (especially page faults) that may occur at various points, without
%% creating recursion in the simulator.

-spec run(#core{}, sim_mem:mem())
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
run(Core, Mem) ->
  insn_fetch(Core, Mem).

%% Sequential control flow: increment PC but stay in current section.
-spec next_pc(#core{}, sim_mem:mem())
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
next_pc(#core{pc_offset = PCOffset} = Core, Mem) ->
  PCOffset1 = (PCOffset + 1) band ((1 bsl 18) - 1),
  insn_fetch(Core#core{pc_offset = PCOffset1}, Mem).

%% Skip next instruction: increment PC by two but stay in current section.
-spec skip(#core{}, sim_mem:mem())
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
skip(#core{pc_offset = PCOffset} = Core, Mem) ->
  PCOffset2 = (PCOffset + 2) band ((1 bsl 18) - 1),
  insn_fetch(Core#core{pc_offset = PCOffset2}, Mem).

%% Instruction Fetch and Effective Address Calculation =========================
%% c.f. Toad-1 Architecture Manual, page 41, Figure 1.11

%% Instruction Fetch.  This always uses local addressing.
insn_fetch(Core, Mem) ->
  %% Arithmetic and stack overflows set trap flags which are checked after
  %% completion of the faulting instruction, i.e., at the start of the next
  %% instruction fetch cycle.
  #core{flags = Flags} = Core,
  %% TRAP_1 and TRAP_2 are adjacent
  case (Flags bsr ?PDP10_PF_TRAP_1) band 3 of
    0 ->
      %% No trap flag set.
      insn_fetch1(Core, Mem);
    1 ->
      %% TRAP_1 (arithmetic overflow) set.
      %% For now, arithmetic overflows are ignored as if the user had installed
      %% a no-op handler.   This is so that C's unsigned arithmetic can work.
      %% TODO: Handle 2.9.6 Overflow Trapping
      Flags1 = Flags band bnot (1 bsl ?PDP10_PF_TRAP_1),
      Core1 = Core#core{flags = Flags1},
      insn_fetch1(Core1, Mem);
    _ ->
      %% TRAP_2 (stack overflow) set.
      %% For now, treat this as a fatal error terminating execution.
      %% TODO: Handle stack overflow faults properly.
      PC = (Core#core.pc_section bsl 18) bor Core#core.pc_offset,
      {Core, Mem, {error, {?MODULE, {stack_fault, PC}}}}
  end.

insn_fetch1(Core, Mem) ->
  PCOffset = Core#core.pc_offset,
  case PCOffset =< 8#17 of
    true ->
      MB = get_ac(Core, PCOffset),
      insn_fetch2(Core, Mem, MB);
    false ->
      PCSection = Core#core.pc_section,
      Address = (PCSection bsl 18) bor PCOffset,
      case sim_mem:read_word(Mem, Address) of
        {ok, MB} -> insn_fetch2(Core, Mem, MB);
        {error, Reason} ->
          EA = #ea{section = PCSection, offset = PCOffset, islocal = true},
          page_fault(Core, Mem, EA, read, Reason, fun insn_fetch/2)
      end
  end.

insn_fetch2(Core, Mem, MB) ->
  IR = bits36(MB, 0, 12), % IR := MB_<0:12>
  ESection = Core#core.pc_section, % E_<6:17> := PC_<6:17>
  calculate_ea(Core, Mem, MB, ESection,
               fun(Core1, Mem1, EA) -> dispatch(Core1, Mem1, IR, EA) end).

-spec calculate_ea(Core :: #core{},
                   Mem :: sim_mem:mem(),
                   MB :: word(),
                   ESection :: word(),
                   Cont :: fun((#core{}, sim_mem:mem(), #ea{})
                               -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}))
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
calculate_ea(Core, Mem, MB, ESection, Cont) ->
  local_format_address_word(Core, Mem, MB, ESection, Cont).

local_format_address_word(Core, Mem, MB, ESection, Cont) ->
  Y = bits36(MB, 18, 35), % Y_<18:35> := MB_<18:35>
  X = bits36(MB, 14, 17), % X := MB_<14:17>
  I = bit36(MB, 13),      % I := MB_<13>
  %% Indexed Address?  Test X field.
  case X of
    0 ->
      %% X field =:= 0.  No Indexing.
      EOffset = Y, % E_<18:35> := Y_<18:35>
      indirect_addressing(Core, Mem, I, ESection, EOffset, _IsLocal = true, Cont);
    _ ->
      %% X field =/= 0.  Indexing.
      CX = get_ac(Core, X), % C(X) hoisted from following blocks
      %% Test Section Number in E_<6:17>.
      case ESection of
        0 ->
          local_index(Core, Mem, I, ESection, CX, Y, Cont);
        _ ->
          %% Section =/= 0.
          %% Test C(X).  Global Index when (C(X)_<0> =:= 0) and (C(X)_<6:17> =/= 0).
          case bit36(CX, 0) =:= 0 andalso bits36(CX, 6, 17) =/= 0 of
            true ->
              global_index(Core, Mem, I, CX, Y, Cont);
            false ->
              local_index(Core, Mem, I, ESection, CX, Y, Cont)
          end
      end
  end.

local_index(Core, Mem, I, ESection, CX, Y, Cont) ->
  EOffset = bits36low18(CX + Y), % E_<18:35> := C(X)_<18:35> + Y_<18:35>
  indirect_addressing(Core, Mem, I, ESection, EOffset, _IsLocal = true, Cont).

global_index(Core, Mem, I, CX, Y, Cont) ->
  Y1 = sext18(Y), % Y_<6:17> =:= 7777 * Y_<18>
  E = CX + Y1, % E_<6:35> := C(X)_<6:35> + Y_<6:35>
  ESection = bits36(E, 6, 17),
  EOffset = bits36(E, 18, 35),
  indirect_addressing(Core, Mem, I, ESection, EOffset, _IsLocal = false, Cont).

indirect_addressing(Core, Mem, I, ESection, EOffset, IsLocal, Cont) ->
  %% Test I bit.
  case I of
    0 ->
      %% I =:= 0.  Done!
      %% E is the Effective Address.
      %% IsLocal signals if E is a global address or if it is a local address
      %% in the last section from which an address word was fetched.  This
      %% matters for instructions that use E and E+1, and also to determine
      %% if an address denotes an accumulator or a memory location.
      Cont(Core, Mem, #ea{section = ESection, offset = EOffset, islocal = IsLocal});
    _ ->
      %% I =:= 1.
      fetch_indirect_word(Core, Mem, ESection, EOffset, IsLocal, Cont)
  end.

fetch_indirect_word(Core, Mem, ESection, EOffset, IsLocal, Cont) ->
  %% Fetch the Indirect Word.
  case c(Core, Mem, ESection, EOffset, IsLocal) of
    {ok, MB} ->
      %% Non-zero section?  Test E_<6:17>.
      case ESection of
        0 ->
          local_format_address_word(Core, Mem, MB, ESection, Cont);
        _ ->
          %% Section =/= 0.
          %% Decode Indirect Word MB_<0:1>.
          case bits36(MB, 0, 1) of
            0 ->
              global_indirect_word(Core, Mem, MB, _I = 0, Cont);
            1 ->
              global_indirect_word(Core, Mem, MB, _I = 1, Cont);
            2 ->
              %% Local Indirect
              local_format_address_word(Core, Mem, MB, ESection, Cont);
            3 ->
              EA = #ea{section = ESection, offset = EOffset, islocal = IsLocal},
              page_fault(Core, Mem, EA, read, indirect_word,
                         fun(Core1, Mem1) ->
                            fetch_indirect_word(Core1, Mem1, ESection, EOffset, IsLocal, Cont)
                          end)
          end
      end;
    {error, Reason} ->
      EA = #ea{section = ESection, offset = EOffset, islocal = IsLocal},
      page_fault(Core, Mem, EA, read, Reason,
                 fun(Core1, Mem1) ->
                   fetch_indirect_word(Core1, Mem1, ESection, EOffset, IsLocal, Cont)
                 end)
  end.

global_indirect_word(Core, Mem, MB, I, Cont) ->
  Y = bits36(MB, 6, 35), % Y:= MB_<6,35>
  X = bits36(MB, 2, 5), % X := MB_<2:5>
  %% Indexed Address?  Test X field.
  case X of
    0 ->
      E = Y, % E_<6:35> := Y_<6:35>
      ESection1 = bits36(E, 6, 17),
      EOffset1 = bits36(E, 18, 35),
      indirect_addressing(Core, Mem, I, ESection1, EOffset1, _IsLocal = false, Cont);
    _ ->
      %% X =/= 0.
      CX = get_ac(Core, X),
      E = CX + Y, % E_<6:35> := C(X)_<6:35> + Y_<6:35>
      ESection1 = bits36(E, 6, 17),
      EOffset1 = bits36(E, 18, 35),
      indirect_addressing(Core, Mem, I, ESection1, EOffset1, _IsLocal = false, Cont)
  end.

%% Instruction Dispatch ========================================================

-spec dispatch(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
dispatch(Core, Mem, IR, EA) ->
  %% Dispatch on the opcode (top 9 bits).
  case IR bsr 4 of
    8#104 -> sim_kernel:handle_JSYS(Core, Mem, IR, EA);
    8#105 -> sim_stack:handle_ADJSP(Core, Mem, IR, EA);
    8#120 -> sim_moves:handle_DMOVE(Core, Mem, IR, EA);
    8#121 -> sim_moves:handle_DMOVN(Core, Mem, IR, EA);
    8#124 -> sim_moves:handle_DMOVEM(Core, Mem, IR, EA);
    8#125 -> sim_moves:handle_DMOVNM(Core, Mem, IR, EA);
    8#200 -> sim_moves:handle_MOVE(Core, Mem, IR, EA);
    8#201 -> sim_moves:handle_MOVEI(Core, Mem, IR, EA);
    8#202 -> sim_moves:handle_MOVEM(Core, Mem, IR, EA);
    8#203 -> sim_moves:handle_MOVES(Core, Mem, IR, EA);
    8#204 -> sim_moves:handle_MOVS(Core, Mem, IR, EA);
    8#205 -> sim_moves:handle_MOVSI(Core, Mem, IR, EA);
    8#206 -> sim_moves:handle_MOVSM(Core, Mem, IR, EA);
    8#207 -> sim_moves:handle_MOVSS(Core, Mem, IR, EA);
    8#210 -> sim_moves:handle_MOVN(Core, Mem, IR, EA);
    8#211 -> sim_moves:handle_MOVNI(Core, Mem, IR, EA);
    8#212 -> sim_moves:handle_MOVNM(Core, Mem, IR, EA);
    8#213 -> sim_moves:handle_MOVNS(Core, Mem, IR, EA);
    8#214 -> sim_moves:handle_MOVM(Core, Mem, IR, EA);
    8#215 -> sim_moves:handle_MOVEI(Core, Mem, IR, EA); % MOVMI = MOVEI
    8#216 -> sim_moves:handle_MOVMM(Core, Mem, IR, EA);
    8#217 -> sim_moves:handle_MOVMS(Core, Mem, IR, EA);
    8#240 -> sim_shifts:handle_ASH(Core, Mem, IR, EA);
    8#241 -> sim_shifts:handle_ROT(Core, Mem, IR, EA);
    8#242 -> sim_shifts:handle_LSH(Core, Mem, IR, EA);
    8#244 -> sim_shifts:handle_ASHC(Core, Mem, IR, EA);
    8#245 -> sim_shifts:handle_ROTC(Core, Mem, IR, EA);
    8#246 -> sim_shifts:handle_LSHC(Core, Mem, IR, EA);
    8#250 -> sim_moves:handle_EXCH(Core, Mem, IR, EA);
    8#251 -> sim_moves:handle_BLT(Core, Mem, IR, EA);
    8#252 -> sim_arithmetic:handle_AOBJP(Core, Mem, IR, EA);
    8#253 -> sim_arithmetic:handle_AOBJN(Core, Mem, IR, EA);
    8#260 -> sim_stack:handle_PUSHJ(Core, Mem, IR, EA);
    8#261 -> sim_stack:handle_PUSH(Core, Mem, IR, EA);
    8#262 -> sim_stack:handle_POP(Core, Mem, IR, EA);
    8#263 -> sim_stack:handle_POPJ(Core, Mem, IR, EA);
    8#300 -> next_pc(Core, Mem); % CAI = no-op
    8#301 -> sim_arithmetic:handle_CAIL(Core, Mem, IR, EA);
    8#302 -> sim_arithmetic:handle_CAIE(Core, Mem, IR, EA);
    8#303 -> sim_arithmetic:handle_CAILE(Core, Mem, IR, EA);
    8#304 -> skip(Core, Mem); % CAIA = SKIPA without the memory read
    8#305 -> sim_arithmetic:handle_CAIGE(Core, Mem, IR, EA);
    8#306 -> sim_arithmetic:handle_CAIN(Core, Mem, IR, EA);
    8#307 -> sim_arithmetic:handle_CAIG(Core, Mem, IR, EA);
    8#310 -> sim_arithmetic:handle_CAM(Core, Mem, IR, EA);
    8#311 -> sim_arithmetic:handle_CAML(Core, Mem, IR, EA);
    8#312 -> sim_arithmetic:handle_CAME(Core, Mem, IR, EA);
    8#313 -> sim_arithmetic:handle_CAMLE(Core, Mem, IR, EA);
    8#314 -> sim_arithmetic:handle_CAMA(Core, Mem, IR, EA);
    8#315 -> sim_arithmetic:handle_CAMGE(Core, Mem, IR, EA);
    8#316 -> sim_arithmetic:handle_CAMN(Core, Mem, IR, EA);
    8#317 -> sim_arithmetic:handle_CAMG(Core, Mem, IR, EA);
    8#320 -> next_pc(Core, Mem); % JUMP = no-op
    8#321 -> sim_arithmetic:handle_JUMPL(Core, Mem, IR, EA);
    8#322 -> sim_arithmetic:handle_JUMPE(Core, Mem, IR, EA);
    8#323 -> sim_arithmetic:handle_JUMPLE(Core, Mem, IR, EA);
    8#324 -> sim_arithmetic:handle_JUMPA(Core, Mem, IR, EA);
    8#325 -> sim_arithmetic:handle_JUMPGE(Core, Mem, IR, EA);
    8#326 -> sim_arithmetic:handle_JUMPN(Core, Mem, IR, EA);
    8#327 -> sim_arithmetic:handle_JUMPG(Core, Mem, IR, EA);
    8#330 -> sim_arithmetic:handle_SKIP(Core, Mem, IR, EA);
    8#331 -> sim_arithmetic:handle_SKIPL(Core, Mem, IR, EA);
    8#332 -> sim_arithmetic:handle_SKIPE(Core, Mem, IR, EA);
    8#333 -> sim_arithmetic:handle_SKIPLE(Core, Mem, IR, EA);
    8#334 -> sim_arithmetic:handle_SKIPA(Core, Mem, IR, EA);
    8#335 -> sim_arithmetic:handle_SKIPGE(Core, Mem, IR, EA);
    8#336 -> sim_arithmetic:handle_SKIPN(Core, Mem, IR, EA);
    8#337 -> sim_arithmetic:handle_SKIPG(Core, Mem, IR, EA);
    8#340 -> sim_arithmetic:handle_AOJ(Core, Mem, IR, EA);
    8#341 -> sim_arithmetic:handle_AOJL(Core, Mem, IR, EA);
    8#342 -> sim_arithmetic:handle_AOJE(Core, Mem, IR, EA);
    8#343 -> sim_arithmetic:handle_AOJLE(Core, Mem, IR, EA);
    8#344 -> sim_arithmetic:handle_AOJA(Core, Mem, IR, EA);
    8#345 -> sim_arithmetic:handle_AOJGE(Core, Mem, IR, EA);
    8#346 -> sim_arithmetic:handle_AOJN(Core, Mem, IR, EA);
    8#347 -> sim_arithmetic:handle_AOJG(Core, Mem, IR, EA);
    8#350 -> sim_arithmetic:handle_AOS(Core, Mem, IR, EA);
    8#351 -> sim_arithmetic:handle_AOSL(Core, Mem, IR, EA);
    8#352 -> sim_arithmetic:handle_AOSE(Core, Mem, IR, EA);
    8#353 -> sim_arithmetic:handle_AOSLE(Core, Mem, IR, EA);
    8#354 -> sim_arithmetic:handle_AOSA(Core, Mem, IR, EA);
    8#355 -> sim_arithmetic:handle_AOSGE(Core, Mem, IR, EA);
    8#356 -> sim_arithmetic:handle_AOSN(Core, Mem, IR, EA);
    8#357 -> sim_arithmetic:handle_AOSG(Core, Mem, IR, EA);
    8#360 -> sim_arithmetic:handle_SOJ(Core, Mem, IR, EA);
    8#361 -> sim_arithmetic:handle_SOJL(Core, Mem, IR, EA);
    8#362 -> sim_arithmetic:handle_SOJE(Core, Mem, IR, EA);
    8#363 -> sim_arithmetic:handle_SOJLE(Core, Mem, IR, EA);
    8#364 -> sim_arithmetic:handle_SOJA(Core, Mem, IR, EA);
    8#365 -> sim_arithmetic:handle_SOJGE(Core, Mem, IR, EA);
    8#366 -> sim_arithmetic:handle_SOJN(Core, Mem, IR, EA);
    8#367 -> sim_arithmetic:handle_SOJG(Core, Mem, IR, EA);
    8#370 -> sim_arithmetic:handle_SOS(Core, Mem, IR, EA);
    8#371 -> sim_arithmetic:handle_SOSL(Core, Mem, IR, EA);
    8#372 -> sim_arithmetic:handle_SOSE(Core, Mem, IR, EA);
    8#373 -> sim_arithmetic:handle_SOSLE(Core, Mem, IR, EA);
    8#374 -> sim_arithmetic:handle_SOSA(Core, Mem, IR, EA);
    8#375 -> sim_arithmetic:handle_SOSGE(Core, Mem, IR, EA);
    8#376 -> sim_arithmetic:handle_SOSN(Core, Mem, IR, EA);
    8#377 -> sim_arithmetic:handle_SOSG(Core, Mem, IR, EA);
    8#400 -> sim_boolean:handle_SETZ(Core, Mem, IR, EA);
    8#401 -> sim_boolean:handle_SETZ(Core, Mem, IR, EA); % SETZI = SETZ
    8#402 -> sim_boolean:handle_SETZM(Core, Mem, IR, EA);
    8#403 -> sim_boolean:handle_SETZB(Core, Mem, IR, EA);
    8#404 -> sim_boolean:handle_AND(Core, Mem, IR, EA);
    8#405 -> sim_boolean:handle_ANDI(Core, Mem, IR, EA);
    8#406 -> sim_boolean:handle_ANDM(Core, Mem, IR, EA);
    8#407 -> sim_boolean:handle_ANDB(Core, Mem, IR, EA);
    8#410 -> sim_boolean:handle_ANDCA(Core, Mem, IR, EA);
    8#411 -> sim_boolean:handle_ANDCAI(Core, Mem, IR, EA);
    8#412 -> sim_boolean:handle_ANDCAM(Core, Mem, IR, EA);
    8#413 -> sim_boolean:handle_ANDCAB(Core, Mem, IR, EA);
    8#414 -> sim_moves:handle_MOVE(Core, Mem, IR, EA); % SETM = MOVE
    8#415 -> sim_boolean:handle_SETMI(Core, Mem, IR, EA); % SETMI = MOVEI in section 0, XMOVEI elsewhere
    8#416 -> sim_boolean:handle_SETMM(Core, Mem, IR, EA);
    8#417 -> sim_boolean:handle_SETMB(Core, Mem, IR, EA);
    8#420 -> sim_boolean:handle_ANDCM(Core, Mem, IR, EA);
    8#421 -> sim_boolean:handle_ANDCMI(Core, Mem, IR, EA);
    8#422 -> sim_boolean:handle_ANDCMM(Core, Mem, IR, EA);
    8#423 -> sim_boolean:handle_ANDCMB(Core, Mem, IR, EA);
    8#424 -> next_pc(Core, Mem); % SETA = no-op
    8#425 -> next_pc(Core, Mem); % SETAI = no-op
    8#426 -> sim_moves:handle_MOVEM(Core, Mem, IR, EA); % SETAM = MOVEM
    8#427 -> sim_moves:handle_MOVEM(Core, Mem, IR, EA); % SETAB = MOVEM
    8#430 -> sim_boolean:handle_XOR(Core, Mem, IR, EA);
    8#431 -> sim_boolean:handle_XORI(Core, Mem, IR, EA);
    8#432 -> sim_boolean:handle_XORM(Core, Mem, IR, EA);
    8#433 -> sim_boolean:handle_XORB(Core, Mem, IR, EA);
    8#434 -> sim_boolean:handle_IOR(Core, Mem, IR, EA);
    8#435 -> sim_boolean:handle_IORI(Core, Mem, IR, EA);
    8#436 -> sim_boolean:handle_IORM(Core, Mem, IR, EA);
    8#437 -> sim_boolean:handle_IORB(Core, Mem, IR, EA);
    8#440 -> sim_boolean:handle_ANDCB(Core, Mem, IR, EA);
    8#441 -> sim_boolean:handle_ANDCBI(Core, Mem, IR, EA);
    8#442 -> sim_boolean:handle_ANDCBM(Core, Mem, IR, EA);
    8#443 -> sim_boolean:handle_ANDCBB(Core, Mem, IR, EA);
    8#444 -> sim_boolean:handle_EQV(Core, Mem, IR, EA);
    8#445 -> sim_boolean:handle_EQVI(Core, Mem, IR, EA);
    8#446 -> sim_boolean:handle_EQVM(Core, Mem, IR, EA);
    8#447 -> sim_boolean:handle_EQVB(Core, Mem, IR, EA);
    8#450 -> sim_boolean:handle_SETCA(Core, Mem, IR, EA);
    8#451 -> sim_boolean:handle_SETCA(Core, Mem, IR, EA); % SETCAI = SETCA
    8#452 -> sim_boolean:handle_SETCAM(Core, Mem, IR, EA);
    8#453 -> sim_boolean:handle_SETCAB(Core, Mem, IR, EA);
    8#454 -> sim_boolean:handle_ORCA(Core, Mem, IR, EA);
    8#455 -> sim_boolean:handle_ORCAI(Core, Mem, IR, EA);
    8#456 -> sim_boolean:handle_ORCAM(Core, Mem, IR, EA);
    8#457 -> sim_boolean:handle_ORCAB(Core, Mem, IR, EA);
    8#460 -> sim_boolean:handle_SETCM(Core, Mem, IR, EA);
    8#461 -> sim_boolean:handle_SETCMI(Core, Mem, IR, EA);
    8#462 -> sim_boolean:handle_SETCMM(Core, Mem, IR, EA);
    8#463 -> sim_boolean:handle_SETCMB(Core, Mem, IR, EA);
    8#464 -> sim_boolean:handle_ORCM(Core, Mem, IR, EA);
    8#465 -> sim_boolean:handle_ORCMI(Core, Mem, IR, EA);
    8#466 -> sim_boolean:handle_ORCMM(Core, Mem, IR, EA);
    8#467 -> sim_boolean:handle_ORCMB(Core, Mem, IR, EA);
    8#470 -> sim_boolean:handle_ORCB(Core, Mem, IR, EA);
    8#471 -> sim_boolean:handle_ORCBI(Core, Mem, IR, EA);
    8#472 -> sim_boolean:handle_ORCBM(Core, Mem, IR, EA);
    8#473 -> sim_boolean:handle_ORCBB(Core, Mem, IR, EA);
    8#474 -> sim_boolean:handle_SETO(Core, Mem, IR, EA);
    8#475 -> sim_boolean:handle_SETO(Core, Mem, IR, EA); % SETOI = SETO
    8#476 -> sim_boolean:handle_SETOM(Core, Mem, IR, EA);
    8#477 -> sim_boolean:handle_SETOB(Core, Mem, IR, EA);
    8#500 -> sim_halfword:handle_HLL(Core, Mem, IR, EA);
    8#501 -> sim_halfword:handle_HLLI(Core, Mem, IR, EA); % XHLLI in non-zero sections
    8#502 -> sim_halfword:handle_HLLM(Core, Mem, IR, EA);
    8#503 -> sim_halfword:handle_HLLS(Core, Mem, IR, EA);
    8#504 -> sim_halfword:handle_HRL(Core, Mem, IR, EA);
    8#505 -> sim_halfword:handle_HRLI(Core, Mem, IR, EA);
    8#506 -> sim_halfword:handle_HRLM(Core, Mem, IR, EA);
    8#507 -> sim_halfword:handle_HRLS(Core, Mem, IR, EA);
    8#510 -> sim_halfword:handle_HLLZ(Core, Mem, IR, EA);
    8#511 -> sim_boolean:handle_SETZ(Core, Mem, IR, EA); % HLLZI = SETZ
    8#512 -> sim_halfword:handle_HLLZM(Core, Mem, IR, EA);
    8#513 -> sim_halfword:handle_HLLZS(Core, Mem, IR, EA);
    8#514 -> sim_halfword:handle_HRLZ(Core, Mem, IR, EA);
    8#515 -> sim_moves:handle_MOVSI(Core, Mem, IR, EA); % HRLZI = MOVSI
    8#516 -> sim_halfword:handle_HRLZM(Core, Mem, IR, EA);
    8#517 -> sim_halfword:handle_HRLZS(Core, Mem, IR, EA);
    8#520 -> sim_halfword:handle_HLLO(Core, Mem, IR, EA);
    8#521 -> sim_halfword:handle_HLLOI(Core, Mem, IR, EA);
    8#522 -> sim_halfword:handle_HLLOM(Core, Mem, IR, EA);
    8#523 -> sim_halfword:handle_HLLOS(Core, Mem, IR, EA);
    8#524 -> sim_halfword:handle_HRLO(Core, Mem, IR, EA);
    8#525 -> sim_halfword:handle_HRLOI(Core, Mem, IR, EA);
    8#526 -> sim_halfword:handle_HRLOM(Core, Mem, IR, EA);
    8#527 -> sim_halfword:handle_HRLOS(Core, Mem, IR, EA);
    8#530 -> sim_halfword:handle_HLLE(Core, Mem, IR, EA);
    8#531 -> sim_boolean:handle_SETZ(Core, Mem, IR, EA); % HLLEI = HLLZI = SETZ
    8#532 -> sim_halfword:handle_HLLEM(Core, Mem, IR, EA);
    8#533 -> sim_halfword:handle_HLLES(Core, Mem, IR, EA);
    8#534 -> sim_halfword:handle_HRLE(Core, Mem, IR, EA);
    8#535 -> sim_halfword:handle_HRLEI(Core, Mem, IR, EA);
    8#536 -> sim_halfword:handle_HRLEM(Core, Mem, IR, EA);
    8#537 -> sim_halfword:handle_HRLES(Core, Mem, IR, EA);
    8#540 -> sim_halfword:handle_HRR(Core, Mem, IR, EA);
    8#541 -> sim_halfword:handle_HRRI(Core, Mem, IR, EA);
    8#542 -> sim_halfword:handle_HRRM(Core, Mem, IR, EA);
    8#543 -> sim_halfword:handle_HRRS(Core, Mem, IR, EA);
    8#544 -> sim_halfword:handle_HLR(Core, Mem, IR, EA);
    8#545 -> sim_halfword:handle_HLRI(Core, Mem, IR, EA);
    8#546 -> sim_halfword:handle_HLRM(Core, Mem, IR, EA);
    8#547 -> sim_halfword:handle_HLRS(Core, Mem, IR, EA);
    8#550 -> sim_halfword:handle_HRRZ(Core, Mem, IR, EA);
    8#551 -> sim_moves:handle_MOVEI(Core, Mem, IR, EA); % HRRZI = MOVEI
    8#552 -> sim_halfword:handle_HRRZM(Core, Mem, IR, EA);
    8#553 -> sim_halfword:handle_HRRZS(Core, Mem, IR, EA);
    8#554 -> sim_halfword:handle_HLRZ(Core, Mem, IR, EA);
    8#555 -> sim_boolean:handle_SETZ(Core, Mem, IR, EA); % HLRZI = HLLZI = SETZ
    8#556 -> sim_halfword:handle_HLRZM(Core, Mem, IR, EA);
    8#557 -> sim_halfword:handle_HLRZS(Core, Mem, IR, EA);
    8#560 -> sim_halfword:handle_HRRO(Core, Mem, IR, EA);
    8#561 -> sim_halfword:handle_HRROI(Core, Mem, IR, EA);
    8#562 -> sim_halfword:handle_HRROM(Core, Mem, IR, EA);
    8#563 -> sim_halfword:handle_HRROS(Core, Mem, IR, EA);
    8#564 -> sim_halfword:handle_HLRO(Core, Mem, IR, EA);
    8#565 -> sim_halfword:handle_HLROI(Core, Mem, IR, EA);
    8#566 -> sim_halfword:handle_HLROM(Core, Mem, IR, EA);
    8#567 -> sim_halfword:handle_HLROS(Core, Mem, IR, EA);
    8#570 -> sim_halfword:handle_HRRE(Core, Mem, IR, EA);
    8#571 -> sim_halfword:handle_HRREI(Core, Mem, IR, EA);
    8#572 -> sim_halfword:handle_HRREM(Core, Mem, IR, EA);
    8#573 -> sim_halfword:handle_HRRES(Core, Mem, IR, EA);
    8#574 -> sim_halfword:handle_HLRE(Core, Mem, IR, EA);
    8#575 -> sim_boolean:handle_SETZ(Core, Mem, IR, EA); % HLREI = HLRZI = SETZ
    8#576 -> sim_halfword:handle_HLREM(Core, Mem, IR, EA);
    8#577 -> sim_halfword:handle_HLRES(Core, Mem, IR, EA);
    8#600 -> next_pc(Core, Mem); % TRN = no-op
    8#601 -> next_pc(Core, Mem); % TLN = no-op
    8#602 -> sim_logical:handle_TRNE(Core, Mem, IR, EA);
    8#603 -> sim_logical:handle_TLNE(Core, Mem, IR, EA);
    8#604 -> skip(Core, Mem); % TRNA = SKIPA without the memory read
    8#605 -> skip(Core, Mem); % TLNA = SKIPA without the memory read
    8#606 -> sim_logical:handle_TRNN(Core, Mem, IR, EA);
    8#607 -> sim_logical:handle_TLNN(Core, Mem, IR, EA);
    8#610 -> sim_logical:handle_TDN(Core, Mem, IR, EA);
    8#611 -> sim_logical:handle_TSN(Core, Mem, IR, EA);
    8#612 -> sim_logical:handle_TDNE(Core, Mem, IR, EA);
    8#613 -> sim_logical:handle_TSNE(Core, Mem, IR, EA);
    8#614 -> sim_logical:handle_TDNA(Core, Mem, IR, EA);
    8#615 -> sim_logical:handle_TSNA(Core, Mem, IR, EA);
    8#616 -> sim_logical:handle_TDNN(Core, Mem, IR, EA);
    8#617 -> sim_logical:handle_TSNN(Core, Mem, IR, EA);
    8#620 -> sim_logical:handle_TRZ(Core, Mem, IR, EA);
    8#621 -> sim_logical:handle_TLZ(Core, Mem, IR, EA);
    8#622 -> sim_logical:handle_TRZE(Core, Mem, IR, EA);
    8#623 -> sim_logical:handle_TLZE(Core, Mem, IR, EA);
    8#624 -> sim_logical:handle_TRZA(Core, Mem, IR, EA);
    8#625 -> sim_logical:handle_TLZA(Core, Mem, IR, EA);
    8#626 -> sim_logical:handle_TRZN(Core, Mem, IR, EA);
    8#627 -> sim_logical:handle_TLZN(Core, Mem, IR, EA);
    8#630 -> sim_logical:handle_TDZ(Core, Mem, IR, EA);
    8#631 -> sim_logical:handle_TSZ(Core, Mem, IR, EA);
    8#632 -> sim_logical:handle_TDZE(Core, Mem, IR, EA);
    8#633 -> sim_logical:handle_TSZE(Core, Mem, IR, EA);
    8#634 -> sim_logical:handle_TDZA(Core, Mem, IR, EA);
    8#635 -> sim_logical:handle_TSZA(Core, Mem, IR, EA);
    8#636 -> sim_logical:handle_TDZN(Core, Mem, IR, EA);
    8#637 -> sim_logical:handle_TSZN(Core, Mem, IR, EA);
    8#640 -> sim_logical:handle_TRC(Core, Mem, IR, EA);
    8#641 -> sim_logical:handle_TLC(Core, Mem, IR, EA);
    8#642 -> sim_logical:handle_TRCE(Core, Mem, IR, EA);
    8#643 -> sim_logical:handle_TLCE(Core, Mem, IR, EA);
    8#644 -> sim_logical:handle_TRCA(Core, Mem, IR, EA);
    8#645 -> sim_logical:handle_TLCA(Core, Mem, IR, EA);
    8#646 -> sim_logical:handle_TRCN(Core, Mem, IR, EA);
    8#647 -> sim_logical:handle_TLCN(Core, Mem, IR, EA);
    8#650 -> sim_logical:handle_TDC(Core, Mem, IR, EA);
    8#651 -> sim_logical:handle_TSC(Core, Mem, IR, EA);
    8#652 -> sim_logical:handle_TDCE(Core, Mem, IR, EA);
    8#653 -> sim_logical:handle_TSCE(Core, Mem, IR, EA);
    8#654 -> sim_logical:handle_TDCA(Core, Mem, IR, EA);
    8#655 -> sim_logical:handle_TSCA(Core, Mem, IR, EA);
    8#656 -> sim_logical:handle_TDCN(Core, Mem, IR, EA);
    8#657 -> sim_logical:handle_TSCN(Core, Mem, IR, EA);
    8#660 -> sim_logical:handle_TRO(Core, Mem, IR, EA);
    8#661 -> sim_logical:handle_TLO(Core, Mem, IR, EA);
    8#662 -> sim_logical:handle_TROE(Core, Mem, IR, EA);
    8#663 -> sim_logical:handle_TLOE(Core, Mem, IR, EA);
    8#664 -> sim_logical:handle_TROA(Core, Mem, IR, EA);
    8#665 -> sim_logical:handle_TLOA(Core, Mem, IR, EA);
    8#666 -> sim_logical:handle_TRON(Core, Mem, IR, EA);
    8#667 -> sim_logical:handle_TLON(Core, Mem, IR, EA);
    8#670 -> sim_logical:handle_TDO(Core, Mem, IR, EA);
    8#671 -> sim_logical:handle_TSO(Core, Mem, IR, EA);
    8#672 -> sim_logical:handle_TDOE(Core, Mem, IR, EA);
    8#673 -> sim_logical:handle_TSOE(Core, Mem, IR, EA);
    8#674 -> sim_logical:handle_TDOA(Core, Mem, IR, EA);
    8#675 -> sim_logical:handle_TSOA(Core, Mem, IR, EA);
    8#676 -> sim_logical:handle_TDON(Core, Mem, IR, EA);
    8#677 -> sim_logical:handle_TSON(Core, Mem, IR, EA);
    _ ->
      PC = (Core#core.pc_section bsl 18) bor Core#core.pc_offset,
      {Core, Mem, {error, {?MODULE, {dispatch, PC, IR, EA}}}}
  end.

%% Page Fault Handling =========================================================

-spec page_fault(#core{}, sim_mem:mem(), #ea{}, atom(), term(), fun())
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
page_fault(Core, Mem, EA, Op, Reason, Cont) ->
  Address = (EA#ea.section bsl 18) bor EA#ea.offset,
  %% This should trap to kernel mode, but for now we treat all faults as fatal.
  PC = (Core#core.pc_section bsl 18) bor Core#core.pc_offset,
  {Core, Mem, {error, {?MODULE, {page_fault, Address, PC, Op, Reason, Cont}}}}.

%% Miscellaneous ===============================================================

%% bits36(X, LEFT, RIGHT)
%%
%% Extracts bits LEFT through RIGHT (inclusive) from a 36-bit number X.
%% Bit 0 is most significant, and 35 least significant.
%% Requires 0 =< LEFT =< RIGHT =< 35.
-spec bits36(word(), 0..35, 0..35) -> word().
bits36(X, LEFT, RIGHT) ->
  true = 0 =< LEFT andalso LEFT =< RIGHT andalso RIGHT =< 35,
  (X bsr (35 - RIGHT)) band ((1 bsl (1 + RIGHT - LEFT)) - 1).

%% bits36low18(X)
%%
%% Extract the right 18 bits from a 36-bit number X.
-spec bits36low18(word()) -> word().
bits36low18(X) -> bits36(X, 18, 35).

%% bit36(X, BIT)
%%
%% Extract bit BIT from a 36-bit number X.
%% Bit 0 is most significant, and 35 least significant.
%% Requires 0 =< BIT =< 35.
-spec bit36(word(), 0..35) -> word().
bit36(X, BIT) -> bits36(X, BIT, BIT).

%% Sign-extend a uint18_t() to the full width of its representation type.
-spec sext18(uint18_t()) -> integer().
sext18(X) ->
  UInt18Sbit = 1 bsl (18 - 1),
  UInt18Max = (1 bsl 18) - 1,
  ((X band UInt18Max) bxor UInt18Sbit) - UInt18Sbit.

%% This implements Word := C(E).
-spec c(#core{}, sim_mem:mem(), #ea{})
      -> {ok, word()} | {error, {sim_mem:prot(), sim_mem:what()} | false}.
c(Core, Mem, #ea{section = Section, offset = Offset, islocal = IsLocal}) ->
  c(Core, Mem, Section, Offset, IsLocal).

-spec c(#core{}, sim_mem:mem(), Section :: uint12_t(), Offset :: uint18_t(), IsLocal :: boolean())
      -> {ok, word()} | {error, {sim_mem:prot(), sim_mem:what()} | false}.
c(Core, Mem, Section, Offset, IsLocal) ->
  case Offset =< 8#17 andalso (IsLocal orelse Section =< 1) of
    true -> {ok, get_ac(Core, Offset)};
    false ->
      Address = (Section bsl 18) bor Offset,
      sim_mem:read_word(Mem, Address)
  end.

%% This implements C(E) := Word.
-spec cset(#core{}, sim_mem:mem(), #ea{}, word())
      -> {ok, #core{}} | {error, {sim_mem:prot(), sim_mem:what()} | false}.
cset(Core, Mem, #ea{section = Section, offset = Offset, islocal = IsLocal}, Word) ->
  case Offset =< 8#17 andalso (IsLocal orelse Section =< 1) of
    true -> {ok, set_ac(Core, Offset, Word)};
    false ->
      Address = (Section bsl 18) bor Offset,
      case sim_mem:write_word(Mem, Address, Word) of
        ok -> {ok, Core};
        {error, _Reason} = Error -> Error
      end
  end.

-spec get_ac(#core{}, 0..8#17) -> word().
get_ac(Core, Nr) -> element(Nr + 1, Core#core.acs).

-spec set_ac(#core{}, 0..8#17, word()) -> #core{}.
set_ac(#core{acs = ACS} = Core, Nr, Val) ->
  Core#core{acs = do_set_ac(ACS, Nr, Val)}.

do_set_ac(ACS, Nr, Val) -> setelement(Nr + 1, ACS, Val).

-spec set_flag(#core{}, 0..12) -> #core{}.
set_flag(#core{flags = Flags} = Core, Flag) ->
  Core#core{flags = Flags bor (1 bsl Flag)}.

-spec set_flags(#core{}, uint13_t()) -> #core{}.
set_flags(#core{flags = Flags0} = Core, Flags1) ->
  Core#core{flags = Flags0 bor Flags1}.

%% Error Formatting ============================================================

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {dispatch, PC, IR, EA} ->
      io_lib:format("DISPATCH: PC=~10.8.0b IR=~5.8.0b EA=~s NYI",
                    [PC, IR, format_ea(EA)]);
    {page_fault, Address, PC, Op, Reason, Cont} ->
      io_lib:format("PAGE FAULT: ADDR=~10.8.0b PC=~10.8.0b OP=~p RSN=~p CONT=~p",
                    [Address, PC, Op, Reason, Cont]);
    {stack_fault, PC} ->
      io_lib:format("STACK FAULT: PC=~10.8.0b", [PC])
  end.

format_ea(#ea{section = Section, offset = Offset, islocal = IsLocal}) ->
  Address = (Section bsl 18) bor Offset,
  Indicator = case IsLocal of true -> "local"; false -> "global" end,
  io_lib:format("~10.8.0b/~s", [Address, Indicator]).
