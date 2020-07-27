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

%% Instruction Fetch and Effective Address Calculation =========================
%% c.f. Toad-1 Architecture Manual, page 41, Figure 1.11

%% Instruction Fetch.  This always uses local addressing.
insn_fetch(Core0, Mem) ->
  %% TODO: Handle 2.9.6 Overflow Trapping
  %% For now, arithmetic overflows are ignored, as if the user had installed a
  %% no-op handler, so that C's unsigned arithmetic can work, and stack overflows
  %% are treated as fatal errors.
  #core{flags = Flags0} = Core0,
  Core = Core0#core{flags = Flags0 band bnot (1 bsl ?PDP10_PF_TRAP_1)},
  PCOffset = Core#core.pc_offset,
  case PCOffset =< 8#17 of
    true ->
      MB = get_ac(Core, PCOffset),
      insn_fetch2(Core, Mem, MB);
    false ->
      Address = (Core#core.pc_section bsl 18) bor PCOffset,
      case sim_mem:read_word(Mem, Address) of
        {ok, MB} -> insn_fetch2(Core, Mem, MB);
        {error, Reason} -> page_fault(Core, Mem, Address, read, Reason, fun insn_fetch/2)
      end
  end.

insn_fetch2(Core, Mem, MB) ->
  IR = bits36(MB, 0, 12), % IR := MB_<0:12>
  ESection = Core#core.pc_section, % E_<6:17> := PC_<6:17>
  calculate_ea(Core, Mem, IR, MB, ESection).

calculate_ea(Core, Mem, IR, MB, ESection) ->
  local_format_address_word(Core, Mem, IR, MB, ESection).

local_format_address_word(Core, Mem, IR, MB, ESection) ->
  Y = bits36(MB, 18, 35), % Y_<18:35> := MB_<18:35>
  X = bits36(MB, 14, 17), % X := MB_<14:17>
  I = bit36(MB, 13),      % I := MB_<13>
  %% Indexed Address?  Test X field.
  case X of
    0 ->
      %% X field =:= 0.  No Indexing.
      EOffset = Y, % E_<18:35> := Y_<18:35>
      indirect_addressing(Core, Mem, IR, I, ESection, EOffset, _IsLocal = true);
    _ ->
      %% X field =/= 0.  Indexing.
      CX = get_ac(Core, X), % C(X) hoisted from following blocks
      %% Test Section Number in E_<6:17>.
      case ESection of
        0 ->
          local_index(Core, Mem, IR, I, ESection, CX, Y);
        _ ->
          %% Section =/= 0.
          %% Test C(X).  Global Index when (C(X)_<0> =:= 0) and (C(X)_<6:17> =/= 0).
          case bit36(CX, 0) =:= 0 andalso bits36(CX, 6, 17) =/= 0 of
            true ->
              global_index(Core, Mem, IR, I, CX, Y);
            false ->
              local_index(Core, Mem, IR, I, ESection, CX, Y)
          end
      end
  end.

local_index(Core, Mem, IR, I, ESection, CX, Y) ->
  EOffset = bits36low18(CX + Y), % E_<18:35> := C(X)_<18:35> + Y_<18:35>
  indirect_addressing(Core, Mem, IR, I, ESection, EOffset, _IsLocal = true).

global_index(Core, Mem, IR, I, CX, Y) ->
  Y1 = sext18(Y), % Y_<6:17> =:= 7777 * Y_<18>
  E = CX + Y1, % E_<6:35> := C(X)_<6:35> + Y_<6:35>
  ESection = bits36(E, 6, 17),
  EOffset = bits36(E, 18, 35),
  indirect_addressing(Core, Mem, IR, I, ESection, EOffset, _IsLocal = false).

indirect_addressing(Core, Mem, IR, I, ESection, EOffset, IsLocal) ->
  %% Test I bit.
  case I of
    0 ->
      %% I =:= 0.  Done!
      %% E is the Effective Address.
      %% IsLocal signals if E is a global address or if it is a local address
      %% in the last section from which an address word was fetched.  This
      %% matters for instructions that use E and E+1, and also to determine
      %% if an address denotes an accumulator or a memory location.
      dispatch(Core, Mem, IR, #ea{section = ESection, offset = EOffset, islocal = IsLocal});
    _ ->
      %% I =:= 1.
      fetch_indirect_word(Core, Mem, IR, ESection, EOffset, IsLocal)
  end.

fetch_indirect_word(Core, Mem, IR, ESection, EOffset, IsLocal) ->
  %% Fetch the Indirect Word.
  case c(Core, Mem, ESection, EOffset, IsLocal) of
    {ok, MB} ->
      %% Non-zero section?  Test E_<6:17>.
      case ESection of
        0 ->
          local_format_address_word(Core, Mem, IR, MB, ESection);
        _ ->
          %% Section =/= 0.
          %% Decode Indirect Word MB_<0:1>.
          case bits36(MB, 0, 1) of
            0 ->
              global_indirect_word(Core, Mem, IR, MB, _I = 0);
            1 ->
              global_indirect_word(Core, Mem, IR, MB, _I = 1);
            2 ->
              %% Local Indirect
              local_format_address_word(Core, Mem, IR, MB, ESection);
            3 ->
              E = (ESection bsl 18) bor EOffset,
              page_fault(Core, Mem, E, read, indirect_word,
                         fun(Core1, Mem1) ->
                            fetch_indirect_word(Core1, Mem1, IR, ESection, EOffset, IsLocal)
                          end)
          end
      end;
    {error, Reason} ->
      E = (ESection bsl 18) bor EOffset,
      page_fault(Core, Mem, E, read, Reason,
                 fun(Core1, Mem1) ->
                   fetch_indirect_word(Core1, Mem1, IR, ESection, EOffset, IsLocal)
                 end)
  end.

global_indirect_word(Core, Mem, IR, MB, I) ->
  Y = bits36(MB, 6, 35), % Y:= MB_<6,35>
  X = bits36(MB, 2, 5), % X := MB_<2:5>
  %% Indexed Address?  Test X field.
  case X of
    0 ->
      E = Y, % E_<6:35> := Y_<6:35>
      ESection1 = bits36(E, 6, 17),
      EOffset1 = bits36(E, 18, 35),
      indirect_addressing(Core, Mem, IR, I, ESection1, EOffset1, _IsLocal = false);
    _ ->
      %% X =/= 0.
      CX = get_ac(Core, X),
      E = CX + Y, % E_<6:35> := C(X)_<6:35> + Y_<6:35>
      ESection1 = bits36(E, 6, 17),
      EOffset1 = bits36(E, 18, 35),
      indirect_addressing(Core, Mem, IR, I, ESection1, EOffset1, _IsLocal = false)
  end.

%% Instruction Dispatch ========================================================

-spec dispatch(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
dispatch(Core, Mem, IR, EA) ->
  %% Dispatch on the opcode (top 9 bits).
  case IR bsr 4 of
    8#104 -> sim_kernel:handle_JSYS(Core, Mem, IR, EA);
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
    8#250 -> sim_moves:handle_EXCH(Core, Mem, IR, EA);
    8#251 -> sim_moves:handle_BLT(Core, Mem, IR, EA);
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
    _ ->
      PC = (Core#core.pc_section bsl 18) bor Core#core.pc_offset,
      {Core, Mem, {error, {?MODULE, {dispatch, PC, IR, EA}}}}
  end.

%% Page Fault Handling =========================================================

-spec page_fault(#core{}, sim_mem:mem(), word(), atom(), term(), fun())
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
page_fault(Core, Mem, Address, Op, Reason, Cont) ->
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
                    [Address, PC, Op, Reason, Cont])
  end.

format_ea(#ea{section = Section, offset = Offset, islocal = IsLocal}) ->
  Address = (Section bsl 18) bor Offset,
  Indicator = case IsLocal of true -> "local"; false -> "global" end,
  io_lib:format("~10.8.0b/~s", [Address, Indicator]).
