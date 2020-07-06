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
        , format_error/1
        ]).

-include("sim_core.hrl").

%% Run a program ===============================================================

-spec run(Mem :: sim_mem:mem(),
          PC :: word(),
          SP :: word(),
          Argc :: word(),
          Argv :: word(),
          Envp :: word()) -> ok | {error, {module(), term()}}.
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

-spec run(#core{}, sim_mem:mem()) -> {#core{}, sim_mem:mem(), ok | {error, {module(), term()}}}.
run(Core, Mem) ->
  insn_fetch(Core, Mem).

%% Instruction Fetch and Effective Address Calculation =========================
%% c.f. Toad-1 Architecture Manual, page 41, Figure 1.11

%% Instruction Fetch.  This always uses local addressing.
insn_fetch(Core, Mem) ->
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
      -> {#core{}, sim_mem:mem(), ok | {error, {module(), term()}}}.
dispatch(Core, Mem, IR, EA) ->
  PC = (Core#core.pc_section bsl 18) bor Core#core.pc_offset,
  {Core, Mem, {error, {?MODULE, {dispatch, PC, IR, EA}}}}. % FIXME

%% Page Fault Handling =========================================================

-spec page_fault(#core{}, sim_mem:mem(), word(), atom(), term(), fun())
      -> {#core{}, sim_mem:mem(), ok | {error, {module(), term()}}}.
page_fault(Core, Mem, Address, Op, Reason, Cont) ->
  %% This should trap to supervisor mode, but for now we treat all faults as fatal.
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

-spec c(#core{}, sim_mem:mem(), Section :: uint12_t(), Offset :: uint18_t(), IsLocal :: boolean())
      -> {ok, word()} | {error, {sim_mem:prot(), sim_mem:what()} | false}.
c(Core, Mem, Section, Offset, IsLocal) ->
  case Offset =< 8#17 andalso (IsLocal orelse Section =< 1) of
    true -> {ok, get_ac(Core, Offset)};
    false ->
      Address = (Section bsl 18) bor Offset,
      sim_mem:read_word(Mem, Address)
  end.

-spec get_ac(#core{}, 0..8#17) -> word().
get_ac(Core, Nr) -> element(Nr + 1, Core#core.acs).

do_set_ac(ACS, Nr, Val) -> setelement(Nr + 1, ACS, Val).

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
