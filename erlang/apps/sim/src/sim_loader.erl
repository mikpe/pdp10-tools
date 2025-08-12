%%% -*- erlang-indent-level: 2 -*-
%%%
%%% simulator for pdp10-elf
%%% Copyright (C) 2020-2025  Mikael Pettersson
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
%%% Loads a pdp10-elf user-mode program into a fresh address space.

-module(sim_loader).

-export([ load/3
        , format_error/1
        ]).

-include_lib("lib/include/libelf.hrl").

-type address() :: non_neg_integer().

-export_type([address/0]).

%% ELF loader ==================================================================

-spec load(file:name_all(), [string()], [string()])
      -> {ok, { sim_mem:mem()
              , PC :: address()
              , SP :: address()
              , Argc :: non_neg_integer()
              , Argv :: address()
              , Envp :: address()
              }}
       | {error, {module(), term()}}.
load(Exe, ArgvStrings, EnvStrings) ->
  case pdp10_stdio:fopen(Exe, [raw, read]) of
    {ok, FP} ->
      try load_fp(FP, [Exe | ArgvStrings], EnvStrings)
      after pdp10_stdio:fclose(FP) end;
    {error, _Reason} = Error -> Error
  end.

load_fp(FP, ArgvStrings, EnvStrings) ->
  case libelf:read_Ehdr(?ELFCLASS36, FP) of
    {ok, Ehdr} -> load(FP, Ehdr, ArgvStrings, EnvStrings);
    {error, _Reason} = Error -> Error
  end.

load(FP, Ehdr, ArgvStrings, EnvStrings) ->
  case Ehdr#elf_Ehdr.e_type of
    ?ET_EXEC ->
      case libelf:read_PhTab(?ELFCLASS36, FP, Ehdr) of
        {ok, PhTab} -> load(FP, Ehdr, PhTab, ArgvStrings, EnvStrings);
        {error, _Reason} = Error -> Error
      end;
    EType -> {error, {?MODULE, {invalid_ehdr_type, EType}}}
  end.

load(FP, Ehdr, PhTab, ArgvStrings, EnvStrings) ->
  Mem = sim_mem:new(),
  case load_phtab(FP, Ehdr, PhTab, 0, Mem) of
    {ok, PC} ->
      {SP, Argv, Envp} = init_stack(Mem, ArgvStrings, EnvStrings),
      {ok, {Mem, PC, SP, _Argc = length(ArgvStrings), Argv, Envp}};
    {error, _Reason} = Error ->
      sim_mem:delete(Mem),
      Error
  end.

init_stack(Mem, ArgvStrings, EnvStrings) ->
  %% TODO: assumes large or small code model output, not tiny
  Stack0 = 8#00001001000 bsl 2, % section 1, page 1, word 0
  Size = ((512 - 2) * 512) bsl 2, % 510 pages (pages 0 and 511 left unmapped)
  map_zero_core(Mem, Stack0, Size),
  {ArgvPointers, Stack1} = store_strings(ArgvStrings, Mem, Stack0),
  %% Stack1 is the start of argv[].
  Stack2 = store_pointers(ArgvPointers, Mem, Stack1),
  %% Stack2 points after argv[] to a word containing a NULL pointer.
  Stack3 = Stack2 + 4,
  {EnvPointers, Stack4} = store_strings(EnvStrings, Mem, Stack3),
  %% Stack4 is the start of envp[].
  Stack5 = store_pointers(EnvPointers, Mem, Stack4),
  %% Stack5 points after envp[] to a word containing a NULL pointer.
  %% This word terminates envp[] and signals a NULL return address to _start().
  { _SP = byte_address_to_global_word_address(Stack5)
  , _Argv = byte_address_to_global_word_address(Stack1)
  , _Envp = byte_address_to_global_word_address(Stack4)
  }.

store_strings(ArgvStrings, Mem, Stack) ->
  store_strings(ArgvStrings, Mem, Stack, []).

store_strings([], _Mem, Stack, Acc) -> {lists:reverse(Acc), Stack};
store_strings([String | Strings], Mem, Stack, Acc) ->
  Pointer = byte_address_to_global_byte_pointer(Stack),
  NewStack = store_string(String, Mem, Stack),
  store_strings(Strings, Mem, NewStack, [Pointer | Acc]).

store_string([B0, B1, B2, B3 | Rest], Mem, Stack) ->
  Word = extint:uint36_from_ext([B0, B1, B2, B3]),
  write_word(Mem, Stack, Word),
  store_string(Rest, Mem, Stack + 4);
store_string(Tail, Mem, Stack) ->
  Pad = lists:duplicate(4 - length(Tail), 0),
  Word = extint:uint36_from_ext(Tail ++ Pad),
  write_word(Mem, Stack, Word),
  Stack + 4.

store_pointers([], _Mem, Stack) -> Stack;
store_pointers([Pointer | Pointers], Mem, Stack) ->
  write_word(Mem, Stack, Pointer),
  store_pointers(Pointers, Mem, Stack + 4).

byte_address_to_global_word_address(ByteAddress) ->
  0 = ByteAddress band 3, % assert
  ByteAddress bsr 2. % EFIW: bits 0 to 5 zero, address in bits 6 to 35.

byte_address_to_global_byte_pointer(ByteAddress) ->
  Y = ByteAddress bsr 2,
  PS = 8#70 + (ByteAddress band 3),
  (PS bsl 30) bor Y.

load_phtab(_FP, Ehdr, _PhTab = [], _PhdrIx, _Mem) ->
  {ok, byte_address_to_global_word_address(Ehdr#elf_Ehdr.e_entry)};
load_phtab(FP, Ehdr, [Phdr | PhTab], PhdrIx, Mem) ->
  case load_phdr(FP, Phdr, PhdrIx, Mem) of
    ok -> load_phtab(FP, Ehdr, PhTab, PhdrIx + 1, Mem);
    {error, _Reason} = Error -> Error
  end.

load_phdr(FP, Phdr, PhdrIx, Mem) ->
  case Phdr#elf_Phdr.p_type of
    ?PT_NULL -> ok;
    ?PT_LOAD ->
      case is_valid_phdr(Phdr) of
        true -> do_load_phdr(FP, Phdr, Mem);
        false -> {error, {?MODULE, {invalid_phdr, PhdrIx}}}
      end;
    PType -> {error, {?MODULE, {invalid_phdr_type, PType}}}
  end.

do_load_phdr(FP, Phdr, Mem) ->
  #elf_Phdr{ p_offset = Offset
           , p_vaddr = VAddr
           , p_filesz = FileSz
           , p_memsz = MemSz
           , p_flags = Flags } = Phdr,
  %% TODO: change this use demand paging to read the file lazily
  case pdp10_stdio:fseek(FP, {bof, Offset}) of
    ok ->
      map_zero_core(Mem, VAddr, MemSz),
      0 = (VAddr band 3),
      case copy_file_to_core(Mem, VAddr, FP, FileSz) of
        ok ->
          case (Flags band ?PF_W) of
            0 -> remap_read_only(Mem, VAddr, MemSz);
            _ -> ok
          end;
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

map_zero_core(Mem, VAddr, Size) when Size > 0 ->
  PFN = VAddr bsr (9 + 2),
  false = sim_mem:mquery(Mem, PFN),
  sim_mem:mmap(Mem, PFN, 4+2, core),
  map_zero_core(Mem, VAddr + 512*4, Size - 512*4);
map_zero_core(_Mem, _VAddr, _Size) -> ok.

copy_file_to_core(Mem, VAddr, FP, Size) when Size >= 4 ->
  case read(FP, 4) of
    {ok, Nonets} ->
      Word = extint:uint36_from_ext(Nonets),
      write_word(Mem, VAddr, Word),
      copy_file_to_core(Mem, VAddr + 4, FP, Size - 4);
    {error, _Reason} = Error -> Error
  end;
copy_file_to_core(Mem, VAddr, FP, Size) when Size > 0 ->
  case read(FP, Size) of
    {ok, Nonets} ->
      Word = extint:uint36_from_ext(Nonets ++ lists:duplicate(4 - Size, 0)),
      write_word(Mem, VAddr, Word);
    {error, _Reason} = Error -> Error
  end;
copy_file_to_core(_Mem, _VAddr, _FP, _Size = 0) -> ok.

read(FP, N) ->
  case pdp10_stdio:fread(1, N, FP) of
    eof -> {error, {?MODULE, eof}};
    Else -> Else % {ok, Nonets} or {error, Reason}
  end.

remap_read_only(Mem, VAddr, Size) when Size > 0 ->
  PFN = VAddr bsr (9 + 2),
  ok = sim_mem:mprotect(Mem, PFN),
  remap_read_only(Mem, VAddr + 512*4, Size - 512*4);
remap_read_only(_Mem, _VAddr, _Size) -> ok.

is_valid_phdr(Phdr) ->
  #elf_Phdr{ p_offset = Offset
           , p_vaddr = VAddr
           , p_filesz = FileSz
           , p_memsz = MemSz
           , p_flags = Flags } = Phdr,
  is_page_aligned(Offset) andalso
  is_page_aligned(VAddr) andalso
  MemSz >= FileSz andalso
  no_excess_flags(Flags).

is_page_aligned(Offset) -> (Offset band (512*4 - 1)) =:= 0.

no_excess_flags(Flags) -> (Flags band 8#7) =:= Flags.

write_word(Mem, ByteAddress, Word) ->
  0 = ByteAddress band 3, % assert
  ok = sim_mem:write_word(Mem, ByteAddress bsr 2, Word).

%% Error Formatting ============================================================

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {invalid_ehdr_type, Type} -> io_lib:format("invalid Ehdr.e_type ~p", [Type]);
    {invalid_phdr, Ix} -> io_lib:format("invalid phdr at index ~p", [Ix]);
    {invalid_phdr_type, Type} -> io_lib:format("invalid Phdr.p_type ~p", [Type]);
    eof -> "premature EOF while reading ELF segment"
  end.
