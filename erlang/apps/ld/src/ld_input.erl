%%% -*- erlang-indent-level: 2 -*-
%%%
%%% input processing for pdp10-elf ld
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
%%% - Process input files in the order given.
%%% - Maintain sets of currently defined and undefined symbols, initially empty
%%%   and the entry symbol, respectiively.
%%% - An input object file is included unconditionally. Its symbol table is read
%%%   and the sets of defined and undefined symbols are updated accordingly.
%%% - An input archive has its members processed in the order stored.
%%% - An input archive member is included if any only if the archive's symbol table
%%%   indicates that the member defines some currently undefined symbol.
%%% - If an archive member is included it is processed exactly like an explicit
%%%   input object file.

-module(ld_input).

-export([ input/2
        , format_error/1
        ]).

-include("ld_internal.hrl").
-include_lib("lib/include/archive.hrl").
-include_lib("lib/include/pdp10_ar.hrl").

%% error reasons
-define(badelf, badelf).
-define(badfile, badfile).
-define(muldef_symbol, muldef_symbol).
-define(noinputfiles, noinputfiles).
-define(undefined_symbols, undefined_symbols).

%% Input Processing ============================================================

-spec input([{file, string()}], [string()])
        -> {ok, [#input{}]} | {error, {module(), term()}}.
input(_Files = [], _UndefSyms) -> mkerror(?noinputfiles);
input(Files, UndefSyms) ->
  UndefMap = maps:from_keys(UndefSyms, false),
  input(Files, _Defmap = maps:new(), UndefMap, _Inputs = []).

input([{file, File} | Files], DefMap, UndefMap, Inputs) ->
  case input_file(File, DefMap, UndefMap, Inputs) of
    {ok, {NewDefMap, NewUndefMap, NewInputs}} ->
      input(Files, NewDefMap, NewUndefMap, NewInputs);
    {error, _Reason} = Error -> Error
  end;
input([], _DefMap, UndefMap, Inputs) ->
  case maps:keys(UndefMap) of
    [] -> {ok, lists:reverse(Inputs)};
    UndefSyms -> mkerror({?undefined_symbols, UndefSyms})
  end.

input_file(File, DefMap, UndefMap, Inputs) ->
  case archive:read(File) of
    {ok, {FP, Archive}} ->
      try
        input_archive(File, FP, Archive, DefMap, UndefMap, Inputs)
      after
        pdp10_stdio:fclose(FP)
      end;
    {error, _} ->
      case pdp10_stdio:fopen(File, [raw, read]) of
        {ok, FP} ->
          try
            input_elf(File, FP, DefMap, UndefMap, Inputs)
          after
            pdp10_stdio:fclose(FP)
          end;
        {error, Reason} -> mkerror({?badfile, File, Reason})
      end
  end.

%% Process input archive =======================================================

input_archive(File, FP, Archive, DefMap, UndefMap, Inputs) ->
  #archive{symtab = SymTab, members = Members} = Archive,
  %% Build a mapping from member offset to the set of symbols defined there.
  OffsetToDefs =
    lists:foldl(
      fun({Symbol, Offset}, Map) ->
        Symbols = maps:get(Offset, Map, #{}),
        maps:put(Offset, maps:put(Symbol, false, Symbols), Map)
      end, maps:new(), SymTab),
  input_archive(Members, OffsetToDefs, File, FP, DefMap, UndefMap, Inputs).

input_archive(_Members = [], _OffsetToDefs, _File, _FP, DefMap, UndefMap, Inputs) ->
  {ok, {DefMap, UndefMap, Inputs}};
input_archive([Member | Members], OffsetToDefs, File, FP, DefMap, UndefMap, Inputs) ->
  case input_member(Member, OffsetToDefs, File, FP, DefMap, UndefMap) of
    false ->
      input_archive(Members, OffsetToDefs, File, FP, DefMap, UndefMap, Inputs);
    {ok, {NewDefMap, NewUndefMap, Input}} ->
      input_archive(Members, OffsetToDefs, File, FP, NewDefMap, NewUndefMap, [Input | Inputs]);
    {error, _Reason} = Error -> Error
  end.

input_member(Member, OffsetToDefs, File, FP, DefMap, UndefMap) ->
  #member{arhdr = ArHdr, location = HdrOffset} = Member,
  MemberDefs = maps:get(HdrOffset, OffsetToDefs, #{}),
  case maps:size(maps:intersect(MemberDefs, UndefMap)) of
    0 -> false;
    _ ->
      Offset = HdrOffset + ?PDP10_ARHDR_SIZEOF,
      #arhdr{ar_name = Name, ar_size = Size} = ArHdr,
      input_elf({File, Name, Offset, Size}, FP, Offset, Offset + Size, DefMap, UndefMap)
  end.

%% Process input object file ===================================================

input_elf(File, FP, DefMap, UndefMap, Inputs) ->
  case input_elf(File, FP, _Base = 0, _Limit = false, DefMap, UndefMap) of
    {ok, {NewDefMap, NewUndefMap, Input}} ->
      {ok, {NewDefMap, NewUndefMap, [Input | Inputs]}};
    {error, _Reason} = Error -> Error
  end.

input_elf(File, FP, Base, Limit, DefMap, UndefMap) ->
  case read_elf_symtab(FP, Base, Limit) of
    {ok, {ShTab, SymTab, StShNdx}} ->
      case update_sym_maps(SymTab, File, DefMap, UndefMap) of
        {ok, {NewDefMap, NewUndefMap}} ->
          Input = #input{file = File, shtab = ShTab, symtab = SymTab, stshndx = StShNdx},
          {ok, {NewDefMap, NewUndefMap, Input}};
        {error, _Reason} = Error -> Error
      end;
    {error, Reason} -> mkerror({?badelf, File, Reason})
  end.

%% Read ELF symtab =============================================================

read_elf_symtab(FP, Base, Limit) ->
  case libelf:read_Ehdr(FP, Base, Limit) of
    {ok, Ehdr} ->
      case libelf:read_ShTab(FP, Base, Limit, Ehdr) of
        {ok, ShTab} ->
          case libelf:read_SymTab(FP, Base, Limit, ShTab) of
            {ok, {SymTab, ShNdx}} -> {ok, {ShTab, SymTab, ShNdx}};
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

%% Maintain def/undef symbol maps ==============================================

update_sym_maps([Sym | Syms], File, DefMap, UndefMap) ->
  case do_update_sym_maps(Sym, File, DefMap, UndefMap) of
    {ok, {NewDefMap, NewUndefMap}} ->
      update_sym_maps(Syms, File, NewDefMap, NewUndefMap);
    {error, _Reason} = Error -> Error
  end;
update_sym_maps([], _File, DefMap, UndefMap) ->
  {ok, {DefMap, UndefMap}}.

do_update_sym_maps(Sym, File, DefMap, UndefMap) ->
  #elf_Sym{st_name = Name} = Sym,
  case classify_sym(Sym) of
    local -> {ok, {DefMap, UndefMap}};
    undefined ->
      case maps:is_key(Name, DefMap) of
        true -> {ok, {DefMap, UndefMap}};
        false ->
          case maps:is_key(Name, UndefMap) of
            true -> {ok, {DefMap, UndefMap}};
            false -> {ok, {DefMap, maps:put(Name, File, UndefMap)}}
          end
      end;
    defined ->
      case maps:get(Name, DefMap, false) of
        false -> {ok, {maps:put(Name, File, DefMap), maps:remove(Name, UndefMap)}};
        File0 -> mkerror({?muldef_symbol, Name, File0, File})
      end
  end.

classify_sym(Sym) ->
  case ?ELF_ST_BIND(Sym#elf_Sym.st_info) of
    ?STB_GLOBAL ->
      case Sym#elf_Sym.st_shndx of
        ?SHN_UNDEF -> undefined;
        _ -> defined
      end;
    _ -> local
  end.

%% Error Formatting ============================================================

mkerror(Reason) ->
  {error, {?MODULE, Reason}}.

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {?badelf, File, Reason} ->
      io:format("invalid ELF file ~s: ~s", [File, error:format(Reason)]);
    {?badfile, File, Reason} ->
      io:format("~s: ~s", [File, error:format(Reason)]);
    {?muldef_symbol, Symbol, File0, File} ->
      io:format("~s: ~s already defined in ~s", [File, Symbol, File0]);
    ?noinputfiles ->
      "no input files";
    {?undefined_symbols, Symbols} ->
      ["undefined symbols:" |
       ["\n" ++ Symbol || Symbol <- Symbols]]
  end.
