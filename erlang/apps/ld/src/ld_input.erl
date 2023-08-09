%%% -*- erlang-indent-level: 2 -*-
%%%
%%% input processing for pdp10-elf ld
%%% Copyright (C) 2020-2023  Mikael Pettersson
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

-module(ld_input).

-export([ input/2
        , format_error/1
        ]).

-include("ld_internal.hrl").

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
  case pdp10_stdio:fopen(File, [raw, read]) of
    {ok, FP} ->
      try
        input_elf(File, FP, DefMap, UndefMap, Inputs)
      after
        pdp10_stdio:fclose(FP)
      end;
    {error, Reason} -> mkerror({?badfile, File, Reason})
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
  case pdp10_elf36:read_Ehdr(FP, Base, Limit) of
    {ok, Ehdr} ->
      case pdp10_elf36:read_ShTab(FP, Base, Limit, Ehdr) of
        {ok, ShTab} ->
          case pdp10_elf36:read_SymTab(FP, Base, Limit, ShTab) of
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
  #elf36_Sym{st_name = Name} = Sym,
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
  case ?ELF36_ST_BIND(Sym#elf36_Sym.st_info) of
    ?STB_GLOBAL ->
      case Sym#elf36_Sym.st_shndx of
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
