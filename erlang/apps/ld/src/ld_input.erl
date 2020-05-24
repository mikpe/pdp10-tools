%%% -*- erlang-indent-level: 2 -*-
%%%
%%% input processing for pdp10-elf ld
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
input(_Files = [], _UndefSyms) -> {error, {?MODULE, ?noinputfiles}};
input(Files, UndefSyms) ->
  UndefMap =
    lists:foldl(fun(UndefSym, UndefMap0) ->
                  maps:put(UndefSym, false, UndefMap0)
                end, maps:new(), UndefSyms),
  input(Files, maps:new(), UndefMap, []).

input([{file, File} | Files], DefMap, UndefMap, Inputs) ->
  case read_file(File) of
    {ok, {ShTab, SymTab, StShNdx}} ->
      case update_sym_maps(SymTab, File, DefMap, UndefMap) of
        {ok, {NewDefMap, NewUndefMap}} ->
          Input = #input{file = File, shtab = ShTab, symtab = SymTab, stshndx = StShNdx},
          input(Files, NewDefMap, NewUndefMap, [Input | Inputs]);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end;
input([], _DefMap, UndefMap, Inputs) ->
  case maps:keys(UndefMap) of
    [] -> {ok, lists:reverse(Inputs)};
    UndefSyms -> {error, {?MODULE, {?undefined_symbols, UndefSyms}}}
  end.

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
        File0 -> {error, {?MODULE, {?muldef_symbol, Name, File0, File}}}
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

read_file(File) ->
  case pdp10_stdio:fopen(File, [read]) of
    {ok, FP} ->
      try read_file(File, FP)
      after pdp10_stdio:fclose(FP)
      end;
    {error, Reason} -> {error, {?MODULE, {?badfile, File, Reason}}}
  end.

read_file(File, FP) ->
  case pdp10_elf36:read_Ehdr(FP) of
    {ok, Ehdr} ->
      case pdp10_elf36:read_ShTab(FP, Ehdr) of
        {ok, ShTab} ->
          case pdp10_elf36:read_SymTab(FP, ShTab) of
            {ok, {SymTab, ShNdx}} -> {ok, {ShTab, SymTab, ShNdx}};
            {error, Reason} -> badelf(File, Reason)
          end;
        {error, Reason} -> badelf(File, Reason)
      end;
    {error, Reason} -> badelf(File, Reason)
  end.

badelf(File, Reason) ->
  {error, {?MODULE, {?badelf, File, Reason}}}.

%% Error Formatting ============================================================

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
