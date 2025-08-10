%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Build symbol tables for pdp10-elf ld
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
%%% The segments have now been assigned load addresses. Scan the inputs and:
%%% - build a GlobalMap mapping each global symbol to its value
%%% - build a FileMap mapping each input file to its local symbol table

-module(ld_symtab).

-export([ build/2
        ]).

-include("ld_internal.hrl").

-type frag() :: {ifile(), ShNdx :: non_neg_integer()}.
-type fragmap() :: #{frag() => non_neg_integer()}.

%% Build Symbol Tables =========================================================

-spec build([#input{}], [#segment{}]) -> {global(), filemap()}.
build(Inputs, Segments) ->
  FragMap = build_fragmap(Segments),
  scan_inputs(Inputs, FragMap).

%% Scan all input files, whose segments now have load addresses in FragMap,
%% build a symbol table for each file, and update the global symbol table.
scan_inputs(Inputs, FragMap) ->
  lists:foldl(fun(Input, {GlobalMap, FileMap}) ->
                scan_input(Input, GlobalMap, FileMap, FragMap)
              end, {_GlobalMap = maps:new(), _FileMap = maps:new()}, Inputs).

scan_input(Input, GlobalMap0, FileMap, FragMap) ->
  #input{file = File, symtab = Syms} = Input,
  {GlobalMap, LocalSymTab} = scan_syms(Syms, File, FragMap, GlobalMap0),
  {GlobalMap, maps:put(File, LocalSymTab, FileMap)}.

scan_syms(Syms, File, FragMap, GlobalMap) ->
  {_LocalSymNdx, NewGlobalMap, LocalSymTab} =
    lists:foldl(
      fun(Sym, {SymNdx1, GlobalMap1, SymTab1}) ->
        {GlobalMap2, SymTab2} = enter_sym(Sym, SymNdx1, File, FragMap, GlobalMap1, SymTab1),
        {SymNdx1 + 1, GlobalMap2, SymTab2}
      end, {_SymNdx = 0, GlobalMap, _SymTab = maps:new()}, Syms),
  {NewGlobalMap, LocalSymTab}.

enter_sym(Sym, SymNdx, File, FragMap, GlobalMap0, SymTab0) ->
  #elf_Sym{st_name = Name, st_value = Value, st_info = Info, st_shndx = ShNdx} = Sym,
  true = is_list(Name), % assert in-core name is string()
  SymVal =
    case ShNdx of
      ?SHN_ABS -> Value;
      ?SHN_UNDEF -> Name; % lookups redirect via GlobalMap
      ?SHN_COMMON -> error('FIXME');
      ?SHN_XINDEX -> error('FIXME');
      _ ->
        true = ShNdx < ?SHN_LORESERVE, % assert
        case maps:get({File, ShNdx}, FragMap, false) of
          false -> error; % not linked/loaded section
          SectionBase -> SectionBase + Value
        end
    end,
  SymTab = maps:put(SymNdx, SymVal, SymTab0),
  GlobalMap =
    case ?ELF_ST_BIND(Info) of
      ?STB_LOCAL -> GlobalMap0;
      ?STB_GLOBAL when ShNdx =:= ?SHN_UNDEF -> GlobalMap0;
      ?STB_GLOBAL -> maps:put(Name, SymVal, GlobalMap0)
    end,
  {GlobalMap, SymTab}.

%% Build fragment map ==========================================================
%%
%% Scan all segments, which now have load addresses assigned, and record
%% the load address for every included fragment (file and section).

-spec build_fragmap([#segment{}]) -> fragmap().
build_fragmap(Segments) ->
  lists:foldl(fun scan_segment/2, _FragMap = maps:new(), Segments).

scan_segment(#segment{phdr = #elf_Phdr{p_vaddr = SegmentBase}, sections = Sections}, FragMap0) ->
  lists:foldl(fun(Section, FragMap) ->
                scan_section(Section, FragMap, SegmentBase)
               end, FragMap0, Sections).

scan_section(#section{shdr = #elf_Shdr{sh_offset = SectionOffset}, frags = Frags}, FragMap0, SegmentBase) ->
  SectionBase = SegmentBase + SectionOffset,
  lists:foldl(fun(Frag, FragMap) ->
                enter_frag(Frag, FragMap, SectionBase)
              end, FragMap0, Frags).

enter_frag(#sectfrag{file = File, shndx = ShNdx, offset = SectionOffset}, FragMap, SectionBase) ->
  maps:put({File, ShNdx}, SectionBase + SectionOffset, FragMap).
