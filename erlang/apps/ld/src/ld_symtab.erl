%%% -*- erlang-indent-level: 2 -*-
%%%
%%% symbol table for pdp10-elf-ld
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

-module(ld_symtab).

-export([ build/2
        ]).

-include("ld_internal.hrl").

%% Build Symbol Tables =========================================================

-spec build([#input{}], [#segment{}]) -> {global(), filemap()}.
build(Inputs, Segments) ->
  FragMap = scan_segments(Segments),
  scan_inputs(Inputs, FragMap).

%% Scan all segments, which now have load addresses assigned, and record
%% the load address for every included fragment (file and section).
scan_segments(Segments) ->
  lists:foldl(fun scan_segment/2, _FragMap = maps:new(), Segments).

scan_segment(#segment{phdr = Phdr, sections = Sections}, FragMap0) ->
  #elf36_Phdr{p_vaddr = SegmentBase} = Phdr,
  lists:foldl(fun(Section, FragMap) ->
                scan_section(Section, FragMap, SegmentBase)
               end, FragMap0, Sections).

scan_section(#section{shdr = Shdr, frags = Frags}, FragMap0, SegmentBase) ->
  #elf36_Shdr{sh_offset = SectionOffset} = Shdr,
  SectionBase = SegmentBase + SectionOffset,
  lists:foldl(fun(Frag, FragMap) ->
                scan_frag(Frag, FragMap, SectionBase)
              end, FragMap0, Frags).

scan_frag(#sectfrag{file = File, shdr = Shdr, shndx = ShNdx}, FragMap, SectionBase) ->
  #elf36_Shdr{sh_offset = SectionOffset} = Shdr,
  maps:put({File, ShNdx}, SectionBase + SectionOffset, FragMap).

%% Scan all input files, whose segments now have load addresses in FragMap,
%% build a symbol table for each file, and update the global symbol table.
scan_inputs(Inputs, FragMap) ->
  lists:foldl(fun(Input, {GlobalMap, FileMap}) ->
                scan_input(Input, GlobalMap, FileMap, FragMap)
              end, {_GlobalMap = maps:new(), _FileMap = maps:new()}, Inputs).

scan_input(Input, GlobalMap0, FileMap, FragMap) ->
  #input{file = File, symtab = Syms} = Input,
  {GlobalMap, SymTab} = scan_syms(Syms, _ShNdx = 0, File, FragMap, GlobalMap0, _SymTab = maps:new()),
  {GlobalMap, maps:put(File, SymTab, FileMap)}.

scan_syms([], _SymNdx, _File, _FragMap, GlobalMap, SymTab) ->
  {GlobalMap, SymTab};
scan_syms([Sym | Syms], SymNdx, File, FragMap, GlobalMap, SymTab) ->
  {NewGlobalMap, NewSymTab} = scan_sym(Sym, SymNdx, File, FragMap, GlobalMap, SymTab),
  scan_syms(Syms, SymNdx + 1, File, FragMap, NewGlobalMap, NewSymTab).

scan_sym(Sym, SymNdx, File, FragMap, GlobalMap0, SymTab0) ->
  #elf36_Sym{st_name = Name, st_value = Value, st_info = Info, st_shndx = ShNdx} = Sym,
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
    case ?ELF36_ST_BIND(Info) of
      ?STB_GLOBAL when ShNdx =/= ?SHN_UNDEF -> maps:put(Name, SymVal, GlobalMap0);
      ?STB_GLOBAL -> GlobalMap0;
      ?STB_LOCAL -> GlobalMap0
    end,
  {GlobalMap, SymTab}.
