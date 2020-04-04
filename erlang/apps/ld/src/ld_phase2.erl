%%% -*- erlang-indent-level: 2 -*-
%%%
%%% linking phase 2 for pdp10-elf ld
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

-module(ld_phase2).

-export([ phase2/1
        ]).

-include("ld_internal.hrl").

%% Phase 2 =====================================================================
%%
%% Citing The System V Application Binary Interface, Ch. 4 Sections:
%%  "In the second phase, sections should be assigned to segments or other units
%% based on their attribute flags. Sections of each particular unrecognized type
%% should be assigned to the same unit unless prevented by incompatible flags,
%% and within a unit, sections of the same unrecognized type should be placed
%% together if possible."
%%
%% The output is the following segments, each containing the indicated sections
%% in that order:
%%
%% * Text Segment (PT_LOAD, PF_R+PF_X)
%%   - .text*
%%   - other SHT_PROGBITS, SHF_ALLOC+SHF_EXECINSTR sections
%%
%% * Rodata Segment (PT_LOAD, PF_R)
%%   - .rodata*
%%   - other SHT_PROGBITS, SHF_ALLOC sections
%%
%% * Data Segment (PT_LOAD, PF_R+PR_W)
%%   - .data*
%%   - other SHT_PROGBITS, SHF_ALLOC+SHF_WRITE sections
%%
%% * BSS Segment (PT_LOAD, PF_R+PF_W)
%%   - .bss*
%%   - other SHT_NOBITS, SHF_ALLOC+SHF_WRITE sections

-spec phase2([#section{}]) -> [#segment{}].
phase2(Sections) ->
  segments_to_list(
    lists:foldl(fun segment_add_section/2, segments_new(), Sections)).

%% Segment indices in segments tuple.
-define(SEG_TEXT,   1).
-define(SEG_RODATA, 2).
-define(SEG_DATA,   3).
-define(SEG_BSS,    4).
-define(SEG__MAX,   ?SEG_BSS).

segments_new() ->
  list_to_tuple([segment_new(Index) || Index <- lists:seq(1, ?SEG__MAX)]).

segment_new(Index) ->
  {segment_props(Index), []}.

segment_props(Index) ->
  case Index of
    ?SEG_TEXT   -> {?PT_LOAD, ?PF_R bor ?PF_X};
    ?SEG_RODATA -> {?PT_LOAD, ?PF_R};
    ?SEG_DATA   -> {?PT_LOAD, ?PF_R bor ?PF_W};
    ?SEG_BSS    -> {?PT_LOAD, ?PF_R bor ?PF_W}
  end.

segment_add_section(Section, Segments) ->
  Index = section_index(Section),
  {Props, Sections} = element(Index, Segments),
  setelement(Index, Segments, {Props, [Section | Sections]}).

section_index(Section) ->
  #section{shdr = #elf36_Shdr{sh_type = Type, sh_flags = Flags}} = Section,
  case {Type, Flags} of
    {?SHT_PROGBITS, ?SHF_ALLOC bor ?SHF_EXECINSTR} -> ?SEG_TEXT;
    {?SHT_PROGBITS, ?SHF_ALLOC bor ?SHF_WRITE} -> ?SEG_DATA;
    {?SHT_PROGBITS, ?SHF_ALLOC} -> ?SEG_RODATA;
    {?SHT_NOBITS, ?SHF_ALLOC bor ?SHF_WRITE} -> ?SEG_BSS
  end.

segments_to_list(Segments) ->
  lists:append([segment_to_list(Segment) || Segment <- tuple_to_list(Segments)]).

segment_to_list({_Props, _Sections = []}) -> [];
segment_to_list({{Type, Flags}, Sections0}) ->
  Sections1 = lists:sort(fun sections_le/2, Sections0),
  {Sections, FileSz, MemSz, Align} = sections_layout(Sections1),
  Phdr = #elf36_Phdr{ p_type = Type
                    , p_offset = 0
                    , p_vaddr = 0
                    , p_paddr = 0
                    , p_filesz = FileSz
                    , p_memsz = MemSz
                    , p_flags = Flags
                    , p_align = Align
                    },
  [#segment{phdr = Phdr, sections = Sections}].

sections_le(Section1, Section2) ->
  Name1 = section_name(Section1),
  Name2 = section_name(Section2),
  case {name_known(Name1), name_known(Name2)} of
    {true, true}   -> Name1 =< Name2;
    {true, false}  -> true;
    {false, true}  -> false;
    {false, false} -> Name1 =< Name2
  end.

section_name(#section{shdr = #elf36_Shdr{sh_name = Name}}) -> Name.

name_known(Name) ->
  case Name of
    ".text"   ++ _ -> true;
    ".rodata" ++ _ -> true;
    ".data"   ++ _ -> true;
    ".bss"    ++ _ -> true;
    _              -> false
  end.

sections_layout(Sections0) ->
  Init = {_Sections = [], _FileSz = 0, _MemSz = 0, _Align = 1},
  {Sections, FileSz, MemSz, Align} =
    lists:foldl(fun section_layout/2, Init, Sections0),
  {lists:reverse(Sections), FileSz, MemSz, Align}.

section_layout(Section0, {Sections, FileSz0, MemSz0, Align0}) ->
  #section{shdr = Shdr0} = Section0,
  #elf36_Shdr{sh_type = ShType, sh_size = ShSize, sh_addralign = ShAlign0} = Shdr0,
  ShAlign = max(1, ShAlign0),
  ShFileSz =
    case ShType of
      ?SHT_NOBITS -> 0;
      ?SHT_PROGBITS -> ShSize
    end,
  ShOffset = (MemSz0 + (ShAlign - 1)) band bnot (ShAlign - 1),
  Shdr = Shdr0#elf36_Shdr{sh_offset = ShOffset},
  Section = Section0#section{shdr = Shdr},
  FileSz = FileSz0 + ShFileSz,
  MemSz = ShOffset + ShSize,
  Align = max(Align0, ShAlign),
  {[Section | Sections], FileSz, MemSz, Align}.
