%%% -*- erlang-indent-level: 2 -*-
%%%
%%% linking phase 2 for pdp10-elf ld
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
%%%
%%%=============================================================================
%%%
%%% Citing The System V Application Binary Interface, Ch. 4 Sections:
%%% "In the second phase, sections should be assigned to segments or other units
%%% based on their attribute flags. Sections of each particular unrecognized type
%%% should be assigned to the same unit unless prevented by incompatible flags,
%%% and within a unit, sections of the same unrecognized type should be placed
%%% together if possible."
%%%
%%% The output consists of the following segments, each containing the indicated
%%% sections in that order:
%%%
%%% * Text Segment (PT_LOAD, PF_R+PF_X)
%%%   - .text*
%%%   - other SHT_PROGBITS, SHF_ALLOC+SHF_EXECINSTR sections
%%%
%%% * Rodata Segment (PT_LOAD, PF_R)
%%%   - .rodata*
%%%   - other SHT_PROGBITS, SHF_ALLOC sections
%%%
%%% * Data Segment (PT_LOAD, PF_R+PR_W)
%%%   - .data*
%%%   - other SHT_PROGBITS, SHF_ALLOC+SHF_WRITE sections
%%%
%%% * BSS Segment (PT_LOAD, PF_R+PF_W)
%%%   - .bss*
%%%   - other SHT_NOBITS, SHF_ALLOC+SHF_WRITE sections

-module(ld_phase2).

-export([ phase2/1
        ]).

-include("ld_internal.hrl").

-record(pre_segment,
        { p_flags  :: non_neg_integer()
        , sections :: [#section{}]
        }).

-record(pre_segments,
        { text   :: #pre_segment{}
        , rodata :: #pre_segment{}
        , data   :: #pre_segment{}
        , bss    :: #pre_segment{}
        }).

%% Linking Phase 2 =============================================================

-spec phase2([#section{}]) -> [#segment{}].
phase2(Sections) ->
  segments_to_list(
    lists:foldl(fun segments_add_section/2, segments_new(), Sections)).

segments_new() ->
  #pre_segments
    { text   = #pre_segment{p_flags = ?PF_R bor ?PF_X, sections = []}
    , rodata = #pre_segment{p_flags = ?PF_R,           sections = []}
    , data   = #pre_segment{p_flags = ?PF_R bor ?PF_W, sections = []}
    , bss    = #pre_segment{p_flags = ?PF_R bor ?PF_W, sections = []}
    }.

segments_add_section(Section, Segments) ->
  Index = section_segment_index(Section),
  #pre_segment{sections = Sections} = PreSegment = element(Index, Segments),
  setelement(Index, Segments, PreSegment#pre_segment{sections = [Section | Sections]}).

section_segment_index(Section) ->
  #section{shdr = #elf36_Shdr{sh_type = Type, sh_flags = Flags}} = Section,
  case {Type, Flags} of
    {?SHT_PROGBITS, ?SHF_ALLOC bor ?SHF_EXECINSTR} -> #pre_segments.text;
    {?SHT_PROGBITS, ?SHF_ALLOC bor ?SHF_WRITE} -> #pre_segments.data;
    {?SHT_PROGBITS, ?SHF_ALLOC} -> #pre_segments.rodata;
    {?SHT_NOBITS, ?SHF_ALLOC bor ?SHF_WRITE} -> #pre_segments.bss
  end.

segments_to_list(#pre_segments{text = Text, rodata = Rodata, data = Data, bss = Bss}) ->
  lists:append([segment_to_list(Segment) || Segment <- [Text, Rodata, Data, Bss]]).

segment_to_list(#pre_segment{sections = []}) -> [];
segment_to_list(#pre_segment{p_flags = Flags, sections = Sections0}) ->
  Sections1 = order_sections(Sections0),
  {Sections, FileSz, MemSz, Align} = sections_layout(Sections1),
  Phdr = #elf36_Phdr{ p_type = ?PT_LOAD
                    , p_offset = 0
                    , p_vaddr = 0
                    , p_paddr = 0
                    , p_filesz = FileSz
                    , p_memsz = MemSz
                    , p_flags = Flags
                    , p_align = Align
                    },
  [#segment{phdr = Phdr, sections = Sections}].

%% Order sections within their segment =========================================
%%
%% Sections are mapped to segments based on their sh_type and sh_flags. This is
%% implemented by section_segment_index/1.
%%
%% Within a segment, sections are ordered by:
%% 1. section name, if the name is recognized (".text*" etc)
%% 2. input order
%%
%% To implement this we need a stable sort. Unfortunately Erlang doesn't provide
%% a usable one:
%% - lists:sort/2 takes an ordering function, but isn't stable
%% - lists:keysort/2 is stable, but doesn't take an ordering function
%%
%% To work around this we annotate each input section with its index in the
%% input. We then use lists:sort/2 with an ordering function which compares
%% these indices when the primary criteria are equal.

order_sections(RevSections) ->
  IndexedSections = index_sections(RevSections),
  OrderedIndexedSections = lists:sort(fun sections_le/2, IndexedSections),
  lists:map(fun unindex_section/1, OrderedIndexedSections).

index_sections(RevSections) ->
  {_Index, IndexedSections} = lists:foldl(fun index_section/2, {-1, []}, RevSections),
  IndexedSections.

index_section(Section, {Index, IndexedSections}) ->
  %% The input is processed from last to first, so the index is decrementing.
  {Index - 1, [{Index, Section} | IndexedSections]}.

unindex_section({_Index, Section}) -> Section.

sections_le({Index1, Section1}, {Index2, Section2}) ->
  Name1 = section_name(Section1),
  Name2 = section_name(Section2),
  case Name1 =:= Name2 of
    true -> Index1 =< Index2;
    false ->
      case {name_known(Name1), name_known(Name2)} of
        {true, false} -> true;
        {false, true} -> false;
        {_, _}        -> Name1 < Name2
      end
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

%% Compute final layout of the sections in a segment ===========================
%%
%% Given a sequence of sections that make up a segment, taking into account their
%% sizes and alignment, compute their offsets in the segment. Also compute the
%% segment's final filesz, memsz, and alignment.

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
