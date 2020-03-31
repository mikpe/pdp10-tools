%%% -*- erlang-indent-level: 2 -*-
%%%
%%% linking phase 1 for pdp10-elf ld
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

-module(ld_phase1).
-export([ phase1/1
        ]).

-include("ld_internal.hrl").

%% Linking Phase 1 =============================================================
%% Citing The System V Application Binary Interface, Ch. 4 Sections:
%%  "In the first phase, input sections that match in name, type and attribute
%% flags should be concatenated into single sections. The concatenation order
%% should satisfy the requirements of any known input section attributes (e.g,
%% SHF_MERGE and SHF_LINK_ORDER). When not otherwise constrained, sections
%% should be emitted in input order. "

-spec phase1([#input{}]) -> {[#section{}], sectionsmap()}.
phase1(Inputs) ->
  phase1(Inputs, _OutputMap = maps:new(), _SectionsMap = maps:new()).

phase1([Input | Inputs], OutputsMap, SectionsMap) ->
  #input{file = File, shtab = ShTab, stshndx = StShNdx} = Input,
  {NewOutputsMap, FileMap} = phase1(File, ShTab, StShNdx, OutputsMap),
  NewSectionsMap = maps:put(File, FileMap, SectionsMap),
  phase1(Inputs, NewOutputsMap, NewSectionsMap);
phase1([], OutputsMap, SectionsMap) ->
  Sections1 = maps:values(OutputsMap),
  Sections2 = lists:map(fun({Nr, Shdr, Frags}) ->
                          {Nr, #section{shdr = Shdr, frags = lists:reverse(Frags)}}
                        end, Sections1),
  Sections3 = lists:keysort(1, Sections2),
  Sections = lists:map(fun({_Nr, Section}) -> Section end, Sections3),
  {Sections, SectionsMap}.

phase1(File, ShTab, StShNdx, OutputsMap) ->
  RelocsMap = relocs_map(ShTab, StShNdx),
  FileMap = maps:new(),
  phase1(ShTab, _ShNdx = 0, File, RelocsMap, OutputsMap, FileMap).

phase1([], _ShNdx, _File, _RelocsMap, OutputsMap, FileMap) ->
  {OutputsMap, FileMap};
phase1([SHdr | ShTab], ShNdx, File, RelocsMap, OutputsMap, FileMap) ->
  {NewOutputsMap, NewFileMap} =
    maybe_output_section(SHdr, ShNdx, File, RelocsMap, OutputsMap, FileMap),
  phase1(ShTab, ShNdx + 1, File, RelocsMap, NewOutputsMap, NewFileMap).

maybe_output_section(Shdr, ShNdx, File, RelocsMap, OutputsMap, FileMap) ->
  case should_output_section(Shdr) of
    true -> output_section(Shdr, ShNdx, File, RelocsMap, OutputsMap, FileMap);
    false -> {OutputsMap, FileMap}
  end.

output_section(Shdr, ShNdx, File, RelocsMap, OutputsMap, FileMap) ->
  SectionKey = section_key(Shdr),
  Output = output_get(SectionKey, OutputsMap, Shdr),
  Relocs = maps:get(ShNdx, RelocsMap, false),
  {NewOutput, FragOffset} = output_append(Output, File, Shdr, Relocs),
  NewOutputsMap = maps:put(SectionKey, NewOutput, OutputsMap),
  NewFileMap = maps:put(Shdr#elf36_Shdr.sh_name, FragOffset, FileMap),
  {NewOutputsMap, NewFileMap}.

output_get(SectionKey, OutputsMap, Shdr) ->
  case maps:get(SectionKey, OutputsMap, false) of
    {_Nr, _Shdr, _Frags} = Output -> Output;
    false ->
      OutputShdr = Shdr#elf36_Shdr{ sh_addr = 0
                                  , sh_offset = 0
                                  , sh_size = 0
                                  , sh_link = 0
                                  , sh_info = 0
                                  , sh_addralign = 0
                                  },
      {_Nr = maps:size(OutputsMap), OutputShdr, _Frags = []}
  end.

output_append({Nr, OutputShdr, Frags}, File, Shdr, Relocs) ->
  OutputAlignment = section_alignment(OutputShdr),
  FragAlignment = section_alignment(Shdr),
  NewAlignment = max(OutputAlignment, FragAlignment),
  OutputSize = section_size(OutputShdr),
  FragSize = section_size(Shdr),
  FragOffset = align(OutputSize, FragAlignment),
  NewSize = FragOffset + FragSize,
  NewOutputShdr = OutputShdr#elf36_Shdr{ sh_size = NewSize
                                       , sh_addralign = NewAlignment
                                       },
  NewShdr = Shdr#elf36_Shdr{sh_offset = FragOffset},
  Frag = #sectfrag{file = File, shdr = NewShdr, relocs = Relocs},
  {{Nr, NewOutputShdr, [Frag | Frags]}, FragOffset}.

section_alignment(Shdr) ->
  case Shdr#elf36_Shdr.sh_addralign of
    0 -> 1;
    Alignment -> Alignment
  end.

section_size(Shdr) ->
  Shdr#elf36_Shdr.sh_size.

align(Offset, Alignment) ->
  (Offset + Alignment - 1) band bnot (Alignment - 1).

relocs_map(ShTab, StShNdx) ->
  lists:foldl(fun(Shdr, RelocsMap) -> relocs_map(Shdr, RelocsMap, StShNdx) end,
              maps:new(), ShTab).

relocs_map(Shdr, RelocsMap, StShNdx) -> % FIXME: ok | error
  case Shdr of
    #elf36_Shdr{sh_type = ?SHT_RELA, sh_link = Link, sh_info = Info} ->
      Link = StShNdx, % assert
      maps:put(Info, Shdr, RelocsMap);
    #elf36_Shdr{} -> RelocsMap
  end.

should_output_section(Shdr) ->
  #elf36_Shdr{sh_type = Type, sh_flags = Flags} = Shdr,
  case should_output_type(Type) of
    true -> should_output_flags(Flags);
    false -> false
  end.

should_output_type(Type) ->
  case Type of
    ?SHT_PROGBITS -> true;
    ?SHT_NOTE -> true;
    ?SHT_NOBITS -> true;
    ?SHT_INIT_ARRAY -> true;
    ?SHT_FINI_ARRAY -> true;
    ?SHT_PREINIT_ARRAY -> true;
    _ -> false
  end.

should_output_flags(Flags) ->
  (Flags band bnot (?SHF_WRITE bor
                    ?SHF_ALLOC bor
                    ?SHF_EXECINSTR bor
                    ?SHF_MERGE bor
                    ?SHF_STRINGS bor
                    ?SHF_TLS)) =:= 0.

section_key(Shdr) ->
  #elf36_Shdr{sh_name = Name, sh_type = Type, sh_flags = Flags} = Shdr,
  {Name, Type, Flags}.
