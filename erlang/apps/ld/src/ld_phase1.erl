%%% -*- erlang-indent-level: 2 -*-
%%%
%%% linking phase 1 for pdp10-elf ld
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
%%% "In the first phase, input sections that match in name, type and attribute
%%% flags should be concatenated into single sections. The concatenation order
%%% should satisfy the requirements of any known input section attributes (e.g,
%%% SHF_MERGE and SHF_LINK_ORDER). When not otherwise constrained, sections
%%% should be emitted in input order."

-module(ld_phase1).

-export([ phase1/1
        ]).

-include("ld_internal.hrl").

-record(section_key,
        { sh_name :: string()
        , sh_type :: non_neg_integer()
        , sh_flags :: non_neg_integer()
        }).

-record(output, % accumulates fragments for a given section
        { nr :: non_neg_integer() % determines output order
        , shdr :: #elf36_Shdr{}
        , frags :: [#sectfrag{}]
        }).

-type outputsmap() :: #{#section_key{} => #output{}}.

-type relocsmap() :: #{non_neg_integer() => #elf36_Shdr{}}.

%% Linking Phase 1 =============================================================

-spec phase1([#input{}]) -> [#section{}].
phase1(Inputs) ->
  OutputsMap = lists:foldl(fun phase1/2, _OutputsMap = maps:new(), Inputs),
  UnsortedOutputs = maps:values(OutputsMap),
  SortedOutputs = lists:keysort(#output.nr, UnsortedOutputs),
  lists:map(fun output_to_section/1, SortedOutputs).

output_to_section(#output{shdr = Shdr, frags = Frags}) ->
  #section{shdr = Shdr, frags = lists:reverse(Frags)}.

-spec phase1(#input{}, outputsmap()) -> outputsmap().
phase1(Input, OutputsMap0) ->
  #input{file = File, shtab = ShTab, stshndx = StShNdx} = Input,
  RelocsMap = relocs_map(ShTab, StShNdx),
  {_NewShNdx, NewOutputsMap} =
    lists:foldl(
      fun(Shdr, {ShNdx, OutputsMap}) ->
        {ShNdx + 1, maybe_output_section(Shdr, ShNdx, File, RelocsMap, OutputsMap)}
      end, {_ShNdx = 0, OutputsMap0}, ShTab),
  NewOutputsMap.

maybe_output_section(Shdr, ShNdx, File, RelocsMap, OutputsMap) ->
  case should_output_section(Shdr) of
    true -> output_section(Shdr, ShNdx, File, RelocsMap, OutputsMap);
    false -> OutputsMap
  end.

should_output_section(Shdr) ->
  #elf36_Shdr{sh_type = Type, sh_flags = Flags} = Shdr,
  should_output_type(Type) andalso should_output_flags(Flags).

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
  (Flags band ?SHF_ALLOC) =/= 0 andalso
  (Flags band bnot (?SHF_WRITE bor
                    ?SHF_ALLOC bor
                    ?SHF_EXECINSTR bor
                    ?SHF_MERGE bor
                    ?SHF_STRINGS bor
                    ?SHF_TLS)) =:= 0.

%% Append a section to the outputs accumulator =================================

output_section(Shdr, ShNdx, File, RelocsMap, OutputsMap) ->
  SectionKey = section_key(Shdr),
  Output = output_get(SectionKey, OutputsMap, Shdr),
  Relocs = relocs_get(ShNdx, RelocsMap),
  NewOutput = output_append(Output, File, Shdr, ShNdx, Relocs),
  maps:put(SectionKey, NewOutput, OutputsMap).

output_get(SectionKey, OutputsMap, Shdr) ->
  case maps:get(SectionKey, OutputsMap, false) of
    false ->
      OutputShdr = Shdr#elf36_Shdr{ sh_addr = 0
                                  , sh_offset = 0
                                  , sh_size = 0
                                  , sh_link = 0
                                  , sh_info = 0
                                  , sh_addralign = 0
                                  },
      #output{nr = maps:size(OutputsMap), shdr = OutputShdr, frags = []};
    Output -> Output
  end.

output_append(Output, File, Shdr, ShNdx, Relocs) ->
  #output{shdr = OutputShdr, frags = Frags} = Output,
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
  Frag = #sectfrag{file = File, shdr = Shdr, shndx = ShNdx, relocs = Relocs, offset = FragOffset},
  Output#output{shdr = NewOutputShdr, frags = [Frag | Frags]}.

section_alignment(Shdr) ->
  case Shdr#elf36_Shdr.sh_addralign of
    0 -> 1;
    Alignment -> Alignment
  end.

section_size(Shdr) ->
  Shdr#elf36_Shdr.sh_size.

align(Offset, Alignment) ->
  (Offset + Alignment - 1) band bnot (Alignment - 1).

section_key(Shdr) ->
  #elf36_Shdr{sh_name = Name, sh_type = Type, sh_flags = Flags} = Shdr,
  #section_key{sh_name = Name, sh_type = Type, sh_flags = Flags}.

%% Relocs Map ==================================================================
%%
%% The Relocs Map is a mapping from a section's section header index to the section
%% header for the SHT_RELA which applies to that section, if any.

-spec relocs_map([#elf36_Shdr{}], non_neg_integer()) -> relocsmap().
relocs_map(ShTab, StShNdx) ->
  lists:foldl(
    fun(Shdr, RelocsMap) ->
      relocs_map(Shdr, RelocsMap, StShNdx)
    end, maps:new(), ShTab).

relocs_map(Shdr, RelocsMap, StShNdx) -> % FIXME: ok | error
  case Shdr of
    #elf36_Shdr{sh_type = ?SHT_RELA, sh_link = Link, sh_info = Info} ->
      Link = StShNdx, % assert
      maps:put(Info, Shdr, RelocsMap);
    #elf36_Shdr{} -> RelocsMap
  end.

-spec relocs_get(non_neg_integer(), relocsmap()) -> #elf36_Shdr{} | false.
relocs_get(ShNdx, RelocsMap) ->
  maps:get(ShNdx, RelocsMap, false).
