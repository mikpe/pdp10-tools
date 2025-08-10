%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Assigning addresses for pdp10-elf ld
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
%%% Finalize the Phdrs for the PT_LOAD segments by assigning virtual addresses
%%% and segment offsets.

-module(ld_assign).

-export([ assign/1
        ]).

-include("ld_internal.hrl").

%% Assign addresses and offsets to PT_LOAD segments ============================

-spec assign([#segment{}]) -> [#segment{}].
assign(Segments) ->
  PhNum = length(Segments),
  true = PhNum < ?PN_XNUM, % assert; TODO: otherwise store PhNum in Shdr0.sh_info
  %% TODO: assumes large or small code model output, not tiny
  VAddr = 8#00002001000 bsl 2, % section 2, page 1, word address to byte address
  Offset = ?ELF36_EHDR_SIZEOF + ?ELF36_PHDR_SIZEOF * PhNum,
  {_Offset, _VAddr, NewSegments} = lists:foldl(fun assign/2, {Offset, VAddr, []}, Segments),
  lists:reverse(NewSegments).

assign(Segment, {Offset, VAddr, NewSegments}) ->
  #segment{phdr = Phdr} = Segment,
  #elf_Phdr{p_filesz = FileSz, p_memsz = MemSz, p_align = Align} = Phdr,
  SegAlign = max(4096, Align), % align to page boundary; TODO: target-specific
  SegOffset = align(Offset, SegAlign),
  SegVAddr = align(VAddr, SegAlign),
  NewPhdr = Phdr#elf_Phdr{p_offset = SegOffset, p_vaddr = SegVAddr, p_align = SegAlign},
  NewSegment = Segment#segment{phdr = NewPhdr},
  {SegOffset + FileSz, SegVAddr + MemSz, [NewSegment | NewSegments]}.

align(Offset, Alignment) ->
  (Offset + (Alignment - 1)) band bnot (Alignment - 1).
