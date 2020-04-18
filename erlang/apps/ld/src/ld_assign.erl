%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Assigning addresses for pdp10-elf-ld
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

-module(ld_assign).

-export([ assign/1
        ]).

-include("ld_internal.hrl").

%% Output ======================================================================

-spec assign([#segment{}]) -> [#segment{}].
assign(Segments) ->
  %% TODO: assumes KL10B-compatible "small" code model output
  VAddr = 8#00001001000 bsl 2, % section 1, page 1
  PhNum = length(Segments),
  true = PhNum < ?PN_XNUM, % assert; TODO: otherwise store PhNum in Shdr0.sh_info
  Offset = ?ELF36_EHDR_SIZEOF + ?ELF36_PHDR_SIZEOF * PhNum,
  assign(Segments, Offset, VAddr, []).

assign(_Segments = [], _Offset, _VAddr, NewSegments) -> lists:reverse(NewSegments);
assign([Segment | Segments], Offset, VAddr, NewSegments) ->
  #segment{phdr = Phdr} = Segment,
  #elf36_Phdr{p_filesz = FileSz, p_memsz = MemSz, p_align = Align} = Phdr,
  SegAlign = max(4096, Align), % align to page boundary; TODO: target-specific
  SegOffset = (Offset + (SegAlign - 1)) band bnot (SegAlign - 1),
  SegVAddr = (VAddr + (SegAlign - 1)) band bnot (SegAlign - 1),
  NewPhdr = Phdr#elf36_Phdr{p_offset = SegOffset, p_vaddr = SegVAddr, p_align = SegAlign},
  NewSegment = Segment#segment{phdr = NewPhdr},
  assign(Segments, SegOffset + FileSz, SegVAddr + MemSz, [NewSegment | NewSegments]).
