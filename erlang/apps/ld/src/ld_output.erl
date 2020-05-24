%%% -*- erlang-indent-level: 2 -*-
%%%
%%% ELF output for pdp10-elf-ld
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

-module(ld_output).

-export([ output/5
        , format_error/1
        ]).

-include("ld_internal.hrl").

%% Output ======================================================================

-spec output(string(), string() | non_neg_integer(), [#segment{}], global(), filemap())
         -> ok | {error, {module(), term()}}.
output(File, Entry, Segments, GlobalMap, FileMap) ->
  case resolve(Entry, GlobalMap) of
    {ok, NewEntry} ->
      case pdp10_stdio:fopen(File, [raw, write, delayed_write]) of
        {ok, FP} ->
          try
            Funs =
              [ fun output_elf_header/6
              , fun output_phtab/6
              , fun output_segments/6
              ],
            output(Funs, NewEntry, Segments, GlobalMap, FileMap, FP, _Offset = 0)
          after pdp10_stdio:fclose(FP)
          end;
        {error, Reason} -> {error, {?MODULE, {cannot_open, File, Reason}}}
      end;
    {error, _Reason} = Error -> Error
  end.

output([], _Entry, _Segments, _GlobalMap, _FileMap, _FP, _Offset) -> ok;
output([Fun | Funs], Entry, Segments, GlobalMap, FileMap, FP, Offset) ->
  case Fun(Entry, Segments, GlobalMap, FileMap, FP, Offset) of
    {ok, NewOffset} -> output(Funs, Entry, Segments, GlobalMap, FileMap, FP, NewOffset);
    {error, _Reason} = Error -> Error
  end.

resolve(Value, _GlobalMap) when is_integer(Value) -> {ok, Value};
resolve(Symbol, GlobalMap) ->
  case maps:get(Symbol, GlobalMap, false) of
    Value when is_integer(Value) -> {ok, Value};
    false -> {error, {?MODULE, {undefined_symbol, Symbol}}}
  end.

output_padding(0, _FP) -> ok;
output_padding(N, FP) when N > 0 ->
  case pdp10_stdio:fputc(0, FP) of
    ok -> output_padding(N - 1, FP);
    {error, _Reason} = Error -> Error
  end.

%% ELF Header Output -----------------------------------------------------------

output_elf_header(Entry, Segments, _GlobalMap, _FileMap, FP, Offset = 0) ->
  PhNum = length(Segments),
  true = PhNum < ?PN_XNUM, % assert; TODO: otherwise store PhNum in Shdr0.sh_info
  PhOff = ?ELF36_EHDR_SIZEOF,
  Ehdr0 = pdp10_elf36:make_Ehdr(),
  Ehdr = Ehdr0#elf36_Ehdr{ e_type = ?ET_EXEC
                         , e_entry = Entry
                         , e_phoff = PhOff
                         , e_phnum = PhNum
                         },
  case pdp10_elf36:write_Ehdr(FP, Ehdr) of
    ok -> {ok, Offset + ?ELF36_EHDR_SIZEOF};
    {error, _Reason} = Error -> Error
  end.

%% Program Header Table Output -------------------------------------------------

output_phtab(_Entry, Segments, _GlobalMap, _FileMap, FP, Offset = ?ELF36_EHDR_SIZEOF) ->
  %% no need to emit alignment padding
  output_phtab(Segments, FP, Offset).

output_phtab(_Segments = [], _FP, Offset) -> {ok, Offset};
output_phtab([#segment{phdr = Phdr} | Segments], FP, Offset) ->
  case pdp10_elf36:write_Phdr(FP, Phdr) of
    ok -> output_phtab(Segments, FP, Offset + ?ELF36_PHDR_SIZEOF);
    {error, _Reason} = Error -> Error
  end.

%% Segments Output -------------------------------------------------------------

output_segments(_Entry, Segments, GlobalMap, FileMap, FP, Offset) ->
  output_segments(Segments, GlobalMap, FileMap, FP, Offset).

output_segments(_Segments = [], _GlobalMap, _FileMap, _FP, Offset) -> {ok, Offset};
output_segments([Segment | Segments], GlobalMap, FileMap, FP, Offset) ->
  case output_segment(Segment, GlobalMap, FileMap, FP, Offset) of
    {ok, NewOffset} -> output_segments(Segments, GlobalMap, FileMap, FP, NewOffset);
    {error, _Reason} = Error -> Error
  end.

output_segment(Segment, GlobalMap, FileMap, FP, Offset) ->
  #segment{phdr = Phdr, sections = Sections} = Segment,
  #elf36_Phdr{p_offset = SegOffset, p_vaddr = SegVAddr} = Phdr,
  case output_padding(SegOffset - Offset, FP) of
    ok -> output_sections(Sections, GlobalMap, FileMap, FP, SegOffset, SegVAddr);
    {error, _Reason} = Error -> Error
  end.

%% Sections Output -------------------------------------------------------------

output_sections(Sections, GlobalMap, FileMap, FP, SegOffset, SegVAddr) ->
  output_sections(Sections, GlobalMap, FileMap, FP, SegOffset, SegVAddr, _Offset = 0).

output_sections(_Sections = [], _GlobalMap, _FileMap, _FP, SegOffset, _SegVAddr, Offset) ->
  {ok, SegOffset + Offset};
output_sections([Section | Sections], GlobalMap, FileMap, FP, SegOffset, SegVAddr, Offset) ->
  case output_section(Section, GlobalMap, FileMap, FP, SegOffset, SegVAddr, Offset) of
    {ok, NewOffset} -> output_sections(Sections, GlobalMap, FileMap, FP, SegOffset, SegVAddr, NewOffset);
    {error, _Reason} = Error -> Error
  end.

output_section(Section, GlobalMap, FileMap, FP, SegOffset, SegVAddr, Offset) ->
  #section{shdr = Shdr, frags = Frags} = Section,
  #elf36_Shdr{sh_offset = ShOffset} = Shdr,
  case output_padding(ShOffset - Offset, FP) of
    ok -> output_frags(Frags, GlobalMap, FileMap, FP, SegOffset + ShOffset, SegVAddr + ShOffset);
    {error, _Reason} = Error -> Error
  end.

%% Fragments Output ------------------------------------------------------------

output_frags(Frags, GlobalMap, FileMap, FP, SectOffset, SectVAddr) ->
  output_frags(Frags, GlobalMap, FileMap, FP, SectOffset, SectVAddr, _Offset = 0).

output_frags(_Frags = [], _GlobalMap, _FileMap, _FP, SectOffset, _SectVAddr, Offset) ->
  {ok, SectOffset + Offset};
output_frags([Frag | Frags], GlobalMap, FileMap, FP, SectOffset, SectVAddr, Offset) ->
  case output_frag(Frag, GlobalMap, FileMap, FP, SectOffset, SectVAddr, Offset) of
    {ok, NewOffset} -> output_frags(Frags, GlobalMap, FileMap, FP, SectOffset, SectVAddr, NewOffset);
    {error, _Reason} = Error -> Error
  end.

%% Single Fragment Output ------------------------------------------------------
%%
%% Copy a section from an input file and append it to the output file.
%% Simultaneously apply associated relocations.

output_frag(Frag, GlobalMap, FileMap, FP, _SectOffset, _SectVAddr, Offset) ->
  case input_init(Frag) of
    {ok, {Input, Relocs}} ->
      Output = output_init(Frag, GlobalMap, FileMap, FP, Relocs),
      case output_frag(Input, Output) of
        {ok, FragOffset} -> {ok, Offset + FragOffset};
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

output_frag(Input, Output) ->
  case input(Input) of
    {ok, {Byte, NewInput}} ->
      case output(Output, Byte) of
        {ok, {Pushback, NewOutput}} ->
          output_frag(input_pushback(NewInput, Pushback), NewOutput);
        {ok, NewOutput} -> output_frag(NewInput, NewOutput);
        {error, _Reason} = Error -> Error
      end;
    eof ->
      input_fini(Input),
      output_fini(Output);
    {error, _Reason} = Error -> Error
  end.

-record(frag_input, {fp, file, sh_name, size, pushback}).

input_init(Frag) ->
  #sectfrag{file = File, shdr = Shdr, relocs = RelocShdr} = Frag,
  case pdp10_stdio:fopen(File, [raw, read]) of
    {ok, FP} ->
      case input_relocs(FP, RelocShdr) of
        {ok, Relocs} ->
          #elf36_Shdr{sh_offset = ShOffset, sh_size = ShSize, sh_name = ShName} = Shdr,
          case pdp10_stdio:fseek(FP, {bof, ShOffset}) of
            ok ->
              Input = #frag_input{fp = FP, file = File, sh_name = ShName, size = ShSize, pushback = []},
              {ok, {Input, Relocs}};
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    {error, Reason} -> {error, {?MODULE, {cannot_open, File, Reason}}}
  end.

input_relocs(_FP, false) -> {ok, []};
input_relocs(FP, Shdr) -> pdp10_elf36:read_RelaTab(FP, Shdr).

input_fini(#frag_input{fp = FP}) ->
  pdp10_stdio:fclose(FP).

input_pushback(#frag_input{pushback = Pushback} = Input, Buffer) ->
  Input#frag_input{pushback = Buffer ++ Pushback}.

input(#frag_input{pushback = [Byte | Pushback]} = Input) ->
  {ok, {Byte, Input#frag_input{pushback = Pushback}}};
input(#frag_input{size = 0}) -> eof;
input(#frag_input{fp = FP, size = Size} = Input) when Size > 0 ->
  case pdp10_stdio:fgetc(FP) of
    {ok, Byte} -> {ok, {Byte, Input#frag_input{size = Size - 1}}};
    eof ->
      #frag_input{file = File, sh_name = ShName} = Input,
      {error, {?MODULE, {premature_eof, File, ShName}}};
    {error, _Reason} = Error -> Error
  end.

-record(frag_output,
        { file
        , sh_name
        , globalmap
        , filemap
        , fp
        , relocs
        , frag_offset
        , buffer
        }).

output_init(Frag, GlobalMap, FileMap, FP, Relocs) ->
  #sectfrag{file = File, shdr = #elf36_Shdr{sh_name = ShName}} = Frag,
  SortedRelocs = lists:keysort(#elf36_Rela.r_offset, Relocs),
  #frag_output{file = File, sh_name = ShName, globalmap = GlobalMap, filemap = FileMap,
               fp = FP, relocs = SortedRelocs, frag_offset = 0, buffer = []}.

output_fini(#frag_output{relocs = [], frag_offset = FragOffset}) -> {ok, FragOffset};
output_fini(#frag_output{file = File, sh_name = ShName}) ->
  {error, {?MODULE, {unresolved_relocs, File, ShName}}}.

output(Output, Byte) ->
  case Output#frag_output.relocs of
    [] -> output_directly(Output, Byte);
    [#elf36_Rela{r_offset = RelOffset} = Rela | _] ->
      FragOffset = Output#frag_output.frag_offset,
      case FragOffset < RelOffset of
        true -> output_directly(Output, Byte);
        false ->
          NewBuffer = [Byte | Output#frag_output.buffer],
          NewFragOffset = FragOffset + 1,
          NewOutput = Output#frag_output{frag_offset = NewFragOffset, buffer = NewBuffer},
          case length(NewBuffer) =:= sizeof_reloc(Rela) of
            true -> output_reloc(NewOutput);
            false -> {ok, NewOutput}
          end
      end
  end.

output_reloc(Output) ->
  #frag_output{ file = File
              , filemap = FileMap
              , globalmap = GlobalMap
              , relocs = [Reloc | Relocs]
              , buffer = Buffer0
              , frag_offset = FragOffset
              } = Output,
  #elf36_Rela{ r_info = Info
             , r_addend = Addend
             } = Reloc,
  Type = ?ELF36_R_TYPE(Info),
  SymNdx = ?ELF36_R_SYM(Info),
  case symbol_value(SymNdx, File, FileMap, GlobalMap) of
    {ok, SymbolValue} ->
      Buffer1 = lists:reverse(Buffer0),
      Length = length(Buffer1),
      Word = buffer_word(Length, Buffer1),
      %% TODO: handle {error,_} returns
      {ok, NewWord} = apply_reloc(Type, SymbolValue + Addend, Word),
      Pushback = word_buffer(Length, NewWord),
      {ok, {Pushback, Output#frag_output{relocs = Relocs, buffer = [], frag_offset = FragOffset - Length}}};
    {error, _Reason} = Error -> Error
  end.

symbol_value(SymNdx, File, FileMap, GlobalMap) ->
  LocalMap = maps:get(File, FileMap),
  case maps:get(SymNdx, LocalMap) of
    Value when is_integer(Value) -> {ok, Value};
    Symbol when is_list(Symbol) ->
      case maps:get(Symbol, GlobalMap, false) of
        Value when is_integer(Value) -> {ok, Value};
        false -> {error, {?MODULE, {undefined_symbol, Symbol}}}
      end
  end.

buffer_word(4, Buffer) -> pdp10_extint:uint36_from_ext(Buffer);
buffer_word(2, Buffer) -> pdp10_extint:uint18_from_ext(Buffer);
buffer_word(1, [Byte]) -> Byte.

word_buffer(4, Word) -> pdp10_extint:uint36_to_ext(Word);
word_buffer(2, Word) -> pdp10_extint:uint18_to_ext(Word);
word_buffer(1, Byte) -> [Byte].

sizeof_reloc(#elf36_Rela{r_info = Info}) ->
  case ?ELF36_R_TYPE(Info) of
    ?R_PDP10_IFIW      -> 4;
    ?R_PDP10_EFIW      -> 4;
    ?R_PDP10_LOCAL_W   -> 2;
    ?R_PDP10_LOCAL_B   -> 4;
    ?R_PDP10_LOCAL_H   -> 4;
    ?R_PDP10_GLOBAL_B  -> 4;
    ?R_PDP10_GLOBAL_H  -> 4;
    ?R_PDP10_LITERAL_W -> 4;
    ?R_PDP10_LITERAL_H -> 2;
    ?R_PDP10_LITERAL_B -> 1
  end.

apply_reloc(Type, Value, Word) ->
  case Type of
    ?R_PDP10_IFIW      ->
      0 = (Value band 3), % assert alignment
      Address = Value div 4,
      %% TODO: check that location and value are in same section
      %% TODO: handle cross-section references
      {ok, (Word band bnot ?PDP10_UINT18_MAX) bor (Address band ?PDP10_UINT18_MAX)};
    ?R_PDP10_EFIW      ->
      0 = (Value band 3), % assert alignment
      Address = Value div 4,
      0 = (Address band bnot ((1 bsl 30) - 1)), % assert
      Mask = 16#1F bsl 30, % clear bit 0, preserve bits 1 to 5
      {ok, (Word band Mask) bor (Address band ((1 bsl 30) - 1))};
    ?R_PDP10_LOCAL_W   ->
      0 = (Value band 3), % assert alignment
      Address = Value div 4,
      %% TODO: handle non-zero sections
      0 = (Address band bnot ?PDP10_UINT18_MAX), % assert section
      {ok, Address band ?PDP10_UINT18_MAX};
    ?R_PDP10_LOCAL_B   ->
      Address = Value div 4,
      %% TODO: handle non-zero sections
      0 = (Address band bnot ?PDP10_UINT18_MAX), % assert section
      P = (3 - (Value band 3)) * 9, % P \in {0, 9, 18, 27}
      S = 9,
      {ok, (P bsl 30) bor (S bsl 24) bor (Address band ?PDP10_UINT18_MAX)};
    ?R_PDP10_LOCAL_H   ->
      0 = (Value band 1), % assert alignment
      Address = Value div 4,
      %% TODO: handle non-zero sections
      0 = (Address band bnot ?PDP10_UINT18_MAX), % assert section
      P = (2 - (Value band 3)) * 9, % P \in {0, 18}
      S = 18,
      {ok, (P bsl 30) bor (S bsl 24) bor (Address band ?PDP10_UINT18_MAX)};
    ?R_PDP10_GLOBAL_B  ->
      Address = Value div 4,
      0 = (Address band bnot ((1 bsl 30) - 1)), % assert
      PS = 8#70 + (Value band 3),
      {ok, (PS bsl 30) bor (Address band ((1 bsl 30) - 1))};
    ?R_PDP10_GLOBAL_H  ->
      0 = (Value band 1), % assert alignment
      Address = Value div 4,
      PS = 8#75 + ((Value band 2) bsr 1),
      {ok, (PS bsl 30) bor (Address band ((1 bsl 30) - 1))};
    ?R_PDP10_LITERAL_W ->
      Value = (Value band ?PDP10_UINT36_MAX), % assert
      {ok, Value};
    ?R_PDP10_LITERAL_H ->
      Value = (Value band ?PDP10_UINT18_MAX), % assert
      {ok, Value};
    ?R_PDP10_LITERAL_B ->
      Value = (Value band ?PDP10_UINT9_MAX), % assert
      {ok, Value}
  end.

output_directly(Output, Byte) ->
  #frag_output{fp = FP, frag_offset = FragOffset} = Output,
  case pdp10_stdio:fputc(Byte, FP) of
    ok -> {ok, Output#frag_output{frag_offset = FragOffset + 1}};
    {error, _Reason} = Error -> Error
  end.

%% Error reporting -------------------------------------------------------------

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {cannot_open, File, Reason0} ->
      io_lib:format("cannot open ~s: ~s", [File, error:format(Reason0)]);
    {premature_eof, File, ShName} ->
      io_lib:format("premature eof: ~s:~s", [File, ShName]);
    {undefined_symbol, Symbol} ->
      io_lib:format("undefined symbol: ~s", [Symbol]);
    {unresolved_relocs, File, ShName} ->
      io_lib:format("unresolved relocations: ~s:~s", [File, ShName])
  end.
