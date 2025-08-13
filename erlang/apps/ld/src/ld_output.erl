%%% -*- erlang-indent-level: 2 -*-
%%%
%%% ELF output for pdp10-elf ld
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
%%% Generate ELF output for the linked input:
%%%
%%% - ELF header
%%% - Program Header table, one Phdr per segment
%%% - Segments
%%%
%%% Relocations are applied while the image data is copied from input sections
%%% to output segments.

-module(ld_output).

-export([ output/5
        , format_error/1
        ]).

-include("ld_internal.hrl").
-include_lib("lib/include/pdp10_relocs.hrl").

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
  Ehdr = Ehdr0#elf_Ehdr{ e_type = ?ET_EXEC
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
  #elf_Phdr{p_offset = SegOffset} = Phdr,
  case output_padding(SegOffset - Offset, FP) of
    ok -> output_sections(Sections, GlobalMap, FileMap, FP, SegOffset);
    {error, _Reason} = Error -> Error
  end.

%% Sections Output -------------------------------------------------------------

output_sections(Sections, GlobalMap, FileMap, FP, SegOffset) ->
  output_sections(Sections, GlobalMap, FileMap, FP, SegOffset, _Offset = 0).

output_sections(_Sections = [], _GlobalMap, _FileMap, _FP, SegOffset, Offset) ->
  {ok, SegOffset + Offset};
output_sections([Section | Sections], GlobalMap, FileMap, FP, SegOffset, Offset) ->
  case output_section(Section, GlobalMap, FileMap, FP, SegOffset, Offset) of
    {ok, NewOffset} -> output_sections(Sections, GlobalMap, FileMap, FP, SegOffset, NewOffset);
    {error, _Reason} = Error -> Error
  end.

output_section(Section, GlobalMap, FileMap, FP, SegOffset, Offset) ->
  #section{shdr = Shdr, frags = Frags} = Section,
  #elf_Shdr{sh_offset = ShOffset} = Shdr,
  case output_padding(ShOffset - Offset, FP) of
    ok -> output_frags(Frags, GlobalMap, FileMap, FP, SegOffset + ShOffset);
    {error, _Reason} = Error -> Error
  end.

%% Fragments Output ------------------------------------------------------------

output_frags(Frags, GlobalMap, FileMap, FP, SectOffset) ->
  output_frags(Frags, GlobalMap, FileMap, FP, SectOffset, _Offset = 0).

output_frags(_Frags = [], _GlobalMap, _FileMap, _FP, SectOffset, Offset) ->
  {ok, SectOffset + Offset};
output_frags([Frag | Frags], GlobalMap, FileMap, FP, SectOffset, Offset) ->
  case output_frag(Frag, GlobalMap, FileMap, FP, Offset) of
    {ok, NewOffset} -> output_frags(Frags, GlobalMap, FileMap, FP, SectOffset, NewOffset);
    {error, _Reason} = Error -> Error
  end.

%% Single Fragment Output ------------------------------------------------------
%%
%% Copy a section from an input file and append it to the output file.
%% Simultaneously apply associated relocations.
%%
%% The implementation is a state machine with two states:
%%
%% - COPY: FragOffset < RelOffset of next reloc, or there is not next reloc
%%   input bytes are written to the output FP, FragOffset is incremented
%%
%% - ACCUM: FragOffset >= RelOffset of next reloc
%%   input bytes are appended to temporary Buffer, FragOffset is incremented
%%   if Buffer length equals operand size of next reloc:
%%      convert Buffer to input word, apply reloc, giving output word
%%      convert output word to Pushback buffer and push that back to input
%%      decrement FragOffset by Buffer length, empty Buffer

output_frag(Frag, GlobalMap, FileMap, FP, Offset) ->
  case input_init(Frag) of
    {ok, {Input, Relocs}} ->
      SortedRelocs = lists:keysort(#elf_Rela.r_offset, Relocs),
      #sectfrag{file = File, shdr = #elf_Shdr{sh_name = ShName}} = Frag,
      case output_frag(Input, File, ShName, FP, SortedRelocs, GlobalMap, FileMap) of
        {ok, FragOffset} -> {ok, Offset + FragOffset};
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

output_frag(Input, File, ShName, FP, Relocs, GlobalMap, FileMap) ->
  case output_frag(Input, Relocs, File, GlobalMap, FileMap, FP, _FragOffset = 0, _Buffer = []) of
    {ok, {_Relocs = [], FragOffset}} ->
      input_fini(Input),
      {ok, FragOffset};
    {ok, {_Relocs, _FragOffset2}} ->
      {error, {?MODULE, {unresolved_relocs, File, ShName}}};
    {error, _Reason} = Error -> Error
  end.

output_frag(Input, Relocs, File, GlobalMap, FileMap, FP, FragOffset, Buffer) ->
  case input(Input) of
    {ok, {Byte, NewInput}} -> output_frag(Byte, NewInput, Relocs, File, GlobalMap, FileMap, FP, FragOffset, Buffer);
    eof -> {ok, {Relocs, FragOffset}};
    {error, _Reason} = Error -> Error
  end.

output_frag(Byte, Input, Relocs, File, GlobalMap, FileMap, FP, FragOffset, Buffer) ->
  case Relocs of
    [] -> output_byte(Byte, Input, Relocs, File, GlobalMap, FileMap, FP, FragOffset);
    [#elf_Rela{r_offset = RelOffset} = Reloc | NewRelocs] ->
      case FragOffset < RelOffset of
        true -> output_byte(Byte, Input, Relocs, File, GlobalMap, FileMap, FP, FragOffset);
        false ->
          NewBuffer = [Byte | Buffer],
          NewFragOffset = FragOffset + 1,
          Length = length(NewBuffer),
          case Length =:= sizeof_reloc(Reloc) of
            true ->
              Word = buffer_to_word(Length, lists:reverse(NewBuffer)),
              case apply_reloc(Reloc, File, GlobalMap, FileMap, Word) of
                {ok, NewWord} ->
                  Pushback = word_to_buffer(Length, NewWord),
                  NewInput = input_pushback(Input, Pushback),
                  output_frag(NewInput, NewRelocs, File, GlobalMap, FileMap, FP, NewFragOffset - Length, _Buffer = []);
                {error, _Reason} = Error -> Error
              end;
            false -> output_frag(Input, Relocs, File, GlobalMap, FileMap, FP, NewFragOffset, NewBuffer)
          end
      end
  end.

output_byte(Byte, Input, Relocs, File, GlobalMap, FileMap, FP, FragOffset) ->
  case pdp10_stdio:fputc(Byte, FP) of
    ok -> output_frag(Input, Relocs, File, GlobalMap, FileMap, FP, FragOffset + 1, _Buffer = []);
    {error, _Reason} = Error -> Error
  end.

buffer_to_word(4, Buffer) -> pdp10_extint:uint36_from_ext(Buffer);
buffer_to_word(2, Buffer) -> pdp10_extint:uint18_from_ext(Buffer);
buffer_to_word(1, [Byte]) -> Byte.

word_to_buffer(4, Word) -> pdp10_extint:uint36_to_ext(Word);
word_to_buffer(2, Word) -> pdp10_extint:uint18_to_ext(Word);
word_to_buffer(1, Byte) -> [Byte].

%% Relocations -----------------------------------------------------------------

sizeof_reloc(#elf_Rela{r_info = Info}) ->
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

apply_reloc(Reloc, File, GlobalMap, FileMap, Word) ->
  #elf_Rela{r_info = Info, r_addend = Addend} = Reloc,
  Type = ?ELF36_R_TYPE(Info),
  SymNdx = ?ELF36_R_SYM(Info),
  case symbol_value(SymNdx, File, FileMap, GlobalMap) of
    {ok, SymbolValue} -> apply_reloc(Type, SymbolValue + Addend, Word);
    {error, _Reason} = Error -> Error
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

%% Symbol values ---------------------------------------------------------------

symbol_value(SymNdx, File, FileMap, GlobalMap) ->
  LocalMap = maps:get(File, FileMap),
  resolve(maps:get(SymNdx, LocalMap), GlobalMap).

resolve(Value, _GlobalMap) when is_integer(Value) -> {ok, Value};
resolve(Symbol, GlobalMap) ->
  case maps:get(Symbol, GlobalMap, false) of
    Value when is_integer(Value) -> {ok, Value};
    false -> {error, {?MODULE, {undefined_symbol, Symbol}}}
  end.

%% Fragment input buffer -------------------------------------------------------
%%
%% An input section consists of its image data and associated relocations.
%% An input object is a streaming reader of that image data with a pushback buffer.

-record(frag_input,
        { fp :: pdp10_stdio:file()
        , file :: ifile()
        , sh_name :: string()
        , size :: non_neg_integer()
        , pushback :: [0..511]
        }).

input_init(Frag) ->
  #sectfrag{file = File, shdr = Shdr, relocs = RelocShdr} = Frag,
  {ActualFile, Base, Limit} = ifile_props(File),
  case pdp10_stdio:fopen(ActualFile, [raw, read]) of
    {ok, FP} ->
      case input_relocs(FP, Base, Limit, RelocShdr) of
        {ok, Relocs} ->
          #elf_Shdr{sh_offset = ShOffset, sh_size = ShSize, sh_name = ShName} = Shdr,
          case pdp10_stdio:fseek(FP, {bof, Base + ShOffset}) of
            ok ->
              Input = #frag_input{fp = FP, file = File, sh_name = ShName, size = ShSize, pushback = []},
              {ok, {Input, Relocs}};
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    {error, Reason} -> {error, {?MODULE, {cannot_open, File, Reason}}}
  end.

ifile_props({Archive, _Name, Offset, Size}) -> {Archive, _Base = Offset, _Limit = Offset + Size};
ifile_props(File) when is_list(File) -> {File, _Base = 0, _Limit = false}.

input_relocs(_FP, _Base, _Limit, false) -> {ok, []};
input_relocs(FP, Base, Limit, Shdr) -> pdp10_elf36:read_RelaTab(FP, Base, Limit, Shdr).

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

%% Error reporting -------------------------------------------------------------

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {cannot_open, File, Reason0} ->
      io_lib:format("cannot open ~s: ~s", [ifile_to_string(File), error:format(Reason0)]);
    {premature_eof, File, ShName} ->
      io_lib:format("premature eof: ~s:~s", [ifile_to_string(File), ShName]);
    {undefined_symbol, Symbol} ->
      io_lib:format("undefined symbol: ~s", [Symbol]);
    {unresolved_relocs, File, ShName} ->
      io_lib:format("unresolved relocations: ~s:~s", [ifile_to_string(File), ShName])
  end.

ifile_to_string({Archive, Name, _Offset, _Size}) -> io_lib:format("~s(~s)", [Archive, Name]);
ifile_to_string(File) when is_list(File) -> File.
