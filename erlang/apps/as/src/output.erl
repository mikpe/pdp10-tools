%%% -*- erlang-indent-level: 2 -*-
%%%
%%% ELF output for pdp10-elf as
%%% Copyright (C) 2013-2019  Mikael Pettersson
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
%%%-----------------------------------------------------------------------------
%%%
%%% Output file layout:
%%%
%%% ELF header
%%% <sections directly defined by the input>
%%% <.strtab, if any symbols>
%%% <.symtab, if any symbols>
%%% <.shstrtab, if any sections>
%%% <section header table, if any sections>
%%%
%%% Processing steps:
%%%
%%% initialize context
%%% for each section:
%%% - add name to .shstrtab, assign sh_name
%%% - assign sh_offset and st_shndx
%%% - update context
%%% for each symbol:
%%% - add name to .strtab, assign st_name
%%% - assign st_shndx
%%% append .strtab to list of sections
%%% append .symtab to list of sections
%%% append .shstrtab to list of sections

-module(output).

-export([ tunit/2
        , format_error/1
        ]).

-include("tunit.hrl").
-include_lib("lib/include/pdp10_elf36.hrl").

-record(strtab,
        { map :: #{string() => non_neg_integer()}
        , dot :: pos_integer()
        }).

-record(context,
        { tunit    :: #tunit{}
        , shnum    :: pos_integer()
        , offset   :: pos_integer()
        , shstrtab :: #strtab{}
        , strtab   :: #strtab{}
        }).

-spec tunit(#tunit{}, string()) -> ok | {error, {module(), term()}}.
tunit(Tunit, File) ->
  emit(layout(Tunit), File).

%% LAYOUT ======================================================================

layout(Tunit) ->
  lists:foldl(
    fun(Fun, Context) -> Fun(Context) end,
    context_new(Tunit),
    [ fun process_sections/1
    , fun process_symbols/1
    , fun create_strtab/1
    , fun create_symtab/1
    , fun create_shstrtab/1
    , fun align_shtab/1
    ]).

context_new(Tunit) ->
  #context{ tunit = Tunit
          , shnum = 1
          , offset = ?ELF36_EHDR_SIZEOF
          , shstrtab = strtab_new()
          , strtab = strtab_new()
          }.

%% Sections --------------------------------------------------------------------

process_sections(Context) ->
  #context{tunit = #tunit{sections = Sections}} = Context,
  lists:foldl(fun process_section/2, Context, maps:values(Sections)).

process_section(Section, Context) ->
  append_section(Context, Section).

append_section(Context, Section) ->
  #section{ dot = Dot
          , name = Name
          , sh_addralign = ShAddrAlign
          } = Section,
  case Dot of
    0 -> Context;
    _ ->
      #context{ tunit = Tunit
              , shnum = ShNum
              , offset = Offset
              , shstrtab = ShStrTab
              } = Context,
      {ShName, NewShStrTab} = strtab_enter(ShStrTab, Name),
      ShOffset = (Offset + ShAddrAlign - 1) band bnot (ShAddrAlign - 1),
      NewSection =
        Section#section{sh_name = ShName, sh_offset = ShOffset, shndx = ShNum},
      NewTunit = tunit:put_section(Tunit, NewSection),
      Context#context{ tunit = NewTunit
                     , shnum = ShNum + 1
                     , offset = ShOffset + Dot
                     , shstrtab = NewShStrTab
                     }
  end.

%% Symbols ---------------------------------------------------------------------

process_symbols(Context) ->
  #context{tunit = #tunit{symbols = Symbols}} = Context,
  lists:foldl(fun process_symbol/2, Context, maps:values(Symbols)).

process_symbol(Symbol, Context) ->
  #symbol{name = Name, section = Section} = Symbol,
  #context{tunit = Tunit, strtab = StrTab} = Context,
  {StName, NewStrTab} = strtab_enter(StrTab, Name),
  StShndx =
    case Section of
      false -> ?SHN_UNDEF;
      abs -> ?SHN_ABS;
      _ ->
        #section{shndx = Shndx} = tunit:get_section(Tunit, Section),
        Shndx % assigned in append_section/2 above
    end,
  NewSymbol = Symbol#symbol{st_name = StName, st_shndx = StShndx},
  NewTunit = tunit:put_symbol(Tunit, NewSymbol),
  Context#context{tunit = NewTunit, strtab = NewStrTab}.

%% Symbol string table (.strtab) -----------------------------------------------

create_strtab(Context) ->
  case maps:size(Context#context.tunit#tunit.symbols) of
    0 -> Context;
    _ ->
      StrTab = Context#context.strtab,
      Image = strtab_image(StrTab),
      Section =
        #section{ name = ".strtab"
                , data = {image, Image}
                , dot = image_size(Image)
                , sh_type = ?SHT_STRTAB
                , sh_flags = ?SHF_MERGE bor ?SHF_STRINGS % FIXME: check
                , sh_link = ?SHN_UNDEF
                , sh_addralign = 1 % FIXME: check
                , sh_entsize = 1 % FIXME: check
                },
      append_section(Context, Section)
  end.

%% Symbol table (.symtab) ------------------------------------------------------

create_symtab(Context) ->
  #context{tunit = Tunit} = Context,
  #tunit{symbols = Symbols} = Tunit,
  case maps:size(Symbols) of
    0 -> Context;
    NrSyms ->
      #section{shndx = StrTabShndx} = tunit:get_section(Tunit, ".strtab"),
      Image = symbols_image(Symbols),
      Size = (NrSyms + 1) * ?ELF36_SYM_SIZEOF,
      Size = image_size(Image), % consistency check
      Section =
        #section{ name = ".symtab"
                , data = {image, Image}
                , dot = Size
                , sh_type = ?SHT_SYMTAB
                , sh_flags = 0
                , sh_link = StrTabShndx
                , sh_addralign = 4 % FIXME: check
                , sh_entsize = ?ELF36_SYM_SIZEOF
                },
      append_section(Context, Section)
  end.

symbols_image(Symbols) ->
  ElfSym0 =
    #elf36_Sym{ st_name = 0
              , st_value = 0
              , st_size = 0
              , st_info = ?ELF36_ST_INFO(?STB_LOCAL, ?STT_NOTYPE)
              , st_other = 0
              , st_shndx = ?SHN_UNDEF
              },
  %% FIXME: local symbols first, followed by the weak or global ones
  [elf36_Sym_image(ElfSym0) |
   lists:map(fun symbol_image/1, maps:values(Symbols))].

symbol_image(Symbol) ->
  #symbol{ st_value = StValue
         , st_size = StSize
         , st_info = StInfo
         , st_name = StName
         , st_shndx = StShndx
         } = Symbol,
  ElfSym =
    #elf36_Sym{ st_name = StName
              , st_value = StValue
              , st_size = StSize
              , st_info = StInfo
              , st_other = ?STV_DEFAULT % FIXME: should be set earlier
              , st_shndx = StShndx
              },
  elf36_Sym_image(ElfSym).

%% FIXME: the code below belongs in a library

elf36_Sym_image(ElfSym) ->
  #elf36_Sym{ st_name = StName
            , st_value = StValue
            , st_size = StSize
            , st_info = StInfo
            , st_other = StOther
            , st_shndx = StShndx
            } = ElfSym,
  [ elf36_Word_image(StName)
  , elf36_Addr_image(StValue)
  , elf36_Word_image(if StSize =:= false -> 0; true -> StSize end)
  , elf36_Uchar_image(StInfo)
  , elf36_Uchar_image(StOther)
  , elf36_Half_image(StShndx)
  ].

elf36_Addr_image(Addr) -> uint36_image(Addr).
elf36_Half_image(Half) -> uint18_image(Half).
elf36_Off_image(Off) -> uint36_image(Off).
elf36_Word_image(Word) -> uint36_image(Word).
elf36_Uchar_image(Uchar) -> uint9_image(Uchar).

uint9_image(Uint9) ->
  Uint9 band 511.

uint18_image(Uint18) ->
  pdp10_extint:uint18_to_ext(Uint18).

uint36_image(Uint36) ->
  pdp10_extint:uint36_to_ext(Uint36).

image_size(Image) -> image_size(Image, 0).

image_size([H | T], Acc) -> image_size(T, image_size(H, Acc));
image_size([], Acc) -> Acc;
image_size(TByte, Acc) when is_integer(TByte), 0 =< TByte, TByte =< 511 -> Acc + 1.

%% Section Header String Table (.shstrtab) -------------------------------------

create_shstrtab(Context) ->
  case Context#context.shnum of
    1 -> Context;
    _ ->
      %% Note that append_section/1 enters the section's name to shstrtab,
      %% updating its contents if the name wasn't already there, which would
      %% invalidate the image recorded in the section.  To avoid that, enter
      %% the name first.
      OldShStrTab = Context#context.shstrtab,
      {_ShName, NewShStrTab} = strtab_enter(OldShStrTab, ".shstrtab"),
      Image = strtab_image(NewShStrTab),
      Section =
        #section{ name = ".shstrtab"
                , data = {image, Image}
                , dot = image_size(Image)
                , sh_type = ?SHT_STRTAB
                , sh_flags = ?SHF_MERGE bor ?SHF_STRINGS % FIXME: check
                , sh_link = ?SHN_UNDEF
                , sh_addralign = 1 % FIXME: check
                , sh_entsize = 1 % FIXME: check
                },
      append_section(Context#context{shstrtab = NewShStrTab}, Section)
  end.

%% Align Section Header Table --------------------------------------------------

align_shtab(Context) ->
  case Context#context.shnum of
    1 ->
      Context#context{shnum = 0, offset = 0};
    _ ->
      Offset = Context#context.offset,
      ShTabOffset = (Offset + (4 - 1)) band bnot (4 - 1),
      Context#context{offset = ShTabOffset}
  end.

%% String Tables ---------------------------------------------------------------
%% FIXME: duplicates code for .ident directive / .comment section

strtab_new() ->
  #strtab{map = maps:new(), dot = 1}. % 1 due to NUL before 1st string

strtab_enter(StrTab = #strtab{map = Map, dot = Dot}, String) ->
  case maps:get(String, Map, false) of
    false -> {Dot, StrTab#strtab{map = maps:put(String, Dot, Map),
                                 dot = Dot + length(String) + 1}}; % +1 for terminating NUL
    Offset -> {Offset, StrTab}
  end.

strtab_image(#strtab{map = Map}) ->
  KVs = maps:to_list(Map),
  VKs = lists:map(fun({K, V}) -> {V, K} end, KVs),
  SortedVKs = lists:sort(VKs),
  [0 | lists:map(fun({_V, K}) -> K ++ [0] end, SortedVKs)].

%% EMIT ========================================================================

emit(Context, File) ->
  case pdp10_stdio:fopen(File, [raw, write, delayed_write]) of
    {ok, FP} ->
      try
        Funs =
          [ fun emit_elf_header/3
          , fun emit_sections/3
          , fun emit_shtab/3
          ],
        emit(Funs, Context, FP, 0)
      after pdp10_stdio:fclose(FP)
      end;
    {error, Reason} -> {error, {?MODULE, {cannot_open, File, Reason}}}
  end.

emit([], _Context, _FP, _Offset) -> ok;
emit([Fun | Funs], Context, FP, Offset) ->
  case Fun(Context, FP, Offset) of
    {ok, NewOffset} -> emit(Funs, Context, FP, NewOffset);
    {error, _Reason} = Error -> Error
  end.

emit_elf_header(Context, FP, Offset = 0) ->
  ShStrTabShndx =
    case tunit:get_section(Context#context.tunit, ".shstrtab") of
      #section{shndx = Shndx} -> Shndx;
      false -> 0
    end,
  ElfHdr =
    #elf36_Ehdr{ e_ident = e_ident()
               , e_type = ?ET_REL
               , e_machine = ?EM_PDP10 % FIXME: target-specific
               , e_version = ?EV_CURRENT
               , e_entry = 0
               , e_phoff = 0
               , e_shoff = Context#context.offset
               , e_flags = 0
               , e_ehsize = ?ELF36_EHDR_SIZEOF
               , e_phentsize = 0
               , e_phnum = 0
               , e_shentsize = ?ELF36_SHDR_SIZEOF
               , e_shnum = Context#context.shnum
               , e_shstrndx = ShStrTabShndx
               },
  emit_image(elf36_Ehdr_image(ElfHdr), ?ELF36_EHDR_SIZEOF, FP, Offset).

emit_sections(Context, FP, Offset = ?ELF36_EHDR_SIZEOF) ->
  #context{tunit = #tunit{sections = SectionsMap}} = Context,
  Sections = lists:sort(fun order_by_sh_offset/2, maps:values(SectionsMap)),
  emit_sections2(Sections, FP, Offset).

emit_sections2([], _FP, Offset) -> {ok, Offset};
emit_sections2([Section | Sections], FP, Offset) ->
  case emit_section(Section, FP, Offset) of
    {ok, NewOffset} -> emit_sections2(Sections, FP, NewOffset);
    {error, _Reason} = Error -> Error
  end.

emit_section(Section, FP, Offset) ->
  case Section#section.dot of
    0 -> {ok, Offset};
    Dot ->
      ShOffset = Section#section.sh_offset,
      NrPadBytes = ShOffset - Offset,
      case emit_padding(NrPadBytes, FP) of
        ok ->
          {image, Image} = Section#section.data,
          emit_image(Image, Dot, FP, ShOffset);
        {error, _Reason} = Error -> Error
      end
  end.

emit_shtab(Context, FP, Offset) ->
  case Context#context.offset of
    0 -> {ok, Offset};
    ShTabOffset ->
      case emit_padding(ShTabOffset - Offset, FP) of
        ok ->
          case emit_shdr0(FP, ShTabOffset) of
            {ok, NewOffset} ->
              #context{tunit = #tunit{sections = SectionsMap}} = Context,
              Sections = lists:sort(fun order_by_shndx/2, maps:values(SectionsMap)),
              emit_shdrs(Sections, FP, NewOffset);
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end
  end.

emit_shdrs([], _FP, Offset) -> {ok, Offset};
emit_shdrs([Section | Sections], FP, Offset) ->
  case emit_shdr(Section, FP, Offset) of
    {ok, NewOffset} -> emit_shdrs(Sections, FP, NewOffset);
    {error, _Reason} = Error -> Error
  end.

emit_shdr(Section, FP, Offset) ->
  case Section#section.dot of
    0 -> {ok, Offset};
    Dot ->
      #section{ sh_name = ShName
              , sh_type = ShType
              , sh_offset = ShOffset
              , sh_flags = ShFlags
              , sh_link = ShLink
              , sh_addralign = ShAddrAlign
              , sh_entsize = ShEntSize
              } = Section,
      ElfShdr =
        #elf36_Shdr{ sh_name = ShName
                   , sh_type = ShType
                   , sh_flags = ShFlags
                   , sh_addr = 0
                   , sh_offset = ShOffset
                   , sh_size = Dot
                   , sh_link = ShLink
                   , sh_info = 0 % FIXME: for symtab, LAST_LOCAL + 1
                   , sh_addralign = ShAddrAlign
                   , sh_entsize = ShEntSize
                   },
      emit_elf36_Shdr(ElfShdr, FP, Offset)
  end.

emit_shdr0(FP, Offset) ->
  ElfShdr0 =
    #elf36_Shdr{ sh_name = 0
               , sh_type = ?SHT_NULL
               , sh_flags = 0
               , sh_addr = 0
               , sh_offset = 0
               , sh_size = 0
               , sh_link = ?SHN_UNDEF
               , sh_info = 0
               , sh_addralign = 0
               , sh_entsize = 0
               },
  emit_elf36_Shdr(ElfShdr0, FP, Offset).

emit_elf36_Shdr(Shdr, FP, Offset) ->
  emit_image(elf36_Shdr_image(Shdr), ?ELF36_SHDR_SIZEOF, FP, Offset).

elf36_Shdr_image(ElfShdr) ->
  #elf36_Shdr{ sh_name = ShName
             , sh_type = ShType
             , sh_flags = ShFlags
             , sh_addr = ShAddr
             , sh_offset = ShOffset
             , sh_size = ShSize
             , sh_link = ShLink
             , sh_info = ShInfo
             , sh_addralign = ShAddrAlign
             , sh_entsize = ShEntSize
             } = ElfShdr,
  [ elf36_Word_image(ShName)
  , elf36_Word_image(ShType)
  , elf36_Word_image(ShFlags)
  , elf36_Addr_image(ShAddr)
  , elf36_Off_image(ShOffset)
  , elf36_Word_image(ShSize)
  , elf36_Word_image(ShLink)
  , elf36_Word_image(ShInfo)
  , elf36_Word_image(ShAddrAlign)
  , elf36_Word_image(ShEntSize)
  ].

emit_padding(0, _FP) -> ok;
emit_padding(N, FP) when N > 0 ->
  case pdp10_stdio:fputc(0, FP) of
    ok -> emit_padding(N - 1, FP);
    {error, _Reason} = Error -> Error
  end.

order_by_sh_offset(Section1, Section2) ->
  Section1#section.sh_offset =< Section2#section.sh_offset.

order_by_shndx(Section1, Section2) ->
  Section1#section.shndx =< Section2#section.shndx.

elf36_Ehdr_image(ElfHdr) ->
  #elf36_Ehdr{ e_ident = EIdent
             , e_type = EType
             , e_machine = EMachine
             , e_version = EVersion
             , e_entry = EEntry
             , e_phoff = EPhOff
             , e_shoff = EShOff
             , e_flags = EFlags
             , e_ehsize = EEhSize
             , e_phentsize = EPhEntSize
             , e_phnum = EPhNum
             , e_shentsize = EShEntSize
             , e_shnum = EShNum
             , e_shstrndx = EShStrNdx
             } = ElfHdr,
  [ EIdent % already a list of bytes
  , elf36_Half_image(EType)
  , elf36_Half_image(EMachine)
  , elf36_Word_image(EVersion)
  , elf36_Addr_image(EEntry)
  , elf36_Off_image(EPhOff)
  , elf36_Off_image(EShOff)
  , elf36_Word_image(EFlags)
  , elf36_Half_image(EEhSize)
  , elf36_Half_image(EPhEntSize)
  , elf36_Half_image(EPhNum)
  , elf36_Half_image(EShEntSize)
  , elf36_Half_image(EShNum)
  , elf36_Half_image(EShStrNdx)
  ].

e_ident() ->
  tuple_to_list(
    erlang:make_tuple(
      ?EI_NIDENT, 0,
      [ {1 + ?EI_MAG0, ?ELFMAG0}
      , {1 + ?EI_MAG1, ?ELFMAG1}
      , {1 + ?EI_MAG2, ?ELFMAG2}
      , {1 + ?EI_MAG3, ?ELFMAG3}
      , {1 + ?EI_CLASS, ?ELFCLASS36}
      , {1 + ?EI_DATA, ?ELFDATA2MSB}
      , {1 + ?EI_VERSION, ?EV_CURRENT}
      , {1 + ?EI_OSABI, ?ELFOSABI_NONE} % TODO: ELFOSABI_LINUX instead?
      , {1 + ?EI_ABIVERSION, 0}
      ])).

emit_image(Image, NrBytes, FP, Offset) ->
  NrBytes = image_size(Image), % assert
  case image_write(Image, FP) of
    ok -> {ok, Offset + NrBytes};
    {error, _Reason} = Error -> Error
  end.

image_write([H | T], FP) ->
  case image_write(H, FP) of
    ok -> image_write(T, FP);
    {error, _Reason} = Error -> Error
  end;
image_write([], _FP) -> ok;
image_write(TByte, FP) when is_integer(TByte), 0 =< TByte, TByte =< 511 ->
  pdp10_stdio:fputc(TByte, FP).

%% Error reporting -------------------------------------------------------------

-spec format_error(term()) -> io_lib:chars().
format_error({cannot_open, File, Reason}) ->
  io_lib:format("opening ~s: ~s", [File, error:format(Reason)]).
