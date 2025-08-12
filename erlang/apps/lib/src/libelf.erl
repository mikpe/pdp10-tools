%%% -*- erlang-indent-level: 2 -*-
%%%
%%% I/O of ELF-32/36/64 entities
%%% Copyright (C) 2013-2025  Mikael Pettersson
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

-module(libelf).

-export([ read_Ehdr/2
        , read_Ehdr/4
        , read_PhTab/3
        , read_PhTab/5
        , read_RelaTab/3
        , read_RelaTab/5
        , read_ShTab/3
        , read_ShTab/5
        , read_SymTab/3
        , read_SymTab/5
        , make_Ehdr/1
        , write_Ehdr/3
        , write_Phdr/3
        , format_error/1
        ]).

-include_lib("lib/include/libelf.hrl").
-include_lib("lib/include/stdint.hrl").

-type elfclass() :: ?ELFCLASS32 | ?ELFCLASS36 | ?ELFCLASS64.
-type iodev() :: stdio8:file() | stdio9:file().

-export_type([ elfclass/0
             , iodev/0
             ]).

-type file() :: {elfclass(), iodev()}.

-type read_field() :: fun((file())
                      -> {ok, integer()} | {error, {module(), term()}}).
-type write_field() :: fun((file(), integer())
                       -> ok | {error, term()}).

-record(record_desc,
        { tag :: atom()
        , fields :: [{read_field(), write_field()}]
        }).

%% I/O of #elf_Ehdr{} ==========================================================

-spec read_Ehdr(elfclass(), iodev())
      -> {ok, #elf_Ehdr{}} | {error, {module(), term()}}.
read_Ehdr(EC, IoDev) ->
  read_Ehdr(EC, IoDev, _Base = 0, _Limit = false).

-spec read_Ehdr(elfclass(), iodev(), non_neg_integer(), false | non_neg_integer())
      -> {ok, #elf_Ehdr{}} | {error, {module(), term()}}.
read_Ehdr(EC, IoDev, Base, Limit) ->
  FP = {EC, IoDev},
  case fseek(FP, {bof, Base}) of
    ok ->
      case read_record(FP, elf_Ehdr_desc()) of
        {ok, Ehdr} = Result ->
          case (Limit =:= false) orelse (ftell(FP) =< Limit) of
            true ->
              case check_Ehdr(Ehdr) of
                ok -> Result;
                {error, _Reason} = Error -> Error
              end;
            false -> {error, {?MODULE, {limit, "Ehdr"}}}
          end;
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

-spec make_Ehdr(elfclass()) -> #elf_Ehdr{}.
make_Ehdr(EC) ->
  {EhSize, PhEntSize, ShEntSize} = ehdr_params(EC),
  Ident =
    tuple_to_list(
      erlang:make_tuple(
        ?EI_NIDENT, 0,
        [ {1 + ?EI_MAG0, ?ELFMAG0}
        , {1 + ?EI_MAG1, ?ELFMAG1}
        , {1 + ?EI_MAG2, ?ELFMAG2}
        , {1 + ?EI_MAG3, ?ELFMAG3}
        , {1 + ?EI_CLASS, EC}
        , {1 + ?EI_DATA, ?ELFDATA2MSB} % TODO: target-specific
        , {1 + ?EI_VERSION, ?EV_CURRENT}
        , {1 + ?EI_OSABI, ?ELFOSABI_NONE} % TODO: ELFOSABI_LINUX instead?
        , {1 + ?EI_ABIVERSION, 0}
        ])),
  #elf_Ehdr{ e_ident = Ident
           , e_type = ?ET_NONE
           , e_machine = ?EM_PDP10 % TODO: target-specific
           , e_version = ?EV_CURRENT
           , e_entry = 0
           , e_phoff = 0
           , e_shoff = 0
           , e_flags = 0
           , e_ehsize = EhSize
           , e_phentsize = PhEntSize
           , e_phnum = 0
           , e_shentsize = ShEntSize
           , e_shnum = 0
           , e_shstrndx = 0
           }.

ehdr_params(?ELFCLASS64) ->
  {?ELF64_EHDR_SIZEOF, ?ELF64_PHDR_SIZEOF, ?ELF64_SHDR_SIZEOF};
ehdr_params(_) ->
  {?ELF32_EHDR_SIZEOF, ?ELF32_PHDR_SIZEOF, ?ELF32_SHDR_SIZEOF}. % ELF-36 is the same

-spec write_Ehdr(elfclass(), iodev(), #elf_Ehdr{})
      -> ok | {error, {module(), term()}}.
write_Ehdr(EC, IoDev, Ehdr) ->
  FP = {EC, IoDev},
  write_record(FP, Ehdr, elf_Ehdr_desc()).

%% ELF-64 has the same field order as ELF-32/36, but Addr/Off fields get larger.
elf_Ehdr_desc() ->
  #record_desc{ tag = elf_Ehdr
              , fields =
                  [ {fun read_e_ident/1, fun write_e_ident/2} % e_ident
                  , {fun read_Half/1,    fun write_Half/2}    % e_type
                  , {fun read_Half/1,    fun write_Half/2}    % e_machine
                  , {fun read_Word/1,    fun write_Word/2}    % e_version
                  , {fun read_Addr/1,    fun write_Addr/2}    % e_entry
                  , {fun read_Off/1,     fun write_Off/2}     % e_phoff
                  , {fun read_Off/1,     fun write_Off/2}     % e_shoff
                  , {fun read_Word/1,    fun write_Word/2}    % e_flags
                  , {fun read_Half/1,    fun write_Half/2}    % e_ehsize
                  , {fun read_Half/1,    fun write_Half/2}    % e_phentsize
                  , {fun read_Half/1,    fun write_Half/2}    % e_phnum
                  , {fun read_Half/1,    fun write_Half/2}    % e_shentsize
                  , {fun read_Half/1,    fun write_Half/2}    % e_shnum
                  , {fun read_Half/1,    fun write_Half/2}    % e_shstrndx
                  ]
               }.

read_e_ident(FP) ->
  read(FP, ?EI_NIDENT, fun(Bytes) -> Bytes end).

write_e_ident(FP, Ident) ->
  ?EI_NIDENT = length(Ident), % assert
  fputs(Ident, FP).

check_Ehdr(Ehdr) ->
  check(Ehdr,
        [ fun check_Ehdr_ei_mag0/1
        , fun check_Ehdr_ei_mag1/1
        , fun check_Ehdr_ei_mag2/1
        , fun check_Ehdr_ei_mag3/1
        , fun check_Ehdr_ei_class/1
        , fun check_Ehdr_ei_data/1
        , fun check_Ehdr_ei_version/1
        , fun check_Ehdr_ei_osabi/1
        , fun check_Ehdr_ei_abiversion/1
        , fun check_Ehdr_ei_pad/1
        , fun check_Ehdr_e_type/1
        , fun check_Ehdr_e_machine/1
        , fun check_Ehdr_e_version/1
        , fun check_Ehdr_e_ehsize/1
        , fun check_Ehdr_e_phentsize/1
        , fun check_Ehdr_e_shentsize/1
        ]).

check(_X, []) -> ok;
check(X, [Check | Checks]) ->
  case Check(X) of
    ok -> check(X, Checks);
    {error, _Reason} = Error -> Error
  end.

check_Ehdr_ei_mag0(Ehdr) ->
  #elf_Ehdr{e_ident = Ident} = Ehdr,
  Mag0 = lists:nth(?EI_MAG0 + 1, Ident),
  case Mag0 of
    ?ELFMAG0 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_mag0, Mag0}}}
  end.

check_Ehdr_ei_mag1(Ehdr) ->
  #elf_Ehdr{e_ident = Ident} = Ehdr,
  Mag1 = lists:nth(?EI_MAG1 + 1, Ident),
  case Mag1 of
    ?ELFMAG1 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_mag1, Mag1}}}
  end.

check_Ehdr_ei_mag2(Ehdr) ->
  #elf_Ehdr{e_ident = Ident} = Ehdr,
  Mag2 = lists:nth(?EI_MAG2 + 1, Ident),
  case Mag2 of
    ?ELFMAG2 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_mag2, Mag2}}}
  end.

check_Ehdr_ei_mag3(Ehdr) ->
  #elf_Ehdr{e_ident = Ident} = Ehdr,
  Mag3 = lists:nth(?EI_MAG3 + 1, Ident),
  case Mag3 of
    ?ELFMAG3 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_mag3, Mag3}}}
  end.

check_Ehdr_ei_class(Ehdr) ->
  #elf_Ehdr{e_ident = Ident} = Ehdr,
  Class = lists:nth(?EI_CLASS + 1, Ident),
  case Class of
    ?ELFCLASS64 -> ok;
    ?ELFCLASS32 -> ok;
    ?ELFCLASS36 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_class, Class}}}
  end.

check_Ehdr_ei_data(Ehdr) ->
  #elf_Ehdr{e_ident = Ident} = Ehdr,
  Data = lists:nth(?EI_DATA + 1, Ident),
  case Data of
    ?ELFDATA2MSB -> ok;
    _ -> {error, {?MODULE, {wrong_ei_data, Data}}}
  end.

check_Ehdr_ei_version(Ehdr) ->
  #elf_Ehdr{e_ident = Ident} = Ehdr,
  Version = lists:nth(?EI_VERSION + 1, Ident),
  case Version of
    ?EV_CURRENT -> ok;
    _ -> {error, {?MODULE, {wrong_ei_version, Version}}}
  end.

check_Ehdr_ei_osabi(Ehdr) ->
  #elf_Ehdr{e_ident = Ident} = Ehdr,
  OSABI = lists:nth(?EI_OSABI + 1, Ident),
  case OSABI of
    ?ELFOSABI_NONE -> ok;
    ?ELFOSABI_LINUX -> ok;
    _ -> {error, {?MODULE, {wrong_ei_osabi, OSABI}}}
  end.

check_Ehdr_ei_abiversion(Ehdr) ->
  #elf_Ehdr{e_ident = Ident} = Ehdr,
  ABIVersion = lists:nth(?EI_ABIVERSION + 1, Ident),
  case ABIVersion of
    0 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_abiversion, ABIVersion}}}
  end.

check_Ehdr_ei_pad(Ehdr) ->
  #elf_Ehdr{e_ident = Ident} = Ehdr,
  Pad = lists:nthtail(?EI_PAD, Ident),
  Zeroes = lists:duplicate(?EI_NIDENT - ?EI_PAD, 0),
  case Pad of
    Zeroes -> ok;
    _ -> {error, {?MODULE, {wrong_ei_pad, Pad}}}
  end.

check_Ehdr_e_type(Ehdr) ->
  #elf_Ehdr{e_type = Type} = Ehdr,
  case Type of
    ?ET_REL -> ok;
    ?ET_EXEC -> ok;
    ?ET_DYN -> ok;
    ?ET_CORE -> ok;
    _ -> {error, {?MODULE, {wrong_e_type, Type}}}
  end.

check_Ehdr_e_machine(Ehdr) ->
  #elf_Ehdr{e_machine = Machine} = Ehdr,
  case Machine of
    ?EM_PDP10 -> ok;
    _ -> {error, {?MODULE, {wrong_e_machine, Machine}}}
  end.

check_Ehdr_e_version(Ehdr) ->
  #elf_Ehdr{e_version = Version} = Ehdr,
  case Version of
    ?EV_CURRENT -> ok;
    _ -> {error, {?MODULE, {wrong_e_version, Version}}}
  end.

check_Ehdr_e_ehsize(Ehdr) ->
  #elf_Ehdr{e_ehsize = EhSize} = Ehdr,
  case EhSize of
    ?ELF64_EHDR_SIZEOF -> ok;
    ?ELF32_EHDR_SIZEOF -> ok; % ELF-36 is the same
    _ -> {error, {?MODULE, {wrong_e_ehsize, EhSize}}}
  end.

check_Ehdr_e_phentsize(Ehdr) ->
  #elf_Ehdr{e_phoff = PhOff, e_phentsize = PhEntSize} = Ehdr,
  case {PhOff, PhEntSize} of
    {0, _} -> ok;
    {_, ?ELF64_PHDR_SIZEOF} -> ok;
    {_, ?ELF32_PHDR_SIZEOF} -> ok; % ELF-36 is the same
    _ -> {error, {?MODULE, {wrong_e_phentsize, PhEntSize}}}
  end.

check_Ehdr_e_shentsize(Ehdr) ->
  #elf_Ehdr{e_shoff = ShOff, e_shentsize = ShEntSize} = Ehdr,
  case {ShOff, ShEntSize} of
    {0, _} -> ok;
    {_, ?ELF64_SHDR_SIZEOF} -> ok;
    {_, ?ELF32_SHDR_SIZEOF} -> ok; % ELF-36 is the same
    _ -> {error, {?MODULE, {wrong_e_shentsize, ShEntSize}}}
  end.

%% I/O of PhTab ================================================================

-spec read_PhTab(elfclass(), iodev(), #elf_Ehdr{})
      -> {ok, [#elf_Phdr{}]} | {error, {module(), term()}}.
read_PhTab(EC, IoDev, Ehdr) ->
  read_PhTab(EC, IoDev, _Base = 0, _Limit = false, Ehdr).

-spec read_PhTab(elfclass(), iodev(), non_neg_integer(), false | non_neg_integer(), #elf_Ehdr{})
      -> {ok, [#elf_Phdr{}]} | {error, {module(), term()}}.
read_PhTab(EC, IoDev, Base, Limit, Ehdr) ->
  FP = {EC, IoDev},
  #elf_Ehdr{ e_phoff = PhOff
           , e_phentsize = PhEntSize
           , e_phnum = PhNum } = Ehdr,
  if PhOff =:= 0; PhNum =:= 0 ->
       {ok, []};
     true ->
       true = PhEntSize =:= ?ELF36_PHDR_SIZEOF, % assert
       %% FIXME: if PhNum = ?PN_XNUM the real PhNum is stored in Shdr0.sh_info
       case fseek(FP, {bof, Base + PhOff}) of
         ok ->
           case do_read_PhTab(FP, PhNum, []) of
             {ok, _PhTab} = Result ->
               case (Limit =:= false) orelse (ftell(FP) =< Limit) of
                 true -> Result;
                 false -> {error, {?MODULE, {limit, "PhTab"}}}
               end;
             {error, _Reason} = Error -> Error
           end;
         {error, _Reason} = Error -> Error
       end
  end.

do_read_PhTab(_FP, 0, Phdrs) -> {ok, lists:reverse(Phdrs)};
do_read_PhTab(FP, PhNum, Phdrs) ->
  case read_Phdr(FP) of
    {ok, Phdr} -> do_read_PhTab(FP, PhNum - 1, [Phdr | Phdrs]);
    {error, _Reason} = Error -> Error
  end.

%% I/O of relocation tables ====================================================

-spec read_RelaTab(elfclass(), iodev(), #elf_Shdr{})
      -> {ok, [#elf_Rela{}]} | {error, {module(), term()}}.
read_RelaTab(EC, IoDev, Shdr) ->
  read_RelaTab(EC, IoDev, _Base = 0, _Limit = false, Shdr).

-spec read_RelaTab(elfclass(), iodev(), non_neg_integer(), false | non_neg_integer(), #elf_Shdr{})
      -> {ok, [#elf_Rela{}]} | {error, {module(), term()}}.
read_RelaTab(EC, IoDev, Base, Limit, Shdr) ->
  FP = {EC, IoDev},
  #elf_Shdr{ sh_type = ShType
           , sh_size = ShSize
           , sh_offset = ShOffset
           , sh_entsize = ShEntSize
           } = Shdr,
  case ShType of
    ?SHT_RELA ->
      case ShEntSize of
        ?ELF36_RELA_SIZEOF ->
          case ShSize rem ?ELF36_RELA_SIZEOF of
            0 ->
              RelaNum = ShSize div ?ELF36_RELA_SIZEOF,
              case RelaNum of
                0 -> {ok, []};
                _ ->
                  case fseek(FP, {bof, Base + ShOffset}) of
                    ok ->
                      case do_read_RelaTab(FP, RelaNum, []) of
                        {ok, _RelaTab} = Result ->
                          case (Limit =:= false) orelse (ftell(FP) =< Limit) of
                            true -> Result;
                            false -> {error, {?MODULE, {limit, "RelaTab"}}}
                          end;
                        {error, _Reason} = Error -> Error
                      end;
                    {error, _Reason} = Error -> Error
                  end
              end;
            _ -> {error, {?MODULE, {wrong_relatab_sh_size, ShSize}}}
          end;
        _ -> {error, {?MODULE, {wrong_relatab_sh_entsize, ShEntSize}}}
      end;
    _ -> {error, {?MODULE, {wrong_relatab_sh_type, ShType}}}
  end.

do_read_RelaTab(_FP, _RelaNum = 0, Relas) -> {ok, lists:reverse(Relas)};
do_read_RelaTab(FP, RelaNum, Relas) when RelaNum > 0 ->
  case read_Rela(FP) of
    {ok, Rela} -> do_read_RelaTab(FP, RelaNum - 1, [Rela | Relas]);
    {error, _Reason} = Error -> Error
  end.

%% I/O of ShTab ================================================================

-spec read_ShTab(elfclass(), iodev(), #elf_Ehdr{})
      -> {ok, [#elf_Shdr{}]} | {error, {module(), term()}}.
read_ShTab(EC, IoDev, Ehdr) ->
  read_ShTab(EC, IoDev, _Base = 0, _Limit = false, Ehdr).

-spec read_ShTab(elfclass(), iodev(), non_neg_integer(), false | non_neg_integer(), #elf_Ehdr{})
      -> {ok, [#elf_Shdr{}]} | {error, {module(), term()}}.
read_ShTab(EC, IoDev, Base, Limit, Ehdr) ->
  FP = {EC, IoDev},
  #elf_Ehdr{ e_shoff = ShOff
           , e_shnum = ShNum0
           , e_shstrndx = ShStrNdx } = Ehdr,
  case ShOff of
    0 -> {ok, []};
    _ ->
      case fseek(FP, {bof, Base + ShOff}) of
        ok ->
          case read_Shdr(FP) of
            {ok, Shdr0} ->
              ShNum = actual_ShNum(ShNum0, Shdr0),
              case do_read_ShTab(FP, ShNum - 1, [Shdr0]) of
                {ok, ShTab} ->
                  case (Limit =:= false) orelse (ftell(FP) =< Limit) of
                    true ->
                      case read_ShStrTab(FP, Base, Limit, ShTab, ShStrNdx, Shdr0) of
                        {ok, ShStrTab} -> read_ShTab_names(ShTab, ShStrTab);
                        {error, _Reason} = Error -> Error
                      end;
                    false -> {error, {?MODULE, {limit, "ShTab"}}}
                  end;
                {error, _Reason} = Error -> Error
              end;
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end
  end.

actual_ShNum(0, #elf_Shdr{sh_size = ShNum} = _Shdr0) -> ShNum;
actual_ShNum(ShNum, _Shdr0) -> ShNum.

do_read_ShTab(_FP, 0, Shdrs) -> {ok, lists:reverse(Shdrs)};
do_read_ShTab(FP, ShNum, Shdrs) ->
  case read_Shdr(FP) of
    {ok, Shdr} -> do_read_ShTab(FP, ShNum - 1, [Shdr | Shdrs]);
    {error, _Reason} = Error -> Error
  end.

read_ShTab_names(ShTab, ShStrTab) ->
  read_ShTab_names(ShTab, ShStrTab, []).

read_ShTab_names([], _ShStrTab, Acc) -> {ok, lists:reverse(Acc)};
read_ShTab_names([Shdr | ShTab], ShStrTab, Acc) ->
  case read_Shdr_name(Shdr, ShStrTab) of
    {ok, NewShdr} -> read_ShTab_names(ShTab, ShStrTab, [NewShdr | Acc]);
    {error, _Reason} = Error -> Error
  end.

read_Shdr_name(Shdr = #elf_Shdr{sh_name = ShName}, ShStrTab) ->
  case get_name(ShStrTab, ShName) of
    {ok, Name} -> {ok, Shdr#elf_Shdr{sh_name = Name}};
    {error, _Reason} = Error -> Error
  end.

%% I/O of ShStrTab =============================================================

read_ShStrTab(FP, Base, Limit, ShTab, ShStrNdx, Shdr0) ->
  case ShStrNdx of
    ?SHN_UNDEF -> {ok, []};
    ?SHN_XINDEX -> read_StrTab(FP, Base, Limit, ShTab, Shdr0#elf_Shdr.sh_link);
    _ -> read_StrTab(FP, Base, Limit, ShTab, ShStrNdx)
  end.

%% I/O of StrTab ===============================================================

read_StrTab(FP, Base, Limit, ShTab, Index) ->
  ShNum = length(ShTab),
  case Index > 0 andalso Index < ShNum of
    true ->
      #elf_Shdr{ sh_type = Type
               , sh_size = Size
               , sh_offset = Offset
               } = lists:nth(Index + 1, ShTab),
      case Type of
        ?SHT_STRTAB ->
          case fseek(FP, {bof, Base + Offset}) of
            ok ->
              case fread(Size, FP) of
                {ok, _StrTab} = Result ->
                  case (Limit =:= false) orelse (ftell(FP) =< Limit) of
                    true -> Result;
                    false -> {error, {?MODULE, {limit, "StrTab"}}}
                  end;
                eof -> {error, {?MODULE, {eof_in_strtab, Index}}};
                {error, _Reason} = Error -> Error
              end;
            {error, _Reason} = Error -> Error
          end;
        _ -> {error, {?MODULE, {wrong_strtab_sh_type, Type, Index}}}
      end;
    false -> {error, {?MODULE, {wrong_strtab_index, Index}}}
  end.

%% reading a name from a StrTab ================================================

get_name(_StrTab, 0) -> {ok, ""}; % TODO: or some marker for "absent"
get_name(StrTab, Index) ->
  try lists:nthtail(Index, StrTab) of
    [C | Tail] -> get_name(C, Tail, [])
  catch _C:_R ->
    {error, {?MODULE, {wrong_index_in_strtab, Index}}}
  end.

get_name(0, _Tail, Acc) -> {ok, lists:reverse(Acc)};
get_name(C1, [C2 | Tail], Acc) -> get_name(C2, Tail, [C1 | Acc]);
get_name(_C, [], _Acc) -> {error, {?MODULE, strtab_not_nul_terminated}}.

%% I/O of SymTab ===============================================================

-spec read_SymTab(elfclass(), iodev(), [#elf_Shdr{}])
      -> {ok, {[#elf_Sym{}],non_neg_integer()}} | {error, {module(), term()}}.
read_SymTab(EC, IoDev, ShTab) ->
  read_SymTab(EC, IoDev, _Base = 0, _Limit = false, ShTab).

-spec read_SymTab(elfclass(), iodev(), non_neg_integer(), false | non_neg_integer(), [#elf_Shdr{}])
      -> {ok, {[#elf_Sym{}],non_neg_integer()}} | {error, {module(), term()}}.
read_SymTab(EC, IoDev, Base, Limit, ShTab) ->
  FP = {EC, IoDev},
  case find_SymTab(ShTab) of
    false -> {ok, {[], ?SHN_UNDEF}};
    {ok, {Shdr, ShNdx}} ->
      #elf_Shdr{ sh_link = ShLink
               , sh_entsize = ShEntSize
               , sh_size = ShSize
               , sh_offset = ShOffset
               } = Shdr,
      if ShEntSize =/= ?ELF36_SYM_SIZEOF ->
           {error, {?MODULE, {wrong_symtab_sh_entsize, ShEntSize}}};
         (ShSize rem ?ELF36_SYM_SIZEOF) =/= 0 ->
           {error, {?MODULE, {wrong_symtab_sh_size, ShSize}}};
         true ->
           case read_StrTab(FP, Base, Limit, ShTab, ShLink) of
             {ok, StrTab} ->
               SymNum = ShSize div ?ELF36_SYM_SIZEOF,
               case SymNum of
                 0 -> {ok, {[], ShNdx}};
                 _ ->
                   case fseek(FP, {bof, Base + ShOffset}) of
                     ok ->
                       case do_read_SymTab(FP, SymNum, []) of
                         {ok, SymTab} ->
                           case (Limit =:= false) orelse (ftell(FP) =< Limit) of
                             true ->
                               case read_SymTab_names(SymTab, StrTab) of
                                 {ok, NewSymTab} -> {ok, {NewSymTab, ShNdx}};
                                 {error, _Reason} = Error -> Error
                               end;
                             false -> {error, {?MODULE, {limit, "SymTab"}}}
                           end;
                         {error, _Reason} = Error -> Error
                       end;
                     {error, _Reason} = Error -> Error
                   end
               end;
             {error, _Reason} = Error -> Error
           end
      end
  end.

do_read_SymTab(_FP, _SymNum = 0, Syms) -> {ok, lists:reverse(Syms)};
do_read_SymTab(FP, SymNum, Syms) when SymNum > 0 ->
  case read_Sym(FP) of
    {ok, Sym} -> do_read_SymTab(FP, SymNum - 1, [Sym | Syms]);
    {error, _Reason} = Error -> Error
  end.

find_SymTab(ShTab) -> find_SymTab(ShTab, 0).

find_SymTab([], _I) -> false;
find_SymTab([#elf_Shdr{sh_type = ?SHT_SYMTAB} = Shdr | _], I) -> {ok, {Shdr, I}};
find_SymTab([_Shdr | ShTab], I) -> find_SymTab(ShTab, I + 1).

read_SymTab_names(SymTab, StrTab) ->
  read_SymTab_names(SymTab, StrTab, []).

read_SymTab_names([], _StrTab, Acc) -> {ok, lists:reverse(Acc)};
read_SymTab_names([Sym | SymTab], StrTab, Acc) ->
  case read_Sym_name(Sym, StrTab) of
    {ok, NewSym} -> read_SymTab_names(SymTab, StrTab, [NewSym | Acc]);
    {error, _Reason} = Error -> Error
  end.

read_Sym_name(Sym = #elf_Sym{st_name = StName}, StrTab) ->
  case get_name(StrTab, StName) of
    {ok, Name} -> {ok, Sym#elf_Sym{st_name = Name}};
    {error, _Reason} = Error -> Error
  end.

%% I/O of #elf_Phdr{} ==========================================================

read_Phdr({EC, _IoDev} = FP) ->
  read_Phdr_reorder(EC, read_record(FP, elf_Phdr_desc(EC))).

-spec write_Phdr(elfclass(), iodev(), #elf_Phdr{})
      -> ok | {error, {module(), term()}}.
write_Phdr(EC, IoDev, Phdr) ->
  FP = {EC, IoDev},
  write_record(FP, write_Phdr_reorder(EC, Phdr), elf_Phdr_desc(EC)).

%% ELF-64 has a different field order than ELF-32/36, and Addr/Off/Xword fields get larger.
elf_Phdr_desc(?ELFCLASS64) ->
  #record_desc{ tag = elf_Phdr
              , fields =
                  [ {fun read_Word/1, fun write_Word/2} % p_type
                  , {fun read_Word/1, fun write_Word/2} % p_flags
                  , {fun read_Off/1,  fun write_Off/2}  % p_offset
                  , {fun read_Addr/1, fun write_Addr/2} % p_vaddr
                  , {fun read_Addr/1, fun write_Addr/2} % p_paddr
                  , {fun read_Xword/1, fun write_Xword/2} % p_filesz
                  , {fun read_Xword/1, fun write_Xword/2} % p_memsz
                  , {fun read_Xword/1, fun write_Xword/2} % p_align
                  ]
              };
elf_Phdr_desc(_) ->
  #record_desc{ tag = elf_Phdr
              , fields =
                  [ {fun read_Word/1, fun write_Word/2} % p_type
                  , {fun read_Off/1,  fun write_Off/2}  % p_offset
                  , {fun read_Addr/1, fun write_Addr/2} % p_vaddr
                  , {fun read_Addr/1, fun write_Addr/2} % p_paddr
                  , {fun read_Word/1, fun write_Word/2} % p_filesz
                  , {fun read_Word/1, fun write_Word/2} % p_memsz
                  , {fun read_Word/1, fun write_Word/2} % p_flags
                  , {fun read_Word/1, fun write_Word/2} % p_align
                  ]
              }.

read_Phdr_reorder(?ELFCLASS64, {ok, {elf_Phdr, Type, Flags, Offset, VAddr, PAddr, FileSz, MemSz, Align}}) ->
  {ok, #elf_Phdr{p_type = Type, p_offset = Offset, p_vaddr = VAddr, p_paddr = PAddr, p_filesz = FileSz,
                 p_memsz = MemSz, p_flags = Flags, p_align = Align}};
read_Phdr_reorder(_, Result) -> Result.

write_Phdr_reorder(?ELFCLASS64, Phdr) ->
  #elf_Phdr{p_type = Type, p_offset = Offset, p_vaddr = VAddr, p_paddr = PAddr, p_filesz = FileSz,
            p_memsz = MemSz, p_flags = Flags, p_align = Align} = Phdr,
  {elf_Phdr, Type, Flags, Offset, VAddr, PAddr, FileSz, MemSz, Align};
write_Phdr_reorder(_, Phdr) -> Phdr.

%% I/O of #elf_Rela{} ==========================================================

read_Rela(FP) -> read_record(FP, elf_Rela_desc()).

%% ELF-64 has the same field order as ELF-32/36, but Addr/Sxword/Xword fields get larger.
elf_Rela_desc() ->
  #record_desc{ tag = elf_Rela
              , fields =
                  [ {fun read_Addr/1,  fun write_Addr/2}    % r_offset
                  , {fun read_Xword/1,  fun write_Xword/2}  % r_info
                  , {fun read_Sxword/1, fun write_Sxword/2} % r_addend
                  ]
              }.

%% I/O of #elf_Shdr{} ==========================================================

read_Shdr(FP) -> read_record(FP, elf_Shdr_desc()).

%% ELF-64 has the same field order as ELF-32/36, but Addr/Off/Xword fields get larger.
elf_Shdr_desc() ->
  #record_desc{ tag = elf_Shdr
              , fields =
                  [ {fun read_Word/1, fun write_Word/2} % sh_name
                  , {fun read_Word/1, fun write_Word/2} % sh_type
                  , {fun read_Xword/1, fun write_Xword/2} % sh_flags
                  , {fun read_Addr/1, fun write_Addr/2} % sh_addr
                  , {fun read_Off/1,  fun write_Off/2}  % sh_offset
                  , {fun read_Xword/1, fun write_Xword/2} % sh_size
                  , {fun read_Word/1, fun write_Word/2} % sh_link
                  , {fun read_Word/1, fun write_Word/2} % sh_info
                  , {fun read_Xword/1, fun write_Xword/2} % sh_addralign
                  , {fun read_Xword/1, fun write_Xword/2} % sh_entsize
                  ]
               }.

%% I/O of #elf_Sym{} ===========================================================

read_Sym({EC, _IoDev} = FP) ->
  read_Sym_reorder(EC, read_record(FP, elf_Sym_desc(EC))).

%% ELF-64 has a different field order than ELF-32/36, and Addr/Xword fields get larger.
elf_Sym_desc(?ELFCLASS64) ->
  #record_desc{ tag = elf_Sym
              , fields =
                  [ {fun read_Word/1,  fun write_Word/2}   % st_name
                  , {fun read_Uchar/1, fun write_Uchar/2}  % st_info
                  , {fun read_Uchar/1, fun write_Uchar/2}  % st_other
                  , {fun read_Half/1,  fun write_Half/2}   % st_shndx
                  , {fun read_Addr/1,  fun write_Addr/2}   % st_value
                  , {fun read_Xword/1,  fun write_Xword/2} % st_size
                  ]
               };
elf_Sym_desc(_) ->
  #record_desc{ tag = elf_Sym
              , fields =
                  [ {fun read_Word/1,  fun write_Word/2}  % st_name
                  , {fun read_Addr/1,  fun write_Addr/2}  % st_value
                  , {fun read_Word/1,  fun write_Word/2}  % st_size
                  , {fun read_Uchar/1, fun write_Uchar/2} % st_info
                  , {fun read_Uchar/1, fun write_Uchar/2} % st_other
                  , {fun read_Half/1,  fun write_Half/2}  % st_shndx
                  ]
               }.

read_Sym_reorder(?ELFCLASS64, {ok, {elf_Sym, Name, Info, Other, Shndx, Value, Size}}) ->
  {ok, #elf_Sym{st_name = Name, st_value = Value, st_size = Size, st_info = Info,
                st_other = Other, st_shndx = Shndx}};
read_Sym_reorder(_, Result) -> Result.

%% I/O of records ==============================================================

read_record(FP, #record_desc{tag = Tag, fields = Fields}) ->
  read_record(FP, Fields, [Tag]).

read_record(FP, [{ReadField, _WriteField} | Fields], Values) ->
  case ReadField(FP) of
    {ok, Value} -> read_record(FP, Fields, [Value | Values]);
    {error, _Reason} = Error -> Error
  end;
read_record(_FP, [], Values) ->
  {ok, list_to_tuple(lists:reverse(Values))}.

write_record(FP, Record, #record_desc{tag = Tag, fields = Fields}) ->
  [Tag | Values] = tuple_to_list(Record),
  do_write_record(FP, Fields, Values).

do_write_record(FP, [{_ReadField, WriteField} | Fields], [Value | Values]) ->
  case WriteField(FP, Value) of
    ok -> do_write_record(FP, Fields, Values);
    {error, _Reason} = Error -> Error
  end;
do_write_record(_FP, _Fields = [], _Values = []) ->
  ok.

%% I/O of scalar items =========================================================

read_Addr({?ELFCLASS64, _} = FP) -> read_u8(FP);
read_Addr(FP)  -> read_u4(FP).

read_Half(FP)  -> read_u2(FP).

read_Off({?ELFCLASS64, _} = FP) -> read_u8(FP);
read_Off(FP)   -> read_u4(FP).

read_Sxword({?ELFCLASS64, _} = FP) -> read_s8(FP);
read_Sxword(FP) -> read_s4(FP).

read_Uchar(FP) -> read_u1(FP).

read_Word(FP)  -> read_u4(FP).

read_Xword({?ELFCLASS64, _} = FP) -> read_u8(FP);
read_Xword(FP) -> read_u4(FP).

read_u1(FP) ->
  case fgetc(FP) of
    eof -> {error, {?MODULE, eof}};
    Other -> Other % {ok, _Byte} or {error, _Reason}
  end.

read_u2({?ELFCLASS36, _} = FP) -> read(FP, 2, fun extint:uint18_from_ext/1);
read_u2(FP) -> read(FP, 2, fun extint:uint16_from_ext/1).

read_u4({?ELFCLASS36, _} = FP) -> read(FP, 4, fun extint:uint36_from_ext/1);
read_u4(FP) -> read(FP, 4, fun extint:uint32_from_ext/1).

read_s4({?ELFCLASS36, _} = FP) -> read(FP, 4, fun sint36_from_ext/1);
read_s4(FP) -> read(FP, 4, fun sint32_from_ext/1).

read_s8(FP) -> read(FP, 8, fun sint64_from_ext/1).

sint32_from_ext(Bytes) ->
  sext:sext(extint:uint32_from_ext(Bytes), 32).

sint36_from_ext(Bytes) ->
  sext:sext(extint:uint36_from_ext(Bytes), 36).

sint64_from_ext(Bytes) ->
  sext:sext(extint:uint64_from_ext(Bytes), 64).

read_u8(FP) -> read(FP, 8, fun extint:uint64_from_ext/1).

read(FP, N, ConvFun) ->
  case fread(N, FP) of
    {ok, Bytes} -> {ok, ConvFun(Bytes)};
    eof -> {error, {?MODULE, eof}};
    {error, _Reason} = Error -> Error
  end.

write_Addr({?ELFCLASS64, _} = FP, Addr) -> write_u8(FP, Addr);
write_Addr(FP, Addr) -> write_u4(FP, Addr).

write_Half(FP, Half) -> write_u2(FP, Half).

write_Off({?ELFCLASS64, _} = FP, Off) -> write_u8(FP, Off);
write_Off(FP, Off) -> write_u4(FP, Off).

write_Sxword({?ELFCLASS64, _} = FP, Sxword) -> write_u8(FP, Sxword band ?UINT64_MAX);
write_Sxword({?ELFCLASS36, _} = FP, Sxword) -> write_u4(FP, Sxword band ?UINT36_MAX);
write_Sxword(FP, Sxword) -> write_u4(FP, Sxword band ?UINT32_MAX).

write_Uchar(FP, Uchar) -> write_u1(FP, Uchar).

write_Word(FP, Word) -> write_u4(FP, Word).

write_Xword({?ELFCLASS64, _} = FP, Xword) -> write_u8(FP, Xword);
write_Xword(FP, Xword) -> write_u4(FP, Xword).

write_u1(FP, Uchar) ->
  fputc(Uchar, FP).

write_u2({?ELFCLASS36, _} = FP, Half) ->
  fputs(extint:uint18_to_ext(Half), FP);
write_u2(FP, Half) ->
  fputs(extint:uint16_to_ext(Half), FP).

write_u4({?ELFCLASS36, _} = FP, Word) ->
  fputs(extint:uint36_to_ext(Word), FP);
write_u4(FP, Word) ->
  fputs(extint:uint32_to_ext(Word), FP).

write_u8(FP, Word) ->
  fputs(extint:uint64_to_ext(Word), FP).

%% I/O dispatchers =============================================================

fgetc({?ELFCLASS36, IoDev}) -> stdio9:fgetc(IoDev);
fgetc({_, IoDev}) -> stdio8:fgetc(IoDev).

fputc(Byte, {?ELFCLASS36, IoDev}) -> stdio9:fputc(Byte, IoDev);
fputc(Byte, {_, IoDev}) -> stdio8:fputc(Byte, IoDev).

fputs(Bytes, {?ELFCLASS36, IoDev}) -> stdio9:fputs(Bytes, IoDev);
fputs(Bytes, {_, IoDev}) -> stdio8:fputs(Bytes, IoDev).

fread(NrBytes, {?ELFCLASS36, IoDev}) -> stdio9:fread(NrBytes, IoDev);
fread(NrBytes, {_, IoDev}) -> stdio8:fread(NrBytes, IoDev).

fseek({?ELFCLASS36, IoDev}, Position) -> stdio9:fseek(IoDev, Position);
fseek({_, IoDev}, Position) -> stdio8:fseek(IoDev, Position).

ftell({?ELFCLASS36, IoDev}) -> stdio9:ftell(IoDev);
ftell({_, IoDev}) -> stdio8:ftell(IoDev).

%% Error Formatting ============================================================

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {wrong_ei_mag0, Mag0} ->
      io_lib:format("wrong ei_mag0 ~p", [Mag0]);
    {wrong_ei_mag1, Mag1} ->
      io_lib:format("wrong ei_mag1 ~p", [Mag1]);
    {wrong_ei_mag2, Mag2} ->
      io_lib:format("wrong ei_mag2 ~p", [Mag2]);
    {wrong_ei_mag3, Mag3} ->
      io_lib:format("wrong ei_mag3 ~p", [Mag3]);
    {wrong_ei_class, Class} ->
      io_lib:format("wrong ei_class ~p", [Class]);
    {wrong_ei_data, Data} ->
      io_lib:format("wrong ei_data ~p", [Data]);
    {wrong_ei_version, Version} ->
      io_lib:format("wrong ei_version ~p", [Version]);
    {wrong_ei_osabi, OSABI} ->
      io_lib:format("wrong ei_osabi ~p", [OSABI]);
    {wrong_ei_abiversion, ABIVersion} ->
      io_lib:format("wrong ei_abiversion ~p", [ABIVersion]);
    {wrong_ei_pad, Pad} ->
      io_lib:format("wrong ei_pad ~p", [Pad]);
    {wrong_e_type, Type} ->
      io_lib:format("wrong e_type ~p", [Type]);
    {wrong_e_machine, Machine} ->
      io_lib:format("wrong e_machine ~p", [Machine]);
    {wrong_e_version, Version} ->
      io_lib:format("wrong e_version ~p", [Version]);
    {wrong_e_ehsize, EhSize} ->
      io_lib:format("wrong e_ehsize ~p", [EhSize]);
    {wrong_e_phentsize, PhEntSize} ->
      io_lib:format("wrong e_phentsize ~p", [PhEntSize]);
    {wrong_e_shentsize, ShEntSize} ->
      io_lib:format("wrong e_shentsize ~p", [ShEntSize]);
    {eof_in_strtab, Index} ->
      io_lib:format("premature EOF in string table at index ~p", [Index]);
    {wrong_relatab_sh_entsize, ShEntSize} ->
      io_lib:format("wrong sh_entsize ~p in relocation table header", [ShEntSize]);
    {wrong_relatab_sh_size, ShSize} ->
      io_lib:format("wrong sh_size ~p in relocation table header", [ShSize]);
    {wrong_relatab_sh_type, ShType} ->
      io_lib:format("wrong sh_type ~p in relocation table header", [ShType]);
    {wrong_strtab_sh_type, ShType, Index} ->
      io_lib:format("wrong sh_type ~p for string table at index ~p",
                    [ShType, Index]);
    {wrong_strtab_index, Index} ->
      io_lib:format("out of range index ~p for string table", [Index]);
    {wrong_symtab_sh_entsize, ShEntSize} ->
      io_lib:format("wrong sh_entsize ~p in symtab section header", [ShEntSize]);
    {wrong_symtab_sh_size, ShSize} ->
      io_lib:format("wrong sh_size ~p in symtab section header", [ShSize]);
    eof ->
      "premature EOF";
    {wrong_index_in_strtab, Index} ->
      io_lib:format("out of range index ~p in string table", [Index]);
    strtab_not_nul_terminated ->
      "string table not NUL-terminated";
    {limit, What} ->
      io_lib:format("~s extends beyond limit of embedded file", [What]);
    _ ->
      io_lib:format("~p", [Reason])
  end.
