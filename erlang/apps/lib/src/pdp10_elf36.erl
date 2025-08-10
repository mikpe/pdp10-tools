%%% -*- erlang-indent-level: 2 -*-
%%%
%%% I/O of PDP10 Elf36 entities
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

-module(pdp10_elf36).

-export([ read_Ehdr/1
        , read_Ehdr/3
        , read_PhTab/2
        , read_PhTab/4
        , read_RelaTab/2
        , read_RelaTab/4
        , read_ShTab/2
        , read_ShTab/4
        , read_SymTab/2
        , read_SymTab/4
        , read_uint36/1
        , make_Ehdr/0
        , write_Ehdr/2
        , write_Phdr/2
        , format_error/1
        ]).

-include_lib("lib/include/pdp10_elf36.hrl").
-include_lib("lib/include/pdp10_stdint.hrl").

-type read_field() :: fun((pdp10_stdio:file())
                      -> {ok, integer()} | {error, {module(), term()}}).
-type write_field() :: fun((pdp10_stdio:file(), integer())
                       -> ok | {error, term()}).

-record(record_desc,
        { tag :: atom()
        , fields :: [{read_field(), write_field()}]
        }).

%% I/O of #elf_Ehdr{} ==========================================================

-spec read_Ehdr(pdp10_stdio:file())
      -> {ok, #elf_Ehdr{}} | {error, {module(), term()}}.
read_Ehdr(FP) ->
  read_Ehdr(FP, _Base = 0, _Limit = false).

%% FIXME: take EI_CLASS as parameter
-spec read_Ehdr(pdp10_stdio:file(), non_neg_integer(), false | non_neg_integer())
      -> {ok, #elf_Ehdr{}} | {error, {module(), term()}}.
read_Ehdr(FP, Base, Limit) ->
  case pdp10_stdio:fseek(FP, {bof, Base}) of
    ok ->
      case read_record(FP, elf_Ehdr_desc()) of
        {ok, Ehdr} = Result ->
          case (Limit =:= false) orelse (pdp10_stdio:ftell(FP) =< Limit) of
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

%% FIXME: take EI_CLASS as parameter
-spec make_Ehdr() -> #elf_Ehdr{}.
make_Ehdr() ->
  Ident =
    tuple_to_list(
      erlang:make_tuple(
        ?EI_NIDENT, 0,
        [ {1 + ?EI_MAG0, ?ELFMAG0}
        , {1 + ?EI_MAG1, ?ELFMAG1}
        , {1 + ?EI_MAG2, ?ELFMAG2}
        , {1 + ?EI_MAG3, ?ELFMAG3}
        , {1 + ?EI_CLASS, ?ELFCLASS36} % TODO: target-specific
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
           , e_ehsize = ?ELF36_EHDR_SIZEOF
           , e_phentsize = ?ELF36_PHDR_SIZEOF
           , e_phnum = 0
           , e_shentsize = ?ELF36_SHDR_SIZEOF
           , e_shnum = 0
           , e_shstrndx = 0
           }.

%% FIXME: take EI_CLASS as parameter
-spec write_Ehdr(pdp10_stdio:file(), #elf_Ehdr{})
      -> ok | {error, {module(), term()}}.
write_Ehdr(FP, Ehdr) ->
  write_record(FP, Ehdr, elf_Ehdr_desc()).

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
    %% FIXME: extend
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
    %% FIXME: extend
    ?ELF36_EHDR_SIZEOF -> ok;
    _ -> {error, {?MODULE, {wrong_e_ehsize, EhSize}}}
  end.

check_Ehdr_e_phentsize(Ehdr) ->
  #elf_Ehdr{e_phoff = PhOff, e_phentsize = PhEntSize} = Ehdr,
  case {PhOff, PhEntSize} of
    {0, _} -> ok;
    %% FIXME: extend
    {_, ?ELF36_PHDR_SIZEOF} -> ok;
    _ -> {error, {?MODULE, {wrong_e_phentsize, PhEntSize}}}
  end.

check_Ehdr_e_shentsize(Ehdr) ->
  #elf_Ehdr{e_shoff = ShOff, e_shentsize = ShEntSize} = Ehdr,
  case {ShOff, ShEntSize} of
    {0, _} -> ok;
    %% FIXME: extend
    {_, ?ELF36_SHDR_SIZEOF} -> ok;
    _ -> {error, {?MODULE, {wrong_e_shentsize, ShEntSize}}}
  end.

%% I/O of PhTab ================================================================

-spec read_PhTab(pdp10_stdio:file(), #elf_Ehdr{})
      -> {ok, [#elf_Phdr{}]} | {error, {module(), term()}}.
read_PhTab(FP, Ehdr) ->
  read_PhTab(FP, _Base = 0, _Limit = false, Ehdr).

-spec read_PhTab(pdp10_stdio:file(), non_neg_integer(), false | non_neg_integer(), #elf_Ehdr{})
      -> {ok, [#elf_Phdr{}]} | {error, {module(), term()}}.
read_PhTab(FP, Base, Limit, Ehdr) ->
  #elf_Ehdr{ e_phoff = PhOff
           , e_phentsize = PhEntSize
           , e_phnum = PhNum } = Ehdr,
  if PhOff =:= 0; PhNum =:= 0 ->
       {ok, []};
     true ->
       true = PhEntSize =:= ?ELF36_PHDR_SIZEOF, % assert
       %% FIXME: if PhNum = ?PN_XNUM the real PhNum is stored in Shdr0.sh_info
       case pdp10_stdio:fseek(FP, {bof, Base + PhOff}) of
         ok ->
           case read_PhTab(FP, PhNum, []) of
             {ok, _PhTab} = Result ->
               case (Limit =:= false) orelse (pdp10_stdio:ftell(FP) =< Limit) of
                 true -> Result;
                 false -> {error, {?MODULE, {limit, "PhTab"}}}
               end;
             {error, _Reason} = Error -> Error
           end;
         {error, _Reason} = Error -> Error
       end
  end.

read_PhTab(_FP, 0, Phdrs) -> {ok, lists:reverse(Phdrs)};
read_PhTab(FP, PhNum, Phdrs) ->
  case read_Phdr(FP) of
    {ok, Phdr} -> read_PhTab(FP, PhNum - 1, [Phdr | Phdrs]);
    {error, _Reason} = Error -> Error
  end.

%% I/O of relocation tables ====================================================

-spec read_RelaTab(pdp10_stdio:file(), #elf_Shdr{})
      -> {ok, [#elf_Rela{}]} | {error, {module(), term()}}.
read_RelaTab(FP, Shdr) ->
  read_RelaTab(FP, _Base = 0, _Limit = false, Shdr).

-spec read_RelaTab(pdp10_stdio:file(), non_neg_integer(), false | non_neg_integer(), #elf_Shdr{})
      -> {ok, [#elf_Rela{}]} | {error, {module(), term()}}.
read_RelaTab(FP, Base, Limit, Shdr) ->
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
                  case pdp10_stdio:fseek(FP, {bof, Base + ShOffset}) of
                    ok ->
                      case read_RelaTab(FP, RelaNum, []) of
                        {ok, _RelaTab} = Result ->
                          case (Limit =:= false) orelse (pdp10_stdio:ftell(FP) =< Limit) of
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

read_RelaTab(_FP, _RelaNum = 0, Relas) -> {ok, lists:reverse(Relas)};
read_RelaTab(FP, RelaNum, Relas) when RelaNum > 0 ->
  case read_Rela(FP) of
    {ok, Rela} -> read_RelaTab(FP, RelaNum - 1, [Rela | Relas]);
    {error, _Reason} = Error -> Error
  end.

%% I/O of ShTab ================================================================

-spec read_ShTab(pdp10_stdio:file(), #elf_Ehdr{})
      -> {ok, [#elf_Shdr{}]} | {error, {module(), term()}}.
read_ShTab(FP, Ehdr) ->
  read_ShTab(FP, _Base = 0, _Limit = false, Ehdr).

-spec read_ShTab(pdp10_stdio:file(), non_neg_integer(), false | non_neg_integer(), #elf_Ehdr{})
      -> {ok, [#elf_Shdr{}]} | {error, {module(), term()}}.
read_ShTab(FP, Base, Limit, Ehdr) ->
  #elf_Ehdr{ e_shoff = ShOff
           , e_shnum = ShNum0
           , e_shstrndx = ShStrNdx } = Ehdr,
  case ShOff of
    0 -> {ok, []};
    _ ->
      case pdp10_stdio:fseek(FP, {bof, Base + ShOff}) of
        ok ->
          case read_Shdr(FP) of
            {ok, Shdr0} ->
              ShNum = actual_ShNum(ShNum0, Shdr0),
              case read_ShTab(FP, ShNum - 1, [Shdr0]) of
                {ok, ShTab} ->
                  case (Limit =:= false) orelse (pdp10_stdio:ftell(FP) =< Limit) of
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

read_ShTab(_FP, 0, Shdrs) -> {ok, lists:reverse(Shdrs)};
read_ShTab(FP, ShNum, Shdrs) ->
  case read_Shdr(FP) of
    {ok, Shdr} -> read_ShTab(FP, ShNum - 1, [Shdr | Shdrs]);
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
          case pdp10_stdio:fseek(FP, {bof, Base + Offset}) of
            ok ->
              case pdp10_stdio:fread(1, Size, FP) of
                {ok, _StrTab} = Result ->
                  case (Limit =:= false) orelse (pdp10_stdio:ftell(FP) =< Limit) of
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

-spec read_SymTab(pdp10_stdio:file(), [#elf_Shdr{}])
      -> {ok, {[#elf_Sym{}],non_neg_integer()}} | {error, {module(), term()}}.
read_SymTab(FP, ShTab) ->
  read_SymTab(FP, _Base = 0, _Limit = false, ShTab).

-spec read_SymTab(pdp10_stdio:file(), non_neg_integer(), false | non_neg_integer(), [#elf_Shdr{}])
      -> {ok, {[#elf_Sym{}],non_neg_integer()}} | {error, {module(), term()}}.
read_SymTab(FP, Base, Limit, ShTab) ->
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
                   case pdp10_stdio:fseek(FP, {bof, Base + ShOffset}) of
                     ok ->
                       case read_SymTab(FP, SymNum, []) of
                         {ok, SymTab} ->
                           case (Limit =:= false) orelse (pdp10_stdio:ftell(FP) =< Limit) of
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

read_SymTab(_FP, _SymNum = 0, Syms) -> {ok, lists:reverse(Syms)};
read_SymTab(FP, SymNum, Syms) when SymNum > 0 ->
  case read_Sym(FP) of
    {ok, Sym} -> read_SymTab(FP, SymNum - 1, [Sym | Syms]);
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

read_Phdr(FP) -> read_record(FP, elf_Phdr_desc()).

-spec write_Phdr(pdp10_stdio:file(), #elf_Phdr{})
      -> ok | {error, {module(), term()}}.
write_Phdr(FP, Phdr) ->
  write_record(FP, Phdr, elf_Phdr_desc()).

%% FIXME: take EI_CLASS as parameter
%% FIXME: ELF-64 stores the fields in a different order
elf_Phdr_desc() ->
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

%% I/O of #elf_Rela{} ==========================================================

read_Rela(FP) -> read_record(FP, elf_Rela_desc()).

%% FIXME: take EI_CLASS as parameter
elf_Rela_desc() ->
  #record_desc{ tag = elf_Rela
              , fields =
                  [ {fun read_Addr/1,  fun write_Addr/2}  % r_offset
                  , {fun read_Word/1,  fun write_Word/2}  % r_info
                  , {fun read_Sword/1, fun write_Sword/2} % r_addend
                  ]
              }.

%% I/O of #elf_Shdr{} ==========================================================

read_Shdr(FP) -> read_record(FP, elf_Shdr_desc()).

%% FIXME: take EI_CLASS as parameter
elf_Shdr_desc() ->
  #record_desc{ tag = elf_Shdr
              , fields =
                  [ {fun read_Word/1, fun write_Word/2} % sh_name
                  , {fun read_Word/1, fun write_Word/2} % sh_type
                  , {fun read_Word/1, fun write_Word/2} % sh_flags (FIXME: Xword)
                  , {fun read_Addr/1, fun write_Addr/2} % sh_addr
                  , {fun read_Off/1,  fun write_Off/2}  % sh_offset
                  , {fun read_Word/1, fun write_Word/2} % sh_size (FIXME: Xword)
                  , {fun read_Word/1, fun write_Word/2} % sh_link
                  , {fun read_Word/1, fun write_Word/2} % sh_info
                  , {fun read_Word/1, fun write_Word/2} % sh_addralign (FIXME: Xword)
                  , {fun read_Word/1, fun write_Word/2} % sh_entsize (FIXME: Xword)
                  ]
               }.

%% I/O of #elf_Sym{} ===========================================================

read_Sym(FP) -> read_record(FP, elf_Sym_desc()).

%% FIXME: take EI_CLASS as parameter
%% FIXME: ELF-64 orders the fields differently
elf_Sym_desc() ->
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

read_Addr(FP)  -> read_uint36(FP).
read_Half(FP)  -> read_uint18(FP).
read_Off(FP)   -> read_uint36(FP).
read_Sword(FP) -> read_sint36(FP).
read_Uchar(FP) -> read_uint9(FP).
read_Word(FP)  -> read_uint36(FP).

read_sint36(FP) ->
  case read_uint36(FP) of
    {ok, UInt36} -> {ok, (UInt36 bxor (1 bsl 35)) - (1 bsl 35)}; % sign-extend
    {error, _Reason} = Error -> Error
  end.

read_uint9(FP) ->
  case pdp10_stdio:fgetc(FP) of
    eof -> {error, {?MODULE, eof}};
    Other -> Other % {ok, _Nonet} or {error, _Reason}
  end.

read_uint18(FP) -> read(FP, 2, fun pdp10_extint:uint18_from_ext/1).

read_uint36(FP) -> read(FP, 4, fun pdp10_extint:uint36_from_ext/1).

read(FP, N, ConvFun) when N >= 0 -> read(FP, N, ConvFun, []).

read(_FP, 0, ConvFun, Acc) -> {ok, ConvFun(lists:reverse(Acc))};
read(FP, N, ConvFun, Acc) ->
  case read_uint9(FP) of
    {ok, Nonet} -> read(FP, N - 1, ConvFun, [Nonet | Acc]);
    {error, _Reason} = Error -> Error
  end.

write_Addr(FP, UInt36) -> write_uint36(FP, UInt36).
write_Half(FP, UInt18) -> write_uint18(FP, UInt18).
write_Off(FP,  UInt36) -> write_uint36(FP, UInt36).
write_Sword(FP, SInt36) -> write_uint36(FP, SInt36 band ?PDP10_UINT36_MAX).
write_Uchar(FP, UInt9) -> write_uint9(FP, UInt9).
write_Word(FP, UInt36) -> write_uint36(FP, UInt36).

write_uint9(FP, UInt9) ->
  pdp10_stdio:fputc(UInt9, FP).

write_uint18(FP, UInt18) ->
  fputs(pdp10_extint:uint18_to_ext(UInt18), FP).

write_uint36(FP, UInt36) ->
  fputs(pdp10_extint:uint36_to_ext(UInt36), FP).

fputs(Nonets, FP) ->
  pdp10_stdio:fputs(Nonets, FP).

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
