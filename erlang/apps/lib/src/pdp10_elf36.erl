%%% -*- erlang-indent-level: 2 -*-
%%%
%%% I/O of PDP10 Elf36 entities
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

-module(pdp10_elf36).

-export([ read_Ehdr/1
        , read_ShTab/2
        , read_SymTab/2
        , format_error/1
        ]).

-include_lib("lib/include/pdp10_elf36.hrl").
-include_lib("lib/include/pdp10_stdint.hrl").

%% I/O of records ==============================================================

-type read_field() :: fun((pdp10_stdio:file())
                      -> {ok, integer()} | {error, {module(), term()}}).

-record(record_desc,
        { tag :: atom()
        , fields :: [read_field()]
        }).

read_record(FP, #record_desc{tag = Tag, fields = Fields}) ->
  read_record(FP, Fields, [Tag]).

read_record(FP, [ReadField | Fields], Values) ->
  case ReadField(FP) of
    {ok, Value} -> read_record(FP, Fields, [Value | Values]);
    {error, _Reason} = Error -> Error
  end;
read_record(_FP, [], Values) ->
  {ok, list_to_tuple(lists:reverse(Values))}.

%% I/O of #elf36_Ehdr{} ========================================================

-spec read_Ehdr(pdp10_stdio:file())
      -> {ok, #elf36_Ehdr{}} | {error, {module(), term()}}.
read_Ehdr(FP) ->
  case read_record(FP, elf36_Ehdr_desc()) of
    {ok, Ehdr} = Result ->
      case check_Ehdr(Ehdr) of
        ok -> Result;
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

elf36_Ehdr_desc() ->
  #record_desc{ tag = elf36_Ehdr
              , fields =
                  [  fun read_e_ident/1         % e_ident
                   , fun read_Half/1            % e_type
                   , fun read_Half/1            % e_machine
                   , fun read_Word/1            % e_version
                   , fun read_Addr/1            % e_entry
                   , fun read_Off/1             % e_phoff
                   , fun read_Off/1             % e_shoff
                   , fun read_Word/1            % e_flags
                   , fun read_Half/1            % e_ehsize
                   , fun read_Half/1            % e_phentsize
                   , fun read_Half/1            % e_phnum
                   , fun read_Half/1            % e_shentsize
                   , fun read_Half/1            % e_shnum
                   , fun read_Half/1            % e_shstrndx
                  ]
               }.

read_e_ident(FP) ->
  read(FP, ?EI_NIDENT, fun(Bytes) -> Bytes end).

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
        , fun check_Ehdr_e_type/1
        , fun check_Ehdr_e_machine/1
        , fun check_Ehdr_e_version/1
        , fun check_Ehdr_e_ehsize/1
        , fun check_Ehdr_e_shentsize/1
        ]).

check(_X, []) -> ok;
check(X, [Check | Checks]) ->
  case Check(X) of
    ok -> check(X, Checks);
    {error, _Reason} = Error -> Error
  end.

check_Ehdr_ei_mag0(Ehdr) ->
  #elf36_Ehdr{e_ident = Ident} = Ehdr,
  Mag0 = lists:nth(?EI_MAG0 + 1, Ident),
  case Mag0 of
    ?ELFMAG0 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_mag0, Mag0}}}
  end.

check_Ehdr_ei_mag1(Ehdr) ->
  #elf36_Ehdr{e_ident = Ident} = Ehdr,
  Mag1 = lists:nth(?EI_MAG1 + 1, Ident),
  case Mag1 of
    ?ELFMAG1 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_mag1, Mag1}}}
  end.

check_Ehdr_ei_mag2(Ehdr) ->
  #elf36_Ehdr{e_ident = Ident} = Ehdr,
  Mag2 = lists:nth(?EI_MAG2 + 1, Ident),
  case Mag2 of
    ?ELFMAG2 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_mag2, Mag2}}}
  end.

check_Ehdr_ei_mag3(Ehdr) ->
  #elf36_Ehdr{e_ident = Ident} = Ehdr,
  Mag3 = lists:nth(?EI_MAG3 + 1, Ident),
  case Mag3 of
    ?ELFMAG3 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_mag3, Mag3}}}
  end.

check_Ehdr_ei_class(Ehdr) ->
  #elf36_Ehdr{e_ident = Ident} = Ehdr,
  Class = lists:nth(?EI_CLASS + 1, Ident),
  case Class of
    ?ELFCLASS36 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_class, Class}}}
  end.

check_Ehdr_ei_data(Ehdr) ->
  #elf36_Ehdr{e_ident = Ident} = Ehdr,
  Data = lists:nth(?EI_DATA + 1, Ident),
  case Data of
    ?ELFDATA2MSB -> ok;
    _ -> {error, {?MODULE, {wrong_ei_data, Data}}}
  end.

check_Ehdr_ei_version(Ehdr) ->
  #elf36_Ehdr{e_ident = Ident} = Ehdr,
  Version = lists:nth(?EI_VERSION + 1, Ident),
  case Version of
    ?EV_CURRENT -> ok;
    _ -> {error, {?MODULE, {wrong_ei_version, Version}}}
  end.

check_Ehdr_ei_osabi(Ehdr) ->
  #elf36_Ehdr{e_ident = Ident} = Ehdr,
  OSABI = lists:nth(?EI_OSABI + 1, Ident),
  case OSABI of
    ?ELFOSABI_NONE -> ok;
    ?ELFOSABI_LINUX -> ok;
    _ -> {error, {?MODULE, {wrong_ei_osabi, OSABI}}}
  end.

check_Ehdr_ei_abiversion(Ehdr) ->
  #elf36_Ehdr{e_ident = Ident} = Ehdr,
  ABIVersion = lists:nth(?EI_ABIVERSION + 1, Ident),
  case ABIVersion of
    0 -> ok;
    _ -> {error, {?MODULE, {wrong_ei_abiversion, ABIVersion}}}
  end.

check_Ehdr_e_type(Ehdr) ->
  #elf36_Ehdr{e_type = Type} = Ehdr,
  case Type of
    ?ET_REL -> ok;
    ?ET_EXEC -> ok;
    ?ET_DYN -> ok;
    ?ET_CORE -> ok;
    _ -> {error, {?MODULE, {wrong_e_type, Type}}}
  end.

check_Ehdr_e_machine(Ehdr) ->
  #elf36_Ehdr{e_machine = Machine} = Ehdr,
  case Machine of
    ?EM_PDP10 -> ok;
    _ -> {error, {?MODULE, {wrong_e_machine, Machine}}}
  end.

check_Ehdr_e_version(Ehdr) ->
  #elf36_Ehdr{e_version = Version} = Ehdr,
  case Version of
    ?EV_CURRENT -> ok;
    _ -> {error, {?MODULE, {wrong_e_version, Version}}}
  end.

check_Ehdr_e_ehsize(Ehdr) ->
  #elf36_Ehdr{e_ehsize = EhSize} = Ehdr,
  case EhSize of
    ?ELF36_EHDR_SIZEOF -> ok;
    _ -> {error, {?MODULE, {wrong_e_ehsize, EhSize}}}
  end.

check_Ehdr_e_shentsize(Ehdr) ->
  #elf36_Ehdr{e_shoff = ShOff, e_shentsize = ShEntSize} = Ehdr,
  case {ShOff, ShEntSize} of
    {0, _} -> ok;
    {_, ?ELF36_SHDR_SIZEOF} -> ok;
    _ -> {error, {?MODULE, {wrong_e_shentsize, ShEntSize}}}
  end.

%% I/O of ShTab ================================================================

-spec read_ShTab(pdp10_stdio:file(), #elf36_Ehdr{})
      -> {ok, {[#elf36_Shdr{}], term()}} | {error, {module(), term()}}.
read_ShTab(FP, Ehdr) ->
  #elf36_Ehdr{ e_shoff = ShOff
             , e_shnum = ShNum0
             , e_shstrndx = ShStrNdx } = Ehdr,
  case ShOff of
    0 -> {ok, {[], []}};
    _ ->
      case pdp10_stdio:fseek(FP, {bof, ShOff}) of
        ok ->
          case read_Shdr(FP) of
            {ok, Shdr0} ->
              ShNum = actual_ShNum(ShNum0, Shdr0),
              case read_ShTab(FP, ShNum - 1, [Shdr0]) of
                {ok, ShTab} ->
                  case read_ShStrTab(FP, ShTab, ShStrNdx, Shdr0) of
                    {ok, ShStrTab} ->
                      %% TODO; now change the sh_names in ShTab to be strings
                      {ok, {ShTab, ShStrTab}};
                    {error, _Reason} = Error -> Error
                  end;
                {error, _Reason} = Error -> Error
              end;
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end
  end.

actual_ShNum(0, #elf36_Shdr{sh_size = ShNum} = _Shdr0) -> ShNum;
actual_ShNum(ShNum, _Shdr0) -> ShNum.

read_ShTab(_FP, 0, Shdrs) -> {ok, lists:reverse(Shdrs)};
read_ShTab(FP, ShNum, Shdrs) ->
  case read_Shdr(FP) of
    {ok, Shdr} -> read_ShTab(FP, ShNum - 1, [Shdr | Shdrs]);
    {error, _Reason} = Error -> Error
  end.

%% I/O of ShStrTab =============================================================

read_ShStrTab(FP, ShTab, ShStrNdx, Shdr0) ->
  case ShStrNdx of
    ?SHN_UNDEF -> {ok, []};
    ?SHN_XINDEX -> read_StrTab(FP, ShTab, Shdr0#elf36_Shdr.sh_link);
    _ -> read_StrTab(FP, ShTab, ShStrNdx)
  end.

%% I/O of StrTab ===============================================================

read_StrTab(FP, ShTab, Index) ->
  ShNum = length(ShTab),
  case Index > 0 andalso Index < ShNum of
    true ->
      #elf36_Shdr{ sh_type = Type
                 , sh_size = Size
                 , sh_offset = Offset
                 } = lists:nth(Index + 1, ShTab),
      case Type of
        ?SHT_STRTAB ->
          case pdp10_stdio:fseek(FP, {bof, Offset}) of
            ok ->
              case pdp10_stdio:fread(1, Size, FP) of
                eof -> {error, {?MODULE, {eof_in_strtab, Index}}};
                Other -> Other % {ok, _StrTab} or {error, _Reason}
              end;
            {error, _Reason} = Error -> Error
          end;
        _ -> {error, {?MODULE, {wrong_strtab_sh_type, Type, Index}}}
      end;
    false -> {error, {?MODULE, {wrong_strtab_index, Index}}}
  end.

%% I/O of SymTab ===============================================================

read_SymTab(FP, ShTab) ->
  case find_SymTab(ShTab) of
    false -> {ok, {[], []}};
    {ok, Shdr} ->
      #elf36_Shdr{ sh_link = ShLink
                 , sh_entsize = ShEntSize
                 , sh_size = ShSize
                 , sh_offset = ShOffset
                 } = Shdr,
      if ShEntSize =/= ?ELF36_SYM_SIZEOF ->
           {error, {?MODULE, {wrong_symtab_sh_entsize, ShEntSize}}};
         (ShSize rem ?ELF36_SYM_SIZEOF) =/= 0 ->
           {error, {?MODULE, {wrong_symtab_sh_size, ShSize}}};
         true ->
           case read_StrTab(FP, ShTab, ShLink) of
             {ok, StrTab} ->
               SymNum = ShSize div ?ELF36_SYM_SIZEOF,
               case SymNum of
                 0 -> {ok, {[], StrTab}};
                 _ ->
                   case pdp10_stdio:fseek(FP, {bof, ShOffset}) of
                     ok ->
                       case read_SymTab(FP, SymNum, []) of
                         {ok, SymTab} -> {ok, {SymTab, StrTab}};
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

find_SymTab([]) -> false;
find_SymTab([#elf36_Shdr{sh_type = ?SHT_SYMTAB} = Shdr | _]) -> {ok, Shdr};
find_SymTab([_Shdr | ShTab]) -> find_SymTab(ShTab).

%% I/O of #elf36_Shdr{} ========================================================

read_Shdr(FP) -> read_record(FP, elf36_Shdr_desc()).

elf36_Shdr_desc() ->
  #record_desc{ tag = elf36_Shdr
              , fields =
                  [  fun read_Word/1            % sh_name
                   , fun read_Word/1            % sh_type
                   , fun read_Word/1            % sh_flags
                   , fun read_Addr/1            % sh_addr
                   , fun read_Off/1             % sh_offset
                   , fun read_Word/1            % sh_size
                   , fun read_Word/1            % sh_link
                   , fun read_Word/1            % sh_info
                   , fun read_Word/1            % sh_addralign
                   , fun read_Word/1            % sh_entsize
                  ]
               }.

%% I/O of #elf36_Sym{} =========================================================

read_Sym(FP) -> read_record(FP, elf36_Sym_desc()).

elf36_Sym_desc() ->
  #record_desc{ tag = elf36_Sym
              , fields =
                  [  fun read_Word/1            % st_name
                   , fun read_Addr/1            % st_value
                   , fun read_Word/1            % st_size
                   , fun read_Uchar/1           % st_info
                   , fun read_Uchar/1           % st_other
                   , fun read_Half/1            % st_shndx
                  ]
               }.

%% I/O of scalar items =========================================================

read_Addr(FP)  -> read_uint36(FP).
read_Half(FP)  -> read_uint18(FP).
read_Off(FP)   -> read_uint36(FP).
read_Uchar(FP) -> read_uint9(FP).
read_Word(FP)  -> read_uint36(FP).

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
    {wrong_e_type, Type} ->
      io_lib:format("wrong e_type ~p", [Type]);
    {wrong_e_machine, Machine} ->
      io_lib:format("wrong e_machine ~p", [Machine]);
    {wrong_e_version, Version} ->
      io_lib:format("wrong e_version ~p", [Version]);
    {wrong_e_ehsize, EhSize} ->
      io_lib:format("wrong e_ehsize ~p", [EhSize]);
    {wrong_e_shentsize, ShEntSize} ->
      io_lib:format("wrong e_shentsize ~p", [ShEntSize]);
    {eof_in_strtab, Index} ->
      io_lib:format("premature EOF in string table at index ~p", [Index]);
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
    _ ->
      io_lib:format("~p", [Reason])
  end.
