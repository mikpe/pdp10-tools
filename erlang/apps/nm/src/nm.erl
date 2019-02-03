%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'nm' clone for PDP10 Elf36 files
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

-module(nm).
-export([main/1]).

-include_lib("lib/include/pdp10_elf36.hrl").

-record(options, {
            print_file_name = false     % -A, -o, --print-file-name
          , dynamic = false             % -D, --dynamic
          , format = $b                 % -B, -P, --portability, -f [bsd|sysv|posix], --format=[bsd|sysv|posix]
          , extern_only = false         % -g, --extern-only
          , numeric_sort = false        % -n, -v, --numeric-sort
          , no_sort = false             % -p, --no-sort
          , print_size = false          % -S, --print-size
          , reverse_sort = false        % -r, --reverse-sort
          , radix = $x                  % -t [dox], --radix=[dox]
          , undefined_only = false      % -u, --undefined-only
          , defined_only = false        % --defined-only
         }).

%% Command-line interface ======================================================

main(Argv) ->
  escript_runtime:start(fun main_/1, Argv).

main_(Argv) ->
  %% NYI options:
  %% -a / --debug-syms
  %% --demangle
  %% --plugin <name>
  %% -l / --line-numbers
  %% --size-sort
  %% -s / --print-armap [TODO]
  %% --target=<bfdname>
  %% -X 32_64
  %% --help [TODO?]
  %% @file [TODO?]
  case getopt:parse(Argv, "AoBDf:gnvpPSrt:uV",
                    [ %% long-only options
                      {"no-demangle", no, no_demangle}
                    , {"special-syms", no, special_syms}
                    , {"defined-only", no, defined_only}
                      %% long aliases for short options
                    , {"print-file-name", no, $A}
                    , {"dynamic", no, $D}
                    , {"format", required, $f}
                    , {"extern-only", no, $g}
                    , {"numeric-sort", no, $n}
                    , {"no-sort", no, $p}
                    , {"portability", no, $P}
                    , {"print-size", no, $S}
                    , {"reverse-sort", no, $r}
                    , {"radix", required, $t}
                    , {"undefined-only", no, $u}
                    , {"version", no, $V}
                    ]) of
    {ok, {Options, Files}} ->
      nm(scan_options(Options), Files);
    {error, ErrMsg} ->
      escript_runtime:errmsg("~s\n", [ErrMsg]),
      usage()
  end.

usage() ->
  escript_runtime:fmterr(
    "Usage: ~s <options> <files..>\n",
    [escript_runtime:progname()]),
  halt(1).

scan_options(Options) ->
  Opts = #options{}, % defaults from the record declaration
  lists:foldl(fun scan_option/2, Opts, Options).

scan_option(no_demangle, Opts) -> % --no-demangle, NYI
  Opts;
scan_option(special_syms, Opts) -> % --special-syms, NYI
  Opts;
scan_option(defined_only, Opts) ->
  Opts#options{defined_only = true};
scan_option($A, Opts) -> % -A, --print-file-name
  Opts#options{print_file_name = true};
scan_option($o, Opts) -> % -o
  Opts#options{print_file_name = true};
scan_option($B, Opts) -> % -B
  Opts#options{format = $b};
scan_option($D, Opts) -> % -D, --dynamic
  Opts#options{dynamic = true};
scan_option({$f, Arg}, Opts) -> % -f <format>, --format=<format>
  Opts#options{format = parse_format(Arg)};
scan_option($g, Opts) -> % -g, --extern-only
  Opts#options{extern_only = true};
scan_option($n, Opts) -> % -n, --numeric-sort
  Opts#options{numeric_sort = true};
scan_option($v, Opts) -> % -v
  Opts#options{numeric_sort = true};
scan_option($p, Opts) -> % -p, --no-sort
  Opts#options{no_sort = true};
scan_option($P, Opts) -> % -P, --portability
  Opts#options{format = $p};
scan_option($S, Opts) -> % -S, --print-size
  Opts#options{print_size = true};
scan_option($r, Opts) -> % -r, --reverse-sort
  Opts#options{reverse_sort = true};
scan_option({$t, Arg}, Opts) -> % -t <radix>, --radix=<radix>
  Opts#options{radix = parse_radix(Arg)};
scan_option($u, Opts) -> % -u, --undefined-only
  Opts#options{undefined_only = true};
scan_option($V, _Opts) -> % -V, --version
  io:format(standard_io, "pdp10-tools nm version 0.1\n", []),
  halt(0).

parse_format(String) ->
  case String of
    [C | _] when C =:= $b; C =:= $B -> $b;
    [C | _] when C =:= $s; C =:= $S -> $s;
    [C | _] when C =:= $p; C =:= $P -> $p;
    _ -> escript_runtime:fatal("invalid format '~s'\n", [String])
  end.

parse_radix(String) ->
  case String of
    [C] when C =:= $d; C =:= $o; C =:= $x -> C;
    _ -> escript_runtime:fatal("invalid radix '~s'\n", [String])
  end.

%% Nm ==========================================================================

nm(Opts, Files0) ->
  {Files, PrintFile} =
    case Files0 of
      [] -> {["a.out"], false};
      [_] -> {Files0, false};
      [_,_|_] -> {Files0, true}
    end,
  [nm1(Opts, PrintFile, File) || File <- Files],
  halt(0).

nm1(Opts, PrintFile, File) ->
  case pdp10_stdio:fopen(File, [read]) of
    {ok, FP} ->
      nm1(Opts, PrintFile, File, FP),
      pdp10_stdio:fclose(FP);
    {error, Reason} ->
      escript_runtime:fatal("failed to open ~s: ~p\n", [File, Reason])
  end.

nm1(Opts, PrintFile, File, FP) ->
  Ehdr = read_ehdr(File, FP),
  {ShTab, _ShStrTab} = read_shtab(File, FP, Ehdr),
  {SymTab, StrTab} = read_symtab(File, FP, ShTab),
  print_symtab(Opts, PrintFile, File, ShTab, SymTab, StrTab).

read_ehdr(File, FP) ->
  case pdp10_elf36_read_ehdr(FP) of
    {ok, Ehdr} ->
      Ehdr;
    {error, Reason} ->
      escript_runtime:fatal("invalid PDP10 ELF36 file ~s: ~p\n",
                            [File, Reason])
  end.

read_shtab(File, FP, Ehdr) ->
  case read_shtab(FP, Ehdr) of
    {ok, ShTab, ShStrTab} -> {ShTab, ShStrTab};
    {error, Reason} ->
      escript_runtime:fatal("failed to read section header table in ~s: ~p\n",
                            [File, Reason])
  end.

read_shtab(FP, Ehdr) ->
  #elf36_Ehdr{ e_shoff = ShOff
             , e_shnum = ShNum0
             , e_shstrndx = ShStrNdx } = Ehdr,
  case ShOff of
    0 -> {ok, {[], []}};
    _ ->
      case pdp10_stdio:fseek(FP, {bof, ShOff}) of
        ok ->
          case pdp10_elf36_read_shdr(FP) of
            {ok, Shdr0} ->
              ShNum = actual_shnum(ShNum0, Shdr0),
              case do_read_shtab(FP, ShNum - 1, [Shdr0]) of
                {ok, ShTab} ->
                  case read_shstrtab(FP, ShTab, ShStrNdx, Shdr0) of
                    {ok, ShStrTab} -> {ok, ShTab, ShStrTab};
                    {error, _Reason} = Error -> Error
                  end;
                {error, _Reason} = Error -> Error
              end;
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end
  end.

actual_shnum(0, #elf36_Shdr{sh_size = ShNum} = _Shdr0) -> ShNum;
actual_shnum(ShNum, _Shdr0) -> ShNum.

do_read_shtab(_FP, 0, Shdrs) -> {ok, lists:reverse(Shdrs)};
do_read_shtab(FP, ShNum, Shdrs) when ShNum > 0 ->
  case pdp10_elf36_read_shdr(FP) of
    {ok, Shdr} -> do_read_shtab(FP, ShNum - 1, [Shdr | Shdrs]);
    {error, _Reason} = Error -> Error
  end.

read_shstrtab(FP, ShTab, ShStrNdx0, Shdr0) ->
  case ShStrNdx0 of
    ?SHN_UNDEF -> {ok, []};
    _ ->
      ShStrNdx =
        case ShStrNdx0 of
          ?SHN_XINDEX -> Shdr0#elf36_Shdr.sh_link;
          _ -> ShStrNdx0
        end,
      read_strtab(FP, ShTab, ShStrNdx)
  end.

read_strtab(FP, ShTab, Index) ->
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
                {ok, _StrTab} = Result -> Result;
                {error, _Reason} = Error -> Error;
                eof -> {error, eof}
              end;
            {error, _Reason} = Error -> Error
          end;
        _ ->
          {error, io_lib:format("invalid type ~p for string table at index ~p",
                                [Type, Index])}
      end;
    false ->
      {error, io_lib:format("invalid string table section index ~p",
                            [Index])}
  end.

read_symtab(File, FP, ShTab) ->
  case read_symtab(FP, ShTab) of
    {ok, SymTab, StrTab} -> {SymTab, StrTab};
    {error, Reason} ->
      escript_runtime:fatal("failed to read symbol table table in ~s: ~p\n",
                            [File, Reason])
  end.

read_symtab(FP, ShTab) ->
  case find_symtab(ShTab) of
    false -> {ok, [], []};
    {ok, Shdr} ->
      #elf36_Shdr{ sh_link = Link
                 , sh_entsize = EntSize
                 , sh_size = Size
                 , sh_offset = Offset
                 } = Shdr,
      case [] of
        _ when EntSize =/= ?ELF36_SYM_SIZEOF ->
          {error, io_lib:format("wrong sh_entsize ~p in symtab section header", [EntSize])};
        _ when (Size rem ?ELF36_SYM_SIZEOF) =/= 0 ->
          {error, io_lib:format("wrong sh_size ~p in symtab section header", [Size])};
        _ ->
          case read_strtab(FP, ShTab, Link) of
            {ok, StrTab} ->
              SymNum = Size div ?ELF36_SYM_SIZEOF,
              case SymNum of
                0 -> {ok, [], StrTab};
                _ ->
                  case pdp10_stdio:fseek(FP, {bof, Offset}) of
                    ok ->
                      case do_read_symtab(FP, SymNum, []) of
                        {ok, SymTab} -> {ok, SymTab, StrTab};
                        {error, _Reason} = Error -> Error
                      end;
                    {error, _Reason} = Error -> Error
                  end
              end;
            {error, _Reason} = Error -> Error
          end
      end
  end.

do_read_symtab(_FP, _SymNum = 0, Syms) -> {ok, lists:reverse(Syms)};
do_read_symtab(FP, SymNum, Syms) when SymNum > 0 ->
  case pdp10_elf36_read_sym(FP) of
    {ok, Sym} -> do_read_symtab(FP, SymNum - 1, [Sym | Syms]);
    {error, _Reason} = Error -> Error
  end.

find_symtab([]) -> false;
find_symtab([#elf36_Shdr{sh_type = ?SHT_SYMTAB} = Shdr | _]) -> {ok, Shdr};
find_symtab([_Shdr | ShTab]) -> find_symtab(ShTab).

print_symtab(Opts, PrintFile, File, ShTab, SymTab, StrTab) ->
  case PrintFile of
    true -> io:format("\n~s:\n", [File]);
    false -> ok
  end,
  [sym_print(Opts, File, ShTab, Sym)
   || Sym <- syms_sort(Opts, syms_assemble(ShTab, SymTab, StrTab))],
  ok.

sym_print(Opts, File, ShTab, {Sym, Value, Name}) ->
  case sym_type_letter(ShTab, Sym) of
    0 -> ok; % ignored
    Type ->
      %% TODO: handle --extern-only, --undefined-only, --defined-only
      %% TODO: handle --format={bsd,sysv,posix}
      if Opts#options.print_file_name -> io:format("~s:", [File]);
         true -> ok
      end,
      case Opts#options.radix of
        $x -> io:format("~*.*.*B", [9, 16, $0, Value]);
        $d -> io:format("~*.*.*B", [11, 10, $0, Value]);
        $o -> io:format("~*.*.*B", [12, 8, $0, Value])
      end,
      io:format(" ~c ", [Type]),
      io:format("~s\n", [case Name of [] -> "(empty)"; _ -> Name end])
  end.

sym_type_letter(ShTab, Sym) ->
  #elf36_Sym{st_info = Info, st_shndx = ShNdx} = Sym,
  Type = ?ELF36_ST_TYPE(Info),
  Bind = ?ELF36_ST_BIND(Info),
  case ShNdx of
    ?SHN_ABS ->
      if Type =:= ?STT_NOTYPE; Type =:= ?STT_OBJECT; Type =:= ?STT_FUNC ->
              case Bind of
                ?STB_GLOBAL -> $A;
                ?STB_LOCAL -> $a;
                _ -> 0
              end;
         true -> 0
      end;
    ?SHN_UNDEF ->
      case Bind of
        ?STB_GLOBAL -> $U;
        _ -> 0
      end;
    ?SHN_COMMON ->
      if Type =:= ?STT_NOTYPE; Type =:= ?STT_OBJECT ->
              case Bind of
                ?STB_GLOBAL -> $C;
                ?STB_LOCAL -> $c;
                _ -> 0
              end;
         true -> 0
      end;
    _ when Type =:= ?STT_GNU_IFUNC -> $i;
    _ when ShNdx >= length(ShTab) -> 0;
    _ ->
      #elf36_Shdr{sh_type = ShType, sh_flags = ShFlags} = lists:nth(ShNdx + 1, ShTab),
      case ShType of
        ?SHT_NOBITS ->
          if (ShFlags band (?SHF_ALLOC bor ?SHF_WRITE)) =:= (?SHF_ALLOC bor ?SHF_WRITE) ->
                  case Bind of
                    ?STB_GLOBAL -> $B;
                    ?STB_LOCAL -> $b;
                    _ -> 0
                  end;
             true -> 0
          end;
        ?SHT_PROGBITS ->
          if (ShFlags band (?SHF_ALLOC bor ?SHF_EXECINSTR)) =:= (?SHF_ALLOC bor ?SHF_EXECINSTR) ->
                  case Bind of
                    ?STB_GLOBAL -> $T;
                    ?STB_LOCAL -> $t;
                    _ -> 0
                  end;
             (ShFlags band (?SHF_ALLOC bor ?SHF_WRITE)) =:= (?SHF_ALLOC bor ?SHF_WRITE) ->
                  case Bind of
                    ?STB_GLOBAL -> $D;
                    ?STB_LOCAL -> $d;
                    _ -> 0
                  end;
             (ShFlags band ?SHF_ALLOC) =/= 0 ->
                  case Bind of
                    ?STB_GLOBAL -> $R;
                    ?STB_LOCAL -> $r;
                    _ -> 0
                  end;
             true -> 0
          end;
        _ -> 0
      end
  end.

syms_sort(Opts, Syms) ->
  case syms_cmpfn(Opts) of
    [] -> Syms;
    Compare -> lists:sort(Compare, Syms)
  end.

syms_cmpfn(Opts) ->
  case Opts#options.no_sort of
    true -> [];
    false ->
      Compare =
        case Opts#options.numeric_sort of
          true -> fun sym_cmp_value/2;
          false -> fun sym_cmp_name/2
        end,
      case Opts#options.reverse_sort of
        true -> fun(Sym1, Sym2) -> Compare(Sym2, Sym1) end;
        false -> Compare
      end
  end.

sym_cmp_value({_, Val1, _}, {_, Val2, _}) -> Val1 =< Val2.

sym_cmp_name({_, _, Name1}, {_, _, Name2}) -> Name1 =< Name2.

syms_assemble(ShTab, SymTab, StrTab) ->
  lists:map(fun(Sym) ->
                    {Sym, sym_value(ShTab, Sym), sym_name(StrTab, Sym)}
            end, SymTab).

sym_name(StrTab, #elf36_Sym{st_name = Index}) ->
  {ok, Name} = get_name(StrTab, Index),
  Name.

get_name(StrTab, Index) ->
  try lists:nthtail(Index, StrTab) of
    [_|_] = Tail -> get_name2(Tail, [])
  catch _C:_R ->
    {error, {bad_strtab_index, Index}}
  end.

get_name2([], _Acc) -> {error, strtab_not_nul_terminated};
get_name2([0 | _Tail], Acc) -> {ok, lists:reverse(Acc)};
get_name2([Ch | Tail], Acc) -> get_name2(Tail, [Ch | Acc]).

sym_value(ShTab, #elf36_Sym{st_shndx = ShNdx, st_value = Value}) ->
  ShNum = length(ShTab),
  Base =
    case ShNdx > 0 andalso ShNdx < ShNum andalso ShNdx =/= ?SHN_ABS andalso ShNdx =/= ?SHN_COMMON of
      true ->
        #elf36_Shdr{ sh_type = Type
                   , sh_flags = Flags
                   , sh_addr = Addr
                   } = lists:nth(ShNdx + 1, ShTab),
        case Type =:= ?SHT_PROGBITS andalso (Flags band ?SHF_ALLOC) =/= 0 of
          true -> Addr;
          false -> 0
        end;
      false -> 0
    end,
  Base + Value.

%% ELF36 =======================================================================
%% (these bits should go into the pdp10_elf36 library module)

pdp10_elf36_read_ehdr(FP) ->
  case pdp10_elf36_read_record(FP, pdp10_elf36_ehdr_desc()) of
    {ok, Ehdr} ->
      case pdp10_elf36_check_ehdr(Ehdr) of
        ok -> {ok, Ehdr};
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

pdp10_elf36_check_ehdr(Ehdr) ->
  check(Ehdr,
        [ fun check_ehdr_ei_mag0/1
        , fun check_ehdr_ei_mag1/1
        , fun check_ehdr_ei_mag2/1
        , fun check_ehdr_ei_mag3/1
        , fun check_ehdr_ei_class/1
        , fun check_ehdr_ei_data/1
        , fun check_ehdr_ei_version/1
        , fun check_ehdr_ei_osabi/1
        , fun check_ehdr_ei_abiversion/1
        , fun check_ehdr_e_type/1
        , fun check_ehdr_e_machine/1
        , fun check_ehdr_e_version/1
        , fun check_ehdr_e_ehsize/1
        , fun check_ehdr_e_shentsize/1
        ]).

pdp10_elf36_read_shdr(FP) ->
  pdp10_elf36_read_record(FP, pdp10_elf36_shdr_desc()).

pdp10_elf36_read_sym(FP) ->
  pdp10_elf36_read_record(FP, pdp10_elf36_sym_desc()).

check(_X, []) -> ok;
check(X, [Fun | Funs]) ->
  case Fun(X) of
    ok -> check(X, Funs);
    {error, _Reason} = Error -> Error
  end.

check_ehdr_ei_mag0(Ehdr) ->
  #elf36_Ehdr{ e_ident = Ident } = Ehdr,
  Mag0 = lists:nth(?EI_MAG0 + 1, Ident),
  case Mag0 of
    ?ELFMAG0 -> ok;
    _ -> {error, io_lib:format("wrong ei_mag0 ~p", [Mag0])}
  end.

check_ehdr_ei_mag1(Ehdr) ->
  #elf36_Ehdr{ e_ident = Ident } = Ehdr,
  Mag1 = lists:nth(?EI_MAG1 + 1, Ident),
  case Mag1 of
    ?ELFMAG1 -> ok;
    _ -> {error, io_lib:format("wrong ei_mag1 ~p", [Mag1])}
  end.

check_ehdr_ei_mag2(Ehdr) ->
  #elf36_Ehdr{ e_ident = Ident } = Ehdr,
  Mag2 = lists:nth(?EI_MAG2 + 1, Ident),
  case Mag2 of
    ?ELFMAG2 -> ok;
    _ -> {error, io_lib:format("wrong ei_mag2 ~p", [Mag2])}
  end.

check_ehdr_ei_mag3(Ehdr) ->
  #elf36_Ehdr{ e_ident = Ident } = Ehdr,
  Mag3 = lists:nth(?EI_MAG3 + 1, Ident),
  case Mag3 of
    ?ELFMAG3 -> ok;
    _ -> {error, io_lib:format("wrong ei_mag3 ~p", [Mag3])}
  end.

check_ehdr_ei_class(Ehdr) ->
  #elf36_Ehdr{ e_ident = Ident } = Ehdr,
  Class = lists:nth(?EI_CLASS + 1, Ident),
  case Class of
    ?ELFCLASS36 -> ok;
    _ -> {error, io_lib:format("wrong ei_class ~p", [Class])}
  end.

check_ehdr_ei_data(Ehdr) ->
  #elf36_Ehdr{ e_ident = Ident } = Ehdr,
  Data = lists:nth(?EI_DATA + 1, Ident),
  case Data of
    ?ELFDATA2MSB -> ok;
    _ -> {error, io_lib:format("wrong ei_data ~p", [Data])}
  end.

check_ehdr_ei_version(Ehdr) ->
  #elf36_Ehdr{ e_ident = Ident } = Ehdr,
  Version = lists:nth(?EI_VERSION + 1, Ident),
  case Version of
    ?EV_CURRENT -> ok;
    _ -> {error, io_lib:format("wrong ei_version ~p", [Version])}
  end.

check_ehdr_ei_osabi(Ehdr) ->
  #elf36_Ehdr{ e_ident = Ident } = Ehdr,
  OSABI = lists:nth(?EI_OSABI + 1, Ident),
  case OSABI of
    ?ELFOSABI_NONE -> ok;
    ?ELFOSABI_LINUX -> ok;
    _ -> {error, io_lib:format("wrong ei_osabi ~p", [OSABI])}
  end.

check_ehdr_ei_abiversion(Ehdr) ->
  #elf36_Ehdr{ e_ident = Ident } = Ehdr,
  ABIVersion = lists:nth(?EI_ABIVERSION + 1, Ident),
  case ABIVersion of
    0 -> ok;
    _ -> {error, io_lib:format("wrong ei_abiversion ~p", [ABIVersion])}
  end.

check_ehdr_e_type(Ehdr) ->
  #elf36_Ehdr{ e_type = Type } = Ehdr,
  case Type of
    ?ET_REL -> ok;
    ?ET_EXEC -> ok;
    ?ET_DYN -> ok;
    ?ET_CORE -> ok;
    _ -> {error, io_lib:format("wrong e_type ~p", [Type])}
  end.

check_ehdr_e_machine(Ehdr) ->
  #elf36_Ehdr{ e_machine = Machine } = Ehdr,
  case Machine of
    ?EM_PDP10 -> ok;
    _ -> {error, io_lib:format("wrong e_machine ~p", [Machine])}
  end.

check_ehdr_e_version(Ehdr) ->
  #elf36_Ehdr{ e_version = Version } = Ehdr,
  case Version of
    ?EV_CURRENT -> ok;
    _ -> {error, io_lib:format("wrong e_version ~p", [Version])}
  end.

check_ehdr_e_ehsize(Ehdr) ->
  #elf36_Ehdr{ e_ehsize = EhSize } = Ehdr,
  case EhSize of
    ?ELF36_EHDR_SIZEOF -> ok;
    _ -> {error, io_lib:format("wrong e_ehsize ~p", [EhSize])}
  end.

check_ehdr_e_shentsize(Ehdr) ->
  #elf36_Ehdr{ e_shoff = ShOff
             , e_shentsize = ShEntSize } = Ehdr,
  case {ShOff, ShEntSize} of
    {0, _} -> ok;
    {_, ?ELF36_SHDR_SIZEOF} -> ok;
    _ -> {error, io_lib:format("wrong e_shentsize ~p", [ShEntSize])}
  end.

%% Input of records ============================================================

-type reader() :: fun((pdp10_stdio:file()) -> {ok, integer()} | {error, any()}).

-record(record_desc, {
            tag :: atom()
          , fields :: [reader()]
         }).

pdp10_elf36_read_record(FP, #record_desc{tag = Tag, fields = FieldReaders}) ->
  pdp10_elf36_read_record(FP, FieldReaders, [Tag]).

pdp10_elf36_read_record(_FP, [], Values) ->
  {ok, list_to_tuple(lists:reverse(Values))};
pdp10_elf36_read_record(FP, [FieldReader | FieldReaders], Values) ->
  case FieldReader(FP) of
    {ok, Value} ->
      pdp10_elf36_read_record(FP, FieldReaders, [Value | Values]);
    {error, _Reason} = Error ->
      Error
  end.

pdp10_elf36_ehdr_desc() ->
  #record_desc{ tag = elf36_Ehdr
              , fields =
                  [
                     fun read_e_ident/1         % e_ident
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
  pdp10_elf36_read(FP, ?EI_NIDENT, fun(L) -> L end).

pdp10_elf36_shdr_desc() ->
  #record_desc{ tag = elf36_Shdr
              , fields =
                  [
                     fun read_Word/1            % sh_name
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

pdp10_elf36_sym_desc() ->
  #record_desc{ tag = elf36_Sym
              , fields =
                  [
                     fun read_Word/1            % st_name
                   , fun read_Addr/1            % st_value
                   , fun read_Word/1            % st_size
                   , fun read_Uchar/1           % st_info
                   , fun read_Uchar/1           % st_other
                   , fun read_Half/1            % st_shndx
                  ]
               }.

%% Input of scalar items =======================================================

read_Uchar(FP) -> pdp10_elf36_read_uint9(FP).
read_Half(FP) -> pdp10_elf36_read_uint18(FP).
read_Word(FP) -> pdp10_elf36_read_uint36(FP).
read_Addr(FP) -> pdp10_elf36_read_uint36(FP).
read_Off(FP) -> pdp10_elf36_read_uint36(FP).

pdp10_elf36_read_uint9(FP) ->
  case pdp10_stdio:fgetc(FP) of
    {ok, _Nonet} = Res -> Res;
    {error, _Reason} = Res -> Res;
    eof -> {error, eof}
  end.

pdp10_elf36_read_uint18(FP) ->
  pdp10_elf36_read(FP, 2, fun make_uint18be/1).

pdp10_elf36_read_uint36(FP) ->
  pdp10_elf36_read(FP, 4, fun make_uint36be/1).

pdp10_elf36_read(FP, N, ConvFun) when N >= 0 ->
  pdp10_elf36_read(FP, N, ConvFun, []).

pdp10_elf36_read(_FP, 0, ConvFun, Acc) ->
  {ok, ConvFun(lists:reverse(Acc))};
pdp10_elf36_read(FP, N, ConvFun, Acc) ->
  case pdp10_elf36_read_uint9(FP) of
    {ok, Nonet} ->
      pdp10_elf36_read(FP, N - 1, ConvFun, [Nonet | Acc]);
    {error, _Reason} = Error ->
      Error
  end.

make_uint18be([B1, B2]) -> % big-endian conversion
  ((B1 band 16#1FF) bsl 9) bor (B2 band 16#1FF).

make_uint36be([B1, B2, B3, B4]) -> % big-endian conversion
  ((B1 band 16#1FF) bsl 27) bor
  ((B2 band 16#1FF) bsl 19) bor
  ((B3 band 16#1FF) bsl  9) bor
   (B4 band 16#1FF).
