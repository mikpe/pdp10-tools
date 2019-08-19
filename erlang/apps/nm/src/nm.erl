%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'nm' clone for pdp10-elf
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
  case pdp10_elf36:read_Ehdr(FP) of
    {ok, Ehdr} ->
      Ehdr;
    {error, Reason} ->
      escript_runtime:fatal("invalid PDP10 ELF36 file ~s: ~s\n",
                            [File, error:format(Reason)])
  end.

read_shtab(File, FP, Ehdr) ->
  case pdp10_elf36:read_ShTab(FP, Ehdr) of
    {ok, {ShTab, ShStrTab}} -> {ShTab, ShStrTab};
    {error, Reason} ->
      escript_runtime:fatal("failed to read section header table in ~s: ~s\n",
                            [File, error:format(Reason)])
  end.

read_symtab(File, FP, ShTab) ->
  case pdp10_elf36:read_SymTab(FP, ShTab) of
    {ok, {SymTab, StrTab}} -> {SymTab, StrTab};
    {error, Reason} ->
      escript_runtime:fatal("failed to read symbol table table in ~s: ~s\n",
                            [File, error:format(Reason)])
  end.

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
