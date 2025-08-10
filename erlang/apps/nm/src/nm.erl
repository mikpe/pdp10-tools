%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'nm' for pdp10-elf
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

-module(nm).
-export([main/1]).

-include_lib("lib/include/pdp10_ar.hrl").
-include_lib("lib/include/archive.hrl").
-include_lib("lib/include/libelf.hrl").

-record(options,
        { print_file_name = false     % -A, -o, --print-file-name
        , dynamic = false             % -D, --dynamic
        , format = $b                 % -B, -P, --portability, -f [bsd|sysv|posix], --format=[bsd|sysv|posix]
        , extern_only = false         % -g, --extern-only
        , numeric_sort = false        % -n, -v, --numeric-sort
        , no_sort = false             % -p, --no-sort
        , print_armap = false         % -s, --print-armap
        , print_size = false          % -S, --print-size
        , reverse_sort = false        % -r, --reverse-sort
        , radix = $x                  % -t [dox], --radix=[dox]
        , undefined_only = false      % -u, --undefined-only
        , defined_only = false        % --defined-only
        , print_file = false          % internal, synthesized
        }).

%% Command-line interface ======================================================

-spec main([string()]) -> no_return().
main(Argv) ->
  %% NYI options:
  %% -a / --debug-syms
  %% --demangle
  %% --plugin <name>
  %% -l / --line-numbers
  %% --size-sort
  %% --target=<bfdname>
  %% -X 32_64
  %% --help [TODO?]
  %% @file [TODO?]
  case my_getopt:parse(Argv, "AoBDf:gnvpPsSrt:uV",
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
                       , {"print-armap", no, $s}
                       , {"print-size", no, $S}
                       , {"reverse-sort", no, $r}
                       , {"radix", required, $t}
                       , {"undefined-only", no, $u}
                       , {"version", no, $V}
                       ]) of
    {ok, {Options, Files}} ->
      nm(scan_options(Options), Files);
    {error, Reason} ->
      escript_runtime:errmsg("~s\n", [error:format(Reason)]),
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
scan_option($s, Opts) -> % -s, --print-armap
  Opts#options{print_armap = true};
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

nm(Opts0, Files0) ->
  {Files, PrintFile} =
    case Files0 of
      [] -> {["a.out"], false};
      [_] -> {Files0, false};
      [_,_|_] -> {Files0, true}
    end,
  Opts = Opts0#options{print_file = PrintFile},
  [nm1(Opts, File) || File <- Files],
  halt(0).

nm1(Opts, File) ->
  case archive:read(File) of
    {ok, {FP, Archive}} ->
      try
        nm_archive(Opts, FP, Archive)
      after
        pdp10_stdio:fclose(FP)
      end;
    {error, _} ->
      case pdp10_stdio:fopen(File, [read]) of
        {ok, FP} ->
          try
            nm1(Opts, File, FP, _Base = 0, _Limit = false)
          after
            pdp10_stdio:fclose(FP)
          end;
        {error, Reason} ->
          escript_runtime:errmsg("failed to open ~s: ~p\n", [File, error:format(Reason)])
      end
  end.

nm_archive(Opts, FP, Archive = #archive{members = Members}) ->
  case Opts#options.print_armap of
    true -> archive:print_armap(Archive);
    false -> ok
  end,
  NewOpts = Opts#options{print_file = true},
  lists:foreach(
    fun(#member{arhdr = #arhdr{ar_name = Name, ar_size = Size}, location = HdrOffset}) ->
      SrcOffset = HdrOffset + ?PDP10_ARHDR_SIZEOF,
      %% Like GNU binutils' nm, this does not recognize members that are archives.
      nm1(NewOpts, Name, FP, SrcOffset, SrcOffset + Size)
    end, Members).

nm1(Opts, File, FP, Base, Limit) ->
  case read_elf_symtab(FP, Base, Limit) of
    {ok, {ShTab, SymTab}} ->
      print_symtab(Opts, File, ShTab, SymTab);
    {error, Reason} ->
      escript_runtime:errmsg("invalid PDP10 ELF36 file ~s: ~s\n", [File, error:format(Reason)])
  end.

%% read ELF symtab =============================================================

read_elf_symtab(FP, Base, Limit) ->
  case libelf:read_Ehdr(FP, Base, Limit) of
    {ok, Ehdr} ->
      case libelf:read_ShTab(FP, Base, Limit, Ehdr) of
        {ok, ShTab} ->
          case libelf:read_SymTab(FP, Base, Limit, ShTab) of
            {ok, {SymTab, _ShNdx}} -> {ok, {ShTab, SymTab}};
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

%% output ELF symtab ===========================================================

print_symtab(Opts, File, ShTab, SymTab) ->
  case Opts#options.print_file of
    true -> io:format("\n~s:\n", [File]);
    false -> ok
  end,
  [sym_print(Opts, File, ShTab, Sym)
   || Sym <- syms_sort(Opts, syms_assemble(ShTab, SymTab))],
  ok.

sym_print(Opts, File, ShTab, {Sym = #elf_Sym{st_name = Name}, Value}) ->
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
  #elf_Sym{st_info = Info, st_shndx = ShNdx} = Sym,
  Type = ?ELF_ST_TYPE(Info),
  Bind = ?ELF_ST_BIND(Info),
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
      #elf_Shdr{sh_type = ShType, sh_flags = ShFlags} = lists:nth(ShNdx + 1, ShTab),
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

sym_cmp_value({_, Val1}, {_, Val2}) -> Val1 =< Val2.

sym_cmp_name({Sym1, _}, {Sym2, _}) ->
  Sym1#elf_Sym.st_name =< Sym2#elf_Sym.st_name.

syms_assemble(ShTab, SymTab) ->
  lists:map(fun(Sym) ->
                    {Sym, sym_value(ShTab, Sym)}
            end, SymTab).

sym_value(ShTab, #elf_Sym{st_shndx = ShNdx, st_value = Value}) ->
  ShNum = length(ShTab),
  Base =
    case ShNdx > 0 andalso ShNdx < ShNum andalso ShNdx =/= ?SHN_ABS andalso ShNdx =/= ?SHN_COMMON of
      true ->
        #elf_Shdr{ sh_type = Type
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
