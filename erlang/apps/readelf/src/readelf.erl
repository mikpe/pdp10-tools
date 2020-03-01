%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'readelf' clone for pdp10-elf
%%% Copyright (C) 2013-2020  Mikael Pettersson
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

-module(readelf).
-export([main/1]).

-include_lib("lib/include/pdp10_elf36.hrl").
-include_lib("lib/include/pdp10_opcodes.hrl").

-record(options,
        { file_header = false
        , segments = false
        , sections = false
        , section_groups = false
        , section_details = false
        , symbols = false
        , dyn_syms = false
        , notes = false
        , relocs = false
        , unwind = false
        , dynamic = false
        , version_info = false
        , arch_specific = false
        , use_dynamic = false
        , archive_index = false
        , histogram = false
        , disassemble = false % extension
        }).

%% Command-line interface ======================================================

main(Argv) ->
  escript_runtime:start(fun main_/1, Argv).

main_(Argv) ->
  case getopt:parse(Argv, "ahlSgtesnrudVADcIvW",
                    [
                      %% long-only options
                      { "dyn-syms",             no, dyn_syms }
                    , { "disassemble",          no, disassemble } % extension
                      %% long aliases for short options
                    , { "all",                  no, $a }
                    , { "file-header",          no, $h }
                    , { "program-headers",      no, $l }
                    , { "segments",             no, $l }
                    , { "section-groups",       no, $g }
                    , { "section-details",      no, $t }
                    , { "headers",              no, $e }
                    , { "symbols",              no, $s }
                    , { "syms",                 no, $s }
                    , { "notes",                no, $n }
                    , { "relocs",               no, $r }
                    , { "unwind",               no, $u }
                    , { "version-info",         no, $V }
                    , { "arch-specific",        no, $A }
                    , { "use-dynamic",          no, $D }
                    , { "archive-index",        no, $c }
                    , { "histogram",            no, $I }
                    , { "version",              no, $v }
                    , { "wide",                 no, $W }
                      %% --{hex,string,relocated}-dump: NYI
                      %% --debug-dump: NYI
                      %% --dwarf-{depth,start}: NYI
                      %% --decompress: NYI
                      %% --help: NYI
                      %% @file: NYI
                    ]) of
    {ok, {Options, Files}} ->
      Opts = scan_options(Options),
      case Files of
        [] -> usage();
        _ ->
          case readelf(Opts, Files) of
            ok -> halt(0);
            {error, Reason} ->
              escript_runtime:fatal("~s\n", [error:format(Reason)])
          end
      end;
    {error, Reason} ->
      escript_runtime:errmsg("~s\n", [error:format(Reason)]),
      usage()
  end.

usage() ->
  escript_runtime:fmterr(
    "Usage: ~s -[ahlSgtesnrudVADcIvW] elffile..\n",
    [escript_runtime:progname()]),
  halt(1).

scan_options(Options) ->
  lists:foldl(fun scan_option/2, #options{}, Options).

scan_option(Option, Opts) ->
  case Option of
    $a ->
      Opts#options{ file_header = true
                  , segments = true
                  , sections = true
                  , symbols = true
                  , relocs = true
                  , dynamic = true
                  , notes = true
                  , version_info = true
                  , arch_specific = true
                  , unwind = true
                  , section_groups = true
                  , histogram = true };
    $h ->
      Opts#options{ file_header = true };
    $l ->
      Opts#options{ segments = true };
    $S ->
      Opts#options{ sections = true };
    $g ->
      Opts#options{ section_groups = true };
    $t ->
      Opts#options{ section_details = true
                  , sections = true };
    $s ->
      Opts#options{ symbols = true };
    dyn_syms ->
      Opts#options{ dyn_syms = true };
    $e ->
      Opts#options{ file_header = true
                  , segments = true
                  , sections = true };
    $n ->
      Opts#options{ notes = true };
    $r ->
      Opts#options{ relocs = true };
    $u ->
      Opts#options{ unwind = true };
    $d ->
      Opts#options{ dynamic = true };
    $V ->
      Opts#options{ version_info = true };
    $A ->
      Opts#options{ arch_specific = true };
    $D ->
      Opts#options{ use_dynamic = true };
    $c ->
      Opts#options{ archive_index = true };
    $I ->
      Opts#options{ histogram = true };
    $v ->
      version();
    $W ->
      Opts; % --wide is default in this readelf
    disassemble -> % extension
      Opts#options{ disassemble = true }
  end.

version() ->
  io:format(standard_io, "pdp10-tools readelf version 0.1\n", []),
  halt(0).

%% readelf =====================================================================

readelf(_Opts, []) -> ok;
readelf(Opts, [File | Files]) ->
  case readelf_file(Opts, File) of
    ok -> readelf(Opts, Files);
    {error, _Reason} = Error -> Error
  end.

readelf_file(Opts, File) ->
  case pdp10_stdio:fopen(File, [raw, read]) of
    {ok, FP} ->
      try readelf_ehdr(Opts, FP)
      after pdp10_stdio:fclose(FP) end;
    {error, _Reason} = Error -> Error
  end.

readelf_ehdr(Opts, FP) ->
  case pdp10_elf36:read_Ehdr(FP) of
    {ok, Ehdr} ->
      case print_ehdr(Opts, Ehdr) of
        ok -> readelf_shtab(Opts, FP, Ehdr);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

readelf_shtab(Opts, FP, Ehdr) ->
  case pdp10_elf36:read_ShTab(FP, Ehdr) of
    {ok, ShTab} ->
      case print_shtab(Opts, ShTab) of
        ok -> readelf_symtab(Opts, FP, ShTab);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

readelf_symtab(Opts, FP, ShTab) ->
  case pdp10_elf36:read_SymTab(FP, ShTab) of
    {ok, {SymTab, ShNdx}} ->
      case print_relatab(Opts, FP, SymTab, ShNdx, ShTab) of
        ok ->
          case print_symtab(Opts, SymTab, ShNdx, ShTab) of
            ok -> disassemble(Opts, FP, ShTab, SymTab);
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

%% print_ehdr ==================================================================

print_ehdr(#options{file_header = false}, _Ehdr) -> ok;
print_ehdr(_Opts, Ehdr = #elf36_Ehdr{}) ->
  EIdent = Ehdr#elf36_Ehdr.e_ident,
  io:format("ELF Header:\n"),
  print_e_ident(EIdent),
  print_ei_class(lists:nth(1 + ?EI_CLASS, EIdent)),
  print_ei_data(lists:nth(1 + ?EI_DATA, EIdent)),
  print_ei_version(lists:nth(1 + ?EI_VERSION, EIdent)),
  print_ei_osabi(lists:nth(1 + ?EI_OSABI, EIdent)),
  print_ei_abiversion(lists:nth(1 + ?EI_ABIVERSION, EIdent)),
  print_e_type(Ehdr#elf36_Ehdr.e_type),
  print_e_machine(Ehdr#elf36_Ehdr.e_machine),
  print_e_version(Ehdr#elf36_Ehdr.e_version),
  print_e_entry(Ehdr#elf36_Ehdr.e_entry),
  print_e_phoff(Ehdr#elf36_Ehdr.e_phoff),
  print_e_shoff(Ehdr#elf36_Ehdr.e_shoff),
  print_e_flags(Ehdr#elf36_Ehdr.e_flags),
  print_e_ehsize(Ehdr#elf36_Ehdr.e_ehsize),
  print_e_phentsize(Ehdr#elf36_Ehdr.e_phentsize),
  print_e_phnum(Ehdr#elf36_Ehdr.e_phnum),
  print_e_shentsize(Ehdr#elf36_Ehdr.e_shentsize),
  print_e_shnum(Ehdr#elf36_Ehdr.e_shnum),
  print_e_shstrndx(Ehdr#elf36_Ehdr.e_shstrndx),
  io:format("\n").

print_e_ident(EIdent) ->
  io:format("  Magic:\t\t\t\t"),
  print_e_ident("", EIdent),
  io:format("\n").

print_e_ident(_Pfx, []) -> ok;
print_e_ident(Pfx, [Byte | Bytes]) ->
  io:format("~s", [Pfx]),
  %% bytes in [256,512[ are unlikely, but handle them correctly anyway
  if Byte > 255 -> io:format("~3.16.0b", [Byte]);
     true -> io:format("~2.16.0b", [Byte])
  end,
  print_e_ident(" ", Bytes).

print_ei_class(EIClass) ->
  io:format("  Class:\t\t\t\t~.10b (~s)\n",
            [EIClass,
             case EIClass of
               ?ELFCLASS32 -> "ELF32";
               ?ELFCLASS64 -> "ELF64";
               ?ELFCLASS36 -> "ELF36";
               _ -> "?"
             end]).

print_ei_data(EIData) ->
  io:format("  Data:\t\t\t\t\t~.10b (~s)\n",
            [EIData,
             case EIData of
               ?ELFDATA2LSB -> "2's complement, little endian";
               ?ELFDATA2MSB -> "2's complement, big endian";
               _ -> "?"
             end]).

print_ei_version(EIVersion) ->
  io:format("  Version:\t\t\t\t~.10b (~s)\n",
            [EIVersion, version_string(EIVersion)]).

print_ei_osabi(EIOSABI) ->
  io:format("  OS/ABI:\t\t\t\t~.10b (~s)\n",
            [EIOSABI,
             case EIOSABI of
               ?ELFOSABI_NONE -> "Generic";
               ?ELFOSABI_LINUX -> "Linux";
               _ -> "?"
             end]).

print_ei_abiversion(EIABIVersion) ->
  io:format("  ABI Version:\t\t\t\t~.10b\n", [EIABIVersion]).

print_e_type(EType) ->
  io:format("  Type:\t\t\t\t\t~.10b (~s)\n",
            [EType,
             case EType of
               ?ET_REL -> "Relocatable file";
               ?ET_EXEC -> "Executable file";
               ?ET_DYN -> "Shared object file";
               ?ET_CORE -> "Core file";
               _ -> "?"
             end]).

print_e_machine(EMachine) ->
  io:format("  Machine:\t\t\t\t~.10b (~s)\n",
            [EMachine,
             case EMachine of
               ?EM_PDP10 -> "Digital Equipment Corp. PDP-10";
               _ -> "?"
             end]).

print_e_version(EVersion) ->
  io:format("  Version:\t\t\t\t~.10b (~s)\n",
            [EVersion, version_string(EVersion)]).

version_string(?EV_CURRENT) -> "current";
version_string(_) -> "?".

print_e_entry(EEntry) ->
  io:format("  Entry point address:\t\t\t0x~.16b\n", [EEntry]).

print_e_phoff(EPhOff) ->
  io:format("  Start of program headers:\t\t~.10b\n", [EPhOff]).

print_e_shoff(EShOff) ->
  io:format("  Start of section headers:\t\t~.10b\n", [EShOff]).

print_e_flags(EFLags) ->
  io:format("  Flags:\t\t\t\t0x~.16b\n", [EFLags]).

print_e_ehsize(EEhSize) ->
  io:format("  Size of this header:\t\t\t~.10b\n", [EEhSize]).

print_e_phentsize(EPhEntSize) ->
  io:format("  Size of program headers:\t\t~.10b\n", [EPhEntSize]).

print_e_phnum(EPhNum) ->
  io:format("  Number of program headers:\t\t~.10b\n", [EPhNum]).

print_e_shentsize(EShEntSize) ->
  io:format("  Size of section headers:\t\t~.10b\n", [EShEntSize]).

print_e_shnum(EShNum) ->
  io:format("  Number of section headers:\t\t~.10b\n", [EShNum]).

print_e_shstrndx(EShStrNdx) ->
  io:format("  Section header string table index:\t~.10b\n", [EShStrNdx]).

%% print_shtab =================================================================

print_shtab(#options{sections = false}, _ShTab) -> ok;
print_shtab(_Opts, ShTab) ->
  io:format("Section Headers:\n"),
  io:format("  [Nr] Name Type Addr Off Size ES Flg Lk Inf Al\n"),
  print_shdrs(ShTab, 0).

print_shdrs([Shdr | Shdrs], I) ->
  io:format("  [~.10b] ~s ~s 0x~.16b ~.10b ~.10b ~.10b ~s ~.10b ~.10b ~.10b\n",
            [ I
            , sh_name(Shdr)
            , sh_type(Shdr)
            , Shdr#elf36_Shdr.sh_addr
            , Shdr#elf36_Shdr.sh_offset
            , Shdr#elf36_Shdr.sh_size
            , Shdr#elf36_Shdr.sh_entsize
            , sh_flags(Shdr)
            , Shdr#elf36_Shdr.sh_link
            , Shdr#elf36_Shdr.sh_info
            , Shdr#elf36_Shdr.sh_addralign
            ]),
  print_shdrs(Shdrs, I + 1);
print_shdrs([], _I) ->
  io:format("Key to Flags:\n"),
  io:format("  W (write), A (alloc), X (execute), M (merge), S (strings), Z (compressed)\n"),
  io:format("  I (info), L (link order), G (group), T (TLS), E (exclude), x (unknown)\n"),
  io:format("  O (extra OS processing required), o (OS specific), p (processor specific)\n"),
  io:format("\n").

sh_name(#elf36_Shdr{sh_name = ShName}) ->
  case ShName of
    "" -> "(empty)";
    _ -> ShName
  end.

sh_type(#elf36_Shdr{sh_type = ShType}) ->
  case ShType of
    ?SHT_NULL -> "NULL";
    ?SHT_PROGBITS -> "PROGBITS";
    ?SHT_SYMTAB -> "SYMTAB";
    ?SHT_STRTAB -> "STRTAB";
    ?SHT_RELA -> "RELA";
    ?SHT_HASH -> "HASH";
    ?SHT_DYNAMIC -> "DYNAMIC";
    ?SHT_NOTE -> "NOTE";
    ?SHT_NOBITS -> "NOBITS";
    ?SHT_REL -> "REL";
    ?SHT_SHLIB -> "SHLIB";
    ?SHT_DYNSYM -> "DYNSYM";
    ?SHT_INIT_ARRAY -> "INIT_ARRAY";
    ?SHT_FINI_ARRAY -> "FINI_ARRAY";
    ?SHT_PREINIT_ARRAY -> "PREINIT_ARRAY";
    ?SHT_GROUP -> "GROUP";
    ?SHT_SYMTAB_SHNDX -> "SYMTAB_SHNDX";
    ?SHT_GNU_INCREMENTAL_INPUTS -> "GNU_INCREMENTAL_INPUTS";
    ?SHT_GNU_ATTRIBUTES -> "GNU_ATTRIBUTES";
    ?SHT_GNU_HASH -> "GNU_HASH";
    ?SHT_GNU_LIBLIST -> "GNU_LIBLIST";
    ?SHT_GNU_verdef -> "GNU_verdef";
    ?SHT_GNU_verneed -> "GNU_verneed";
    ?SHT_GNU_versym -> "GNU_versym";
    _ -> io_lib:format("~.10b", [ShType])
  end.

sh_flags(#elf36_Shdr{sh_flags = ShFlags}) ->
  sh_flags("WAXxMSILOGTZ", 0, ShFlags, 0, []).

sh_flags([C | Flags], I, ShFlags, Mask, Acc) ->
  {NewMask, NewAcc} =
    case I =/= 3 andalso (ShFlags band (1 bsl I)) =/= 0 of
      true -> {Mask bor (1 bsl I), [C | Acc]};
      false -> {Mask, Acc}
    end,
  sh_flags(Flags, I + 1, ShFlags, NewMask, NewAcc);
sh_flags([], _I, ShFlags, Mask, Acc) ->
  {NewMask, NewAcc} =
    case ShFlags band ?SHF_EXCLUDE =/= 0 of
      true -> {Mask bor ?SHF_EXCLUDE, [$E | Acc]};
      false -> {Mask, Acc}
    end,
  case ShFlags of
    0 -> "-";
    NewMask -> lists:reverse(NewAcc);
    _ -> io_lib:format("0x~.16b", [ShFlags])
  end.

%% print_relatab ===============================================================

print_relatab(#options{relocs = false}, _FP, _SymTab, _ShNdx, _ShTab) -> ok;
print_relatab(_Opts, _FP, _SymTab, _SymTabShNdx = ?SHN_UNDEF, _ShTab) -> ok;
print_relatab(_Opts, FP, SymTab, SymTabShNdx, ShTab) ->
  print_relatab2(ShTab, FP, SymTab, SymTabShNdx, _Any = false).

print_relatab2([Shdr | ShTab], FP, SymTab, SymTabNdx, Any) ->
  NewAny =
    case Shdr of
      #elf36_Shdr{sh_type = ?SHT_RELA, sh_link = ShLink} ->
        case ShLink of
          SymTabNdx ->
            print_relatab(Shdr, FP, SymTab);
          _ ->
            escript_runtime:errmsg("Relocation section '~s' has bogus sh_link ~p\n",
                                   [sh_name(Shdr), ShLink])
         end,
         true;
      #elf36_Shdr{} -> Any
    end,
  print_relatab2(ShTab, FP, SymTab, SymTabNdx, NewAny);
print_relatab2([], _FP, _SymTab, _SymTabNdx, _Any = false) ->
  io:format("There are no relocation sections in this file.\n\n");
print_relatab2([], _FP, _SymTab, _SymTabNdx, _Any = true) ->
  ok.

print_relatab(Shdr, FP, SymTab) ->
  case pdp10_elf36:read_RelaTab(FP, Shdr) of
    {ok, Relas} ->
      NrRelas = length(Relas),
      io:format("Relocation section '~s' at offset 0x~.16b contains ~.10b entr~s:\n",
                [sh_name(Shdr), Shdr#elf36_Shdr.sh_offset, NrRelas,
                 case NrRelas of 1 -> "y"; _ -> "ies" end]),
      io:format("  Offset  Info      Type         Symbol's Value Symbol's Name + Addend\n"),
      lists:foreach(fun(Rela) -> print_rela(Rela, SymTab) end, Relas),
      io:format("\n");
    {error, _Reason} = Error -> Error
  end.

print_rela(Rela, SymTab) ->
  #elf36_Rela{r_offset = Offset, r_info = Info, r_addend = Addend} = Rela,
  SymNdx = ?ELF36_R_SYM(Info),
  Type = r_type(Rela),
  case SymNdx of
    ?SHN_UNDEF -> print_rela(Offset, Info, Type, _Value = 0, _Name = "", Addend);
    _ ->
      try lists:nth(SymNdx + 1, SymTab) of
        Sym = #elf36_Sym{st_value = Value} ->
          print_rela(Offset, Info, Type, Value, st_name(Sym), Addend)
      catch _:_ ->
        escript_runtime:errmsg("Relocation refers to bogus symbol index ~p\n", [SymNdx])
      end
  end.

print_rela(Offset, Info, Type, Value, Name, Addend) ->
  io:format("~*.*.*b ~*.*.*b ~*s ~*.*.*b ~s + ~.10b\n",
            [ 9, 16, $0, Offset
            , 9, 16, $0, Info
            , -17, Type
            , 9, 16, $0, Value
            , Name
            , Addend
            ]).

r_type(#elf36_Rela{r_info = Info}) ->
  case ?ELF36_R_TYPE(Info) of
    ?R_PDP10_NONE -> "R_PDP10_NONE";
    ?R_PDP10_IFIW -> "R_PDP10_IFIW";
    ?R_PDP10_EFIW -> "R_PDP10_EFIW";
    ?R_PDP10_LOCAL_W -> "R_PDP10_LOCAL_W";
    ?R_PDP10_LOCAL_B -> "R_PDP10_LOCAL_B";
    ?R_PDP10_LOCAL_H -> "R_PDP10_LOCAL_H";
    ?R_PDP10_GLOBAL_B -> "R_PDP10_GLOBAL_B";
    ?R_PDP10_GLOBAL_H -> "R_PDP10_GLOBAL_H";
    ?R_PDP10_LITERAL_W -> "R_PDP10_LITERAL_W";
    ?R_PDP10_LITERAL_H -> "R_PDP10_LITERAL_H";
    ?R_PDP10_LITERAL_B -> "R_PDP10_LITERAL_B";
    Type -> io_lib:format("~.10b", [Type])
  end.

%% print_symtab ================================================================

print_symtab(#options{symbols = false}, _SymTab, _ShNdx, _ShTab) -> ok;
print_symtab(_Opts, _SymTab, ?SHN_UNDEF, _ShTab) -> ok;
print_symtab(_Opts, SymTab, ShNdx, ShTab) ->
  Shdr = lists:nth(1 + ShNdx, ShTab),
  io:format("Symbol table '~s' in section ~.10b contains ~.10b entries:\n",
            [sh_name(Shdr), ShNdx, length(SymTab)]),
  io:format("  Num Value Size Type Bind Vis Ndx Name\n"),
  print_syms(SymTab, 0).

print_syms([Sym | Syms], I) ->
  io:format("  ~.10b 0x~.16b ~.10b ~s ~s ~s ~.10b ~s\n",
            [ I
            , Sym#elf36_Sym.st_value
            , Sym#elf36_Sym.st_size
            , st_type(Sym)
            , st_bind(Sym)
            , st_vis(Sym)
            , Sym#elf36_Sym.st_shndx
            , st_name(Sym)
            ]),
  print_syms(Syms, I + 1);
print_syms([], _I) ->
  io:format("\n").

st_type(#elf36_Sym{st_info = StInfo}) ->
  case ?ELF36_ST_TYPE(StInfo) of
    ?STT_NOTYPE -> "NOTYPE";
    ?STT_OBJECT -> "OBJECT";
    ?STT_FUNC -> "FUNC";
    ?STT_SECTION -> "SECTION";
    ?STT_FILE -> "FILE";
    ?STT_COMMON -> "COMMON";
    ?STT_TLS -> "TLS";
    ?STT_RELC -> "RELC";
    ?STT_SRELC -> "SRELC";
    ?STT_GNU_IFUNC -> "GNU_IFUNC";
    StType -> io_lib:format("~.10b", [StType])
  end.

st_bind(#elf36_Sym{st_info = StInfo}) ->
  case ?ELF36_ST_BIND(StInfo) of
    ?STB_LOCAL -> "LOCAL";
    ?STB_GLOBAL -> "GLOBAL";
    ?STB_WEAK -> "WEAK";
    ?STB_GNU_UNIQUE -> "GNU_UNIQUE";
    StBind -> io_lib:format("~.10b", [StBind])
  end.

st_vis(#elf36_Sym{st_other = StOther}) ->
  case ?ELF36_ST_VISIBILITY(StOther) of
    ?STV_DEFAULT -> "DEFAULT";
    ?STV_INTERNAL -> "INTERNAL";
    ?STV_HIDDEN -> "HIDDEN";
    ?STV_PROTECTED -> "PROTECTED";
    StVis -> io_lib:format("~.10b", [StVis])
  end.

st_name(#elf36_Sym{st_name = StName}) ->
  case StName of
    "" -> "(empty)";
    _ -> StName
  end.

%% disassemble =================================================================

disassemble(#options{disassemble = false}, _FP, _ShTab, _SymTab) -> ok;
disassemble(_Opts, FP, ShTab, SymTab) ->
  disassemble_sections(ShTab, 0, FP, SymTab).

disassemble_sections([], _ShNdx, _FP, _SymTab) -> ok;
disassemble_sections([Shdr | ShTab], ShNdx, FP, SymTab) ->
  case disassemble_section(Shdr, ShNdx, FP, SymTab) of
    ok -> disassemble_sections(ShTab, ShNdx + 1, FP, SymTab);
    {error, _Reason} = Error -> Error
  end.

disassemble_section(Shdr, ShNdx, FP, SymTab) ->
  case Shdr of
    #elf36_Shdr{ sh_type = ?SHT_PROGBITS
               , sh_flags = ?SHF_ALLOC bor ?SHF_EXECINSTR
               , sh_offset = ShOffset
               , sh_size = ShSize
               } ->
      io:format("Disassembly of section nr ~.10b ~s:\n\n",
                [ShNdx, sh_name(Shdr)]),
      case pdp10_stdio:fseek(FP, {bof, ShOffset}) of
        ok -> disassemble_insns(0, ShSize, FP, labels(SymTab, ShNdx));
        {error, _Reason} = Error -> Error
      end;
    #elf36_Shdr{} ->
      ok
  end.

disassemble_insns(Offset, Size, FP, Labels) when Offset < Size ->
  case pdp10_elf36:read_uint36(FP) of
    {ok, InsnWord} ->
      RestLabels = print_labels(Labels, Offset),
      io:format(" 0x~.16b:\t0~12.8.0b\t", [Offset, InsnWord]),
      disassemble_insn(InsnWord),
      disassemble_insns(Offset + 4, Size, FP, RestLabels);
    {error, _Reason} = Error -> Error
  end;
disassemble_insns(_ShOffset, _ShSize, _FP, _Labels) -> ok.

disassemble_insn(InsnWord) ->
  Models = ?PDP10_KL10_271up,
  High13 = InsnWord bsr (36 - 13),
  Extended = false,
  Section0 = false,
  case pdp10_opcodes:insn_from_high13(Models, High13, Extended, Section0) of
    #pdp10_insn_desc{name = Name, format = Format} ->
      io:format("~s ", [Name]),
      case Format of
        ?PDP10_INSN_A_OPCODE -> ok;
        _ -> io:format("~.10b,", [(InsnWord bsr 23) band 16#F])
      end,
      case InsnWord band (1 bsl 22) of
        0 -> ok;
        _ -> io:format("@")
      end,
      io:format("~.10b", [InsnWord band ((1 bsl 18) - 1)]),
      case (InsnWord bsr 18) band 16#F of
        0 -> ok;
        IxReg -> io:format("(~.10b)", [IxReg])
      end,
      io:format("\n");
    false ->
      io:format("(bad)\n")
  end.

print_labels([], _Offset) -> [];
print_labels(Labels = [Label | RestLabels], Offset) ->
  #elf36_Sym{st_value = StValue} = Label,
  if StValue < Offset -> print_labels(RestLabels, Offset);
     StValue =:= Offset ->
       io:format("0x~9.16.0b <~s>:\n", [Offset, st_name(Label)]),
       print_labels(RestLabels, Offset);
     true -> Labels
  end.

labels(SymTab, TextShNdx) ->
  lists:sort(fun sym_cmp_by_value/2,
             lists:filter(fun(Sym) -> sym_is_label(Sym, TextShNdx) end,
                          SymTab)).

sym_is_label(Sym, TextShNdx) ->
  #elf36_Sym{st_shndx = StShNdx, st_info = StInfo} = Sym,
  StShNdx =:= TextShNdx andalso
  ?ELF36_ST_TYPE(StInfo) =:= ?STT_FUNC andalso % TODO: type of a label?
  (case ?ELF36_ST_BIND(StInfo) of
     ?STB_GLOBAL -> true;
     ?STB_LOCAL -> true;
     ?STB_WEAK -> true;
     _ -> false
   end).

sym_cmp_by_value(Sym1, Sym2) ->
  Sym1#elf36_Sym.st_value =< Sym2#elf36_Sym.st_value.
