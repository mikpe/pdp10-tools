%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'readelf' for pdp10-elf
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

-module(readelf).
-export([main/1]).

-include_lib("lib/include/libelf.hrl").
-include_lib("lib/include/pdp10_opcodes.hrl").
-include_lib("lib/include/pdp10_relocs.hrl").

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

-spec main([string()]) -> no_return().
main(Argv) ->
  case my_getopt:parse(Argv, "ahlSgtesnrudVADcIvW",
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
  case libelf:read_Ehdr(?ELFCLASS36, FP) of
    {ok, Ehdr} ->
      print_ehdr(Opts, Ehdr),
      readelf_shtab(Opts, FP, Ehdr);
    {error, _Reason} = Error -> Error
  end.

readelf_shtab(Opts, FP, Ehdr) ->
  case libelf:read_ShTab(?ELFCLASS36, FP, Ehdr) of
    {ok, ShTab} ->
      print_shtab(Opts, ShTab),
      print_phtab(Opts, FP, Ehdr),
      readelf_symtab(Opts, FP, ShTab);
    {error, _Reason} = Error -> Error
  end.

readelf_symtab(Opts, FP, ShTab) ->
  case libelf:read_SymTab(?ELFCLASS36, FP, ShTab) of
    {ok, {SymTab, ShNdx}} ->
      print_relatab(Opts, FP, SymTab, ShNdx, ShTab),
      print_symtab(Opts, SymTab, ShNdx, ShTab),
      disassemble(Opts, FP, ShTab, SymTab);
    {error, _Reason} = Error -> Error
  end.

%% print_ehdr ==================================================================

print_ehdr(#options{file_header = false}, _Ehdr) -> ok;
print_ehdr(_Opts, Ehdr = #elf_Ehdr{}) ->
  EIdent = Ehdr#elf_Ehdr.e_ident,
  io:format("ELF Header:\n"),
  print_e_ident(EIdent),
  print_ei_class(lists:nth(1 + ?EI_CLASS, EIdent)),
  print_ei_data(lists:nth(1 + ?EI_DATA, EIdent)),
  print_ei_version(lists:nth(1 + ?EI_VERSION, EIdent)),
  print_ei_osabi(lists:nth(1 + ?EI_OSABI, EIdent)),
  print_ei_abiversion(lists:nth(1 + ?EI_ABIVERSION, EIdent)),
  print_e_type(Ehdr#elf_Ehdr.e_type),
  print_e_machine(Ehdr#elf_Ehdr.e_machine),
  print_e_version(Ehdr#elf_Ehdr.e_version),
  print_e_entry(Ehdr#elf_Ehdr.e_entry),
  print_e_phoff(Ehdr#elf_Ehdr.e_phoff),
  print_e_shoff(Ehdr#elf_Ehdr.e_shoff),
  print_e_flags(Ehdr#elf_Ehdr.e_flags),
  print_e_ehsize(Ehdr#elf_Ehdr.e_ehsize),
  print_e_phentsize(Ehdr#elf_Ehdr.e_phentsize),
  print_e_phnum(Ehdr#elf_Ehdr.e_phnum),
  print_e_shentsize(Ehdr#elf_Ehdr.e_shentsize),
  print_e_shnum(Ehdr#elf_Ehdr.e_shnum),
  print_e_shstrndx(Ehdr#elf_Ehdr.e_shstrndx),
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
  io:format("  Entry point address:\t\t\t0~12.8.0b\n", [EEntry]).

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
  io:format("  [~.10b] ~s ~s 0~12.8.0b 0~12.8.0b ~.10b ~.10b ~s ~.10b ~.10b ~.10b\n",
            [ I
            , sh_name(Shdr)
            , sh_type(Shdr)
            , Shdr#elf_Shdr.sh_addr
            , Shdr#elf_Shdr.sh_offset
            , Shdr#elf_Shdr.sh_size
            , Shdr#elf_Shdr.sh_entsize
            , sh_flags(Shdr)
            , Shdr#elf_Shdr.sh_link
            , Shdr#elf_Shdr.sh_info
            , Shdr#elf_Shdr.sh_addralign
            ]),
  print_shdrs(Shdrs, I + 1);
print_shdrs([], _I) ->
  io:format("Key to Flags:\n"),
  io:format("  W (write), A (alloc), X (execute), M (merge), S (strings), Z (compressed)\n"),
  io:format("  I (info), L (link order), G (group), T (TLS), E (exclude), x (unknown)\n"),
  io:format("  O (extra OS processing required), o (OS specific), p (processor specific)\n"),
  io:format("\n").

sh_name(#elf_Shdr{sh_name = ShName}) ->
  case ShName of
    "" -> "(empty)";
    _ -> ShName
  end.

sh_type(#elf_Shdr{sh_type = ShType}) ->
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

sh_flags(#elf_Shdr{sh_flags = ShFlags}) ->
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

%% print_phtab =================================================================

print_phtab(#options{segments = false}, _FP, _Ehdr) -> ok;
print_phtab(Opts, FP, Ehdr) ->
  case libelf:read_PhTab(?ELFCLASS36, FP, Ehdr) of
    {ok, []} ->
      io:format("There are no program headers in this file.\n\n");
    {ok, PhTab} ->
      io:format("Program Headers:\n"),
      io:format("  Type   Offset        VirtAddr      PhysAddr      FileSiz     MemSiz      Flg Align\n"),
      lists:foreach(fun print_phdr/1, PhTab),
      disassemble_phtab(Opts, FP, PhTab),
      io:format("\n");
    {error, Reason} ->
      escript_runtime:errmsg("Error reading program headers: ~s\n",
                             [error:format(Reason)])
  end.

disassemble_phtab(#options{disassemble = false}, _FP, _PhTab) -> ok;
disassemble_phtab(_Opts, FP, PhTab) -> do_disassemble_phtab(PhTab, 0, FP).

do_disassemble_phtab([], _PhNdx, _FP) -> ok;
do_disassemble_phtab([Phdr | PhTab], PhNdx, FP) ->
  case disassemble_phdr(Phdr, PhNdx, FP) of
    ok -> do_disassemble_phtab(PhTab, PhNdx + 1, FP);
    {error, _Reason} = Error -> Error
  end.

disassemble_phdr(Phdr, PhNdx, FP) ->
  case Phdr of
    #elf_Phdr{ p_type = ?PT_LOAD
             , p_offset = Offset
             , p_vaddr = VAddr
             , p_filesz = Size
             , p_flags = Flags
             } when (Flags band ?PF_X) =/= 0 ->
      io:format("\nDisassembly of segment nr ~.10b:\n", [PhNdx]),
      disassemble_unit(Offset, Size, VAddr, FP, _Labels = []);
    #elf_Phdr{} ->
      ok
  end.

print_phdr(Phdr) ->
  io:format("  ~-6s 0~12.8.0b 0~12.8.0b 0~12.8.0b 0x~9.16.0b 0x~9.16.0b ~-3s 0x~.16b\n",
            [ p_type(Phdr)
            , Phdr#elf_Phdr.p_offset
            , Phdr#elf_Phdr.p_vaddr
            , Phdr#elf_Phdr.p_paddr
            , Phdr#elf_Phdr.p_filesz
            , Phdr#elf_Phdr.p_memsz
            , p_flags(Phdr)
            , Phdr#elf_Phdr.p_align
            ]).

p_type(#elf_Phdr{p_type = PType}) ->
  case PType of
    ?PT_NULL -> "NULL";
    ?PT_LOAD -> "LOAD";
    ?PT_DYNAMIC -> "DYNAMIC";
    ?PT_INTERP -> "INTERP";
    ?PT_NOTE -> "NOTE";
    ?PT_SHLIB -> "SHLIB";
    ?PT_PHDR -> "PHDR";
    ?PT_TLS -> "TLS";
    _ -> io_lib:format("~.10b", [PType])
  end.

p_flags(#elf_Phdr{p_flags = PFlags0}) ->
  case lists:foldl(fun({Bit, Ch}, {PFlags, Acc}) ->
                     case PFlags band Bit of
                       0 -> {PFlags, [$\s | Acc]};
                       _ -> {PFlags band bnot Bit, [Ch | Acc]}
                     end
                   end, {PFlags0, []}, [{?PF_R, $R}, {?PF_W, $W}, {?PF_X, $E}]) of
    {0, Acc} -> lists:reverse(Acc);
    {_, _Acc} -> io_lib:format("0x~.16b", [PFlags0])
  end.

%% print_relatab ===============================================================

print_relatab(#options{relocs = false}, _FP, _SymTab, _ShNdx, _ShTab) -> ok;
print_relatab(_Opts, _FP, _SymTab, _SymTabShNdx = ?SHN_UNDEF, _ShTab) -> ok;
print_relatab(_Opts, FP, SymTab, SymTabShNdx, ShTab) ->
  print_relatab2(ShTab, FP, SymTab, SymTabShNdx, _Any = false).

print_relatab2([Shdr | ShTab], FP, SymTab, SymTabNdx, Any) ->
  NewAny =
    case Shdr of
      #elf_Shdr{sh_type = ?SHT_RELA, sh_link = ShLink} ->
        case ShLink of
          SymTabNdx ->
            print_relatab(Shdr, FP, SymTab);
          _ ->
            escript_runtime:errmsg("Relocation section '~s' has bogus sh_link ~p\n",
                                   [sh_name(Shdr), ShLink])
         end,
         true;
      #elf_Shdr{} -> Any
    end,
  print_relatab2(ShTab, FP, SymTab, SymTabNdx, NewAny);
print_relatab2([], _FP, _SymTab, _SymTabNdx, _Any = false) ->
  io:format("There are no relocation sections in this file.\n\n");
print_relatab2([], _FP, _SymTab, _SymTabNdx, _Any = true) ->
  ok.

print_relatab(Shdr, FP, SymTab) ->
  case libelf:read_RelaTab(?ELFCLASS36, FP, Shdr) of
    {ok, Relas} ->
      NrRelas = length(Relas),
      io:format("Relocation section '~s' at offset 0x~.16b contains ~.10b entr~s:\n",
                [sh_name(Shdr), Shdr#elf_Shdr.sh_offset, NrRelas,
                 case NrRelas of 1 -> "y"; _ -> "ies" end]),
      io:format("  Offset  Info      Type         Symbol's Value Symbol's Name + Addend\n"),
      lists:foreach(fun(Rela) -> print_rela(Rela, SymTab) end, Relas),
      io:format("\n");
    {error, Reason} ->
      escript_runtime:errmsg("Error reading relocation section '~s': ~s\n",
                             [sh_name(Shdr), error:format(Reason)])
  end.

print_rela(Rela, SymTab) ->
  #elf_Rela{r_offset = Offset, r_info = Info, r_addend = Addend} = Rela,
  SymNdx = ?ELF36_R_SYM(Info),
  Type = r_type(Rela),
  case SymNdx of
    ?SHN_UNDEF -> print_rela(Offset, Info, Type, _Value = 0, _Name = "", Addend);
    _ ->
      try lists:nth(SymNdx + 1, SymTab) of
        Sym = #elf_Sym{st_value = Value} ->
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

r_type(#elf_Rela{r_info = Info}) ->
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
  io:format("  ~.10b 0~12.8.0b ~.10b ~s ~s ~s ~.10b ~s\n",
            [ I
            , Sym#elf_Sym.st_value
            , Sym#elf_Sym.st_size
            , st_type(Sym)
            , st_bind(Sym)
            , st_vis(Sym)
            , Sym#elf_Sym.st_shndx
            , st_name(Sym)
            ]),
  print_syms(Syms, I + 1);
print_syms([], _I) ->
  io:format("\n").

st_type(#elf_Sym{st_info = StInfo}) ->
  case ?ELF_ST_TYPE(StInfo) of
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

st_bind(#elf_Sym{st_info = StInfo}) ->
  case ?ELF_ST_BIND(StInfo) of
    ?STB_LOCAL -> "LOCAL";
    ?STB_GLOBAL -> "GLOBAL";
    ?STB_WEAK -> "WEAK";
    ?STB_GNU_UNIQUE -> "GNU_UNIQUE";
    StBind -> io_lib:format("~.10b", [StBind])
  end.

st_vis(#elf_Sym{st_other = StOther}) ->
  case ?ELF_ST_VISIBILITY(StOther) of
    ?STV_DEFAULT -> "DEFAULT";
    ?STV_INTERNAL -> "INTERNAL";
    ?STV_HIDDEN -> "HIDDEN";
    ?STV_PROTECTED -> "PROTECTED"
  end.

st_name(#elf_Sym{st_name = StName}) ->
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
    #elf_Shdr{ sh_type = ?SHT_PROGBITS
             , sh_flags = ?SHF_ALLOC bor ?SHF_EXECINSTR
             , sh_addr = ShAddr
             , sh_offset = ShOffset
             , sh_size = ShSize
             } ->
      io:format("Disassembly of section nr ~.10b ~s:\n\n",
                [ShNdx, sh_name(Shdr)]),
      disassemble_unit(ShOffset, ShSize, ShAddr, FP, labels(SymTab, ShNdx));
    #elf_Shdr{} ->
      ok
  end.

disassemble_unit(FileOffset, Size, VAddr, FP, Labels) ->
  case pdp10_stdio:fseek(FP, {bof, FileOffset}) of
    ok -> disassemble_insns(0, Size, VAddr, FP, Labels);
    {error, _Reason} = Error -> Error
  end.

disassemble_insns(Offset, Size, VAddr, FP, Labels) when Offset < Size ->
  case read_uint36(FP) of
    {ok, InsnWord} ->
      RestLabels = print_labels(Labels, Offset),
      io:format(" 0~12.8.0b:\t0~12.8.0b\t", [VAddr + Offset, InsnWord]),
      disassemble_insn(InsnWord),
      disassemble_insns(Offset + 4, Size, VAddr, FP, RestLabels);
    {error, _Reason} = Error -> Error
  end;
disassemble_insns(_Offset, _Size, _VAddr, _FP, _Labels) -> ok.

%% TODO: for ELF-64 read 4 nonets from 8 octets and combine
read_uint36(FP) ->
  case pdp10_stdio:fread(1, 4, FP) of
    {ok, [B0, B1, B2, B3]} ->
      {ok, (((((B0 bsl 9) bor B1) bsl 9) bor B2) bsl 9) bor B3};
    eof -> {error, eof};
    {error, _Reason} = Error -> Error
  end.

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
        _ -> io:format("0~2.8.0b,", [(InsnWord bsr 23) band 16#F])
      end,
      case InsnWord band (1 bsl 22) of
        0 -> ok;
        _ -> io:format("@")
      end,
      io:format("0~6.8.0b", [InsnWord band ((1 bsl 18) - 1)]),
      case (InsnWord bsr 18) band 16#F of
        0 -> ok;
        IxReg -> io:format("(0~2.8.0b)", [IxReg])
      end,
      io:format("\n");
    false ->
      io:format("(bad)\n")
  end.

print_labels([], _Offset) -> [];
print_labels(Labels = [Label | RestLabels], Offset) ->
  #elf_Sym{st_value = StValue} = Label,
  if StValue < Offset -> print_labels(RestLabels, Offset);
     StValue =:= Offset ->
       io:format("0~12.8.0b <~s>:\n", [Offset, st_name(Label)]),
       print_labels(RestLabels, Offset);
     true -> Labels
  end.

labels(SymTab, TextShNdx) ->
  lists:sort(fun sym_cmp_by_value/2,
             lists:filter(fun(Sym) -> sym_is_label(Sym, TextShNdx) end,
                          SymTab)).

sym_is_label(Sym, TextShNdx) ->
  #elf_Sym{st_shndx = StShNdx, st_info = StInfo} = Sym,
  StShNdx =:= TextShNdx andalso
  ?ELF_ST_TYPE(StInfo) =:= ?STT_FUNC andalso % TODO: type of a label?
  (case ?ELF_ST_BIND(StInfo) of
     ?STB_GLOBAL -> true;
     ?STB_LOCAL -> true;
     ?STB_WEAK -> true;
     _ -> false
   end).

sym_cmp_by_value(Sym1, Sym2) ->
  Sym1#elf_Sym.st_value =< Sym2#elf_Sym.st_value.
