%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Converts pdp10-elf executables to KLH10-bootable files
%%% Copyright (C) 2023-2025  Mikael Pettersson
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
%%% This converts pdp10-elf executables to files bootable by KLH10.
%%%
%%% ELF executables are in general not restricted to section 0, which means we
%%% have to generate what KLH10 calls "DEC EXE sharable SAVE format" files.
%%%
%%% By default the output is in KLH10's default C36 format.  The --format=h36
%%% option changes the output to the more compact H36 format, but in that case
%%% you MUST execute "set ld_fmt=h36" in KLH10's shell before loading the file.

-module(elf2boot).
-export([ main/1
        , format_error/1
        ]).

-include_lib("lib/include/libelf.hrl").

-record(options,
        { verbose :: boolean()  % -v/--verbose
        , output :: undefined | string()
        , format :: c36 | h36
        }).

%% Command-line interface ======================================================

-spec main([string()]) -> no_return().
main(Argv) ->
  case my_getopt:parse(Argv, "Vvo:f:",
                       [ {"version", no, $V}
                       , {"verbose", no, $v}
                       , {"output", required, $o}
                       , {"format", required, $f}
                       ]) of
    {ok, {Options, Files}} ->
      elf2boot(scan_options(Options), Files);
    {error, Reason} ->
      escript_runtime:errmsg("~s\n", [error:format(Reason)]),
      usage()
  end.

usage() ->
  escript_runtime:fmterr(
    "Usage: ~s [-V] [-v] [-f FORMAT] [-o OUTFILE] INFILE\n",
    [escript_runtime:progname()]),
  halt(1).

scan_options(Options) ->
  Opts = #options{verbose = false, format = c36},
  lists:foldl(fun scan_option/2, Opts, Options).

scan_option($V, _Opts) -> % -V / --version
  io:format(standard_io, "pdp10-tools elf2boot version 0.0.1\n", []),
  halt(0);
scan_option($v, Opts) -> % -v / --verbose
  Opts#options{verbose = true};
scan_option({$o, Output}, Opts) -> % -o / --output
  Opts#options{output = Output};
scan_option({$f, Format}, Opts) -> % -f / --format
  case Format of
    "h36" -> Opts#options{format = h36};
    "c36" -> Opts#options{format = c36};
    _ ->
      escript_runtime:errmsg("Invalid format: ~s\n", [Format]),
      usage()
  end.

%% elf2boot ====================================================================

-type pagenr() :: non_neg_integer().
-type wordnr() :: non_neg_integer().
-type word() :: non_neg_integer().

-record(frag,
        { nrwords :: non_neg_integer()
        , src :: pagenr() % offset in input ELF file
               | [word()] % stub
        , mem :: pagenr() % offset in physical memory
        , excluded :: [{wordnr(), wordnr()}]
        }).

elf2boot(Opts, [InFile]) ->
  elf2boot(Opts, InFile,
           case Opts#options.output of
             undefined -> filename:basename(InFile, ".out") ++ ".exe";
             Output -> Output
           end);
elf2boot(_Opts, _Files) ->
  usage().

elf2boot(Opts, InFile, OutFile) ->
  case read_elf(InFile) of
    {ok, {InFP, EntryWordNr, Frags}} ->
      print_entry(Opts, EntryWordNr, Frags),
      try
        write_boot(Opts, InFP, EntryWordNr, Frags, OutFile)
      after
        libelf:fclose(InFP)
      end;
    {error, Reason} ->
      escript_runtime:errmsg("~s\n", [error:format(Reason)]),
     halt(1)
  end.

%% Writing bootable file in "DEC EXE sharable SAVE format" =====================
%%
%% - Groups of consecutive pages are copied from the file to memory. Source and
%%   destination offsets, and group lengths, are measured in pages. Page numbers
%%   are 27-bit long, allowing data to be read into addresses above section 0.
%%   The length of a group is between 1 and 512, inclusive.
%%
%% - There can be at most 512 page copy groups.
%%
%% - One of the page copy groups must set up an entry vector in section 0.
%%
%% - Consequently the entry vector refers to an XJRSTF which performs a long
%%   jump to the real entry point.

-define(MAX_GROUP_NRWORDS, (512*512)). % 512 pages x 512 words/page

-record(group,
        { frag :: #frag{}
        , dst :: pagenr() % offset in output file
        }).

write_boot(Opts, InFP, EntryWordNr, Frags, OutFile) ->
  #frag{mem = EntryPageNr} = EntryFrag = entry_frag(EntryWordNr),
  Groups = groups([EntryFrag | Frags]),
  print_groups(Opts, Groups),
  case outfp_fopen(Opts#options.format, OutFile) of
    {ok, OutFP} ->
      try
        write_boot_1(Opts, InFP, EntryPageNr, Groups, OutFP)
      after
        outfp_fclose(OutFP)
      end;
    {error, _Reason} = Error -> Error
  end.

write_boot_1(Opts, InFP, EntryPageNr, Groups, OutFP) ->
  NrGroups = length(Groups),
  true = NrGroups =< 512, % assert
  HdrNrWords = 1 + NrGroups * 2 + 3 + 1,
  HdrNrPages = nrwords_to_nrpages(HdrNrWords),
  HdrWords = build_decexe_hdr(Groups, EntryPageNr, HdrNrPages),
  print_header(Opts, HdrWords),
  ok = write_words(HdrWords, OutFP),
  ok = write_groups(Groups, HdrNrPages, OutFP, InFP).

write_groups([], _HdrNrPages, _OutFP, _InFP) -> ok;
write_groups([Group | Groups], HdrNrPages, OutFP, InFP) ->
  case write_group(Group, HdrNrPages, OutFP, InFP) of
    ok -> write_groups(Groups, HdrNrPages, OutFP, InFP);
    {error, _Reason} = Error -> Error
  end.

write_group(Group, HdrNrPages, OutFP, InFP) ->
  #group{frag = #frag{nrwords = NrWords, src = Src, excluded = Excluded}, dst = DstPageNr} = Group,
  case padto(OutFP, HdrNrPages + DstPageNr) of
    ok ->
      case write_src(NrWords, Src, Excluded, OutFP, InFP) of
        ok ->
          NrPages = nrwords_to_nrpages(NrWords),
          padto(OutFP, HdrNrPages + DstPageNr + NrPages);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

write_src(NrWords, Src, Excluded, DstFP, SrcFP) ->
  case Src of
    SrcPageNr when is_integer(SrcPageNr) ->
      iocpy(DstFP, SrcFP, SrcPageNr, Excluded, NrWords);
    Words when is_list(Words) ->
      write_words(Words, DstFP)
  end.

padto(DstFP, DstPageNr) ->
  DstWordOffset = nrpages_to_nrwords(DstPageNr),
  CurWordOffset = outfp_ftellw(DstFP),
  true = CurWordOffset =< DstWordOffset, % assert
  case CurWordOffset =:= DstWordOffset of
    true -> ok;
    false ->
      case outfp_fseekw(DstFP, DstWordOffset - 1) of
        ok -> outfp_fputw(0, DstFP);
        {error, _Reason} = Error -> Error
      end
  end.

write_words([], _DstFP) -> ok;
write_words([Word | Words], DstFP) ->
  case outfp_fputw(Word, DstFP) of
    ok -> write_words(Words, DstFP);
    {error, _Reason} = Error -> Error
  end.

%% Copy data between I/O devices ===============================================

iocpy(DstFP, SrcFP, SrcPageNr, Excluded, NrWords) ->
  SrcWordNr = nrpages_to_nrwords(SrcPageNr),
  case infp_fseekw(SrcFP, SrcWordNr) of
    ok -> iocpy_loop(DstFP, SrcFP, SrcWordNr, Excluded, NrWords);
    {error, _Reason} = Error -> Error
  end.

iocpy_loop(_DstFP, _SrcFP, _SrcWordNr, _Excluded, _NrWords = 0) -> ok;
iocpy_loop(DstFP, SrcFP, SrcWordNr, Excluded, NrWords) ->
  case iocpy_fgetw(SrcFP, SrcWordNr, Excluded) of
    {ok, Word} ->
      case outfp_fputw(Word, DstFP) of
        ok -> iocpy_loop(DstFP, SrcFP, SrcWordNr + 1, Excluded, NrWords - 1);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error;
    eof -> {error, eof}
  end.

iocpy_fgetw(SrcFP, SrcWordNr, Excluded) ->
  case is_excluded(SrcWordNr, Excluded) of
    true ->
      ok = infp_fseekw(SrcFP, SrcWordNr + 1),
      {ok, 0};
    false ->
     infp_fgetw(SrcFP)
  end.

%% Word-oriented input file abstraction ========================================

infp_fseekw(InFP, WordOffset) ->
  Wordsize = infp_wordsize(InFP),
  libelf:fseek(InFP, {bof, WordOffset * Wordsize}).

infp_fgetw(InFP) ->
  Wordsize = infp_wordsize(InFP),
  case libelf:fread(Wordsize, InFP) of
    {ok, Bytes} ->
      case Wordsize of
        8 -> {ok, extint:uint36_from_s64(Bytes)};
        4 -> {ok, extint:uint36_from_ext(Bytes)}
      end;
    EofOrError -> EofOrError
  end.

infp_wordsize({EC, _IoDev}) -> wordsize(EC).

%% Word-oriented output file abstraction =======================================
%%
%% Allow selecting KLH's C36 or H36 as output format.  H36 is denser (9 octets per
%% pair of 36-bit words) and matches what our stdio9 implements.  C36 is less
%% dense (5 octets per 36-bit word) but is easier to use since it's KLH's default.
%%
%% See klh10/src/wfio.c for details.

outfp_fopen(c36, FileName) ->
  case stdio8:fopen(FileName, [raw, write, delayed_write]) of
    {ok, IoDev} -> {ok, {c36, IoDev}};
    Error -> Error
  end;
outfp_fopen(h36, FileName) ->
  case stdio9:fopen(FileName, [raw, write, delayed_write]) of
    {ok, OutFP} -> {ok, {h36, OutFP}};
    Error -> Error
  end.

outfp_fclose({c36, IoDev}) ->
  stdio8:fclose(IoDev);
outfp_fclose({h36, OutFP}) ->
  stdio9:fclose(OutFP).

outfp_ftellw({c36, IoDev}) ->
  ByteOffset = stdio8:ftell(IoDev),
  0 = ByteOffset rem 5, % assert
  ByteOffset div 5;
outfp_ftellw({h36, OutFP}) ->
  ByteOffset = stdio9:ftell(OutFP),
  0 = ByteOffset band 3, % assert
  ByteOffset bsr 2.

outfp_fseekw({c36, IoDev}, WordOffset) ->
  stdio8:fseek(IoDev, {bof, WordOffset*5});
outfp_fseekw({h36, OutFP}, WordOffset) ->
  stdio9:fseek(OutFP, {bof, WordOffset*4}).

outfp_fputw(Word, {c36, IoDev}) ->
  stdio8:fputs(extint:uint36_to_c36(Word), IoDev);
outfp_fputw(Word, {h36, OutFP}) ->
  stdio9:fputs(extint:uint36_to_ext(Word), OutFP).

%% Optional debugging output ===================================================

print_entry(Opts, EntryWordNr, Frags) ->
  case Opts#options.verbose of
    true -> io:format("ELF:\tentry ~s\n\tfrags ~p\n", [format_lh(EntryWordNr), Frags]);
    false -> ok
  end.

print_groups(Opts, Groups) ->
  case Opts#options.verbose of
    true -> lists:foreach(fun print_group/1, Groups);
    false -> ok
  end.

print_group(Group) ->
  #group{frag = Frag, dst = DstPageNr} = Group,
  io:format("GROUP:\n"),
  io:format("\tdst ~s\n", [format_lh(DstPageNr bsl 9)]),
  print_frag(Frag).

print_frag(Frag) ->
  #frag{nrwords = NrWords, src = Src, mem = MemPageNr, excluded = Excluded} = Frag,
  io:format("\tnrwords ~p\n", [NrWords]),
  io:format("\tmem ~s\n", [format_lh(MemPageNr bsl 9)]),
  case Src of
    SrcPageNr when is_integer(SrcPageNr) ->
      io:format("\tsrc ~s\n", [format_lh(SrcPageNr bsl 9)]);
    Words when is_list(Words) ->
      io:format("\tsrc words:\n"),
      print_words(Words)
  end,
  io:format("\texcluded ~p\n", [Excluded]).

print_header(Opts, HdrWords) ->
  case Opts#options.verbose of
    true ->
      io:format("EXE HDR:\n"),
      print_words(HdrWords);
    false -> ok
  end.

print_words(Words) ->
  lists:foreach(fun print_word/1, Words).

print_word(Word) ->
  io:format("\t~s\n", [format_lh(Word)]).

format_lh(Word) ->
  LO18 = Word band ((1 bsl 18) - 1),
  HI18 = Word bsr 18,
  io_lib:format("~6.8.0b,,~6.8.0b", [HI18, LO18]).

%% Build DEC EXE header from the output groups =================================

%% DEC sharable save format - block IDs
-define(DECSSF_DIR, 8#01776).
-define(DECSSF_EV,  8#01775).
-define(DECSSF_END, 8#01777).

build_decexe_hdr(Groups, EntryPageNr, HdrNrPages) ->
  build_decexe_dir(Groups, HdrNrPages) ++
  [ word(?DECSSF_EV, 3)
  , word(0, 6)
  , word(0, EntryPageNr bsl 9) % *word* address of entry vector
  , word(?DECSSF_END, 1)
  ].

build_decexe_dir(Groups = [_|_], HdrNrPages) ->
  [ word(?DECSSF_DIR, 1 + 2 * length(Groups))
  | lists:flatmap(fun(Group) -> build_decexe_dir_entry(Group, HdrNrPages) end, Groups)
  ].

build_decexe_dir_entry(Group, HdrNrPages) ->
  #group{frag = #frag{nrwords = NrWords, mem = MemPageNr}, dst = DstPageNr} = Group,
  true = NrWords > 0 andalso NrWords =< ?MAX_GROUP_NRWORDS, % assert
  NrPages = nrwords_to_nrpages(NrWords),
  [ word927(0, HdrNrPages + DstPageNr)
  , word927(NrPages - 1, MemPageNr)
  ].

word927(H9, L27) ->
  (H9 bsl 27) bor L27.

word(H18, L18) ->
  (H18 bsl 18) bor L18.

%% Convert input frags to output groups ========================================
%%
%% - translate input frags to page groups in the output
%%   * this is not 1-to-1 since large frags must be split into multiple groups
%%
%% - compute file offsets for the output sources
%%   * the final offsets depend on the initial header size, which depends on the
%%     number of output groups, so these are relative the end of the header

groups(Frags) ->
  groups(Frags, _PageNr = 0, _Groups = []).

groups([], _PageNr, Groups) ->
  lists:reverse(Groups);
groups([Frag | Frags], PageNr, Groups) ->
  groups(Frag, Frags, PageNr, Groups).

groups(Frag, Frags, PageNr, Groups) ->
  NrWords = Frag#frag.nrwords,
  case NrWords > ?MAX_GROUP_NRWORDS of
    true ->
      NrPages = nrwords_to_nrpages(?MAX_GROUP_NRWORDS),
      Group = #group{frag = Frag#frag{nrwords = ?MAX_GROUP_NRWORDS}, dst = PageNr},
      SrcPageNr = Frag#frag.src + NrPages,
      NewFrag = Frag#frag{ nrwords = NrWords - ?MAX_GROUP_NRWORDS
                         , src = SrcPageNr
                         , mem = (Frag#frag.mem) + NrPages
                         , excluded = trim_excluded(SrcPageNr bsl 9, Frag#frag.excluded)
                         },
      groups(NewFrag, Frags, PageNr + NrPages, [Group | Groups]);
    false ->
      NewGroups =
        case NrWords of
          0 -> Groups;
          _ ->
           Group = #group{frag = Frag, dst = PageNr},
           [Group | Groups]
        end,
      NrPages = nrwords_to_nrpages(NrWords),
      groups(Frags, PageNr + NrPages, NewGroups)
  end.

%% Synthesize a frag for the entry vector ======================================
%%
%% - The format requires a 3-word entry vector.
%% - The entry vector must be in section 0, but needs to be able to jump to the
%%   real entry point in section > 0.
%% - We use XJRSTF and a flag-PC double-word to jump to the real entry point,
%%   and lay them out as an entry vector followed by the flag-PC double-word.

entry_frag(EntryWordNr) ->
  MemPageNr = 1, % section 0 page 1 TODO: use Frags to find a free section 0 page
  Src =
    [ 8#254240_001003 % 0,,001000 XJRSTF 001003 ; entry vector word 0: start instr
    , 8#254240_001003 % 0,,001001 XJRSTF 001003 ; entry vector word 1: restart instr
    , 8#000000_000000 % 0,,001002 0             ; entry vector word 2: version info
    , 8#000000_000000 % 0,,001003 0             ; flag-PC double-word: program flags
    , EntryWordNr     % 0,,001004 EntryWordNr   ; flag-PC double-word: PC
    ],
  #frag{nrwords = length(Src), src = Src, mem = MemPageNr, excluded = []}.

%% Reading ELF =================================================================

-spec read_elf(string()) -> {ok, {libelf:file(), word(), [#frag{}]}} | {error, any()}.
read_elf(File) ->
  case libelf:fopen(File, [raw, read]) of
    {ok, {EC, IoDev} = FP} ->
      case libelf:read_Ehdr(EC, IoDev) of
        {ok, Ehdr} -> read_elf(FP, Ehdr);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

read_elf({EC, IoDev} = FP, Ehdr) ->
  Log2NrBytesPerWord = log2_nr_bytes_per_word(EC),
  Wordsize = 1 bsl Log2NrBytesPerWord,
  case Ehdr of
    #elf_Ehdr{e_type = ?ET_EXEC, e_entry = Entry} when Entry band (Wordsize - 1) =:= 0 ->
      case libelf:read_PhTab(EC, IoDev, Ehdr) of
        {ok, PhTab} ->
          case frags(EC, Ehdr, PhTab) of
            {ok, Frags} -> {ok, {FP, Entry bsr Log2NrBytesPerWord, Frags}};
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    _ -> {error, {?MODULE, invalid_ehdr}}
  end.

frags(EC, Ehdr, PhTab) -> frags(PhTab, EC, Ehdr, 0, []).

frags([], _EC, _Ehdr, _PhdrIx, Frags) -> {ok, lists:reverse(Frags)};
frags([Phdr | PhTab], EC, Ehdr, PhdrIx, Frags) ->
  case frag(EC, Ehdr, Phdr) of
    error -> {error, {?MODULE, {invalid_phdr, PhdrIx}}};
    false -> frags(PhTab, EC, Ehdr, PhdrIx + 1, Frags);
    Frag -> frags(PhTab, EC, Ehdr, PhdrIx + 1, [Frag | Frags])
  end.

frag(EC, Ehdr, Phdr) ->
  case Phdr of
    #elf_Phdr{p_type = ?PT_NULL} -> false;
    #elf_Phdr{p_type = ?PT_LOAD, p_filesz = 0} -> false;
    #elf_Phdr{ p_type = ?PT_LOAD
             , p_offset = Offset
             , p_vaddr = VAddr
             , p_filesz = FileSz
             , p_memsz = MemSz
             , p_flags = Flags
             } ->
      Log2NrBytesPerWord = log2_nr_bytes_per_word(EC),
      Wordsize = 1 bsl Log2NrBytesPerWord,
      Log2NrWordsPerPage = 9,
      Pagesize = Wordsize bsl Log2NrWordsPerPage,
      case ((Offset band (Pagesize - 1)) =:= 0 andalso
            (VAddr band (Pagesize - 1)) =:= 0 andalso
            MemSz >= FileSz andalso
            no_excess_flags(Flags)) of
        true ->
          #frag{ nrwords = (FileSz + (Wordsize - 1)) bsr Log2NrBytesPerWord
               , src = (Offset + (Pagesize - 1)) bsr (Log2NrBytesPerWord + Log2NrWordsPerPage)
               , mem = (VAddr + (Pagesize - 1)) bsr (Log2NrBytesPerWord + Log2NrWordsPerPage)
               , excluded = build_excluded(EC, Offset, Ehdr)
               };
        false -> error
      end;
    _ -> error
  end.

no_excess_flags(Flags) ->
  (Flags band 8#7) =:= Flags.

%% GNU ld tends to create executables with the ELF header and program and section
%% tables mapped into the main PF_X PT_LOAD segment.  We need to avoid processing
%% such meta-data as if it was proper PDP-10 encoded code or data.
build_excluded(_EC = ?ELFCLASS64, _Offset = 0, Ehdr) ->
  #elf_Ehdr{ e_phoff = PhOff
           , e_shoff = ShOff
           , e_ehsize = EhSize
           , e_phentsize = PhEntSize
           , e_phnum = PhNum
           , e_shentsize = ShEntSize
           , e_shnum = ShNum
           } = Ehdr,
  ShArea =
    case ShNum of
      0 -> [];
      _ -> [{ShOff bsr 3, ((ShOff + ShNum * ShEntSize) bsr 3) - 1}]
    end,
  PhArea =
    case PhNum of
      0 -> [];
      _ -> [{PhOff bsr 3, ((PhOff + PhNum * PhEntSize) bsr 3) - 1}]
    end,
  EhArea = [{0, (EhSize bsr 3) - 1}],
  EhArea ++ PhArea ++ ShArea;
build_excluded(_EC, _Offset, _Ehdr) -> [].

is_excluded(_WordNr, _Excluded = []) -> false;
is_excluded(WordNr, [{StartNr, EndNr} | Rest]) ->
  (WordNr >= StartNr andalso WordNr =< EndNr) orelse is_excluded(WordNr, Rest).

trim_excluded(_SrcWordNr, []) -> [];
trim_excluded(SrcWordNr, [{_, End} | Rest]) when SrcWordNr > End -> trim_excluded(SrcWordNr, Rest);
trim_excluded(SrcWordNr, [First | Rest]) -> [First | trim_excluded(SrcWordNr, Rest)].

%% Operations on bytes and pages ===============================================

log2_nr_bytes_per_word(?ELFCLASS64) -> 3;
log2_nr_bytes_per_word(?ELFCLASS36) -> 2.

wordsize(EC) -> 1 bsl log2_nr_bytes_per_word(EC).

-define(LOG2_NR_WORDS_PER_PAGE, 9). % 512 words per page

nrpages_to_nrwords(NrPages) ->
  NrPages bsl ?LOG2_NR_WORDS_PER_PAGE.

nrwords_to_nrpages(NrWords) ->
  (NrWords + ((1 bsl ?LOG2_NR_WORDS_PER_PAGE) - 1)) bsr ?LOG2_NR_WORDS_PER_PAGE.

%% Error Formatting ============================================================

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    invalid_ehdr -> "invalid Ehdr";
    {invalid_phdr, PhdrIx} -> io_lib:format("invalid PHdr at index ~p", [PhdrIx])
  end.
