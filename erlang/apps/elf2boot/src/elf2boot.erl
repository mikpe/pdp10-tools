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
%%% Our representation of 36-bit words in octet files matches what KLH10 calls
%%% "high-density" or "H36" format, which isn't its default. You'll need to
%%% "set ld_fmt=h36" before loading the bootable file.

-module(elf2boot).
-export([ main/1
        , format_error/1
        ]).

-include_lib("lib/include/pdp10_elf36.hrl").

-record(options,
        { verbose :: boolean()  % -v/--verbose
        , output :: undefined | string()
        }).

%% Command-line interface ======================================================

-spec main([string()]) -> no_return().
main(Argv) ->
  case my_getopt:parse(Argv, "Vvo:",
                       [ {"version", no, $V}
                       , {"verbose", no, $v}
                       , {"output", no, $o}
                       ]) of
    {ok, {Options, Files}} ->
      elf2boot(scan_options(Options), Files);
    {error, Reason} ->
      escript_runtime:errmsg("~s\n", [error:format(Reason)]),
      usage()
  end.

usage() ->
  escript_runtime:fmterr(
    "Usage: ~s [-V] [-v] [-o OUTFILE] INFILE\n",
    [escript_runtime:progname()]),
  halt(1).

scan_options(Options) ->
  Opts = #options{ verbose = false
                 },
  lists:foldl(fun scan_option/2, Opts, Options).

scan_option($V, _Opts) -> % -V / --version
  io:format(standard_io, "pdp10-tools elf2boot version 0.0.1\n", []),
  halt(0);
scan_option($v, Opts) -> % -v / --verbose
  Opts#options{verbose = true};
scan_option({$o, Output}, Opts) -> % -o / --output
  Opts#options{output = Output}.

%% elf2boot ====================================================================

-type pagenr() :: non_neg_integer().
-type word() :: non_neg_integer().

-record(frag,
        { nrwords :: non_neg_integer()
        , src :: pagenr() % offset in input ELF file
               | [word()] % stub
        , mem :: pagenr() % offset in physical memory
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
    {ok, {InFP, Entry, Frags}} ->
      print_entry(Opts, Entry, Frags),
      try
        write_boot(Opts, InFP, Entry, Frags, OutFile)
      after
        pdp10_stdio:fclose(InFP)
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

write_boot(Opts, InFP, Entry, Frags, OutFile) ->
  #frag{mem = EntryPageNr} = EntryFrag = entry_frag(Entry),
  Groups = groups([EntryFrag | Frags]),
  print_groups(Opts, Groups),
  case outfp_fopen(OutFile) of
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
  #group{frag = #frag{nrwords = NrWords, src = Src}, dst = DstPageNr} = Group,
  case padto(OutFP, HdrNrPages + DstPageNr) of
    ok ->
      case write_src(NrWords, Src, OutFP, InFP) of
        ok ->
          NrPages = nrwords_to_nrpages(NrWords),
          padto(OutFP, HdrNrPages + DstPageNr + NrPages);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

write_src(NrWords, Src, DstFP, SrcFP) ->
  case Src of
    SrcPageNr when is_integer(SrcPageNr) ->
      iocpy(DstFP, SrcFP, SrcPageNr, NrWords);
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

iocpy(DstFP, SrcFP, SrcPageNr, NrWords) ->
  SrcWordOffset = nrpages_to_nrwords(SrcPageNr),
  case infp_fseekw(SrcFP, SrcWordOffset) of
    ok -> iocpy(DstFP, SrcFP, NrWords);
    {error, _Reason} = Error -> Error
  end.

iocpy(_DstFP, _SrcFP, _NrWords = 0) -> ok;
iocpy(DstFP, SrcFP, NrWords) ->
  case infp_fgetw(SrcFP) of
    {ok, Word} ->
      case outfp_fputw(Word, DstFP) of
        ok -> iocpy(DstFP, SrcFP, NrWords - 1);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error;
    eof -> {error, eof}
  end.

%% Word-oriented input file abstraction ========================================

infp_fseekw(InFP, WordOffset) ->
  pdp10_stdio:fseek(InFP, {bof, WordOffset*4}).

infp_fgetw(SrcFP) ->
  infp_fgetw(_NrBytes = 4, SrcFP, _Acc = 0).

infp_fgetw(0, _SrcFP, Word) -> {ok, Word};
infp_fgetw(N, SrcFP, Acc) ->
  case pdp10_stdio:fgetc(SrcFP) of
    {ok, Nonet} -> infp_fgetw(N - 1, SrcFP, (Acc bsl 9) bor Nonet);
    Else -> Else
  end.

%% Word-oriented output file abstraction =======================================

outfp_fopen(FileName) ->
  pdp10_stdio:fopen(FileName, [raw, write, delayed_write]).

outfp_fclose(OutFP) ->
  pdp10_stdio:fclose(OutFP).

outfp_ftellw(OutFP) ->
  ByteOffset = pdp10_stdio:ftell(OutFP),
  0 = ByteOffset band 3, % assert
  ByteOffset bsr 2.

outfp_fseekw(OutFP, WordOffset) ->
  pdp10_stdio:fseek(OutFP, {bof, WordOffset*4}).

outfp_fputw(Word, OutFP) ->
  pdp10_stdio:fputs(pdp10_extint:uint36_to_ext(Word), OutFP).

%% Optional debugging output ===================================================

print_entry(Opts, Entry, Frags) ->
  case Opts#options.verbose of
    true -> io:format("ELF:\tentry ~s\n\tfrags ~p\n", [format_lh(Entry bsr 2), Frags]);
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
  #frag{nrwords = NrWords, src = Src, mem = MemPageNr} = Frag,
  io:format("\tnrwords ~p\n", [NrWords]),
  io:format("\tmem ~s\n", [format_lh(MemPageNr bsl 9)]),
  case Src of
    SrcPageNr when is_integer(SrcPageNr) ->
      io:format("\tsrc ~s\n", [format_lh(SrcPageNr bsl 9)]);
    Words when is_list(Words) ->
      io:format("\tsrc words:\n"),
      print_words(Words)
  end.

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
      NewFrag = Frag#frag{ nrwords = NrWords - ?MAX_GROUP_NRWORDS
                         , src = (Frag#frag.src) + NrPages
                         , mem = (Frag#frag.mem) + NrPages
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

entry_frag(Entry) ->
  0 = Entry band 3, % assert
  EntryPC = Entry bsr 2,
  MemPageNr = 1, % section 0 page 1 TODO: use Frags to find a free section 0 page
  Src =
    [ 8#254240_001003 % 0,,001000 XJRSTF 001003 ; entry vector word 0: start instr
    , 8#254240_001003 % 0,,001001 XJRSTF 001003 ; entry vector word 1: restart instr
    , 8#000000_000000 % 0,,001002 0             ; entry vector word 2: version info
    , 8#000000_000000 % 0,,001003 0             ; flag-PC double-word: program flags
    , EntryPC         % 0,,001004 EntryPC       ; flag-PC double-word: PC
    ],
  #frag{nrwords = length(Src), src = Src, mem = MemPageNr}.

%% Reading ELF =================================================================

-spec read_elf(string()) -> {ok, {pdp10_stdio:file(), word(), [#frag{}]}} | {error, any()}.
read_elf(File) ->
  case pdp10_stdio:fopen(File, [raw, read]) of
    {ok, FP} ->
      case pdp10_elf36:read_Ehdr(FP) of
        {ok, Ehdr} -> read_elf(FP, Ehdr);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

read_elf(FP, Ehdr) ->
  case Ehdr of
    #elf36_Ehdr{e_type = ?ET_EXEC, e_entry = Entry} when Entry band 3 =:= 0 ->
      case pdp10_elf36:read_PhTab(FP, Ehdr) of
        {ok, PhTab} ->
          case frags(PhTab) of
            {ok, Frags} -> {ok, {FP, Entry, Frags}};
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    _ -> {error, {?MODULE, invalid_ehdr}}
  end.

frags(PhTab) -> frags(PhTab, 0, []).

frags([], _PhdrIx, Frags) -> {ok, lists:reverse(Frags)};
frags([Phdr | PhTab], PhdrIx, Frags) ->
  case frag(Phdr) of
    error -> {error, {?MODULE, {invalid_phdr, PhdrIx}}};
    false -> frags(PhTab, PhdrIx + 1, Frags);
    Frag -> frags(PhTab, PhdrIx + 1, [Frag | Frags])
  end.

frag(Phdr) ->
  case Phdr of
    #elf36_Phdr{p_type = ?PT_NULL} -> false;
    #elf36_Phdr{p_type = ?PT_LOAD, p_filesz = 0} -> false;
    #elf36_Phdr{ p_type = ?PT_LOAD
               , p_offset = Offset
               , p_vaddr = VAddr
               , p_filesz = FileSz
               , p_memsz = MemSz
               , p_flags = Flags
               } ->
      case (is_page_aligned(Offset) andalso
            is_page_aligned(VAddr) andalso
            MemSz >= FileSz andalso
            no_excess_flags(Flags)) of
        true ->
          #frag{nrwords = nrbytes_to_nrwords(FileSz), src = nrbytes_to_nrpages(Offset), mem = nrbytes_to_nrpages(VAddr)};
        false -> error
      end;
    _ -> error
  end.

no_excess_flags(Flags) ->
  (Flags band 8#7) =:= Flags.

%% Operations on bytes and pages ===============================================

-define(LOG2_NR_WORDS_PER_PAGE, 9). % 512 words per page
-define(LOG2_NR_BYTES_PER_WORD, 2). % 4 bytes (nonets) per word
-define(LOG2_NR_BYTES_PER_PAGE, (2 + 9)). % 4 bytes per word, 512 words per page

is_page_aligned(Address) ->
  (Address band ((1 bsl ?LOG2_NR_BYTES_PER_PAGE) - 1)) =:= 0.

nrbytes_to_nrpages(NrBytes) ->
  (NrBytes + ((1 bsl ?LOG2_NR_BYTES_PER_PAGE) - 1)) bsr ?LOG2_NR_BYTES_PER_PAGE.

nrpages_to_nrwords(NrPages) ->
  NrPages bsl ?LOG2_NR_WORDS_PER_PAGE.

nrbytes_to_nrwords(NrBytes) ->
  (NrBytes + ((1 bsl ?LOG2_NR_BYTES_PER_WORD) - 1)) bsr ?LOG2_NR_BYTES_PER_WORD.

nrwords_to_nrpages(NrWords) ->
  (NrWords + ((1 bsl ?LOG2_NR_WORDS_PER_PAGE) - 1)) bsr ?LOG2_NR_WORDS_PER_PAGE.

%% Error Formatting ============================================================

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    invalid_ehdr -> "invalid Ehdr";
    {invalid_phdr, PhdrIx} -> io_lib:format("invalid PHdr at index ~p", [PhdrIx])
  end.
