%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'ar' clone for pdp10-elf
%%% Copyright (C) 2013-2023  Mikael Pettersson
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

-module(ar).
-export([main/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("lib/include/pdp10_ar.hrl").
-include_lib("lib/include/pdp10_elf36.hrl").

%% in-core version of the ar header
-record(arhdr,
        { ar_name       :: string() | non_neg_integer()
        , ar_date       :: non_neg_integer()
        , ar_uid        :: non_neg_integer()
        , ar_gid        :: non_neg_integer()
        , ar_mode       :: non_neg_integer()
        , ar_size       :: non_neg_integer()
        }).

-type offset() :: non_neg_integer(). % offset of ar header in archive file

-record(member,
        { arhdr         :: #arhdr{}
        , location      :: offset()          % at this offset in old archive
                         | string()          % in this external file
        }).

-record(archive,
        { symtab        :: [{string(), offset()}] | false
        , members       :: [#member{}] % sorted by offset in output archive
        }).

-record(options,
        { operation     :: $d | $q | $r | $s | $t | $x | print_armap
        , mod_c = false :: boolean() % c modifier present
        , mod_u = false :: boolean() % u modifier present
        , mod_v = false :: boolean() % v modifier present
        , mod_D = false :: boolean() % D modifier present
        , mod_o = false :: boolean() % o modifier present
        , mod_O = false :: boolean() % O modifier present
        , mod_S = false :: boolean() % S modifier present
        }).

-type file() :: pdp10_stdio:file().

%% Command-line interface ======================================================

-spec main([string()]) -> no_return().
main(Argv) ->
  case parse_argv(Argv) of
    {ok, {Opts, ArchiveFile, Files}} ->
      ar(Opts, ArchiveFile, Files),
      halt(0);
    {error, ErrMsg} ->
      escript_runtime:errmsg("~s\n", [ErrMsg]),
      usage()
  end.

usage() ->
  Progname = escript_runtime:progname(),
  escript_runtime:fmterr(
    "Usage: ~s [-][dqrstx][csSuvV] <archive> <member..>\n"
    "       ~s --print-armap <archive>\n",
    [Progname, Progname]),
  halt(1).

parse_argv(["--print-armap" | Argv]) -> parse_archive(Argv, #options{operation = print_armap});
parse_argv([[$- | Arg] | Argv]) -> parse_operation(Arg, Argv);
parse_argv([Arg | Argv]) -> parse_operation(Arg, Argv);
parse_argv([]) -> {error, "no operation specified"}.

parse_operation(Arg, Argv) ->
  %% m - NYI
  %% p - NYI
  case Arg of
    [$d | Mod] -> parse_modifiers(Mod, Argv, $d, []);
    [$q | Mod] -> parse_modifiers(Mod, Argv, $q, []); % TODO: f
    [$r | Mod] -> parse_modifiers(Mod, Argv, $r, [$u]); % TODO: a, b/i, f
    [$s | Mod] -> parse_modifiers(Mod, Argv, $s, []);
    [$t | Mod] -> parse_modifiers(Mod, Argv, $t, [$O]);
    [$x | Mod] -> parse_modifiers(Mod, Argv, $x, [$o]);
    [$V | _] -> version();
    [C | _] -> {error, io_lib:format("invalid operation: ~c", [C])};
    [] -> {error, "no operation specified"}
  end.

parse_modifiers(Mod, Argv, Op, OpMods) ->
  Opts = #options{operation = Op},
  parse_modifiers2(Mod, Argv, Opts, OpMods).

parse_modifiers2([], Argv, Opts, _OpMods) ->
  %% TODO: check for a or b/i modifier -> get relpos arg
  %% TODO: check for N modifier -> get count arg
  parse_archive(Argv, Opts);
parse_modifiers2([C | Mod], Argv, Opts, OpMods) ->
  case parse_modifier(C, Opts, OpMods) of
    {ok, NewOpts} -> parse_modifiers2(Mod, Argv, NewOpts, OpMods);
    {error, _Reason} = Error -> Error
  end.

parse_modifier(C, Opts, OpMods) ->
  %% a - NYI
  %% b/i - NYI
  %% N - NYI
  %% f - NYI
  %% P - NYI
  %% T - NYI
  case C of
    $c -> {ok, Opts#options{mod_c = true}};
    $D -> {ok, Opts#options{mod_D = true}};
    $U -> {ok, Opts#options{mod_D = false}};
    $o -> check_opmods($o, OpMods, Opts#options{mod_o = true});
    $O -> check_opmods($O, OpMods, Opts#options{mod_O = true});
    $s -> {ok, Opts#options{mod_S = false}};
    $S -> {ok, Opts#options{mod_S = true}};
    $u -> check_opmods($u, OpMods, Opts#options{mod_u = true});
    $v -> {ok, Opts#options{mod_v = true}};
    $V -> version();
    _ -> {error, io_lib:format("invalid modifier: ~c", [C])}  end.

check_opmods(C, OpMods, Opts) ->
  case lists:member(C, OpMods) of
    true -> {ok, Opts};
    false -> {error, io_lib:format("invalid modifier for operation: ~c", [C])}
  end.

version() ->
  io:format(standard_io, "pdp10-tools ar version 0.1\n", []),
  halt(0).

parse_archive(Argv, Opts) ->
  case Argv of
    [ArchiveFile | Files] ->
      {ok, {Opts, ArchiveFile, Files}};
    [] ->
      {error, "no archive specified"}
  end.

%% ar dispacher ================================================================

ar(Opts, ArchiveFile, Files) ->
  case Opts#options.operation of
    Op when Op =:= $d; Op =:= $q; Op =:= $r; Op =:= $s ->
      ar_dqrs(Opts, ArchiveFile, Files);
    Op when Op =:= $t; Op =:= $x ->
      ar_tx(Opts, ArchiveFile, Files);
    print_armap ->
      ar_print_armap(ArchiveFile)
  end.

%% ar d/q/r/s code =============================================================

ar_dqrs(Opts, ArchiveFile, Files) ->
  case read_output_archive(ArchiveFile) of
    {ok, {FP, Archive}} ->
      case ar_dqrs(Opts, ArchiveFile, FP, Archive, Files) of
        {ok, TmpFile} -> file:rename(TmpFile, ArchiveFile);
        {error, Reason} -> escript_runtime:fatal("~p\n", [Reason])
      end;
    {error, Reason} ->
      escript_runtime:fatal("failed to read ~s: ~p\n", [ArchiveFile, Reason])
  end.

read_output_archive(ArchiveFile) ->
  case read_archive_file(ArchiveFile) of
    {ok, {_FP, _Archive}} = Result -> Result;
    {error, {file, enoent}} ->
      FP = false,
      {ok, Archive} = make_archive(_SymTab = [], _Members = []),
      {ok, {FP, Archive}};
    {error, _Reason} = Error -> Error
  end.

ar_dqrs(Opts, ArchiveFile, OldFP, Archive, Files) ->
  try
    {ok, NewArchive} = ar_dqrs_dispatch(Opts, Archive, Files),
    write_tmp_archive(Opts, ArchiveFile, OldFP, NewArchive)
  after
    case OldFP of
      false -> ok;
      _ -> pdp10_stdio:fclose(OldFP)
    end
  end.

ar_dqrs_dispatch(Opts, Archive, Files) ->
  case Opts#options.operation of
    $d -> ar_d(Opts, Archive, Files);
    $q -> ar_q(Opts, Archive, Files);
    $r -> ar_r(Opts, Archive, Files);
    $s -> ar_s(Opts, Archive, Files)
  end.

ar_d(Opts, Archive, Files) ->
  LabelledArchive = to_labelled_archive(Archive),
  NewLabelledArchive =
    lists:foldl(fun(File, LabelledArchive0) ->
                  ar_d_1(Opts, LabelledArchive0, File)
                end, LabelledArchive, Files),
  {ok, from_labelled_archive(NewLabelledArchive)}.

ar_d_1(Opts, LabelledArchive, File) ->
  Name = filename:basename(File),
  case lookup_label(LabelledArchive, Name) of
    false ->
      case Opts#options.mod_v of
        true -> io:format(standard_io, "No member named ~s~n", [File]);
        false -> ok
      end,
      LabelledArchive;
    Label ->
      case Opts#options.mod_v of
        true -> io:format(standard_io, "d - ~s~n", [File]);
        false -> ok
      end,
      delete_labelled_member(LabelledArchive, Name, Label)
  end.

ar_q(Opts, Archive, Files) ->
  LabelledArchive = to_labelled_archive(Archive),
  NewLabelledArchive =
    lists:foldl(fun(File, LabelledArchive0) ->
                   ar_q_1(Opts, LabelledArchive0, File)
                end, LabelledArchive, Files),
  {ok, from_labelled_archive(NewLabelledArchive)}.

ar_q_1(Opts, LabelledArchive, File) ->
  case file:read_file_info(File, [{time, posix}]) of
    {ok, #file_info{mtime = Date, uid = Uid, gid = Gid, mode = Mode,
                    size = OctetSize}} ->
      NonetSize = (OctetSize div 9) * 8 + ((OctetSize rem 9) * 8) div 9,
      Name = filename:basename(File),
      ArHdr = #arhdr{ ar_name = Name
                    , ar_date = Date
                    , ar_uid = Uid
                    , ar_gid = Gid
                    , ar_mode = Mode
                    , ar_size = NonetSize
                    },
      Member = #member{arhdr = ArHdr, location = File},
      case Opts#options.mod_v of
        true -> io:format(standard_io, "a - ~s~n", [File]);
        false -> ok
      end,
      %% FIXME: this differs from GNU ar which treats 'ar qs' as 'ar r',
      %% i.e. performing in-place replacement of existing members
      append_labelled_member(LabelledArchive, Member);
    {error, Reason} ->
      escript_runtime:fatal("~s: ~s~n", [File, file:format_error(Reason)])
  end.

ar_r(Opts, Archive, Files) ->
  LabelledArchive = to_labelled_archive(Archive),
  NewLabelledArchive =
    lists:foldl(fun(File, LabelledArchive0) ->
                   ar_r_1(Opts, LabelledArchive0, File)
                end, LabelledArchive, Files),
  {ok, from_labelled_archive(NewLabelledArchive)}.

ar_r_1(Opts, LabelledArchive, File) ->
  case file:read_file_info(File, [{time, posix}]) of
    {ok, #file_info{mtime = Date, uid = Uid, gid = Gid, mode = Mode,
                    size = OctetSize}} ->
      NonetSize = (OctetSize div 9) * 8 + ((OctetSize rem 9) * 8) div 9,
      Name = filename:basename(File),
      ArHdr = #arhdr{ ar_name = Name
                    , ar_date = Date
                    , ar_uid = Uid
                    , ar_gid = Gid
                    , ar_mode = Mode
                    , ar_size = NonetSize
                    },
      Member = #member{arhdr = ArHdr, location = File},
      %% FIXME: this doesn't match GNU ar when duplicate Names occur
      case lookup_label(LabelledArchive, Name) of
        false ->
          case Opts#options.mod_v of
            true -> io:format(standard_io, "a - ~s~n", [File]);
            false -> ok
          end,
          append_labelled_member(LabelledArchive, Member);
        Label ->
          case Opts#options.mod_v of
            true -> io:format(standard_io, "r - ~s~n", [File]);
            false -> ok
          end,
          update_labelled_member(LabelledArchive, Label, Member)
      end;
    {error, Reason} ->
      escript_runtime:fatal("~s: ~s~n", [File, file:format_error(Reason)])
  end.

ar_s(_Opts, Archive, _Files) ->
  {ok, Archive#archive{symtab = false}}.

%% ar t/x code =================================================================

ar_tx(Opts, ArchiveFile, Files) ->
  FileSet = ar_tx_fileset_from_list(Files),
  case read_archive_file(ArchiveFile) of
    {ok, {FP, Archive}} ->
      try
        case ar_tx_loop(Opts, FP, Archive#archive.members, FileSet) of
          {ok, []} ->
            ok;
          {ok, RestFiles} ->
            [escript_runtime:errmsg("no entry ~s in archive\n", [File])
             || File <- RestFiles],
            halt(1);
          {error, Reason} ->
            escript_runtime:fatal("~p\n", [Reason])
        end
      after
        pdp10_stdio:fclose(FP)
      end;
    {error, Reason} ->
      escript_runtime:fatal("failed to read ~s: ~p\n", [ArchiveFile, Reason])
  end.

ar_tx_loop(Opts, FP, Members, FileSet) ->
  case Members of
    [] ->
      {ok, ar_tx_fileset_to_list(FileSet)};
    [Member | RestMembers] ->
      case ar_tx_should_process_member(Member, FileSet) of
        {true, RestFileSet} ->
          Status =
            case Opts#options.operation of
              $t -> ar_t_member(Opts, Member);
              $x -> ar_x_member(Opts, FP, Member)
            end,
          case Status of
            ok -> ar_tx_loop(Opts, FP, RestMembers, RestFileSet);
            {error, _Reason} = Error -> Error
          end;
        false ->
          ar_tx_loop(Opts, FP, RestMembers, FileSet)
      end
  end.

ar_tx_should_process_member(Member, FileSet) ->
  case ar_tx_fileset_is_none(FileSet) of
    true -> {true, FileSet};
    false ->
      %% Note: this relies on read_archive/1 finalising member names.
      File = Member#member.arhdr#arhdr.ar_name,
      case fileset_is_element(File, FileSet) of
        true -> {true, fileset_delete(File, FileSet)};
        false -> false
      end
  end.

ar_tx_fileset_from_list([]) -> false;
ar_tx_fileset_from_list(Files) -> fileset_from_list(Files).

ar_tx_fileset_to_list(false) -> [];
ar_tx_fileset_to_list(FileSet) -> fileset_to_list(FileSet).

ar_tx_fileset_is_none(false) -> true;
ar_tx_fileset_is_none(_FileSet) -> false.

fileset_delete(File, FileSet) -> maps:remove(File, FileSet).
fileset_from_list(Files) -> maps:from_keys(Files, []).
fileset_is_element(File, FileSet) -> maps:is_key(File, FileSet).
fileset_to_list(FileSet) -> maps:keys(FileSet).

%% ar t ========================================================================

ar_t_member(Opts, #member{arhdr = ArHdr})->
  case Opts#options.mod_v of
    true ->
      #arhdr{ ar_date = Date
            , ar_uid = Uid
            , ar_gid = Gid
            , ar_mode = Mode
            , ar_size = Size
            } = ArHdr,
      io:format(standard_io,
                "~s~s~s ~B/~B ~10B ~s ",
                [ rwx(Mode bsr 6)
                , rwx(Mode bsr 3)
                , rwx(Mode)
                , Uid
                , Gid
                , Size
                , date_string(Date)
                ]);
    false -> ok
  end,
  io:format(standard_io, "~s~n", [ArHdr#arhdr.ar_name]).

rwx(Mode) -> [r(Mode), w(Mode), x(Mode)].

r(Mode) -> test_bit(Mode, 4, $r).
w(Mode) -> test_bit(Mode, 2, $w).
x(Mode) -> test_bit(Mode, 1, $x).

test_bit(Mode, Bit, Ch) ->
  if Mode band Bit =/= 0 -> Ch;
     true -> $-
  end.

%% Convert Unix time, seconds since 1970-01-01, to human-readable format
%% as per strftime(3) "%b %e %H:%M %Y".
date_string(UnixTime) ->
  {{Y, M, D}, {HH, MM, _SS}} = unixtime_to_localtime(UnixTime),
  io_lib:format("~s ~2B ~2..0B:~2..0B ~B",
                [abbreviated_month_name(M), D, HH, MM, Y]).

abbreviated_month_name(M) ->
  %% FIXME: should take locale into consideration
  element(M, {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}).

unixtime_to_localtime(SecondsSinceEpoch) ->
  calendar:universal_time_to_local_time(
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
      + SecondsSinceEpoch)).

%% ar x ========================================================================

ar_x_member(Opts, ArchiveFP, Member) ->
  #member{ arhdr = #arhdr{ar_name = Name, ar_size = Size, ar_mode = Mode}
         , location = HdrOffset
         } = Member,
  SrcOffset = HdrOffset + ?PDP10_ARHDR_SIZEOF,
  case Opts#options.mod_v of
    true -> io:format(standard_io, "x - ~s~n", [Name]);
    false -> ok
  end,
  case pdp10_stdio:fopen(Name, [raw, write, delayed_write]) of
    {ok, MemberFP} ->
      Status = iocpy(MemberFP, ArchiveFP, SrcOffset, Size),
      pdp10_stdio:fclose(MemberFP),
      case Status of
        ok -> file:change_mode(Name, Mode band 8#0777);
        Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

%% ar --print-armap code =======================================================

ar_print_armap(ArchiveFile) ->
  case read_archive_file(ArchiveFile) of
    {ok, {FP, Archive}} ->
      try
        ar_print_armap_1(Archive)
      after
        pdp10_stdio:fclose(FP)
      end;
    {error, Reason} ->
      escript_runtime:fatal("failed to read ~s: ~p\n", [ArchiveFile, Reason])
  end.

ar_print_armap_1(Archive) ->
  #archive{symtab = SymTab, members = Members} = Archive,
  case SymTab of
    false ->
      io:format(standard_io, "No archive index\n", []);
    _ ->
      io:format(standard_io, "Archive index:\n", []),
      OffsetToNameMap =
        maps:from_list(
          lists:map(
            fun(#member{location = Offset, arhdr = #arhdr{ar_name = Name}}) ->
              {Offset, Name}
            end, Members)),
      lists:foreach(
        fun({Symbol, Offset}) ->
          ar_print_armap(Symbol, Offset, OffsetToNameMap)
        end, SymTab)
  end.

ar_print_armap(Symbol, Offset, OffsetToNameMap) ->
  Name = maps:get(Offset, OffsetToNameMap, "<unknown>"),
  io:format(standard_io, "~s in ~s at ~p\n", [Symbol, Name, Offset]).

%% archive output ==============================================================

write_tmp_archive(Opts, ArchiveFile, OldFP, Archive) ->
  {StrTab, RawArchive} = archive_strtabify(Archive),
  case mkstemp(filename:dirname(ArchiveFile), ".artmp") of
    {ok, {TmpFile, TmpFP}} ->
      try
        case write_archive(Opts, TmpFP, StrTab, RawArchive, OldFP) of
          ok -> {ok, TmpFile};
          {error, _Reason} = Error -> Error
        end
      after
        pdp10_stdio:fclose(TmpFP)
      end;
    {error, _Reason} = Error -> Error
  end.

archive_strtabify(Archive) ->
  #archive{members = Members} = Archive,
  {NewMembers, {_Offset, StrTabRev}} =
    lists:mapfoldl(fun member_strtabify/2, {0, ""}, Members),
  NewArchive = Archive#archive{members = NewMembers},
  {lists:reverse(StrTabRev), NewArchive}.

member_strtabify(Member, Acc = {Offset, StrTabRev}) ->
  ArHdr = Member#member.arhdr,
  Name = ArHdr#arhdr.ar_name,
  Length = length(Name),
  case Length < 16 of
    true ->
      {Member, Acc};
    false ->
      NewArHdr = ArHdr#arhdr{ar_name = Offset},
      NewMember = Member#member{arhdr = NewArHdr},
      NewStrTabRev = [16#0A, 16#2F | lists:reverse(Name, StrTabRev)],
      NewOffset = Offset + Length + 2,
      {NewMember, {NewOffset, NewStrTabRev}}
  end.

write_archive(Opts, DstFP, StrTab, RawArchive, OldFP) ->
  SymTab =
    case Opts#options.mod_S of
      true -> maps:new();
      false -> archive_symtab(RawArchive, OldFP)
    end,
  case write_ar_mag(DstFP) of
    {error, _Reason} = Error -> Error;
    ok ->
      case write_symtab(DstFP, SymTab, StrTab) of
        {error, _Reason} = Error -> Error;
        ok ->
          case write_strtab(DstFP, StrTab) of
            {error, _Reason} = Error -> Error;
            ok ->
              #archive{members = Members} = RawArchive,
              write_members(DstFP, Members, OldFP)
          end
      end
  end.

write_strtab(_FP, []) -> ok;
write_strtab(FP, StrTab) ->
  Size = length(StrTab),
  ArHdr = #arhdr{ ar_name = "//"
                , ar_date = 0
                , ar_uid = 0
                , ar_gid = 0
                , ar_mode = 0
                , ar_size = Size
                },
  case write_arhdr(FP, ArHdr) of
    {error, _Reason} = Error -> Error;
    ok ->
      case fputs(StrTab, FP) of
        {error, _Reason} = Error -> Error;
        ok -> write_padding(FP, Size)
      end
  end.

write_members(DstFP, Members, OldFP) ->
  case Members of
    [] -> ok;
    [Member | RestMembers] ->
      case write_member(DstFP, Member, OldFP) of
        {error, _Reason} = Error -> Error;
        ok -> write_members(DstFP, RestMembers, OldFP)
      end
  end.

write_member(DstFP, Member, OldFP) ->
  #member{arhdr = ArHdr, location = Location} = Member,
  #arhdr{ar_size = Size} = ArHdr,
  case write_arhdr(DstFP, ArHdr) of
    {error, _Reason} = Error -> Error;
    ok ->
      case write_member_data(DstFP, Size, Location, OldFP) of
        {error, _Reason} = Error -> Error;
        ok -> write_padding(DstFP, Size)
      end
  end.

write_member_data(DstFP, Size, HdrOffset, OldFP) when is_integer(HdrOffset) ->
  SrcOffset = HdrOffset + ?PDP10_ARHDR_SIZEOF,
  iocpy(DstFP, OldFP, SrcOffset, Size);
write_member_data(DstFP, Size, SrcFile, _OldFP) ->
  case pdp10_stdio:fopen(SrcFile, [raw, read]) of
    {ok, SrcFP} ->
      try
        iocpy(DstFP, SrcFP, Size)
      after
        pdp10_stdio:fclose(SrcFP)
      end;
    {error, _Reason} = Error -> Error
  end.

write_padding(FP, Size) ->
  case Size band 1 of
    0 -> ok;
    1 -> pdp10_stdio:fputc(16#0A, FP)
  end.

%% create a temporary file =====================================================

-spec mkstemp(string(), string()) -> {ok, {string(), file()}} | {error, any()}.
mkstemp(Dir, Prefix) ->
  mkstemp(Dir, Prefix ++ os:getpid() ++ "_", 0).

mkstemp(Dir, Prefix, Count) when Count < 100 ->
  Path = filename:join(Dir, Prefix ++ integer_to_list(Count)),
  case pdp10_stdio:fopen(Path, [raw, read, write, exclusive]) of
    {ok, FP} -> {ok, {Path, FP}};
    {error, _Reason} -> mkstemp(Dir, Prefix, Count + 1)
  end;
mkstemp(_Dir, _Prefix, _Count) -> {error, eexist}.

%% copy data between I/O devices ===============================================

iocpy(DstFP, SrcFP, SrcOffset, NrBytes) ->
  case pdp10_stdio:fseek(SrcFP, {bof, SrcOffset}) of
    ok -> iocpy(DstFP, SrcFP, NrBytes);
    {error, _Reason} = Error -> Error
  end.

iocpy(_DstFP, _SrcFP, _NrBytes = 0) -> ok;
iocpy(DstFP, SrcFP, NrBytes) ->
  case pdp10_stdio:fgetc(SrcFP) of
    {ok, Byte} ->
      case pdp10_stdio:fputc(Byte, DstFP) of
        ok -> iocpy(DstFP, SrcFP, NrBytes - 1);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error;
    eof -> {error, eof}
  end.

%% archive input ===============================================================

-spec read_archive_file(string())
      -> {ok, {file(), #archive{}}} | {error, any()}.
read_archive_file(ArchiveFile) ->
  case pdp10_stdio:fopen(ArchiveFile, [raw, read]) of
    {ok, FP} ->
      try
        case read_archive_fp(FP) of
          {ok, Archive} -> {ok, {FP, Archive}};
          {error, _Reason} = Error ->
            pdp10_stdio:fclose(FP),
            Error
        end
      catch error:Reason ->
        pdp10_stdio:fclose(FP),
        {error, Reason}
      end;
    {error, _Reason} = Error -> Error
  end.

-spec read_archive_fp(file()) -> {ok, #archive{}} | {error, any()}.
read_archive_fp(FP) ->
  case read_ar_mag(FP) of
    ok -> read_archive_symtab(FP);
    {error, _Reason} = Error -> Error
  end.

read_archive_symtab(FP) ->
  case read_arhdr(FP) of
    {ok, ArHdr} ->
      case ArHdr#arhdr.ar_name of
        "/" ->
          case read_symtab(FP, ArHdr#arhdr.ar_size) of
            {ok, SymTab} -> read_archive_strtab(FP, SymTab);
            {error, _Reason} = Error -> Error
          end;
        _ -> read_archive_strtab(FP, _SymTab = false, ArHdr)
      end;
    {error, eof} ->
      make_archive(_SymTab = [], _Members = []);
    {error, _Reason} = Error -> Error
  end.

read_archive_strtab(FP, SymTab) ->
  case read_arhdr(FP) of
    {ok, ArHdr} -> read_archive_strtab(FP, SymTab, ArHdr);
    {error, eof} ->
      make_archive(_SymTab = [], _Members = []);
    {error, _Reason} = Error -> Error
  end.

read_archive_strtab(FP, SymTab, ArHdr) ->
  case ArHdr#arhdr.ar_name of
    "//" ->
      case read_strtab(FP, ArHdr#arhdr.ar_size) of
        {ok, StrTab} -> read_archive_members(FP, SymTab, StrTab, []);
        {error, _Reason} = Error -> Error
      end;
    _ -> read_archive_members(FP, SymTab, strtab_empty(), [], ArHdr)
  end.

read_archive_members(FP, SymTab, StrTab, Members) ->
  case read_arhdr(FP) of
    {ok, ArHdr} ->
      read_archive_members(FP, SymTab, StrTab, Members, ArHdr);
    {error, eof} ->
      make_archive(SymTab, lists:reverse(Members));
    {error, _Reason} = Error ->
      Error
  end.

read_archive_members(FP, SymTab, StrTab, Members, ArHdr) ->
  case finalise_ar_name(StrTab, ArHdr#arhdr.ar_name) of
    {ok, Name} ->
      SrcOffset = pdp10_stdio:ftell(FP),
      HdrOffset = SrcOffset - ?PDP10_ARHDR_SIZEOF,
      Member = #member{arhdr = ArHdr#arhdr{ar_name = Name},
                       location = HdrOffset},
      NewMembers = [Member | Members],
      case skip_member(FP, ArHdr#arhdr.ar_size) of
        ok ->
          read_archive_members(FP, SymTab, StrTab, NewMembers);
        eof ->
          make_archive(SymTab, lists:reverse(NewMembers));
        {error, _Reason} = Error ->
          Error
      end;
    {error, _Reason} = Error -> Error
  end.

make_archive(SymTab, Members) ->
  case check_symtab(SymTab, Members) of
    ok -> {ok, #archive{symtab = SymTab, members = Members}};
    {error, _Reason} = Error -> Error
  end.

check_symtab(_SymTab = false, _Members) -> ok;
check_symtab(SymTab, Members) ->
  Offsets =
    lists:foldl(
      fun(#member{location = Offset}, AccOffsets) when is_integer(Offset) ->
        maps:put(Offset, [], AccOffsets)
      end, maps:new(), Members),
  case lists:search(fun({_Name, Offset}) when is_integer(Offset) ->
                      not maps:is_key(Offset, Offsets)
                    end, SymTab) of
    false -> ok;
    {value, {Name, Offset}} -> {error, {invalid_symtab, Name, Offset}}
  end.

finalise_ar_name(_StrTab, Name) when is_list(Name) -> {ok, Name};
finalise_ar_name(StrTab, Offset) when is_integer(Offset) ->
  case strtab_lookup(StrTab, Offset) of
    false -> {error, invalid_strtab_offset};
    Name -> {ok, Name}
  end.

%% Unfortunately fseek() and file:position/2 allow seeking past the end of
%% the file, so we seek to the last byte of the member, read that, and then
%% optionally read a padding byte so the next header starts at an even offset.
%% An EOF when reading the member's last byte is an error, while an EOF when
%% reading the padding byte simply means the end of the archive.
skip_member(_FP, _Size = 0) -> ok;
skip_member(FP, Size) when Size > 0 ->
  case pdp10_stdio:fseek(FP, {cur, Size - 1}) of
    ok ->
      case pdp10_stdio:fgetc(FP) of
        {ok, _Byte} -> read_padding(FP, Size);
        eof -> {error, eof};
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

read_padding(FP, Size) ->
  case Size band 1 of
    0 -> ok;
    1 ->
      case pdp10_stdio:fgetc(FP) of
        {ok, 16#0A} -> ok;
        {ok, Ch} -> {error, {invalid_padding, Ch}};
        eof -> eof;
        {error, _Reason} = Error -> Error
      end
  end.

%% labelled archives ===========================================================
%%
%% - members are ordered as stored in the archive
%% - members can be appended at the end of the archive, inserted before or
%%   after another named member, updated in place, or deleted
%% - there can be multiple members with the same name
%%
%% To support this we use a structure, labelled archives, that uses symbolic
%% labels to address and order members.
%%
%% A label is a pair of integers {I, J}.
%%
%% A member in the pre-existing archive is labelled by its position I in
%% the archive, as the pair {I, 0}.
%%
%% A member appended after some pre-existing member with label {I, 0} is
%% labelled {I, J+1}, where J is the number of members appended after {I, 0}.
%%
%% A member inserted before a pre-existing member with label {I, 0} is treated
%% is if appended after the member with label {I-1, 0}.
%%
%% Members appended at the end of the archive are treated as if appended after
%% the pre-existing archive's last member.  For an empty archive the label of
%% the imaginary last member is defined to be {0, 0}.
%%
%% In all cases Erlang orders the labels as we require, so ordering the members
%% by their labels yields a correctly ordered archive.
%%
%% A separate structure, name_to_labels, maps each member name to an ordered
%% list of the labels of its occurrences in the archive. This supports having
%% multiple members with the same name, and the "N count" modifier.

-type label() :: {non_neg_integer(), non_neg_integer()}.

-record(labelled_archive,
        { name_to_labels :: #{string() => nonempty_list(label())}
        , last_label :: label()
        , members :: gb_trees:tree(label(), {non_neg_integer(), [] | #member{}})
        }).

to_labelled_archive(#archive{members = Members}) ->
  HeadLabel = {0, 0},
  HeadMember = {_NrAfter = 0, _Member = []},
  Head = {HeadLabel, HeadMember},
  Init =
    { _PrevIndex = 0
    , _NameToLabels = maps:new()
    , _LabelledMembers = [Head]
    },
  {LastIndex, NameToLabels, LabelledMembers} = lists:foldl(fun to_labelled_member/2, Init, Members),
  #labelled_archive
    { name_to_labels = NameToLabels
    , last_label = {LastIndex, 0}
    , members = gb_trees:from_orddict(lists:reverse(LabelledMembers))
    }.

to_labelled_member(Member, {PrevIndex, NameToLabels, LabelledMembers}) ->
  Name = Member#member.arhdr#arhdr.ar_name,
  Index = PrevIndex + 1,
  Label = {Index, 0},
  NameLabels = maps:get(Name, NameToLabels, []),
  NewNameToLabels = maps:put(Name, [Label | NameLabels], NameToLabels),
  NewLabelledMembers = [{Label, {_NrAfter = 0, Member}} | LabelledMembers],
  {Index, NewNameToLabels, NewLabelledMembers}.

from_labelled_archive(#labelled_archive{members = Members}) ->
  [{{0, 0}, {_NrAfter, []}} | ActualMembers] = gb_trees:to_list(Members),
  #archive{symtab = false, members = lists:map(fun from_labelled_member/1, ActualMembers)}.

from_labelled_member({_Label, {_NrAfter, Member}}) -> Member.

lookup_label(LabelledArchive, Name) ->
  #labelled_archive{name_to_labels = NameToLabels} = LabelledArchive,
  case maps:get(Name, NameToLabels, []) of
    [Label | _Labels] -> Label; % TODO: handle "N count"
    [] -> false
  end.

delete_labelled_member(LabelledArchive, Name, Label) ->
  #labelled_archive{name_to_labels = NameToLabels, members = Members} = LabelledArchive,
  NewMembers = gb_trees:delete(Label, Members),
  NewNameToLabels =
    case maps:get(Name, NameToLabels) -- [Label] of
      [] -> maps:remove(Name, NameToLabels);
      Labels -> maps:put(Name, Labels, NameToLabels)
    end,
  LabelledArchive#labelled_archive{name_to_labels = NewNameToLabels, members = NewMembers}.

append_labelled_member(LabelledArchive, Member) ->
  #labelled_archive{name_to_labels = NameToLabels, last_label = AfterLabel, members = Members} = LabelledArchive,
  #member{arhdr = #arhdr{ar_name = Name}} = Member,
  {NrAfter, AfterMember} = gb_trees:get(AfterLabel, Members),
  {AfterI, 0} = AfterLabel,
  NewLabel = {AfterI, NrAfter + 1},
  NameLabels = maps:get(Name, NameToLabels, []),
  NewNameLabels = ordsets:add_element(NewLabel, NameLabels),
  NewNameToLabels = maps:put(Name, NewNameLabels, NameToLabels),
  NewMembers1 = gb_trees:update(AfterLabel, {NrAfter + 1, AfterMember}, Members),
  NewMembers2 = gb_trees:insert(NewLabel, {0, Member}, NewMembers1),
  LabelledArchive#labelled_archive{name_to_labels = NewNameToLabels, members = NewMembers2}.

update_labelled_member(LabelledArchive, Label, Member) ->
  #labelled_archive{members = Members} = LabelledArchive,
  {NrAfter, _OldMember} = gb_trees:get(Label, Members),
  NewMembers = gb_trees:update(Label, {NrAfter, Member}, Members),
  LabelledArchive#labelled_archive{members = NewMembers}.

%% assemble symbol table =======================================================

archive_symtab(#archive{members = Members}, ArchiveFP) ->
  {_Offset, SymTab} =
    lists:foldl(fun(Member, Acc) ->
                  archive_symtab(Member, Acc, ArchiveFP)
                end, {0, maps:new()}, Members),
  maps:to_list(SymTab).

archive_symtab(Member, {Offset, SymTab}, ArchiveFP) ->
  Size = ?PDP10_ARHDR_SIZEOF + pad_size(Member#member.arhdr#arhdr.ar_size),
  NewOffset = Offset + Size,
  NewSymTab = archive_symtab(Offset, Member, ArchiveFP, SymTab),
  {NewOffset, NewSymTab}.

archive_symtab(Offset, Member, ArchiveFP, SymTab) ->
  case read_member_symtab(ArchiveFP, Member) of
    false -> SymTab;
    Symbols ->
      lists:foldl(
        fun(Symbol, Acc) ->
          case maps:is_key(Symbol, Acc) of
            false -> maps:put(Symbol, Offset, Acc);
            true -> Acc % defined by earlier member
          end
        end, SymTab, Symbols)
  end.

%% member symbol table =========================================================
%%
%% Read the symbol table of a member. For now this only recognizes pdp10-elf.

read_member_symtab(ArchiveFP, Member) ->
  #member{arhdr = ArHdr, location = Location} = Member,
  case Location of
    HdrOffset when is_integer(HdrOffset) -> % member in the initial input archive
      Offset = HdrOffset + ?PDP10_ARHDR_SIZEOF,
      Size = ArHdr#arhdr.ar_size,
      read_member_symtab(ArchiveFP, Offset, Offset + Size);
    File when is_list(File) -> % file added to the output archive
      case pdp10_stdio:fopen(File, [raw, read]) of
        {ok, MemberFP} ->
          try
            read_member_symtab(MemberFP, _Base = 0, _Limit = false)
          after
            pdp10_stdio:fclose(MemberFP)
          end;
        {error, _Reason} -> false
      end
  end.

read_member_symtab(FP, Base, Limit) ->
  case pdp10_elf36:read_Ehdr(FP, Base, Limit) of
    {ok, Ehdr} ->
      case pdp10_elf36:read_ShTab(FP, Base, Limit, Ehdr) of
        {ok, ShTab} ->
          case pdp10_elf36:read_SymTab(FP, Base, Limit, ShTab) of
            {ok, {SymTab, _ShNdx}} -> filter_member_symtab(SymTab);
            {error, _Reason} -> false
          end;
        {error, _Reason} -> false
      end;
    {error, _Reason} -> false
  end.

filter_member_symtab(SymTab) ->
  lists:filtermap(
    fun(#elf36_Sym{st_info = Info, st_shndx = ShNdx, st_name = Name}) ->
      case ?ELF36_ST_BIND(Info) of
        ?STB_GLOBAL when ShNdx =/= ?SHN_UNDEF -> {true, Name};
        ?STB_WEAK -> {true, Name}; % FIXME: does a later non-weak definition override this one?
        _ -> false
      end
    end, SymTab).

%% symbol table ================================================================
%%
%% The symbol table is stored as a sequence of three pieces of data:
%% 1. The COUNT of symbols in the table, as a 4-byte big-endian integer.
%% 2. A sequence of COUNT offsets to the AR headers for the members defining
%%    those symbols. Each offset is a 4-byte big-endian integer.
%% 3. A sequence of COUNT NUL-terminated names for those symbols.
%%
%% On input the symbol table is first a list of {Offset, Name} pairs (PreSymTab).
%% Once the members are known and have been labelled, PreSymTab is converted to
%% a map from each symbol's NAME to the LABEL for its defining member.
%%
%% On output the symbol table is first recomputed, unless it is still valid.
%% Any change to the archive's members invalidates the internal symbol table.
%%
%% Once the symbol table is known its size is computed and the offsets of the
%% members in the output archive are computed and recorded in a map. This map
%% is consulted during output to convert member labels to member offsets.

-define(WORDSIZE, 4).

read_symtab(FP, Size) when Size >= ?WORDSIZE ->
  case read_word_be(FP) of
    {ok, NrSymbols} when Size >= (NrSymbols + 1) * ?WORDSIZE ->
      case read_words_be(FP, NrSymbols) of
        {ok, Offsets} ->
          case read_string(FP, Size - (NrSymbols + 1) * ?WORDSIZE) of
            {ok, StrBuf} ->
              case read_padding(FP, Size) of
                {error, _Reason} = Error -> Error;
                _ -> make_pre_symtab(Offsets, StrBuf) % ok or eof
              end;
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    {ok, _NrSymbols} -> {error, invalid_symbol_table};
    {error, _Reason} = Error -> Error
  end;
read_symtab(_FP, _Size) -> {error, invalid_symbol_table}.

make_pre_symtab(Offsets, StrBuf) ->
  case split_strbuf(StrBuf) of
    {ok, Names} ->
      case safe_zip(Names, Offsets) of
        {ok, _PreSymTab} = Result -> Result;
        {error, _Reason} -> {error, invalid_symbol_table}
      end;
    {error, _Reason} = Error -> Error
  end.

%% Split StrBuf into a list of NUL-terminated Names.
split_strbuf(StrBuf) -> split_strbuf(StrBuf, []).

split_strbuf([], Names) -> {ok, lists:reverse(Names)};
split_strbuf(String, Names) ->
  case split_string(String) of
    {ok, {[_|_] = Name, Rest}} -> split_strbuf(Rest, [Name | Names]);
    _ -> {error, invalid_symbol_table}
  end.

split_string(String) -> split_string(String, []).

split_string([16#00 | String], Name) -> {ok, {lists:reverse(Name), String}};
split_string([Ch | String], Name) -> split_string(String, [Ch | Name]);
split_string([], _Name) -> {error, missing_nul}.

safe_zip(As, Bs) ->
  try
    {ok, lists:zip(As, Bs)}
  catch
    error:Reason -> {error, Reason}
  end.

read_words_be(FP, NrWords) -> read_words_be(FP, NrWords, []).

read_words_be(_FP, 0, Words) -> {ok, lists:reverse(Words)};
read_words_be(FP, N, Words) ->
  case read_word_be(FP) of
    {ok, Word} -> read_words_be(FP, N - 1, [Word | Words]);
    {error, _Reason} = Error -> Error
  end.

write_symtab(FP, SymTab, StrTab) ->
  case lists:foldl(fun write_symtab_foldf/2, {[], []}, SymTab) of
    {[], []} -> ok;
    {Offsets, Strings} ->
      NrSymbols = length(Offsets),
      SymTabSize = ?WORDSIZE * (1 + NrSymbols) + length(Strings),
      InitialOffset =
        ?PDP10_SARMAG + special_member_size(SymTabSize) + special_member_size(length(StrTab)),
      ArHdr = #arhdr{ ar_name = "/"
                    , ar_date = 0
                    , ar_uid = 0
                    , ar_gid = 0
                    , ar_mode = 0
                    , ar_size = SymTabSize
                    },
      case write_arhdr(FP, ArHdr) of
        ok ->
          case write_word_be(FP, NrSymbols) of
            ok ->
              case write_offsets(FP, Offsets, InitialOffset) of
                ok ->
                  case fputs(Strings, FP) of
                    ok -> write_padding(FP, SymTabSize);
                    {error, _Reason} = Error -> Error
                  end;
                {error, _Reason} = Error -> Error
              end
          end;
        {error, _Reason} = Error -> Error
      end
  end.

special_member_size(0) -> 0;
special_member_size(Size) -> ?PDP10_ARHDR_SIZEOF + pad_size(Size).

pad_size(Size) ->
  Size + (Size band 1).

write_symtab_foldf({String, Offset}, {Offsets, Strings}) ->
  {[Offset | Offsets], String ++ [16#00] ++ Strings}.

write_offsets(_FP, [], _InitialOffset) -> ok;
write_offsets(FP, [Offset | Offsets], InitialOffset) ->
  case write_word_be(FP, InitialOffset + Offset) of
    ok -> write_offsets(FP, Offsets, InitialOffset);
    {error, _Reason} = Error -> Error
  end.

%% FIXME: functionally equivalent to pdp10_elf36:read_uint36/1
read_word_be(FP) -> read_word_be(FP, ?WORDSIZE, []).

read_word_be(_FP, 0, [B4, B3, B2, B1]) ->
  {ok, ((B1 band 16#1FF) bsl 27) bor
       ((B2 band 16#1FF) bsl 18) bor
       ((B3 band 16#1FF) bsl  9) bor
        (B4 band 16#1FF)};
read_word_be(FP, N, Acc) ->
  case pdp10_stdio:fgetc(FP) of
    {ok, Byte} -> read_word_be(FP, N - 1, [Byte | Acc]);
    eof -> {error, premature_eof};
    {error, _Reason} = Error -> Error
  end.

write_word_be(FP, Word) ->
  B1 = (Word bsr 27) band 16#1FF,
  B2 = (Word bsr 18) band 16#1FF,
  B3 = (Word bsr  9) band 16#1FF,
  B4 = Word band 16#1FF,
  fputs([B1, B2, B3, B4], FP).

%% string table ================================================================
%%
%% The string table is stored as a sequence of strings each terminated by "/\n".
%% It's referenced from ar_names on the form "/<offset>" where <offset> is the
%% offset in the string table for the start of the corresponding <name>. Offsets
%% may not refer to interior points in strings. (TODO: check this)
%%
%% During input we read the string table, create a mapping from offsets to names,
%% and consult that for ar_names that reference the string table.

read_strtab(FP, Size) ->
  case read_string(FP, Size) of
    {ok, String} ->
      case read_padding(FP, Size) of
        ok -> scan_strtab(String);
        eof -> {ok, strtab_empty()};
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

scan_strtab(String) ->
  scan_strtab(String, 0, strtab_empty()).

scan_strtab([], _Offset, StrTab) -> {ok, StrTab};
scan_strtab([16#0A], _Offset, StrTab) -> {ok, StrTab};
scan_strtab(String, Offset, StrTab) ->
  case split_strtab(String) of
    {ok, {First, Rest}} ->
      scan_strtab(Rest, Offset + length(First) + 2,
                  strtab_insert(StrTab, Offset, First));
    {error, _Reason} = Error -> Error
  end.

split_strtab(String) ->
  split_strtab(String, []).

split_strtab([16#2F, 16#0A | Rest], First) ->
  {ok, {lists:reverse(First), Rest}};
split_strtab([16#2F | _Rest], _First) ->
  {error, invalid_strtab_entry_terminator};
split_strtab([16#0A | _Rest], _First) ->
  {error, invalid_strtab_entry_terminator};
split_strtab([Ch | Rest], First) ->
  split_strtab(Rest, [Ch | First]).

strtab_empty() ->
  maps:new().

strtab_insert(StrTab, Offset, String) ->
  maps:put(Offset, String, StrTab).

strtab_lookup(StrTab, Offset) ->
  maps:get(Offset, StrTab, false).

%% descriptor-based record I/O =================================================

-type read_field() :: fun((pdp10_stdio:file()) -> {ok, term()} | {error, term()}).
-type write_field() :: fun((pdp10_stdio:file(), term()) -> ok | {error, term()}).
-type read_tail() :: fun((pdp10_stdio:file()) -> ok | {error, term()}).
-type write_tail() :: fun((pdp10_stdio:file()) -> ok | {error, term()}).

-record(record_desc,
        { tag :: atom()
        , fields :: [{read_field(), write_field()}]
        , tail :: {read_tail(), write_tail()}
        }).

read_record(FP, #record_desc{tag = Tag, fields = Fields, tail = Tail}) ->
  read_record(FP, Fields, Tail, [Tag]).

read_record(FP, [{Reader, _Writer} | Fields], Tail, Values) ->
  case Reader(FP) of
    {ok, Value} ->
      read_record(FP, Fields, Tail, [Value | Values]);
    {error, _Reason} = Error ->
      Error
  end;
read_record(FP, _Fields = [], _Tail = {Reader, _Writer}, Values) ->
  case Reader(FP) of
    ok -> {ok, list_to_tuple(lists:reverse(Values))};
    {error, _Reason} = Error -> Error
  end.

write_record(FP, Record, #record_desc{tag = Tag, fields = Fields, tail = Tail}) ->
  [Tag | Values] = tuple_to_list(Record),
  write_record(FP, Fields, Tail, Values).

write_record(FP, [{_Reader, Writer} | Fields], Tail, [Value | Values]) ->
  case Writer(FP, Value) of
    ok -> write_record(FP, Fields, Tail, Values);
    {error, _Reason} = Error -> Error
  end;
write_record(FP, _Fields = [], _Tail = {_Reader, Writer}, _Values = []) ->
  Writer(FP).

%% raw archive output ==========================================================

arhdr_desc() ->
  7 = record_info(size, arhdr), % assert
  #record_desc{ tag = arhdr
              , fields =
                  [ { fun read_ar_name/1, fun write_ar_name/2 } % ar_name
                  , { fun read_ar_date/1, fun write_ar_date/2 } % ar_date
                  , { fun read_ar_uid/1,  fun write_ar_uid/2  } % ar_uid
                  , { fun read_ar_gid/1,  fun write_ar_gid/2  } % ar_gid
                  , { fun read_ar_mode/1, fun write_ar_mode/2 } % ar_mode
                  , { fun read_ar_size/1, fun write_ar_size/2 } % ar_size
                  ]
              , tail =
                    { fun read_ar_fmag/1, fun write_ar_fmag/1 } % ar_fmag
              }.

write_arhdr(FP, ArHdr) ->
  write_record(FP, ArHdr, arhdr_desc()).

write_ar_date(FP, PosixSecs) ->
  write_number(FP, PosixSecs, 10, 12).

write_ar_fmag(FP) ->
  fputs(?PDP10_ARFMAG, FP).

write_ar_gid(FP, Gid) ->
  write_number(FP, Gid, 10, 6).

write_ar_mag(FP) ->
  fputs(?PDP10_ARMAG, FP).

write_ar_mode(FP, Mode) ->
  write_number(FP, Mode, 8, 8).

write_ar_name(FP, Name0) ->
  Name =
    case Name0 of
      _ when is_integer(Name0) -> "/" ++ integer_to_list(Name0, 10);
      _ when Name0 =:= "/"; Name0 =:= "//" -> Name0;
      _ when is_list(Name0) -> Name0 ++ "/"
    end,
  write_string(FP, Name, 16).

write_ar_size(FP, Size) ->
  write_number(FP, Size, 10, 10).

write_ar_uid(FP, Uid) ->
  write_number(FP, Uid, 10, 6).

%% write Number in Base, padding with spaces to exactly FieldSize characters
write_number(FP, Number, Base, FieldSize) ->
  String = integer_to_list(Number, Base),
  write_string(FP, String, FieldSize).

%% write String, padding with spaces to exactly FieldSize characters
write_string(FP, String, FieldSize) ->
  Length = length(String),
  true = Length =< FieldSize,
  fputs(String ++ lists:duplicate(FieldSize - Length, $\s), FP).

fputs(String, FP) ->
  pdp10_stdio:fputs(String, FP).

%% raw archive input ===========================================================

read_arhdr(FP) ->
  read_record(FP, arhdr_desc()).

read_ar_date(FP) ->
  read_number(FP, 10, 12).

read_ar_fmag(FP) ->
  case read_string(FP, 2) of
    {ok, ?PDP10_ARFMAG} -> ok;
    {ok, Str} -> {error, {invalid_arfmag, Str}};
    {error, _Reason} = Error -> Error
  end.

read_ar_gid(FP) ->
  read_number(FP, 10, 6).

read_ar_mag(FP) ->
  case read_string(FP, ?PDP10_SARMAG) of
    {ok, ?PDP10_ARMAG} -> ok;
    {ok, Str} -> {error, {invalid_armag, Str}};
    {error, _Reason} = Error -> Error
  end.

read_ar_mode(FP) ->
  read_number(FP, 8, 8).

read_ar_name(FP) ->
  case read_string(FP, 16) of
    {ok, String0} ->
      String = trim_trailing_spaces(String0),
      case String of
        "/" -> {ok, String};  % archive symbol table
        "//" -> {ok, String}; % archive string table
        [$/ | Numeral] ->     % offset into archive string table
          strtol(Numeral, 10);
        _ ->
          case string:split(String ++ "$", "/") of
            [FileName, "$"] -> {ok, FileName};
            _ -> {error, {invalid_name, String}}
          end
      end;
    {error, _Reason} = Error -> Error
  end.

read_ar_size(FP) ->
  read_number(FP, 10, 10).

read_ar_uid(FP) ->
  read_number(FP, 10, 6).

read_number(FP, Base, FieldSize) ->
  case read_string(FP, FieldSize) of
    {ok, String} -> strtol(trim_trailing_spaces(String), Base);
    {error, _Reason} = Error -> Error
  end.

trim_trailing_spaces(String) ->
  string:trim(String, trailing, [$\s]).

strtol(String, Base) ->
  case strtol:parse(String, Base) of
    {ok, {Value, _Rest = []}} -> {ok, Value};
    {ok, {_Value, _Rest}} -> {error, trailing_garbage};
    {error, _Reason} = Error -> Error
  end.

%% read FieldSize characters
read_string(FP, FieldSize) ->
  case pdp10_stdio:fread(1, FieldSize, FP) of
    eof -> {error, eof};
    Result -> Result
  end.
