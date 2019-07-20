%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'ar' clone for PDP10
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
%%%
%%%=============================================================================
%%%
%%% Requirements:
%%%
%%% - members are ordered as stored in the archive
%%% - members can be appended at the end of the archive, inserted before or
%%%   after another named member, updated in place, or deleted
%%% - there can be multiple members with the same name
%%%
%%% Recall that Erlang orders lists lexicographically.
%%%
%%% Define a label to be a non-empty list of integers.
%%%
%%% Members in the pre-existing archive are labelled with their positions I in
%%% the archive, as singleton lists [I].
%%%
%%% Members appended after some pre-existing member are labelled [I,J+1], where
%%% [I] is the label of the pre-existing member, and J is the number of newly
%%% appended members after [I].
%%%
%%% Members inserted before a pre-existing member with label [I] are treated as
%%% if appended after a member with label [I-1].
%%%
%%% Members appended at the end of the archive are treated as if appended after
%%% the pre-existing archive's last member, with label [N].  For an empty
%%% archive the label of the imaginary last member is defined to be [0].
%%%
%%% The in-core version of an archive stores the members in a gb_tree with their
%%% labels as keys.  A separate structure maps each member name to an ordered
%%% list of the labels of its occurrences in the archive.

-module(ar).
-export([main/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("ar/include/pdp10_ar.hrl").

%% in-core version of the ar header
-record(arhdr,
        { ar_name       :: string() | non_neg_integer()
        , ar_date       :: non_neg_integer()
        , ar_uid        :: non_neg_integer()
        , ar_gid        :: non_neg_integer()
        , ar_mode       :: non_neg_integer()
        , ar_size       :: non_neg_integer()
        }).

-record(member,
        { arhdr         :: #arhdr{}
        , data          :: non_neg_integer() % at this offset in old archive
                         | string()          % in this external file
        }).

-type label() :: nonempty_list(integer()).

-record(archive,
        { symtab        % TODO: implement SymTab
        , members       :: gb_trees:tree(label(),
                                         {non_neg_integer(), #member{} | []})
        , labelmap      :: #{string() => nonempty_list(label())}
        }).

-record(options,
        { operation     % d, q, r, t, or x
        , mod_c = false % true iff c modifier present
        , mod_u = false % true iff u modifier present
        , mod_v = false % true iff v modifier present
        , mod_D = false % true iff D modifier present
        , mod_o = false % true iff o modifier present
        , mod_O = false % true iff O modifier present
        }).

-type file() :: pdp10_stdio:file().

%% Command-line interface ======================================================

main(Argv) ->
  escript_runtime:start(fun main_/1, Argv).

main_(Argv) ->
  case parse_argv(Argv) of
    {ok, {Opts, ArchiveFile, Files}} ->
      ar(Opts, ArchiveFile, Files),
      halt(0);
    {error, ErrMsg} ->
      escript_runtime:errmsg("~s\n", [ErrMsg]),
      usage()
  end.

usage() ->
  escript_runtime:fmterr(
    "Usage: ~s [-]{d,q,r,t,x}[cuvV] <archive> <member..>\n",
    [escript_runtime:progname()]),
  halt(1).

parse_argv([[$- | Arg] | Argv]) -> parse_operation(Arg, Argv);
parse_argv([Arg | Argv]) -> parse_operation(Arg, Argv);
parse_argv([]) -> {error, "no operation specified"}.

parse_operation(Arg, Argv) ->
  %% m - NYI
  %% p - NYI
  %% s - NYI - TODO
  case Arg of
    [$d | Mod] -> parse_modifiers(Mod, Argv, $d, []);
    [$q | Mod] -> parse_modifiers(Mod, Argv, $q, []); % TODO: f
    [$r | Mod] -> parse_modifiers(Mod, Argv, $r, [$u]); % TODO: a, b/i, f
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
  %% s - NYI - TODO
  %% S - NYI - TODO
  %% T - NYI
  case C of
    $c -> {ok, Opts#options{mod_c = true}};
    $D -> {ok, Opts#options{mod_D = true}};
    $U -> {ok, Opts#options{mod_D = false}};
    $o -> check_opmods($o, OpMods, Opts#options{mod_o = true});
    $O -> check_opmods($O, OpMods, Opts#options{mod_O = true});
    $u -> check_opmods($u, OpMods, Opts#options{mod_u = true});
    $v -> {ok, Opts#options{mod_v = true}};
    $V -> version();
    _ -> {error, io_lib:format("invalid modifier: ~c", [C])}  end.

check_opmods(C, OpMods, Opts) ->
  case lists:member(C, OpMods) of
    true -> {op, Opts};
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
    Op when Op =:= $d; Op =:= $q; Op =:= $r ->
      ar_dqr(Opts, ArchiveFile, Files);
    Op when Op =:= $t; Op =:= $x ->
      ar_tx(Opts, ArchiveFile, Files)
  end.

%% ar d/q/r code ===============================================================

ar_dqr(Opts, ArchiveFile, Files) ->
  case read_output_archive(ArchiveFile) of
    {ok, {FP, Archive}} ->
      case ar_dqr(Opts, ArchiveFile, FP, Archive, Files) of
        {ok, TmpFile} -> file:rename(TmpFile, ArchiveFile);
        {error, Reason} -> escript_runtime:fatal("~p\n", [Reason])
      end;
    {error, Reason} ->
      escript_runtime:fatal("failed to read ~s: ~p\n", [ArchiveFile, Reason])
  end.

read_output_archive(ArchiveFile) ->
  case read_archive_file(ArchiveFile) of
    {ok, {_FP, _Archive}} = Result -> Result;
    {error, enoent} ->
      FP = [],
      Archive = make_archive(symtab_none(), _Members = []),
      {ok, {FP, Archive}};
    {error, _Reason} = Error -> Error
  end.

ar_dqr(Opts, ArchiveFile, OldFP, Archive, Files) ->
  try
    case ar_dqr_dispatch(Opts, Archive, Files) of
      {ok, NewArchive} -> write_tmp_archive(ArchiveFile, OldFP, NewArchive);
      {error, _Reason} = Error -> Error
    end
  after
    case OldFP of
      [] -> ok;
      _ -> pdp10_stdio:fclose(OldFP)
    end
  end.

ar_dqr_dispatch(Opts, Archive, Files) ->
  case Opts#options.operation of
    $d -> ar_d(Opts, Archive, Files);
    $q -> ar_q(Opts, Archive, Files);
    $r -> ar_r(Opts, Archive, Files)
  end.

ar_d(Opts, Archive, Files) ->
  NewArchive =
    lists:foldl(fun(File, Archive0) ->
                  ar_d_1(Opts, Archive0, File)
                end, Archive, Files),
  {ok, NewArchive}.

ar_d_1(Opts, Archive, File) ->
  Name = filename:basename(File),
  case archive_lookup_label(Archive, Name) of
    false ->
      case Opts#options.mod_v of
        true -> io:format(standard_io, "No member named ~s~n", [File]);
        false -> ok
      end,
      Archive;
    Label ->
      case Opts#options.mod_v of
        true -> io:format(standard_io, "d - ~s~n", [File]);
        false -> ok
      end,
      archive_delete_member(Archive, Name, Label)
  end.

ar_q(Opts, Archive, Files) ->
  LastLabel = archive_last_label(Archive),
  NewArchive =
    lists:foldl(fun(File, Archive0) ->
                   ar_q_1(Opts, Archive0, LastLabel, File)
                end, Archive, Files),
  {ok, NewArchive}.

ar_q_1(Opts, Archive, LastLabel, File) ->
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
      Member = #member{arhdr = ArHdr, data = File},
      case Opts#options.mod_v of
        true -> io:format(standard_io, "a - ~s~n", [File]);
        false -> ok
      end,
      %% FIXME: this differs from GNU ar which treats 'ar qs' as 'ar r',
      %% i.e. performing in-place replacement of existing members
      archive_insert_member_after(Archive, LastLabel, Member);
    {error, Reason} ->
      escript_runtime:fatal("~s: ~s~n", [File, file:format_error(Reason)])
  end.

ar_r(Opts, Archive, Files) ->
  LastLabel = archive_last_label(Archive),
  NewArchive =
    lists:foldl(fun(File, Archive0) ->
                   ar_r_1(Opts, Archive0, LastLabel, File)
                end, Archive, Files),
  {ok, NewArchive}.

ar_r_1(Opts, Archive, LastLabel, File) ->
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
      Member = #member{arhdr = ArHdr, data = File},
      %% FIXME: this doesn't match GNU ar when duplicate Names occur
      case archive_lookup_label(Archive, Name) of
        false ->
          case Opts#options.mod_v of
            true -> io:format(standard_io, "a - ~s~n", [File]);
            false -> ok
          end,
          archive_insert_member_after(Archive, LastLabel, Member);
        Label ->
          case Opts#options.mod_v of
            true -> io:format(standard_io, "r - ~s~n", [File]);
            false -> ok
          end,
          archive_update_member(Archive, Label, Member)
      end;
    {error, Reason} ->
      escript_runtime:fatal("~s: ~s~n", [File, file:format_error(Reason)])
  end.

%% ar t/x code =================================================================

ar_tx(Opts, ArchiveFile, Files) ->
  FileSet = ar_tx_fileset_from_list(Files),
  case read_archive_file(ArchiveFile) of
    {ok, {FP, Archive}} ->
      try
        case ar_tx_loop(Opts, FP, archive_members_iterator(Archive), FileSet) of
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
  case members_iterator_next(Members) of
    none ->
      {ok, ar_tx_fileset_to_list(FileSet)};
    {_Label, Member, RestMembers} ->
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

fileset_delete(File, FileSet) -> gb_sets:delete(File, FileSet).
fileset_from_list(Files) -> gb_sets:from_list(Files).
fileset_is_element(File, FileSet) -> gb_sets:is_element(File, FileSet).
fileset_to_list(FileSet) -> gb_sets:to_list(FileSet).

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
         , data = SrcOffset
         } = Member,
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

%% archive output ==============================================================

write_tmp_archive(ArchiveFile, OldFP, Archive) ->
  {StrTab, RawArchive} = archive_strtabify(Archive),
  case mkstemp(filename:dirname(ArchiveFile), ".artmp") of
    {ok, {TmpFile, TmpFP}} ->
      try
        case write_archive(TmpFP, StrTab, RawArchive, OldFP) of
          ok -> {ok, TmpFile};
          {error, _Reason} = Error -> Error
        end
      after
        pdp10_stdio:fclose(TmpFP)
      end;
    {error, _Reason} = Error -> Error
  end.

archive_strtabify(Archive) ->
  {NewArchive, {_Offset, StrTabRev}} =
    archive_members_mapfoldl(Archive, {0, []}, fun member_strtabify/2),
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

write_archive(DstFP, StrTab, RawArchive, OldFP) ->
  %% FIXME: handle SymTab
  case write_ar_mag(DstFP) of
    {error, _Reason} = Error -> Error;
    ok ->
      case write_strtab(DstFP, StrTab) of
        {error, _Reason} = Error -> Error;
        ok ->
          Members = archive_members_iterator(RawArchive),
          write_members(DstFP, Members, OldFP)
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
  case members_iterator_next(Members) of
    none -> ok;
    {_Label, Member, RestMembers} ->
      case write_member(DstFP, Member, OldFP) of
        {error, _Reason} = Error -> Error;
        ok -> write_members(DstFP, RestMembers, OldFP)
      end
  end.

write_member(DstFP, Member, OldFP) ->
  #member{arhdr = ArHdr, data = SrcData} = Member,
  #arhdr{ar_size = Size} = ArHdr,
  case write_arhdr(DstFP, ArHdr) of
    {error, _Reason} = Error -> Error;
    ok ->
      case write_member_data(DstFP, Size, SrcData, OldFP) of
        {error, _Reason} = Error -> Error;
        ok -> write_padding(DstFP, Size)
      end
  end.

write_member_data(DstFP, Size, SrcOffset, OldFP) when is_integer(SrcOffset) ->
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
          case read_symtab(FP, ArHdr) of
            {ok, SymTab} -> read_archive_strtab(FP, SymTab);
            {error, _Reason} = Error -> Error
          end;
        _ -> read_archive_strtab(FP, symtab_none(), ArHdr)
      end;
    {error, eof} ->
      {ok, make_archive(symtab_none(), _Members = [])};
    {error, _Reason} = Error -> Error
  end.

read_archive_strtab(FP, SymTab) ->
  case read_arhdr(FP) of
    {ok, ArHdr} -> read_archive_strtab(FP, SymTab, ArHdr);
    {error, eof} ->
      {ok, make_archive(SymTab, _Members = [])};
    {error, _Reason} = Error -> Error
  end.

read_archive_strtab(FP, SymTab, ArHdr) ->
  case ArHdr#arhdr.ar_name of
    "//" ->
      case read_strtab(FP, ArHdr) of
        {ok, StrTab} -> read_archive_members(FP, SymTab, StrTab, []);
        {error, _Reason} = Error -> Error
      end;
    _ -> read_archive_members(FP, SymTab, strtab_none(), [], ArHdr)
  end.

read_archive_members(FP, SymTab, StrTab, Members) ->
  case read_arhdr(FP) of
    {ok, ArHdr} ->
      read_archive_members(FP, SymTab, StrTab, Members, ArHdr);
    {error, eof} ->
      {ok, make_archive(SymTab, lists:reverse(Members))};
    {error, _Reason} = Error ->
      Error
  end.

read_archive_members(FP, SymTab, StrTab, Members, ArHdr) ->
  case finalise_ar_name(StrTab, ArHdr#arhdr.ar_name) of
    {ok, Name} ->
      SrcOffset = pdp10_stdio:ftell(FP),
      Member = #member{arhdr = ArHdr#arhdr{ar_name = Name},
                       data = SrcOffset},
      NewMembers = [Member | Members],
      case skip_member(FP, ArHdr#arhdr.ar_size) of
        ok ->
          read_archive_members(FP, SymTab, StrTab, NewMembers);
        eof ->
          {ok, make_archive(SymTab, lists:reverse(NewMembers))};
        {error, _Reason} = Error ->
          Error
      end;
    {error, _Reason} = Error -> Error
  end.

finalise_ar_name(_StrTab, Name) when is_list(Name) -> {ok, Name};
finalise_ar_name(StrTab, Offset) when is_integer(Offset) ->
  case strtab_lookup(StrTab, Offset) of
    {ok, _Name} = Result -> Result;
    false -> {error, invalid_strtab_offset}
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

%% cooked archives =============================================================

make_archive(SymTab, Members) ->
  {LabelledMembers, LabelMap} = make_archive(Members),
  #archive{ symtab = SymTab
          , members = gb_trees:from_orddict(lists:reverse(LabelledMembers))
          , labelmap = maps:map(fun(_Name, Labels) -> lists:reverse(Labels) end,
                                LabelMap)
          }.

make_archive(Members) ->
  make_archive(Members, 1, [{[0], {0, []}}], maps:new()).

make_archive([], _Index, LabelledMembers, LabelMap) ->
  {LabelledMembers, LabelMap};
make_archive([Member | Members], Index, LabelledMembers, LabelMap) ->
  Name = Member#member.arhdr#arhdr.ar_name,
  Label = [Index],
  NameLabels = maps:get(Name, LabelMap, []),
  NewLabelMap = maps:put(Name, [Label | NameLabels], LabelMap),
  NewLabelledMembers = [{Label, {0, Member}} | LabelledMembers],
  make_archive(Members, Index + 1, NewLabelledMembers, NewLabelMap).

archive_members_iterator(#archive{members = Members}) ->
  {[0], {_NrRight, []}, Iterator} = gb_trees:next(gb_trees:iterator(Members)),
  Iterator.

members_iterator_next(Iterator) ->
  case gb_trees:next(Iterator) of
    none -> none;
    {Label, {_NrRight, Member}, NewIterator} -> {Label, Member, NewIterator}
  end.

archive_last_label(#archive{members = Members}) ->
  {LastLabel, {_NrRight, _Member}} = gb_trees:largest(Members),
  LastLabel.

archive_lookup_label(Archive, Name) ->
  #archive{labelmap = LabelMap} = Archive,
  case maps:find(Name, LabelMap) of
    {ok, [Label | _Labels]} -> Label; % TODO: handle "N count"
    error -> false
  end.

archive_delete_member(Archive, Name, Label) ->
  #archive{members = Members, labelmap = LabelMap} = Archive,
  NewMembers = gb_trees:delete(Label, Members),
  NewLabelMap =
    case maps:get(Name, LabelMap) -- [Label] of
      [] -> maps:remove(Name, LabelMap);
      Labels -> maps:put(Name, Labels, LabelMap)
    end,
  Archive#archive{ symtab = symtab_none()
                 , members = NewMembers
                 , labelmap = NewLabelMap
                 }.

archive_insert_member_after(Archive, AfterLabel, Member) ->
  #archive{members = Members, labelmap = LabelMap} = Archive,
  Name = Member#member.arhdr#arhdr.ar_name,
  {NrRight, AfterMember} = gb_trees:get(AfterLabel, Members),
  NewLabel = AfterLabel ++ [NrRight + 1],
  NameLabels = maps:get(Name, LabelMap, []),
  NewNameLabels = ordsets:add_element(NewLabel, NameLabels),
  NewLabelMap = maps:put(Name, NewNameLabels, LabelMap),
  NewMembers = gb_trees:insert(NewLabel, {0, Member},
                               gb_trees:update(AfterLabel,
                                               {NrRight + 1, AfterMember},
                                               Members)),
  Archive#archive{ symtab = symtab_none()
                 , members = NewMembers
                 , labelmap = NewLabelMap
                 }.

archive_update_member(Archive, Label, Member) ->
  #archive{members = Members} = Archive,
  NewMembers = gb_trees:update(Label, Member, Members),
  Archive#archive{symtab = symtab_none(), members = NewMembers}.

archive_members_mapfoldl(Archive, Init, Fun) ->
  [HiddenMember = {[0], {_, []}} | OrigMembers] =
    gb_trees:to_list(Archive#archive.members),
  {UpdatedMembers, Result} =
    lists:mapfoldl(fun({Label, {NrRight, Member}}, Acc) ->
                     {NewMember, NewAcc} = Fun(Member, Acc),
                     {{Label, {NrRight, NewMember}}, NewAcc}
                   end, Init, OrigMembers),
  NewMembers = gb_trees:from_orddict([HiddenMember | UpdatedMembers]),
  {Archive#archive{members = NewMembers}, Result}.

%% symbol table ================================================================

-define(WORDSIZE, 4).

read_symtab(FP, #arhdr{ar_size = Size}) ->
  true = Size >= ?WORDSIZE, % assert
  case read_word_be(FP) of
    {ok, NrSymbols} ->
      true = Size >= (NrSymbols + 1) * ?WORDSIZE, % assert
      case read_words_be(FP, NrSymbols) of
        {ok, Offsets} ->
          case read_string(FP, Size - (NrSymbols + 1) * ?WORDSIZE) of
           {ok, StrBuf} ->
             case read_padding(FP, Size) of
               {error, _Reason} = Error -> Error;
               _ -> make_symtab(Offsets, StrBuf) % ok or eof
             end;
           {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

make_symtab(Offsets, StrBuf) ->
  case split_strbuf(StrBuf) of
    {ok, Names} ->
      case safe_zip(Offsets, Names) of
        {ok, OffsetNamePairs} ->
          lists:foldl(fun({Offset, Name}, SymTab) ->
                        symtab_insert(SymTab, Name, Offset)
                      end, symtab_new(), OffsetNamePairs);
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

symtab_none() -> [].
symtab_new() -> gb_trees:empty().
symtab_insert(SymTab, Name, Offset) -> gb_trees:insert(Name, Offset, SymTab).
-ifdef(notdef).
symtab_lookup(SymTab, Name) when SymTab =/= [] ->
  case gb_trees:lookup(SymTab, Name) of
    {value, Offset} -> {ok, Offset};
    none -> false
  end.
-endif.

read_words_be(FP, NrWords) -> read_words_be(FP, NrWords, []).

read_words_be(_FP, 0, Words) -> {ok, lists:reverse(Words)};
read_words_be(FP, N, Words) ->
  case read_word_be(FP) of
    {ok, Word} -> read_words_be(FP, N - 1, [Word | Words]);
    {error, _Reason} = Error -> Error
  end.

%% FIXME: functionally equivalent to nm:pdp10_elf36_read_uint36/1
read_word_be(FP) -> read_word_be(FP, ?WORDSIZE, []).

read_word_be(_FP, 0, [B4, B3, B2, B1]) ->
  {ok, ((B1 band 16#1FF) bsl 27) bor
       ((B2 band 16#1FF) bsl 19) bor
       ((B3 band 16#1FF) bsl  9) bor
        (B4 band 16#1FF)};
read_word_be(FP, N, Acc) ->
  case pdp10_stdio:fgetc(FP) of
    {ok, Byte} -> read_word_be(FP, N - 1, [Byte | Acc]);
    eof -> {error, invalid_symbol_table};
    {error, _Reason} = Error -> Error
  end.

%% string table ================================================================

read_strtab(FP, #arhdr{ar_size = Size}) ->
  case read_string(FP, Size) of
    {ok, String} ->
      case read_padding(FP, Size) of
        ok -> scan_strtab(String);
        eof -> {ok, strtab_none()};
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

scan_strtab(String) -> scan_strtab(String, 0, strtab_new()).

scan_strtab([], _Offset, StrTab) -> {ok, StrTab};
scan_strtab([16#0A], _Offset, StrTab) -> {ok, StrTab};
scan_strtab(String, Offset, StrTab) ->
  case split_strtab(String) of
    {ok, {First, Rest}} ->
      scan_strtab(Rest, Offset + length(First) + 2,
                  strtab_insert(StrTab, Offset, First));
    {error, _Reason} = Error -> Error
  end.

split_strtab(String) -> split_strtab(String, []).

split_strtab([16#2F, 16#0A | Rest], First) ->
  {ok, {lists:reverse(First), Rest}};
split_strtab([16#2F | _Rest], _First) ->
  {error, invalid_strtab_entry_terminator};
split_strtab([16#0A | _Rest], _First) ->
  {error, invalid_strtab_entry_terminator};
split_strtab([Ch | Rest], First) ->
  split_strtab(Rest, [Ch | First]).

strtab_none() -> [].
strtab_new() -> gb_trees:empty().
strtab_insert(StrTab, Offset, String) -> gb_trees:insert(Offset, String, StrTab).
strtab_lookup(StrTab, Offset) when StrTab =/= [] ->
  case gb_trees:lookup(StrTab, Offset) of
    {value, String} -> {ok, String};
    none -> false % ar_name doesn't match the start of a strtab entry
  end.

%% descriptor-based record I/O =================================================

-type read_field() :: fun((pdp10_stdio:file()) -> {ok, term()} | {error, term()}).
-type write_field() :: fun((pdp10_stdio:file(), term()) -> ok | {error, term()}).
-type read_tail() :: fun((pdp10_stdio:file()) -> ok | {error, term()}).
-type write_tail() :: fun((pdp10_stdio:file()) -> ok | {error, term()}).

-record(record_desc,
          { tag :: atom()
          , fields :: [{read_field(), write_field()}]
          , tail = [] :: [] | {read_tail(), write_tail()}
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
read_record(FP, _Fields = [], Tail, Values) ->
  case Tail of
    [] -> ok;
    {Reader, _Writer} ->
      case Reader(FP) of
        ok -> ok;
        {error, _Reason} = Error -> Error
      end
  end,
  {ok, list_to_tuple(lists:reverse(Values))}.

write_record(FP, Record, #record_desc{tag = Tag, fields = Fields, tail = Tail}) ->
  [Tag | Values] = tuple_to_list(Record),
  write_record(FP, Fields, Tail, Values).

write_record(FP, [{_Reader, Writer} | Fields], Tail, [Value | Values]) ->
  case Writer(FP, Value) of
    ok -> write_record(FP, Fields, Tail, Values);
    {error, _Reason} = Error -> Error
  end;
write_record(_FP, _Fields = [], _Tail = [], _Values = []) -> ok;
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
    {ok, Str} -> {errror, {invalid_armag, Str}};
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
          case strtol:parse(Numeral, 10) of
            {ok, Value, []} -> {ok, Value};
            {ok, _Value, _} -> {error, trailing_garbage};
            {error, _Reason} = Error -> Error
          end;
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
    {ok, String} ->
      case strtol:parse(trim_trailing_spaces(String), Base) of
        {ok, Value, []} -> {ok, Value};
        {ok, _Value, _} -> {error, trailing_garbage};
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

trim_trailing_spaces(String) ->
  string:trim(String, trailing, [$\s]).

%% read FieldSize characters
read_string(FP, FieldSize) ->
  case pdp10_stdio:fread(1, FieldSize, FP) of
    eof -> {error, eof};
    Result -> Result
  end.
