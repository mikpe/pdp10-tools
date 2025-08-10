%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'ar' and 'ranlib' for pdp10-elf
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

-module(ar).
-export([main/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("lib/include/pdp10_ar.hrl").
-include_lib("lib/include/archive.hrl").
-include_lib("lib/include/libelf.hrl").

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

%% Command-line interface ======================================================

-spec main([string()]) -> no_return().
main(Argv) ->
  Progname = escript_runtime:progname(),
  case string:find(Progname, "ranlib") of
    nomatch -> main_ar(Argv);
    _ -> main_ranlib(Argv)
  end.

-spec main_ranlib([string()]) -> no_return().
main_ranlib(Argv) ->
  %% In ranlib emulation mode we do not accept any options.
  case Argv of
    [[C | _] = Archive] when C =/= $- -> main_ar(["-s", Archive]);
    _ -> usage_ranlib()
  end.

usage_ranlib() ->
  Progname = escript_runtime:progname(),
  escript_runtime:fmterr(
    "Usage: ~s <archive>\n",
    [Progname]),
  halt(1).

-spec main_ar([string()]) -> no_return().
main_ar(Argv) ->
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
        ok -> ok;
        {error, Reason} -> escript_runtime:fatal("~p\n", [Reason])
      end;
    {error, Reason} ->
      escript_runtime:fatal("failed to read ~s: ~p\n", [ArchiveFile, Reason])
  end.

read_output_archive(ArchiveFile) ->
  case archive:read(ArchiveFile) of
    {ok, {_FP, _Archive}} = Result -> Result;
    {error, {file, enoent}} ->
      FP = false,
      Archive = archive:new(),
      {ok, {FP, Archive}};
    {error, _Reason} = Error -> Error
  end.

ar_dqrs(Opts, ArchiveFile, OldFP, Archive, Files) ->
  try
    {ok, NewArchive} = ar_dqrs_dispatch(Opts, Archive, Files),
    archive:write(Opts#options.mod_S, ArchiveFile, OldFP, NewArchive)
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
  case archive:read(ArchiveFile) of
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
      Status = archive:iocpy(MemberFP, ArchiveFP, SrcOffset, Size),
      pdp10_stdio:fclose(MemberFP),
      case Status of
        ok -> file:change_mode(Name, Mode band 8#0777);
        Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

%% ar --print-armap code =======================================================

ar_print_armap(ArchiveFile) ->
  case archive:read(ArchiveFile) of
    {ok, {FP, Archive}} ->
      try
        archive:print_armap(Archive)
      after
        pdp10_stdio:fclose(FP)
      end;
    {error, Reason} ->
      escript_runtime:fatal("failed to read ~s: ~p\n", [ArchiveFile, Reason])
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
        , members :: #{label() => {non_neg_integer(), [] | #member{}}}
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
    , members = maps:from_list(LabelledMembers)
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
  [{{0, 0}, {_NrAfter, []}} | ActualMembers] = lists:keysort(1, maps:to_list(Members)),
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
  NewMembers = maps:remove(Label, Members),
  NewNameToLabels =
    case maps:get(Name, NameToLabels) -- [Label] of
      [] -> maps:remove(Name, NameToLabels);
      Labels -> maps:put(Name, Labels, NameToLabels)
    end,
  LabelledArchive#labelled_archive{name_to_labels = NewNameToLabels, members = NewMembers}.

append_labelled_member(LabelledArchive, Member) ->
  #labelled_archive{name_to_labels = NameToLabels, last_label = AfterLabel, members = Members} = LabelledArchive,
  #member{arhdr = #arhdr{ar_name = Name}} = Member,
  {NrAfter, AfterMember} = maps:get(AfterLabel, Members),
  {AfterI, 0} = AfterLabel,
  NewLabel = {AfterI, NrAfter + 1},
  NameLabels = maps:get(Name, NameToLabels, []),
  NewNameLabels = ordsets:add_element(NewLabel, NameLabels),
  NewNameToLabels = maps:put(Name, NewNameLabels, NameToLabels),
  NewMembers1 = maps:update(AfterLabel, {NrAfter + 1, AfterMember}, Members),
  NewMembers2 = maps:put(NewLabel, {0, Member}, NewMembers1),
  LabelledArchive#labelled_archive{name_to_labels = NewNameToLabels, members = NewMembers2}.

update_labelled_member(LabelledArchive, Label, Member) ->
  #labelled_archive{members = Members} = LabelledArchive,
  {NrAfter, _OldMember} = maps:get(Label, Members),
  NewMembers = maps:update(Label, {NrAfter, Member}, Members),
  LabelledArchive#labelled_archive{members = NewMembers}.
