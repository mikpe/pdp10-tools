%%% -*- erlang-indent-level: 2 -*-
%%%
%%% input processing pass 1 for pdp10-elf as
%%% Copyright (C) 2013-2024  Mikael Pettersson
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

-module(input_pass1).

-export([ format_error/1
        , pass1/1
        , section_dot_comment/0
        , section_dot_text/0
        ]).

-export_type([ sectionsmap/0
             ]).

-include("tunit.hrl").
-include_lib("lib/include/pdp10_elf36.hrl").

%% Pass 1 ----------------------------------------------------------------------
%%
%% - scan, parse, annotate stmts with locations
%% - maintain current and previous section and subsection, and stack thereof
%% - interpret sectioning stmts, accumulate annotated stmts in subsections

-type sectionname() :: string().
-type subsectionnr() :: non_neg_integer().
-type locationandstmt() :: {scan:location(), stmt()}.
-type subsectionsmap() :: #{subsectionnr() => [locationandstmt()]}.
-type sectionsmap() :: #{sectionname() => {#section{}, subsectionsmap()}}.
-type sectionandsub() :: {sectionname(), subsectionnr()}.

-record(ctx,
        { sections_map :: sectionsmap()
        , stack :: [{Current :: sectionandsub(), Previous :: sectionandsub()}]
        , current :: sectionandsub()
        , previous :: sectionandsub() | []
        , stmts :: [locationandstmt()]
        }).

-spec pass1([string()]) -> {ok, sectionsmap()} | {error, {module(), term()}}.
pass1(Files) ->
  pass1_files(Files, ctx_init()).

pass1_files([], Ctx) -> {ok, ctx_fini(Ctx)};
pass1_files([File | Files], Ctx) ->
  case pass1_file(File, Ctx) of
    {ok, NewCtx} -> pass1_files(Files, NewCtx);
    {error, _Reason} = Error -> Error
  end.

pass1_file(File, Ctx) ->
  case scan_state_open(File) of
    {ok, ScanState} ->
      try pass1_process(ScanState, Ctx)
      after scan:fclose(ScanState)
      end;
    {error, _Reason} = Error -> Error
  end.

pass1_process(ScanState, Ctx) ->
  case parse:stmt(ScanState) of
    eof -> {ok, Ctx};
    {ok, {Location, Stmt}} ->
      case pass1_stmt(Location, Ctx, Stmt) of
        {ok, NewCtx} -> pass1_process(ScanState, NewCtx);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

pass1_stmt(Location, Ctx, Stmt) ->
  case Stmt of
    #s_dot_data{} -> dot_data(Location, Ctx, Stmt);
    #s_dot_popsection{} -> dot_popsection(Location, Ctx, Stmt);
    #s_dot_previous{} -> dot_previous(Location, Ctx, Stmt);
    #s_dot_section{} -> dot_section(Location, Ctx, Stmt);
    #s_dot_subsection{} -> dot_subsection(Location, Ctx, Stmt);
    #s_dot_text{} -> dot_text(Location, Ctx, Stmt);
    _ -> {ok, ctx_append(Ctx, Location, Stmt)}
  end.

dot_data(Location, Ctx, #s_dot_data{nr = SubsectionNr}) ->
  {ok, ctx_data(Ctx, Location, SubsectionNr)}.

dot_popsection(Location, Ctx0, #s_dot_popsection{}) ->
  case ctx_try_popsection(Ctx0) of
    {ok, _Ctx} = Result -> Result;
    false -> fmterr(Location, ".popsection with empty section stack", [])
  end.

dot_previous(Location, Ctx0, #s_dot_previous{}) ->
  case ctx_try_previous(Ctx0) of
    {ok, _Ctx} = Result -> Result;
    false -> fmterr(Location, ".previous with empty section stack", [])
  end.

dot_section(Location, Ctx, Stmt) ->
  #s_dot_section{ name = SectionName
                , nr = SubsectionNrOpt
                , sh_flags = ShFlags
                , sh_type = ShType
                , sh_entsize = ShEntSize
                } = Stmt,
  ctx_section(Ctx, Location, SectionName, SubsectionNrOpt,
              ShFlags, ShType, ShEntSize).

dot_subsection(_Location, Ctx, #s_dot_subsection{nr = SubsectionNr}) ->
  {ok, ctx_subsection(Ctx, SubsectionNr)}.

dot_text(Location, Ctx, #s_dot_text{nr = SubsectionNr}) ->
  {ok, ctx_text(Ctx, Location, SubsectionNr)}.

%% Context utilities
%% INV: any section name in current/previous/stack is bound in sections_map

ctx_init() ->
  SectionName = ".text",
  Section = section_dot_text(),
  SubsectionsMap = #{},
  SubsectionNr = 0,
  #ctx{ sections_map = #{SectionName => {Section, SubsectionsMap}}
      , stack = []
      , current = {SectionName, SubsectionNr}
      , previous = []
      , stmts = []
      }.

ctx_fini(Ctx) ->
  (ctx_flush(Ctx))#ctx.sections_map.

ctx_flush(Ctx) ->
  #ctx{ sections_map = SectionsMap0
      , current = {SectionName, SubsectionNr}
      , stmts = Stmts
      } = Ctx,
  {Section, SubsectionsMap0} = maps:get(SectionName, SectionsMap0), % must exist
  SubsectionsMap = maps:put(SubsectionNr, Stmts, SubsectionsMap0),
  SectionsMap = maps:put(SectionName, {Section, SubsectionsMap}, SectionsMap0),
  Ctx#ctx{sections_map = SectionsMap}.

ctx_try_popsection(Ctx0) -> % implements .popsection
  Ctx = ctx_flush(Ctx0),
  #ctx{ sections_map = SectionsMap
      , stack = Stack
      } = Ctx,
  case Stack of
    [] -> false;
    [{Current = {SectionName, SubsectionNr}, Previous} | RestStack] ->
      Stmts = get_subsection(SectionName, SubsectionNr, SectionsMap),
      {ok, Ctx#ctx{ stack = RestStack
                  , current = Current
                  , previous = Previous
                  , stmts = Stmts
                  }}
  end.

ctx_try_previous(Ctx0) -> % implements .previous
  Ctx = ctx_flush(Ctx0),
  #ctx{ sections_map = SectionsMap
      , current = Current
      , previous = Previous
      } = Ctx,
  case Previous of
    [] -> false;
    {SectionName, SubsectionNr} ->
      Stmts = get_subsection(SectionName, SubsectionNr, SectionsMap),
      {ok, Ctx#ctx{ current = Previous
                  , previous = Current
                  , stmts = Stmts
                  }}
  end.

ctx_section(Ctx0, Location, SectionName, SubsectionNrOpt,
            ShFlags, ShType, ShEntSize) ->
  {IsPushsection, SubsectionNr} =
    case SubsectionNrOpt of
      false -> {false, 0};
      _ -> {true, SubsectionNrOpt}
    end,
  Ctx = ctx_flush(Ctx0),
  #ctx{ sections_map = SectionsMap0
      , stack = Stack
      , current = Current
      , previous = Previous
      } = Ctx,
  case enter_section(Location, SectionName, SubsectionNr, SectionsMap0,
                     ShFlags, ShType, ShEntSize) of
    {ok, {Stmts, SectionsMap}} ->
      NewStack =
        case IsPushsection of
          true -> [{Current, Previous} | Stack];
          false -> Stack
        end,
      {ok, Ctx#ctx{ sections_map = SectionsMap
                  , stack = NewStack
                  , current = {SectionName, SubsectionNr}
                  , previous = Current
                  , stmts = Stmts
                  }};
    {error, _Reason} = Error -> Error
  end.

ctx_subsection(Ctx0, SubsectionNr) -> % implements .subsection <nr>
  Ctx = ctx_flush(Ctx0),
  #ctx{ sections_map = SectionsMap
      , current = Current = {SectionName, _CurSubsectionNr}
      } = Ctx,
  Stmts = enter_subsection(SectionName, SubsectionNr, SectionsMap),
  Ctx#ctx{ current = {SectionName, SubsectionNr}
         , previous = Current
         , stmts = Stmts
         }.

ctx_data(Ctx, Location, SubsectionNr) -> % implements .data <nr>
  ctx_enter_section(Ctx, Location, ".data", SubsectionNr).

ctx_text(Ctx, Location, SubsectionNr) -> % implements .text <nr>
  ctx_enter_section(Ctx, Location, ".text", SubsectionNr).

%% switch to known system section, e.g. .data or .text
ctx_enter_section(Ctx0, Location, SectionName, SubsectionNr) ->
  Ctx = ctx_flush(Ctx0),
  #ctx{ sections_map = SectionsMap0
      , current = Current
      } = Ctx,
  {ok, {Stmts, SectionsMap}} =
    enter_section(Location, SectionName, SubsectionNr, SectionsMap0),
  Ctx#ctx{ sections_map = SectionsMap
         , current = {SectionName, SubsectionNr}
         , previous = Current
         , stmts = Stmts
         }.

ctx_append(Ctx, Location, Stmt) ->
  #ctx{stmts = Stmts} = Ctx,
  Ctx#ctx{stmts = [{Location, Stmt} | Stmts]}.

enter_section(Location, SectionName, SubsectionNr, SectionsMap) ->
  enter_section(Location, SectionName, SubsectionNr, SectionsMap,
                _ShFlags = 0, _ShType = 0, _ShEntSize = 0).

enter_section(Location, SectionName, SubsectionNr, SectionsMap0,
              ShFlags, ShType, ShEntSize) ->
  {Section0, SubsectionsMap} = get_section(SectionName, SectionsMap0),
  case update_section(Location, Section0, ShFlags, ShType, ShEntSize) of
    {ok, Section} ->
      SectionsMap = maps:put(SectionName, {Section, SubsectionsMap}, SectionsMap0),
      Stmts = maps:get(SubsectionNr, SubsectionsMap, []),
      {ok, {Stmts, SectionsMap}};
    {error, _Reason} = Error -> Error
  end.

get_section(SectionName, SectionsMap) ->
  case maps:get(SectionName, SectionsMap, false) of
    {_Section, _SubsectionsMap} = Result -> Result;
    false ->
      case section_from_name(SectionName) of
        #section{} = Section ->
          SubsectionsMap = #{},
          {Section, SubsectionsMap};
        false ->
          Section = make_section(SectionName),
          SubsectionsMap = #{},
          {Section, SubsectionsMap}
      end
  end.

make_section(SectionName) ->
  %% update_section/5 will set sh_flags, sh_type, and sh_entsize
  #section{ name = SectionName
          , data = {stmts, []}
          , dot = 0
          , shndx = 0
          , sh_name = 0
          , sh_type = 0
          , sh_offset = 0
          , sh_flags = 0
          , sh_link = ?SHN_UNDEF
          , sh_info = 0
          , sh_addralign = 1
          , sh_entsize = 0
          }.

update_section(Location, Section0, ShFlags, ShType, ShEntSize) ->
  case update_sh_flags(Location, Section0, ShFlags) of
    {ok, Section1} ->
      case update_sh_type(Location, Section1, ShType) of
        {ok, Section2} ->
          update_sh_entsize(Location, Section2, ShEntSize);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

update_sh_flags(Location, Section, ShFlags) ->
  case ShFlags of
    0 -> {ok, Section};
    _ ->
      case Section#section.sh_flags of
        0 -> {ok, Section#section{sh_flags = ShFlags}};
        ShFlags -> {ok, Section};
        ShFlags0 ->
          case may_update_sh_flags(Section, ShFlags) of
            true -> {ok, Section#section{sh_flags = ShFlags0 bor ShFlags}};
            false -> fmterr(Location, "cannot change section flags", [])
          end
      end
  end.

may_update_sh_flags(Section, ShFlags) ->
  %% Processor and application-specific flags may be added to an existing
  %% section.  The range of application-specific flags isn't defined in the
  %% ELF spec: we interpret that as any flag outside of the reserved ranges.
  ReservedMask = (?SHF_COMPRESSED * 2 - 1) bor ?SHF_MASKOS,
  case (ShFlags band ReservedMask) =:= 0 of
    true -> true;
    false ->
      case ShFlags of
         ?SHF_ALLOC ->
           case Section#section.name of
             ".interp" -> true;
             ".strtab" -> true;
             ".symtab" -> true;
             _         -> false
           end;
         ?SHF_EXECINSTR -> Section#section.name =:= ".note.GNU-stack";
         _ -> false
      end
  end.

update_sh_type(Location, Section, ShType) ->
  case ShType of
    0 -> {ok, Section};
    _ ->
      case Section#section.sh_type of
        0 -> {ok, Section#section{sh_type = ShType}};
        ShType -> {ok, Section};
        _ -> fmterr(Location, "cannot change section type", [])
      end
  end.

update_sh_entsize(Location, Section, ShEntSize) ->
  case ShEntSize of
    0 -> {ok, Section};
    _ ->
      case Section#section.sh_entsize of
        0 -> {ok, Section#section{sh_entsize = ShEntSize}};
        ShEntSize -> {ok, Section};
        _ -> fmterr(Location, "cannot change section element size", [])
      end
  end.

enter_subsection(SectionName, SubsectionNr, SectionsMap) ->
  {_Section, SubsectionsMap} = maps:get(SectionName, SectionsMap), % must exist
  maps:get(SubsectionNr, SubsectionsMap, []). % may be absent

get_subsection(SectionName, SubsectionNr, SectionsMap) ->
  {_Section, SubsectionsMap} = maps:get(SectionName, SectionsMap), % must exist
  maps:get(SubsectionNr, SubsectionsMap). % must exist

%% Scan state utilities

scan_state_open(File) ->
  case File of
    "--" -> scan:stdin();
    "-" -> scan:stdin();
    _ -> scan:fopen(File)
  end.

%% Predefined Sections ---------------------------------------------------------

section_from_name(SectionName) ->
  case SectionName of
    ".data" -> section_dot_data();
    ".rodata" -> section_dot_rodata();
    ".text" -> section_dot_text();
    _ -> false
  end.

-spec section_dot_comment() -> #section{}.
section_dot_comment() -> % ".comment"
  #section{ name = ".comment"
          , data = {stmts, []}
          , dot = 0
          , shndx = 0
          , sh_name = 0
          , sh_type = ?SHT_PROGBITS
          , sh_offset = 0
          , sh_flags = ?SHF_MERGE bor ?SHF_STRINGS
          , sh_link = ?SHN_UNDEF
          , sh_info = 0
          , sh_addralign = 1
          , sh_entsize = 1
          }.

-spec section_dot_data() -> #section{}.
section_dot_data() -> % ".data"
  #section{ name = ".data"
          , data = {stmts, []}
          , dot = 0
          , shndx = 0
          , sh_name = 0
          , sh_type = ?SHT_PROGBITS
          , sh_offset = 0
          , sh_flags = ?SHF_ALLOC bor ?SHF_WRITE
          , sh_link = ?SHN_UNDEF
          , sh_info = 0
          , sh_addralign = 4 % FIXME: target-specific
          , sh_entsize = 0
          }.

-spec section_dot_rodata() -> #section{}.
section_dot_rodata() -> % ".rodata"
  #section{ name = ".rodata"
          , data = {stmts, []}
          , dot = 0
          , shndx = 0
          , sh_name = 0
          , sh_type = ?SHT_PROGBITS
          , sh_offset = 0
          , sh_flags = ?SHF_ALLOC
          , sh_link = ?SHN_UNDEF
          , sh_info = 0
          , sh_addralign = 4 % FIXME: target-specific
          , sh_entsize = 0
          }.

-spec section_dot_text() -> #section{}.
section_dot_text() -> % ".text"
  #section{ name = ".text"
          , data = {stmts, []}
          , dot = 0
          , shndx = 0
          , sh_name = 0
          , sh_type = ?SHT_PROGBITS
          , sh_offset = 0
          , sh_flags = ?SHF_ALLOC bor ?SHF_EXECINSTR
          , sh_link = ?SHN_UNDEF
          , sh_info = 0
          , sh_addralign = 4 % FIXME: target-specific
          , sh_entsize = 0
          }.

%% Error reporting -------------------------------------------------------------

fmterr({FileName, LineNr}, Fmt, Args) ->
  {error, {?MODULE, {FileName, LineNr, Fmt, Args}}}.

-spec format_error(term()) -> io_lib:chars().
format_error({FileName, LineNr, Fmt, Args}) ->
  io_lib:format("file ~s line ~p: " ++ Fmt, [FileName, LineNr | Args]).
