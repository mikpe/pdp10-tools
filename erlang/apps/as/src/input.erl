%%% -*- erlang-indent-level: 2 -*-
%%%
%%% input processing phase for pdp10-elf as
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

-module(input).

-export([ files/1
        , format_error/1
        ]).

-include("tunit.hrl").
-include_lib("lib/include/pdp10_elf36.hrl").

-spec files([string()]) -> {ok, #tunit{}} | {error, {module(), term()}}.
files(Files0) ->
  Files =
    case Files0 of
      [] -> ["--"]; % alias for stdin
      _ -> Files0
    end,
  pass1(Files).

%% Pass 1 ----------------------------------------------------------------------
%%
%% - scan, parse, annotate stmts with locations
%% - maintain current and previous section and subsection, and stack thereof
%% - interpret sectioning stmts, accumulate annotated stmts in subsections

-type section() :: string().
-type subsection() :: non_neg_integer().

-type sectionandsub() :: {section(), subsection()}.

-record(ctx,
        { sections_map :: #{section() => #{subsection() => [stmt()]}}
        , stack :: [{Current :: sectionandsub(), Previous :: sectionandsub()}]
        , current :: sectionandsub()
        , previous :: sectionandsub() | []
        , stmts :: [{scan_state:location(), stmt()}]
        }).

pass1(Files) ->
  pass1_files(Files, ctx_init()).

pass1_files([], Ctx) -> pass2(ctx_fini(Ctx));
pass1_files([File | Files], Ctx) ->
  case pass1_file(File, Ctx) of
    {ok, NewCtx} -> pass1_files(Files, NewCtx);
    {error, _Reason} = Error -> Error
  end.

pass1_file(File, Ctx) ->
  case scan_state_open(File) of
    {ok, ScanState} ->
      try pass1_process(ScanState, Ctx)
      after scan_state:fclose(ScanState)
      end;
    {error, _Reason} = Error -> Error
  end.

pass1_process(ScanState, Ctx) ->
  case parse:stmt(ScanState) of
    eof -> {ok, Ctx};
    {ok, Stmt} ->
      case pass1_stmt(scan_state_location(ScanState), Ctx, Stmt) of
        {ok, NewCtx} -> pass1_process(ScanState, NewCtx);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

pass1_stmt(Location, Ctx, Stmt) ->
  case Stmt of
    #s_dot_popsection{} -> dot_popsection(Location, Ctx, Stmt);
    #s_dot_previous{} -> dot_previous(Location, Ctx, Stmt);
    #s_dot_pushsection{} -> dot_pushsection(Location, Ctx, Stmt);
    #s_dot_subsection{} -> dot_subsection(Location, Ctx, Stmt);
    #s_dot_text{} -> dot_text(Location, Ctx, Stmt);
    _ -> {ok, ctx_append(Ctx, Location, Stmt)}
  end.

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

dot_pushsection(_Location, Ctx, #s_dot_pushsection{name = Section, nr = Subsection}) ->
  {ok, ctx_pushsection(Ctx, Section, Subsection)}.

dot_subsection(_Location, Ctx, #s_dot_subsection{nr = Subsection}) ->
  {ok, ctx_subsection(Ctx, Subsection)}.

dot_text(_Location, Ctx, #s_dot_text{nr = Subsection}) ->
  {ok, ctx_text(Ctx, Subsection)}.

%% Context utilities

ctx_init() ->
  InitialSection = ".text",
  InitialSubsection = 0,
  #ctx{ sections_map = #{InitialSection => #{}}
      , stack = []
      , current = {InitialSection, InitialSubsection}
      , previous = []
      , stmts = []
      }.

ctx_fini(Ctx) ->
  (ctx_flush(Ctx))#ctx.sections_map.

ctx_flush(Ctx) ->
  #ctx{ sections_map = SectionsMap0
      , current = {Section, Subsection}
      , stmts = Stmts
      } = Ctx,
  SubsectionsMap0 = maps:get(Section, SectionsMap0, #{}),
  SubsectionsMap = maps:put(Subsection, Stmts, SubsectionsMap0),
  SectionsMap = maps:put(Section, SubsectionsMap, SectionsMap0),
  Ctx#ctx{sections_map = SectionsMap}.

ctx_try_popsection(Ctx0) -> % implements .popsection
  Ctx = ctx_flush(Ctx0),
  #ctx{ sections_map = SectionsMap
      , stack = Stack
      } = Ctx,
  case Stack of
    [] -> false;
    [{Current = {Section, Subsection}, Previous} | RestStack] ->
      SubsectionsMap = maps:get(Section, SectionsMap), % must exist
      Stmts = maps:get(Subsection, SubsectionsMap), % must exist
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
    {Section, Subsection} ->
      SubsectionsMap = maps:get(Section, SectionsMap), % must exist
      Stmts = maps:get(Subsection, SubsectionsMap), % must exist
      {ok, Ctx#ctx{ current = Previous
                  , previous = Current
                  , stmts = Stmts
                  }}
  end.

ctx_pushsection(Ctx0, Section, Subsection) -> % implements .pushsection
  Ctx = ctx_flush(Ctx0),
  #ctx{ sections_map = SectionsMap
      , stack = Stack
      , current = Current
      , previous = Previous
      } = Ctx,
  SubsectionsMap = maps:get(Section, SectionsMap, #{}),
  Stmts = maps:get(Subsection, SubsectionsMap, []),
  Ctx#ctx{ stack = [{Current, Previous} | Stack]
         , current = {Section, Subsection}
         , previous = Current
         , stmts = Stmts
         }.

ctx_subsection(Ctx0, Subsection) -> % implements .subsection <nr>
  Ctx = ctx_flush(Ctx0),
  #ctx{ sections_map = SectionsMap
      , current = Current = {Section, _CurSubsection}
      } = Ctx,
  SubsectionsMap = maps:get(Section, SectionsMap), % must exist
  Stmts = maps:get(Subsection, SubsectionsMap, []),
  Ctx#ctx{ current = {Section, Subsection}
         , previous = Current
         , stmts = Stmts
         }.

ctx_text(Ctx0, Subsection) -> % implements .text <nr>
  Ctx = ctx_flush(Ctx0),
  #ctx{ sections_map = SectionsMap
      , current = Current
      } = Ctx,
  Section = ".text",
  SubsectionsMap = maps:get(Section, SectionsMap, #{}),
  Stmts = maps:get(Subsection, SubsectionsMap, []),
  Ctx#ctx{ current = {Section, Subsection}
         , previous = Current
         , stmts = Stmts
         }.

ctx_append(Ctx, Location, Stmt) ->
  #ctx{stmts = Stmts} = Ctx,
  Ctx#ctx{stmts = [{Location, Stmt} | Stmts]}.

%% Scan state utilities

scan_state_open(File) ->
  case File of
    "--" -> scan_state:stdin();
    "-" -> scan_state:stdin();
    _ -> scan_state:fopen(File)
  end.

scan_state_location(ScanState) ->
  {ok, Location = {_FileName, _LineNr}} = scan_state:location(ScanState),
  Location.

%% Pass 2 ----------------------------------------------------------------------
%%
%% - process subsections in order
%% - interpret stmts

pass2(SectionsMap) ->
  pass2_sections(maps:to_list(SectionsMap), tunit_init()).

pass2_sections([], Tunit) -> {ok, Tunit};
pass2_sections([{SectionName, SubsectionsMap} | Sections], Tunit0) ->
  %% TODO: handle creation of new sections here
  #section{} = tunit:get_section(Tunit0, SectionName),
  Tunit = Tunit0#tunit{cursect = SectionName},
  case pass2_subsections(SectionName, SubsectionsMap, Tunit) of
    {ok, NewTunit} -> pass2_sections(Sections, NewTunit);
    {error, _Reason} = Error -> Error
  end.

pass2_subsections(_SectionName = ".text", SubsectionsMap, Tunit) ->
  pass2_subsections(lists:sort(maps:to_list(SubsectionsMap)), Tunit).

pass2_subsections([], Tunit) -> {ok, Tunit};
pass2_subsections([{_Nr, StmtsRev} | Subsections], Tunit) ->
  case pass2_stmts(lists:reverse(StmtsRev), Tunit) of
    {ok, NewTunit} ->
      %% GAS documentation states that each sub-section is padded to make its
      %% size a multiple of 4 bytes, but also that other implementations may
      %% do differently.  We do not insert any implicit padding.
      pass2_subsections(Subsections, NewTunit);
    {error, _Reason} = Error -> Error
  end.

pass2_stmts([], Tunit) -> {ok, Tunit};
pass2_stmts([{Location, Stmt} | Stmts], Tunit) ->
  case pass2_stmt(Location, Tunit, Stmt) of
    {ok, NewTunit} -> pass2_stmts(Stmts, NewTunit);
    {error, _Reason} = Error -> Error
  end.

pass2_stmt(Location, Tunit, Stmt) ->
  case Stmt of
    #s_dot_file{} -> dot_file(Location, Tunit, Stmt);
    #s_dot_globl{} -> dot_globl(Location, Tunit, Stmt);
    #s_dot_ident{} -> dot_ident(Location, Tunit, Stmt);
    #s_dot_size{} -> dot_size(Location, Tunit, Stmt);
    #s_dot_type{} -> dot_type(Location, Tunit, Stmt);
    #s_label{} -> label(Location, Tunit, Stmt);
    #s_local_label{} -> local_label(Location, Tunit, Stmt);
    #s_insn{} -> insn(Location, Tunit, Stmt)
  end.

dot_file(_Location, Tunit, #s_dot_file{string = String}) ->
  Symbol = #symbol{ name = String
                  , section = false
                  , st_value = 0
                  , st_size = 0
                  , st_info = ?ELF_ST_INFO(?STB_LOCAL, ?STT_FILE)
                  , st_name = 0
                  , st_shndx = ?SHN_ABS
                  },
  {ok, tunit:put_symbol(Tunit, Symbol)}.

dot_globl(Location, Tunit, #s_dot_globl{name = Name}) ->
  case tunit:get_symbol(Tunit, Name) of
    false ->
      Symbol =
        #symbol{ name = Name
               , section = false
               , st_value = false
               , st_size = false
               , st_info = ?ELF_ST_INFO(?STB_GLOBAL, ?STT_NOTYPE)
               , st_name = 0
               , st_shndx = 0
               },
      {ok, tunit:put_symbol(Tunit, Symbol)};
    #symbol{st_info = StInfo} = OldSymbol ->
      case ?ELF_ST_BIND(StInfo) of
        ?STB_GLOBAL -> {ok, Tunit};
        ?STB_LOCAL -> % FIXME: assumed local-by-default, are there hard-local symbols?
          Symbol = OldSymbol#symbol{st_info = ?ELF_ST_INFO(?STB_GLOBAL, ?ELF_ST_TYPE(StInfo))},
          {ok, tunit:put_symbol(Tunit, Symbol)};
        Bind ->
          fmterr(Location, "symbol ~s has previous incompatible binding type ~p", [Name, Bind])
      end
  end.

dot_ident(_Location, Tunit, #s_dot_ident{} = Stmt) ->
  #section{data = {stmts, Stmts}} = OldSection =
    case tunit:get_section(Tunit, ".comment") of
      false -> section_dot_comment();
      Section -> Section
    end,
  NewSection = OldSection#section{data = {stmts, [Stmt | Stmts]}},
  {ok, tunit:put_section(Tunit, NewSection)}.

dot_size(Location, Tunit, #s_dot_size{name = Name}) ->
  #tunit{cursect = Cursect} = Tunit,
  #section{dot = Dot} = tunit:get_section(Tunit, Cursect),
  case tunit:get_symbol(Tunit, Name) of
    #symbol{st_size = StSize} when StSize =/= false ->
      fmterr(Location, "size of symbol ~s already defined", [Name]);
    #symbol{section = Section} when Section =/= Cursect ->
      fmterr(Location, "symbol ~s not defined in same section as dot", [Name]);
    #symbol{st_value = StValue} = OldSymbol when StValue =< Dot -> % note: false > integer()
      Symbol = OldSymbol#symbol{st_size = Dot - StValue},
      {ok, tunit:put_symbol(Tunit, Symbol)};
    #symbol{st_value = StValue} when StValue =/= false, StValue > Dot ->
      fmterr(Location, "cannot make symbol ~s negative size", [Name]);
    _ ->
      fmterr(Location, "symbol ~s not defined", [Name])
  end.

dot_type(Location, Tunit, #s_dot_type{name = Name}) ->
  case tunit:get_symbol(Tunit, Name) of
    false ->
      Symbol =
        #symbol{ name = Name
               , section = false
               , st_value = false
               , st_size = false
               , st_info = ?ELF_ST_INFO(?STB_LOCAL, ?STT_FUNC)
               , st_name = 0
               , st_shndx = 0
               },
      {ok, tunit:put_symbol(Tunit, Symbol)};
    #symbol{st_info = StInfo} = OldSymbol ->
      case ?ELF_ST_TYPE(StInfo) of
        ?STT_FUNC -> {ok, Tunit};
        ?STT_NOTYPE ->
          Symbol = OldSymbol#symbol{st_info = ?ELF_ST_INFO(?ELF_ST_BIND(StInfo), ?STT_FUNC)},
          {ok, tunit:put_symbol(Tunit, Symbol)};
        Type ->
          fmterr(Location, "symbol ~s has previous incompatible type ~p", [Name, Type])
      end
  end.

label(Location, Tunit, #s_label{name = Name}) ->
  case tunit:get_symbol(Tunit, Name) of
    #symbol{section = false, st_value = false} = Symbol -> define_label(Tunit, Symbol);
    #symbol{} -> fmterr(Location, "label ~s already defined", [Name]);
    false -> define_new_label(Tunit, Name)
  end.

define_new_label(Tunit, Name) ->
  define_label(Tunit, #symbol{name = Name, st_size = false, st_info = 0}).

define_label(Tunit, Symbol) ->
  #tunit{cursect = Cursect} = Tunit,
  #section{dot = Dot} = tunit:get_section(Tunit, Cursect),
  {ok, tunit:put_symbol(Tunit, Symbol#symbol{section = Cursect, st_value = Dot})}.

local_label(_Location, Tunit, #s_local_label{number = Number}) ->
  Serial = local_label_serial(Tunit, Number) + 1,
  Name = local_label_name(Number, Serial),
  define_new_label(tunit:put_local_label(Tunit, Number, Serial), Name).

local_label_serial(Tunit, Number) ->
  case tunit:get_local_label(Tunit, Number) of
    false -> 0;
    Serial -> Serial
  end.

local_label_name(Number, Serial) ->
  lists:flatten(io_lib:format(".L~.10b\^B~.10b", [Number, Serial])).

insn(Location, Tunit, #s_insn{} = Stmt) ->
  #tunit{cursect = Cursect} = Tunit,
  #section{data = {stmts, Stmts}, dot = Dot} = Section = tunit:get_section(Tunit, Cursect),
  case Dot rem 4 of % FIXME: target-specific
    0 ->
      NewStmt = insn_fixup(Tunit, Stmt),
      NewSection =
        Section#section{ data = {stmts, [NewStmt | Stmts]}
                       , dot = Dot + 4 % FIXME: target-specific
                       },
      {ok, tunit:put_section(Tunit, NewSection)};
    _ -> fmterr(Location, "misaligned address for instruction", [])
  end.

insn_fixup(Tunit, Insn) ->
  case Insn#s_insn.address of
    #e_local_label{number = Number, direction = Direction} ->
      LabelSerial = local_label_serial(Tunit, Number),
      ReferenceSerial =
        case Direction of
          $b -> LabelSerial;
          $f -> LabelSerial + 1
        end,
      Name = local_label_name(Number, ReferenceSerial),
      Insn#s_insn{address = #e_symbol{name = Name}};
    _ -> Insn
  end.

%% Initialization --------------------------------------------------------------

tunit_init() ->
  SectionText = section_dot_text(),
  Tunit = tunit:put_section(tunit:new(), SectionText),
  Tunit#tunit{cursect = SectionText#section.name}.

%% Predefined Sections ---------------------------------------------------------

section_dot_comment() -> % ".comment"
  #section{ name = ".comment"
          , data = {stmts, []}
          , dot = false % do not allow dot or labels here
          , shndx = 0
          , sh_name = 0
          , sh_type = ?SHT_PROGBITS
          , sh_offset = 0
          , sh_flags = ?SHF_MERGE bor ?SHF_STRINGS
          , sh_link = ?SHN_UNDEF
          , sh_addralign = 1
          , sh_entsize = 1
          }.

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
          , sh_addralign = 4 % FIXME: target-specific
          , sh_entsize = 0
          }.

%% Error reporting -------------------------------------------------------------

fmterr({FileName, LineNr}, Fmt, Args) ->
  {error, {?MODULE, {FileName, LineNr, Fmt, Args}}}.

-spec format_error(term()) -> io_lib:chars().
format_error({FileName, LineNr, Fmt, Args}) ->
  io_lib:format("file ~s line ~p: " ++ Fmt, [FileName, LineNr | Args]).
