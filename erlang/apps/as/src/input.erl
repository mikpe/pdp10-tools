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
files(Files) ->
  NewFiles =
    case Files of
      [] -> ["--"];
      _ -> Files
    end,
  files(NewFiles, tunit_init()).

files([], Tunit) -> {ok, Tunit};
files([File | Files], Tunit) ->
  case file(File, Tunit) of
    {ok, NewTunit} -> files(Files, NewTunit);
    {error, _Reason} = Error -> Error
  end.

file(File, Tunit) ->
  case scan_state_open(File) of
    {ok, ScanState} ->
      try process(ScanState, Tunit)
      after scan_state:fclose(ScanState)
      end;
    {error, _Reason} = Error -> Error
  end.

%% Open next input file, support "--" and "-" as aliases for stdin.
scan_state_open(File) ->
  case File of
    "--" -> scan_state:stdin();
    "-" -> scan_state:stdin();
    _ -> scan_state:fopen(File)
  end.

process(ScanState, Tunit) ->
  case parse:stmt(ScanState) of
    eof -> {ok, Tunit};
    {ok, Stmt} ->
      case interpret(ScanState, Tunit, Stmt) of
        {ok, NewTunit} -> process(ScanState, NewTunit);
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

interpret(ScanState, Tunit, Stmt) ->
  case Stmt of
    #s_dot_file{} -> dot_file(ScanState, Tunit, Stmt);
    #s_dot_globl{} -> dot_globl(ScanState, Tunit, Stmt);
    #s_dot_ident{} -> dot_ident(ScanState, Tunit, Stmt);
    #s_dot_size{} -> dot_size(ScanState, Tunit, Stmt);
    #s_dot_text{} -> dot_text(ScanState, Tunit, Stmt);
    #s_dot_type{} -> dot_type(ScanState, Tunit, Stmt);
    #s_label{} -> label(ScanState, Tunit, Stmt);
    #s_local_label{} -> local_label(ScanState, Tunit, Stmt);
    #s_insn{} -> insn(ScanState, Tunit, Stmt)
  end.

dot_file(_ScanState, Tunit, #s_dot_file{string = String}) ->
  Symbol = #symbol{ name = String
                  , section = false
                  , st_value = 0
                  , st_size = 0
                  , st_info = ?ELF_ST_INFO(?STB_LOCAL, ?STT_FILE)
                  , st_name = 0
                  , st_shndx = ?SHN_ABS
                  },
  {ok, tunit:put_symbol(Tunit, Symbol)}.

dot_globl(ScanState, Tunit, #s_dot_globl{name = Name}) ->
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
          fmterr(ScanState, "symbol ~s has previous incompatible binding type ~p", [Name, Bind])
      end
  end.

dot_ident(_ScanState, Tunit, #s_dot_ident{} = Stmt) ->
  #section{data = {stmts, Stmts}} = OldSection =
    case tunit:get_section(Tunit, ".comment") of
      false -> section_dot_comment();
      Section -> Section
    end,
  NewSection = OldSection#section{data = {stmts, [Stmt | Stmts]}},
  {ok, tunit:put_section(Tunit, NewSection)}.

dot_size(ScanState, Tunit, #s_dot_size{name = Name}) ->
  #tunit{cursect = Cursect} = Tunit,
  #section{dot = Dot} = tunit:get_section(Tunit, Cursect),
  case tunit:get_symbol(Tunit, Name) of
    #symbol{st_size = StSize} when StSize =/= false ->
      fmterr(ScanState, "size of symbol ~s already defined", [Name]);
    #symbol{section = Section} when Section =/= Cursect ->
      fmterr(ScanState, "symbol ~s not defined in same section as dot", [Name]);
    #symbol{st_value = StValue} = OldSymbol when StValue =< Dot -> % note: false > integer()
      Symbol = OldSymbol#symbol{st_size = Dot - StValue},
      {ok, tunit:put_symbol(Tunit, Symbol)};
    #symbol{st_value = StValue} when StValue =/= false, StValue > Dot ->
      fmterr(ScanState, "cannot make symbol ~s negative size", [Name]);
    _ ->
      fmterr(ScanState, "symbol ~s not defined", [Name])
  end.

dot_text(_ScanState, Tunit, #s_dot_text{}) ->
  %% just check that .text has been pre-created
  #section{} = tunit:get_section(Tunit, ".text"),
  {ok, Tunit#tunit{cursect = ".text"}}.

dot_type(ScanState, Tunit, #s_dot_type{name = Name}) ->
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
          fmterr(ScanState, "symbol ~s has previous incompatible type ~p", [Name, Type])
      end
  end.

label(ScanState, Tunit, #s_label{name = Name}) ->
  case tunit:get_symbol(Tunit, Name) of
    #symbol{section = false, st_value = false} = Symbol -> define_label(Tunit, Symbol);
    #symbol{} -> fmterr(ScanState, "label ~s already defined", [Name]);
    false -> define_new_label(Tunit, Name)
  end.

define_new_label(Tunit, Name) ->
  define_label(Tunit, #symbol{name = Name, st_size = false, st_info = 0}).

define_label(Tunit, Symbol) ->
  #tunit{cursect = Cursect} = Tunit,
  #section{dot = Dot} = tunit:get_section(Tunit, Cursect),
  {ok, tunit:put_symbol(Tunit, Symbol#symbol{section = Cursect, st_value = Dot})}.

local_label(_ScanState, Tunit, #s_local_label{number = Number}) ->
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

insn(ScanState, Tunit, #s_insn{} = Stmt) ->
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
    _ -> fmterr(ScanState, "misaligned address for instruction", [])
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

fmterr(ScanState, Fmt, Args) ->
  {ok, FileName} = scan_state:filename(ScanState),
  {ok, LineNr} = scan_state:linenr(ScanState),
  {error, {?MODULE, {FileName, LineNr, Fmt, Args}}}.

-spec format_error(term()) -> io_lib:chars().
format_error({FileName, LineNr, Fmt, Args}) ->
  io_lib:format("file ~s line ~p: " ++ Fmt, [FileName, LineNr | Args]).
