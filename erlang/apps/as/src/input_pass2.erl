%%% -*- erlang-indent-level: 2 -*-
%%%
%%% input processing pass 2 for pdp10-elf as
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

-module(input_pass2).

-export([ format_error/1
        , pass2/1
        ]).

-include("tunit.hrl").
-include_lib("lib/include/pdp10_elf36.hrl").

%% Pass 2 ----------------------------------------------------------------------
%%
%% - process subsections in order
%% - interpret stmts

-spec pass2(input_pass1:sectionsmap()) -> {ok, #tunit{}} | {error, {module(), term()}}.
pass2(SectionsMap) ->
  pass2_sections(maps:to_list(SectionsMap), tunit_init()).

pass2_sections([], Tunit) -> {ok, Tunit};
pass2_sections([{SectionName, {Section, SubsectionsMap}} | Sections], Tunit0) ->
  Tunit = (tunit:put_section(Tunit0, Section))#tunit{cursect = SectionName},
  case pass2_subsections(SectionName, SubsectionsMap, Tunit) of
    {ok, NewTunit} -> pass2_sections(Sections, NewTunit);
    {error, _Reason} = Error -> Error
  end.

pass2_subsections(SectionName, SubsectionsMap, Tunit) ->
  case SectionName of
    ".data" ++ _ -> ok;
    ".debug" ++ _ -> ok;
    ".rodata" ++ _ -> ok;
    ".text" ++ _ -> ok
  end,
  pass2_subsections(lists:sort(maps:to_list(SubsectionsMap)), Tunit).

pass2_subsections([], Tunit) -> {ok, Tunit};
pass2_subsections([{_SubsectionNr, StmtsRev} | Subsections], Tunit) ->
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
    #s_dot_ascii{} -> dot_ascii(Location, Tunit, Stmt);
    #s_dot_byte{} -> dot_byte(Location, Tunit, Stmt);
    #s_dot_file{} -> dot_file(Location, Tunit, Stmt);
    #s_dot_globl{} -> dot_globl(Location, Tunit, Stmt);
    #s_dot_ident{} -> dot_ident(Location, Tunit, Stmt);
    #s_dot_long{} -> dot_long(Location, Tunit, Stmt);
    #s_dot_short{} -> dot_short(Location, Tunit, Stmt);
    #s_dot_size{} -> dot_size(Location, Tunit, Stmt);
    #s_dot_type{} -> dot_type(Location, Tunit, Stmt);
    #s_dot_2byte{} -> dot_2byte(Location, Tunit, Stmt);
    #s_dot_4byte{} -> dot_4byte(Location, Tunit, Stmt);
    #s_label{} -> label(Location, Tunit, Stmt);
    #s_local_label{} -> local_label(Location, Tunit, Stmt);
    #s_insn{} -> insn(Location, Tunit, Stmt)
  end.

dot_ascii(_Location, Tunit, #s_dot_ascii{z = Z, strings = Strings} = Stmt) ->
  #tunit{cursect = Cursect} = Tunit,
  #section{data = {stmts, Stmts}, dot = Dot} = Section = tunit:get_section(Tunit, Cursect),
  Size0 = lists:sum(lists:map(fun erlang:length/1, Strings)),
  Size =
    case Z of
      true -> Size0 + length(Strings);
      false -> Size0
    end,
  NewSection =
    Section#section{ data = {stmts, [Stmt | Stmts]}
                   , dot = Dot + Size
                   },
  {ok, tunit:put_section(Tunit, NewSection)}.

dot_byte(Location, Tunit, #s_dot_byte{} = Stmt0) ->
  Size = 1,
  Align = 1,
  integer_data_directive(Location, Tunit, Stmt0, Size, Align, ".byte",
                         fun(Stmt) -> Stmt#s_dot_byte.exprs end,
                         fun(Stmt, Exprs) -> Stmt#s_dot_byte{exprs = Exprs} end).

dot_file(_Location, Tunit, #s_dot_file{string = String}) ->
  Symbol = #symbol{ name = String
                  , section = abs
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
      false -> input_pass1:section_dot_comment();
      Section -> Section
    end,
  NewSection = OldSection#section{data = {stmts, [Stmt | Stmts]}},
  {ok, tunit:put_section(Tunit, NewSection)}.

dot_long(Location, Tunit, #s_dot_long{} = Stmt0) ->
  Size = 4, % FIXME: target-specific
  Align = Size,
  integer_data_directive(Location, Tunit, Stmt0, Size, Align, ".long",
                         fun(Stmt) -> Stmt#s_dot_long.exprs end,
                         fun(Stmt, Exprs) -> Stmt#s_dot_long{exprs = Exprs} end).

integer_data_directive(Location, Tunit, Stmt, Size, Align, Lexeme, GetExpr, SetExprs) ->
  Exprs = GetExpr(Stmt),
  #tunit{cursect = Cursect} = Tunit,
  #section{data = {stmts, Stmts}, dot = Dot} = Section = tunit:get_section(Tunit, Cursect),
  case Dot rem Align of
    0 ->
      NewExprs = [expr_fixup(Tunit, Expr) || Expr <- Exprs],
      NewStmt = SetExprs(Stmt, NewExprs),
      NewSection =
        Section#section{ data = {stmts, [NewStmt | Stmts]}
                       , dot = Dot + Size * length(NewExprs)
                       },
      {ok, tunit:put_section(Tunit, NewSection)};
    _ -> fmterr(Location, "misaligned address for ~s", [Lexeme])
  end.

dot_short(Location, Tunit, #s_dot_short{} = Stmt0) ->
  Size = 2, % FIXME: target-specific
  Align = Size,
  integer_data_directive(Location, Tunit, Stmt0, Size, Align, ".short",
                         fun(Stmt) -> Stmt#s_dot_short.exprs end,
                         fun(Stmt, Exprs) -> Stmt#s_dot_short{exprs = Exprs} end).

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

dot_type(Location, Tunit, #s_dot_type{name = Name, type = Type}) ->
  StType =
    case Type of
      function -> ?STT_FUNC;
      object -> ?STT_OBJECT
    end,
  case tunit:get_symbol(Tunit, Name) of
    false ->
      Symbol =
        #symbol{ name = Name
               , section = false
               , st_value = false
               , st_size = false
               , st_info = ?ELF_ST_INFO(?STB_LOCAL, StType)
               , st_name = 0
               , st_shndx = 0
               },
      {ok, tunit:put_symbol(Tunit, Symbol)};
    #symbol{st_info = StInfo} = OldSymbol ->
      case ?ELF_ST_TYPE(StInfo) of
        StType -> {ok, Tunit};
        ?STT_NOTYPE ->
          Symbol = OldSymbol#symbol{st_info = ?ELF_ST_INFO(?ELF_ST_BIND(StInfo), StType)},
          {ok, tunit:put_symbol(Tunit, Symbol)};
        Other ->
          fmterr(Location, "symbol ~s has previous incompatible type ~p", [Name, Other])
      end
  end.

dot_2byte(Location, Tunit, #s_dot_2byte{} = Stmt0) ->
  Size = 2, % FIXME: target-specific
  Align = 1,
  integer_data_directive(Location, Tunit, Stmt0, Size, Align, ".2byte",
                         fun(Stmt) -> Stmt#s_dot_2byte.exprs end,
                         fun(Stmt, Exprs) -> Stmt#s_dot_2byte{exprs = Exprs} end).

dot_4byte(Location, Tunit, #s_dot_4byte{} = Stmt0) ->
  Size = 4, % FIXME: target-specific
  Align = 1,
  integer_data_directive(Location, Tunit, Stmt0, Size, Align, ".4byte",
                         fun(Stmt) -> Stmt#s_dot_4byte.exprs end,
                         fun(Stmt, Exprs) -> Stmt#s_dot_4byte{exprs = Exprs} end).

label(Location, Tunit, #s_label{name = Name}) ->
  case tunit:get_symbol(Tunit, Name) of
    #symbol{section = false, st_value = false} = Symbol -> define_label(Tunit, Symbol);
    #symbol{} -> fmterr(Location, "label ~s already defined", [Name]);
    false -> define_new_label(Tunit, Name)
  end.

define_new_label(Tunit, Name) ->
  Symbol =
    #symbol{ name = Name
           , section = false % overridden below
           , st_value = false % overridden below
           , st_size = false
           , st_info = 0
           , st_name = 0
           , st_shndx = 0
           },
  define_label(Tunit, Symbol).

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
  Address = Insn#s_insn.address,
  Insn#s_insn{address = expr_fixup(Tunit, Address)}.

expr_fixup(Tunit, Expr) ->
  #expr{operand1 = Operand1, operand2 = Operand2} = Expr,
  Expr#expr{operand1 = operand_fixup(Tunit, Operand1),
            operand2 = operand_fixup(Tunit, Operand2)}.

operand_fixup(Tunit, {Number, Direction}) ->
  LabelSerial = local_label_serial(Tunit, Number),
  ReferenceSerial =
    case Direction of
      $b -> LabelSerial;
      $f -> LabelSerial + 1
    end,
  local_label_name(Number, ReferenceSerial);
operand_fixup(_Tunit, Operand) -> Operand.

%% Initialization --------------------------------------------------------------

tunit_init() ->
  SectionText = input_pass1:section_dot_text(),
  Cursect = SectionText#section.name,
  tunit:put_section(tunit:new(Cursect), SectionText).

%% Error reporting -------------------------------------------------------------

fmterr({FileName, LineNr}, Fmt, Args) ->
  {error, {?MODULE, {FileName, LineNr, Fmt, Args}}}.

-spec format_error(term()) -> io_lib:chars().
format_error({FileName, LineNr, Fmt, Args}) ->
  io_lib:format("file ~s line ~p: " ++ Fmt, [FileName, LineNr | Args]).
