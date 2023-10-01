%%% -*- erlang-indent-level: 2 -*-
%%%
%%% sections assembler for pdp10-elf as
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

-module(assemble).

-export([ tunit/1
        , format_error/1
        ]).

-include("tunit.hrl").
-include_lib("lib/include/pdp10_elf36.hrl").

-spec tunit(#tunit{}) -> {ok, #tunit{}} | {error, {module(), term()}}.
tunit(Tunit) ->
  sections(maps:values(Tunit#tunit.sections), Tunit).

sections([], Tunit) -> {ok, Tunit};
sections([Section | Sections], Tunit) ->
  case section(Section, Tunit) of
    {ok, NewTunit} -> sections(Sections, NewTunit);
    {error, _Reason} = Error -> Error
  end.

section(Section, Tunit) ->
  case Section of
    #section{ name = ".data" ++ _
            , sh_type = ?SHT_PROGBITS
            , sh_flags = ?SHF_ALLOC bor ?SHF_WRITE
            } -> stmts(Section, Tunit);
    #section{ name = ".rodata" ++ _
            , sh_type = ?SHT_PROGBITS
            , sh_flags = ?SHF_ALLOC
            } -> stmts(Section, Tunit);
    #section{ name = ".text" ++ _
            , sh_type = ?SHT_PROGBITS
            , sh_flags = ?SHF_ALLOC bor ?SHF_EXECINSTR
            } -> stmts(Section, Tunit);
    #section{ name = ".comment"
            , sh_type = ?SHT_PROGBITS
            , sh_flags = ?SHF_MERGE bor ?SHF_STRINGS
            } -> comment(Section, Tunit);
    #section{ name = Name } ->
      {error, {?MODULE, {cannot_assemble, Name}}}
  end.

%% Assemble .comment -----------------------------------------------------------
%%
%% The image starts with a NUL, followed by the strings, all NUL-terminated.

comment(Section = #section{data = {stmts, Stmts}}, Tunit) ->
  Image = comment_image(lists:reverse(Stmts)),
  NewSection = Section#section{data = {image, Image}, dot = image_size(Image)},
  {ok, tunit:put_section(Tunit, NewSection)}.

comment_image(Stmts) -> comment_image(Stmts, []).

comment_image([], []) -> [];
comment_image([], Acc) -> lists:reverse([0 | Acc]);
comment_image([#s_dot_ident{string = String} | Stmts], Acc) ->
  comment_image(Stmts, [[0 | String] | Acc]).

%% FIXME: duplicated
image_size(Image) -> image_size(Image, 0).

image_size([H | T], Acc) -> image_size(T, image_size(H, Acc));
image_size([], Acc) -> Acc;
image_size(TByte, Acc) when is_integer(TByte), 0 =< TByte, TByte =< 511 ->
  Acc + 1.

%% Assemble user-defined contents ----------------------------------------------

stmts(Section = #section{name = Name, data = {stmts, Stmts}}, Tunit0) ->
  SectionSymbol = #symbol{ name = Name
                         , section = Name
                         , st_value = 0
                         , st_size = 0
                         , st_info = ?ELF_ST_INFO(?STB_LOCAL, ?STT_SECTION)
                         , st_name = 0
                         , st_shndx = 0
                         },
  Tunit1 = tunit:put_symbol(Tunit0, SectionSymbol),
  {ok, {Image, Relocs}} = stmts_image(lists:reverse(Stmts), Tunit1, Name),
  Tunit = tunit:put_section(Tunit1, Section#section{data = {image, Image}}),
  case Relocs of
    [] -> {ok, Tunit};
    _ ->
      RelocationSection =
        #section{ name = ".rela" ++ Name
                , data = {relocs, Name, Relocs}
                , dot = ?ELF36_RELA_SIZEOF * length(Relocs)
                , shndx = 0
                , sh_name = 0
                , sh_type = ?SHT_RELA
                , sh_offset = 0
                , sh_flags = ?SHF_INFO_LINK
                , sh_link = ?SHN_UNDEF % assigned during output
                , sh_info = ?SHN_UNDEF % assigned during output
                , sh_addralign = 4
                , sh_entsize = ?ELF36_RELA_SIZEOF
                },
      {ok, tunit:put_section(Tunit, RelocationSection)}
  end.

stmts_image(Stmts, Tunit, SectionName) ->
  stmts_image(Stmts, Tunit, SectionName, 0, [], []).

stmts_image([], _Tunit, _SectionName, _Dot, AccImage, AccRelocs) ->
  {ok, {lists:reverse(AccImage), lists:reverse(AccRelocs)}};
stmts_image([Stmt | Stmts], Tunit, SectionName, Dot, AccImage, AccRelocs) ->
  {ok, {Image, NewDot, NewRelocs}} = stmt_image(Stmt, Tunit, SectionName, Dot),
  stmts_image(Stmts, Tunit, SectionName, NewDot, [Image | AccImage], NewRelocs ++ AccRelocs).

stmt_image(Stmt, Tunit, SectionName, Dot) ->
  case Stmt of
    #s_dot_ascii{} -> dot_ascii_image(Stmt, Tunit, SectionName, Dot);
    #s_dot_byte{} -> dot_byte_image(Stmt, Tunit, SectionName, Dot);
    #s_dot_long{} -> dot_long_image(Stmt, Tunit, SectionName, Dot);
    #s_dot_short{} -> dot_short_image(Stmt, Tunit, SectionName, Dot);
    #s_insn{} -> insn_image(Stmt, Tunit, SectionName, Dot)
  end.

dot_ascii_image(#s_dot_ascii{z = Z, strings = Strings}, _Tunit, _SectionName, Dot) ->
  Image =
    case Z of
      true -> [String ++ [0] || String <- Strings];
      false -> Strings
    end,
  Size = lists:foldl(fun(String, Sum) -> Sum + length(String) end, 0, Image),
  {ok, {Image, Dot + Size, _Relocs = []}}.

dot_byte_image(#s_dot_byte{exprs = Exprs}, Tunit, SectionName, Dot) ->
  integer_data_directive(Exprs, Tunit, SectionName, Dot, _Size = 1, _Context = byte,
                         fun(Value) -> Value band ?PDP10_UINT9_MAX end).

dot_long_image(#s_dot_long{exprs = Exprs}, Tunit, SectionName, Dot) ->
  integer_data_directive(Exprs, Tunit, SectionName, Dot, _Size = 4, _Context = long,
                         fun pdp10_extint:uint36_to_ext/1).

integer_data_directive(Exprs, Tunit, SectionName, Dot, Size, Context, ValueToExt) ->
  {ok, {Values, Relocs}} = exprs_values(Exprs, Tunit, SectionName, Dot, Size, Context),
  {ok, {lists:map(ValueToExt, Values), Dot + Size * length(Values), Relocs}}.

dot_short_image(#s_dot_short{exprs = Exprs}, Tunit, SectionName, Dot) ->
  integer_data_directive(Exprs, Tunit, SectionName, Dot, _Size = 2, _Context = short,
                         fun pdp10_extint:uint18_to_ext/1).

insn_image(Stmt, Tunit, SectionName, Dot) ->
  #s_insn{ high13 = High13
         , at = At
         , address = AddressExpr
         , index = Index
         } = Stmt,
  {ok, {Address, Relocs}} = expr_value(AddressExpr, Tunit, SectionName, Dot, _Context = ifiw),
  Word = (((High13 band ((1 bsl 13) - 1)) bsl (36 - 13)) bor
          ((case At of true -> 1; false -> 0 end) bsl (36 - 14)) bor
          ((Index band ((1 bsl 4) - 1)) bsl (36 - 18)) bor
          (Address band ((1 bsl 18) - 1))),
  {ok, {pdp10_extint:uint36_to_ext(Word), Dot + 4, Relocs}}.

exprs_values(Exprs, Tunit, SectionName, Dot, Size, Context) ->
  exprs_values(Exprs, Tunit, SectionName, Dot, Size, Context, [], []).

exprs_values([], _Tunit, _SectionName, _Dot, _Size, _Context, AccValues, AccRelocs) ->
  {ok, {lists:reverse(AccValues), AccRelocs}};
exprs_values([Expr | Exprs], Tunit, SectionName, Dot, Size, Context, AccValues, AccRelocs) ->
  {ok, {Value, Relocs}} = expr_value(Expr, Tunit, SectionName, Dot, Context),
  exprs_values(Exprs, Tunit, SectionName, Dot + Size, Size, Context, [Value | AccValues], Relocs ++ AccRelocs).

expr_value(Expr, Tunit, DotSection, Dot, Context) ->
  #expr{modifier = Modifier} = Expr,
  case expr_value(Expr, Tunit, DotSection, Dot) of
    {Symbol, Offset} -> make_rel(Context, Modifier, Dot, Symbol, Offset);
    Value -> make_abs(Context, Modifier, Value)
  end.

%% An expression can evaluate to:
%% - Value when is_integer(Value)
%% - {Symbol, Addend} when is_string(Symbol), is_integer(Addend)
expr_value(Expr, Tunit, DotSection, Dot) ->
  #expr{operand1 = Opnd1, operator = Op, operand2 = Opnd2} = Expr,
  case {Op, operand_value(Opnd1, Tunit, DotSection, Dot), operand_value(Opnd2, Tunit, DotSection, Dot)} of
    {'+', false, Value2} -> Value2;
    {'+', {Sect1, Off1}, Value2} when is_integer(Value2) -> {Sect1, Off1 + Value2};
    {'+', Value1, {Sect2, Off2}} when is_integer(Value1) -> {Sect2, Off2 + Value1};
    {'+', Value1, Value2} when is_integer(Value1), is_integer(Value2) -> Value1 + Value2;
    {'-', false, Value2} when is_integer(Value2) -> -Value2;
    {'-', {Sect, Off1}, {Sect, Off2}} -> Off1 - Off2;
    {'-', {Sect1, Off1}, Value2} when is_integer(Value2) -> {Sect1, Off1 - Value2};
    {'-', Value1, Value2} when is_integer(Value1), is_integer(Value2) -> Value1 - Value2
  end.

%% An operand can evaluate to:
%% - false
%%   if the operand is absent
%% - Value when is_integer(Value)
%%   if the operand is an integer or an absolute symbol
%% - {Section, Offset} when is_string(Section), is_integer(Offset)
%%   if the operand is a label defined in the current translation unit
%% - {Symbol, 0} when is_string(Symbol)
%%   if the operand is an undefined symbol
operand_value(Operand, Tunit, DotSection, Dot) ->
  case Operand of
    false -> false;
    Value when is_integer(Value) -> Value;
    "." -> {DotSection, Dot};
    Symbol when is_list(Symbol) ->
      case tunit:get_symbol(Tunit, Symbol) of
        #symbol{section = abs, st_value = Value} when Value =/= false -> Value;
        #symbol{section = Section, st_value = Value} when Value =/= false -> {Section, Value};
        _ -> {Symbol, 0}
      end
  end.

make_rel(Context, Modifier, Dot, Symbol, Addend) ->
  Reloc =
    case {Context, Modifier} of
      {ifiw, false}  -> ?R_PDP10_IFIW;
      {long, false}  -> ?R_PDP10_LITERAL_W;
      {long, w}      -> ?R_PDP10_EFIW;
      {long, b}      -> ?R_PDP10_GLOBAL_B;
      {long, h}      -> ?R_PDP10_GLOBAL_H;
      {short, false} -> ?R_PDP10_LITERAL_H;
      {byte, false}  -> ?R_PDP10_LITERAL_B
    end,
  Rela = #rela{offset = Dot, type = Reloc, symbol = Symbol, addend = Addend},
  {ok, {_Value = 0, [Rela]}}.

make_abs(Context, Modifier, Value) ->
  Word =
    case {Context, Modifier} of
      {ifiw, false}  -> make_abs18(Value);
      {long, false}  -> make_abs36(Value);
      {long, w}      -> make_abs36(Value);
      {long, b}      -> make_abs36_b(Value);
      {long, h}      -> make_abs36_h(Value);
      {short, false} -> make_abs18(Value);
      {byte, false}  -> make_abs9(Value)
    end,
  {ok, {Word, _Relocs = []}}.

%% Produces a one-word global byte pointer to a 9-bit byte.
%% TODO: produce a one-word local byte pointer if -mno-extended.
make_abs36_b(Value) when Value >= 0, Value =< ((1 bsl 32) - 1) ->
  PS = 8#70 + (Value band 3),
  Y = Value bsr 2,
  (PS bsl 30) + Y.

%% Produces a one-word global byte pointer to an aligned 18-bit halfword.
%% TODO: produce a one-word local byte pointer if -mno-extended.
make_abs36_h(Value) when Value >= 0, Value =< ((1 bsl 32) - 1), (Value band 1) =:= 0 ->
  PS = 8#75 + ((Value band 2) bsr 1),
  Y = Value bsr 2,
  (PS bsl 30) + Y.

make_abs36(Value) ->
  case Value of
    _ when Value >= 0, Value =< ?PDP10_UINT36_MAX ->
      Value;
    _ when Value >= ?PDP10_INT36_MIN, Value =< ?PDP10_INT36_MAX ->
      Value band ((1 bsl 36) - 1)
  end.

make_abs18(Value) ->
  case Value of
    _ when Value >= 0, Value =< ?PDP10_UINT18_MAX ->
      Value;
    _ when Value >= ?PDP10_INT18_MIN, Value =< ?PDP10_INT18_MAX ->
      Value band ((1 bsl 18) - 1)
  end.

make_abs9(Value) ->
  case Value of
    _ when Value >= 0, Value =< ?PDP10_UINT9_MAX ->
      Value;
    _ when Value >= ?PDP10_INT9_MIN, Value =< ?PDP10_INT9_MAX ->
      Value band ((1 bsl 9) - 1)
  end.

%% Error reporting -------------------------------------------------------------

-spec format_error(term()) -> io_lib:chars().
format_error({cannot_assemble, Name}) ->
  io_lib:format("don't know how to assemble section ~s", [Name]).
