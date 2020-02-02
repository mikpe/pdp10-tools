%%% -*- erlang-indent-level: 2 -*-
%%%
%%% sections assembler for pdp10-elf as
%%% Copyright (C) 2013-2020  Mikael Pettersson
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

stmts(Section = #section{data = {stmts, Stmts}}, Tunit) ->
  case stmts_image(lists:reverse(Stmts), Tunit) of
    {ok, Image} ->
      {ok, tunit:put_section(Tunit, Section#section{data = {image, Image}})};
    {error, _Reason} = Error -> Error
  end.

stmts_image(Stmts, Tunit) -> stmts_image(Stmts, Tunit, []).

stmts_image([], _Tunit, Acc) -> {ok, lists:reverse(Acc)};
stmts_image([Stmt | Stmts], Tunit, Acc) ->
  case stmt_image(Stmt, Tunit) of
    {ok, Image} -> stmts_image(Stmts, Tunit, [Image | Acc]);
    {error, _Reason} = Error -> Error
  end.

stmt_image(Stmt, Tunit) ->
  case Stmt of
    #s_dot_ascii{} -> dot_ascii_image(Stmt, Tunit);
    #s_dot_byte{} -> dot_byte_image(Stmt, Tunit);
    #s_dot_long{} -> dot_long_image(Stmt, Tunit);
    #s_dot_short{} -> dot_short_image(Stmt, Tunit);
    #s_insn{} -> insn_image(Stmt, Tunit)
  end.

dot_ascii_image(#s_dot_ascii{z = Z, strings = Strings}, _Tunit) ->
  Image =
    case Z of
      true -> [String ++ [0] || String <- Strings];
      false -> Strings
    end,
  {ok, Image}.

dot_byte_image(#s_dot_byte{exprs = Exprs}, Tunit) ->
  integer_data_directive(Exprs, Tunit, fun(Value) -> Value band ?PDP10_UINT9_MAX end).

dot_long_image(#s_dot_long{exprs = Exprs}, Tunit) ->
  integer_data_directive(Exprs, Tunit, fun pdp10_extint:uint36_to_ext/1).

integer_data_directive(Exprs, Tunit, ValueToExt) ->
  case exprs_values(Exprs, Tunit) of
    {ok, Values} -> {ok, lists:map(ValueToExt, Values)};
    {error, _Reason} = Error -> Error
  end.

dot_short_image(#s_dot_short{exprs = Exprs}, Tunit) ->
  integer_data_directive(Exprs, Tunit, fun pdp10_extint:uint18_to_ext/1).

insn_image(Stmt, Tunit) ->
  #s_insn{ high13 = High13
         , at = At
         , address = AddressExpr
         , index = Index
         } = Stmt,
  case expr_value(AddressExpr, Tunit) of
    {ok, Address} ->
      Word = (((High13 band ((1 bsl 13) - 1)) bsl (36 - 13)) bor
              ((case At of true -> 1; false -> 0 end) bsl (36 - 14)) bor
              ((Index band ((1 bsl 4) - 1)) bsl (36 - 18)) bor
              (Address band ((1 bsl 18) - 1))),
      {ok, pdp10_extint:uint36_to_ext(Word)};
    {error, _Reason} = Error -> Error
  end.

exprs_values(Exprs, Tunit) ->
  exprs_values(Exprs, Tunit, []).

exprs_values([], _Tunit, Acc) -> {ok, lists:reverse(Acc)};
exprs_values([Expr | Exprs], Tunit, Acc) ->
  case expr_value(Expr, Tunit) of
    {ok, Value} -> exprs_values(Exprs, Tunit, [Value | Acc]);
    {error, _Reason} = Error -> Error
  end.

expr_value(Expr, Tunit) ->
  case Expr of
    #expr{symbol = false, offset = Value} -> {ok, Value};
    #expr{symbol = Name, offset = Offset} ->
      case tunit:get_symbol(Tunit, Name) of
        #symbol{st_value = Value} when Value =/= false -> {ok, Value + Offset};
        _ -> {error, {?MODULE, {undefined_symbol, Name}}}
      end
  end.

%% Error reporting -------------------------------------------------------------

-spec format_error(term()) -> io_lib:chars().
format_error({cannot_assemble, Name}) ->
  io_lib:format("don't know how to assemble section ~s", [Name]);
format_error({undefined_symbol, Name}) ->
  io_lib:format("reference to undefined symbol ~s", [Name]).
