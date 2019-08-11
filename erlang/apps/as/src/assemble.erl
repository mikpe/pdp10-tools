%%% -*- erlang-indent-level: 2 -*-
%%%
%%% sections assembler for pdp10-elf as
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

-module(assemble).

-export([ tunit/1
        ]).

-include("tunit.hrl").
-include_lib("lib/include/pdp10_elf36.hrl").

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
    #section{ name = ".text" ++ _
            , sh_type = ?SHT_PROGBITS
            , sh_flags = ?SHF_ALLOC bor ?SHF_EXECINSTR
            } -> text(Section, Tunit);
    #section{ name = ".comment"
            , sh_type = ?SHT_PROGBITS
            , sh_flags = ?SHF_MERGE bor ?SHF_STRINGS
            } -> comment(Section, Tunit);
    #section{ name = Name } ->
      {error, io_lib:format("don't know how to assemble section ~s", [Name])}
  end.

%% Assemble .comment -----------------------------------------------------------
%%
%% The image starts with a NUL, followed by the strings, all NUL-terminated.

comment(Section = #section{data = {stmts, Stmts}}, Tunit) ->
  Image = comment_image(Stmts),
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
image_size(TByte, Acc) when is_integer(TByte), 0 =< TByte, TByte =< 511 -> Acc + 1.

%% Assemble .text --------------------------------------------------------------

text(Section = #section{data = {stmts, Stmts}}, Tunit) ->
  Image = text_image(Stmts),
  {ok, tunit:put_section(Tunit, Section#section{data = {image, Image}})}.

text_image(Stmts) -> text_image(Stmts, []).

text_image([], Acc) -> Acc; % the input Stmts were in reverse order
text_image([Stmt | Stmts], Acc) ->
  text_image(Stmts, [insn_image(Stmt) | Acc]).

insn_image(Insn) ->
  #s_insn{ high13 = High13
         , at = At
         , address = Address
         , index = Index
         } = Insn,
  Word = (((High13 band ((1 bsl 13) - 1)) bsl (36 - 13)) bor
          ((case At of true -> 1; false -> 0 end) bsl (36 - 14)) bor
          ((Index band ((1 bsl 4) - 1)) bsl (36 - 18)) bor
          (Address band ((1 bsl 18) - 1))),
  %% big-endian conversion
  [(Word bsr 27) band 511,
   (Word bsr 18) band 511,
   (Word bsr  9) band 511,
   Word          band 511].
