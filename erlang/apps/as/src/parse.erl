%%% -*- erlang-indent-level: 2 -*-
%%%
%%% parser for pdp10-elf as
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

-module(parse).

-export([ stmt/1
        , format_error/1
        ]).

-include("token.hrl").
-include("tunit.hrl").
-include_lib("lib/include/pdp10_opcodes.hrl").
-include_lib("lib/include/pdp10_elf36.hrl").

-type location() :: scan:location().

-spec stmt(scan:scan_state())
      -> {ok, {location(), stmt()}} | eof | {error, {module(), term()}}.
stmt(ScanState) ->
  case scan:token(ScanState) of
    {ok, {Location, ?T_DOT_ASCII}} -> dot_ascii(ScanState, Location);
    {ok, {Location, ?T_DOT_ASCIZ}} -> dot_asciz(ScanState, Location);
    {ok, {Location, ?T_DOT_BYTE}} -> dot_byte(ScanState, Location);
    {ok, {Location, ?T_DOT_DATA}} -> dot_data(ScanState, Location);
    {ok, {Location, ?T_DOT_FILE}} -> dot_file(ScanState, Location);
    {ok, {Location, ?T_DOT_GLOBL}} -> dot_globl(ScanState, Location);
    {ok, {Location, ?T_DOT_HWORD}} -> dot_hword(ScanState, Location);
    {ok, {Location, ?T_DOT_IDENT}} -> dot_ident(ScanState, Location);
    {ok, {Location, ?T_DOT_LONG}} -> dot_long(ScanState, Location);
    {ok, {Location, ?T_DOT_POPSECTION}} -> dot_popsection(ScanState, Location);
    {ok, {Location, ?T_DOT_PREVIOUS}} -> dot_previous(ScanState, Location);
    {ok, {Location, ?T_DOT_PUSHSECTION}} -> dot_pushsection(ScanState, Location);
    {ok, {Location, ?T_DOT_SECTION}} -> dot_section(ScanState, Location);
    {ok, {Location, ?T_DOT_SHORT}} -> dot_short(ScanState, Location);
    {ok, {Location, ?T_DOT_SIZE}} -> dot_size(ScanState, Location);
    {ok, {Location, ?T_DOT_SUBSECTION}} -> dot_subsection(ScanState, Location);
    {ok, {Location, ?T_DOT_TEXT}} -> dot_text(ScanState, Location);
    {ok, {Location, ?T_DOT_TYPE}} -> dot_type(ScanState, Location);
    {ok, {Location, ?T_DOT_WORD}} -> dot_word(ScanState, Location);
    {ok, {Location, {?T_SYMBOL, Name}}} -> stmt_after_symbol(ScanState, Location, Name);
    {ok, {Location, {?T_UINTEGER, UInt}}} -> stmt_after_uinteger(ScanState, Location, UInt);
    {ok, {_Location, ?T_NEWLINE}} -> stmt(ScanState);
    {ok, {_Location, ?T_EOF}} -> eof;
    ScanRes -> badtok("expected directive, label, or instruction", ScanRes)
  end.

%% Instructions and labels -----------------------------------------------------
%%
%% Recognize:
%%
%% <label> ::= <symbol> ":" | <uinteger> ":"
%%
%% <insn> ::= <symbol> (<accumulator> ",")? <address> <newline>
%%
%% <accumulator> ::= <uinteger> [uint <= 0xF]
%%
%% <address> ::= "@"? <displacement>? <index>?
%%
%% <displacement> ::= <uinteger> | <symbol> | <uinteger>[bf]
%%
%% <index> ::= "(" <accumulator> ")"
%%
%% Examples:
%%
%% foo:
%% popj 017,
%% pushj 017,bar
%% movei 1,@fum(2)
%% jump bar
%%
%% TODO: <displacement> should be <expr> and permit parentheses and
%% various operators.
%%
%% Ambiguous example:
%%
%% <symbol> (<uinteger>) <newline>
%%
%% This is ambiguous since we have no special notation for <register>, and the
%% same kind of parentheses are used for expression grouping in the displacement
%% as for the index register.
%%
%% This might denote an insn with a parenthesized displacement and no index,
%% or it might denote an insn with an index but no displacement.
%%
%% However, an index always uses the displacement, even if zero, so we require
%% an explicit displacement if an index is used.
%%
%% This means that "opcode (...)" is interpreted as having a displacement but no
%% index.  Use "opcode 0(index)" if an index with zero displacement is needed.

stmt_after_symbol(ScanState, Location, Name) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_COLON}} -> {ok, {Location, #s_label{name = Name}}};
    {ok, {_Location, ?T_NEWLINE}} -> make_insn(Location, Name, false, false, false, false);
    {ok, {_Location, {?T_UINTEGER, UInt}}} -> insn_uint(ScanState, Location, Name, UInt);
    {ok, {_Location, {?T_SYMBOL, Symbol}}} -> insn_symbol(ScanState, Location, Name, Symbol);
    {ok, {_Location, {?T_LOCAL_LABEL, Number, Direction}}} ->
      insn_local_label(ScanState, Location, Name, Number, Direction);
    ScanRes -> badtok("junk after symbol", ScanRes)
  end.

%% <stmt> ::= <uinteger> . ":"
stmt_after_uinteger(ScanState, Location, UInt) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_COLON}} -> {ok, {Location, #s_local_label{number = UInt}}};
    ScanRes -> badtok("junk after symbol", ScanRes)
  end.

%% Seen "<symbol> <uinteger>".  The <uinteger> is the <accumulator> if followed
%% by ",", otherwise (the start of) the <displacement>.
insn_uint(ScanState, Location, Name, UInt) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_COMMA}} -> % the Uint is the Accumulator, parse EA next
      insn_ea(ScanState, Location, Name, _AccOrDev = UInt);
    {ok, {_Location, ?T_LPAREN}} -> % the Uint is the Displacement, parse Index next
      Displacement = mk_integer_expr(UInt),
      insn_ea_index(ScanState, Location, Name, _AccOrDev = false, _At = false, Displacement);
    {ok, {_Location, ?T_NEWLINE}} -> % the Uint is the Displacement
      Displacement = mk_integer_expr(UInt),
      make_insn(Location, Name, _AccOrDev = false, _At = false, Displacement, _Index = false);
    ScanRes -> badtok("junk after <symbol> <uinteger>", ScanRes)
  end.

%% Seen "<symbol> <symbol2>".  The <symbol2> is (the start of) the <displacement>.
%% TODO: permit <symbol2> to be the <accumulator> (named register or device).
insn_symbol(ScanState, Location, Name, Symbol2) ->
  Displacement = mk_symbol_expr(Symbol2),
  insn_ea_disp(ScanState, Location, Name, _AccOrDev = false, _At = false, Displacement).

%% Seen "<symbol> <local label>".  The <local label> is (the start of) the <displacement>.
insn_local_label(ScanState, Location, Name, Number, Direction) ->
  Displacement = mk_local_label_expr(Number, Direction),
  insn_ea_disp(ScanState, Location, Name, _AccOrDev = false, _At = false, Displacement).

%% <symbol> <accordev> "," . [ ["@"] <displacement> ["(" <index> ")"] ] <newline>
insn_ea(ScanState, Location, Name, AccOrDev) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_NEWLINE}} ->
      make_insn(Location, Name, AccOrDev, _At = false, _Displacement = false, _Index = false);
    {ok, {_Location, ?T_AT}} -> insn_ea_at(ScanState, Location, Name, AccOrDev);
    {ok, {_Lcation, {?T_UINTEGER, UInt}}} ->
      Displacement = mk_integer_expr(UInt),
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = false, Displacement);
    {ok, {_Location, {?T_SYMBOL, Symbol}}} ->
      Displacement = mk_symbol_expr(Symbol),
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = false, Displacement);
    {ok, {_Location, {?T_LOCAL_LABEL, Number, Direction}}} ->
      Displacement = mk_local_label_expr(Number, Direction),
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = false, Displacement);
    ScanRes -> badtok("junk after comma", ScanRes)
  end.

%% <symbol> [<accordev> ","] "@" . <displacement> ["(" <index> ")"] <newline>
insn_ea_at(ScanState, Location, Name, AccOrDev) ->
  case scan:token(ScanState) of
    {ok, {_Location, {?T_UINTEGER, UInt}}} ->
      Displacement = mk_integer_expr(UInt),
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = true, Displacement);
    {ok, {_Location, {?T_SYMBOL, Symbol}}} ->
      Displacement = mk_symbol_expr(Symbol),
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = true, Displacement);
    {ok, {_Location, {?T_LOCAL_LABEL, Number, Direction}}} ->
      Displacement = mk_local_label_expr(Number, Direction),
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = true, Displacement);
    ScanRes -> badtok("junk after @", ScanRes)
  end.

%% <symbol> [<accordev> ","] ["@"] <displacement> . ["(" <index> ")"] <newline>
insn_ea_disp(ScanState, Location, Name, AccOrDev, At, Displacement) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_LPAREN}} -> insn_ea_index(ScanState, Location, Name, AccOrDev, At, Displacement);
    {ok, {_Location, ?T_NEWLINE}} -> make_insn(Location, Name, AccOrDev, At, Displacement, _Index = false);
    ScanRes -> badtok("junk after <displacement>", ScanRes)
  end.

%% <symbol> [<accordev> ","] ["@"] <displacement> "(" . <index> ")" <newline>
insn_ea_index(ScanState, Location, Name, AccOrDev, At, Displacement) ->
  case scan:token(ScanState) of
    {ok, {_Location1, {?T_UINTEGER, Index}}} when Index =< 8#17 ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_RPAREN}} ->
          case scan:token(ScanState) of
            {ok, {_Location3, ?T_NEWLINE}} ->
              make_insn(Location, Name, AccOrDev, At, Displacement, Index);
            ScanRes -> badtok("junk after <index>", ScanRes)
          end;
        ScanRes -> badtok("junk in <index>", ScanRes)
      end;
    ScanRes -> badtok("junk in <index>", ScanRes)
  end.

make_insn(Location, Name, AccOrDev, At, Displacement, Index) ->
  Models = ?PDP10_KL10_271, % FIXME: make dynamic
  case pdp10_opcodes:insn_from_name(Models, Name, AccOrDev =/= false) of
    false -> badinsn(Location, "invalid mnemonic ~s", Name);
    #pdp10_insn_desc{ high13 = High13
                    , format = Format
                    , e_unused = EUnused
                    , extended = false % TODO: handle extended opcodes
                    } ->
      case make_high13(Location, Name, AccOrDev, High13, Format) of
        {error, _Reason} = Error -> Error;
        {ok, FinalHigh13} ->
          case check_e(Location, Name, At, Displacement, Index, EUnused) of
            {error, _Reason} = Error -> Error;
            ok ->
              Stmt = #s_insn{ high13 = FinalHigh13
                            , at = At
                            , address = case Displacement of
                                          false -> mk_integer_expr(0);
                                          _ -> Displacement
                                        end
                            , index = if Index =:= false -> 0; true -> Index end
                            },
              {ok, {Location, Stmt}}
          end
      end
  end.

make_high13(Location, Name, AccOrDev, High13, Format) ->
  case {Format, AccOrDev} of
    {?PDP10_INSN_A_OPCODE, false} ->
      {ok, High13};
    {?PDP10_INSN_A_OPCODE, _} ->
      badinsn(Location, "~s: extraneous accumulator operand", Name);
    {?PDP10_INSN_IO, false} ->
      badinsn(Location, "~s: missing device operand", Name);
    {?PDP10_INSN_IO, _} ->
      make_high13_io(High13, AccOrDev);
    {?PDP10_INSN_BASIC, false} ->
      make_high13_basic(High13, _Accumulator = 0);
    {?PDP10_INSN_BASIC, _} ->
      make_high13_basic(High13, AccOrDev);
    {?PDP10_INSN_A_NONZERO, false} ->
      badinsn(Location, "~s: missing accumulator operand", Name);
    {?PDP10_INSN_A_NONZERO, 0} ->
      badinsn(Location, "~s: accumulator must not be zero", Name);
    {?PDP10_INSN_A_NONZERO, _} ->
      make_high13_basic(High13, AccOrDev)
  end.

make_high13_basic(High13, Accumulator) ->
  Mask = ((1 bsl 4) - 1),
  {ok, (High13 band bnot Mask) bor (Accumulator band Mask)}.

make_high13_io(High13, Device) ->
  Mask = ((1 bsl 7) - 1),
  {ok, (High13 band bnot (Mask bsl 3)) bor ((Device band Mask) bsl 3)}.

check_e(Location, Name, At, Displacement, Index, EUnused) ->
  HaveE = At orelse Displacement =/= false orelse Index =/= false,
  case {EUnused, HaveE} of
    {true, false} -> ok;
    {true, true} -> badinsn(Location, "~s: extraneous address operand", Name);
    {false, false} -> badinsn(Location, "~s: missing address operand", Name);
    {false, true} -> ok
  end.

badinsn(Location, Fmt, Mnemonic) ->
  fmterr(Location, Fmt, [Mnemonic]).

%% Directives ------------------------------------------------------------------

dot_ascii(ScanState, Location) ->
  dot_ascii(ScanState, Location, ".ascii", _Z = false).

dot_asciz(ScanState, Location) ->
  dot_ascii(ScanState, Location, ".asciz", _Z = true).

dot_ascii(ScanState, Location, Lexeme, Z) ->
  case string_list(ScanState) of
    {ok, {Strings, Follow}} ->
      case Follow of
        {_Location, ?T_NEWLINE} ->
          {ok, {Location, #s_dot_ascii{z = Z, strings = Strings}}};
        _ -> badtok("junk after " ++ Lexeme ++ " <strings>", {ok, Follow})
      end;
    {error, _Reason} = Error -> Error
  end.

dot_byte(ScanState, Location) ->
  integer_data_directive(ScanState, Location,
                         fun(Exprs) -> #s_dot_byte{exprs = Exprs} end).

dot_data(ScanState, Location) ->
  case scan:token(ScanState) of
    {ok, {_Location1, ?T_NEWLINE}} -> {ok, {Location, #s_dot_data{nr = 0}}};
    {ok, {_Location1, {?T_UINTEGER, Nr}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_NEWLINE}} -> {ok, {Location, #s_dot_data{nr = Nr}}};
        ScanRes -> badtok("junk after .data <nr>", ScanRes)
      end;
    ScanRes -> badtok("junk after .data", ScanRes)
  end.

dot_file(ScanState, Location) ->
  dot_file_or_ident(ScanState, Location,
                    fun(String) -> #s_dot_file{string = String} end,
                   "junk after .file").

dot_ident(ScanState, Location) ->
  dot_file_or_ident(ScanState, Location,
                    fun(String) -> #s_dot_ident{string = String} end,
                   "junk after .ident").

dot_file_or_ident(ScanState, Location, MkStmt, ErrMsg) ->
  case scan:token(ScanState) of
    {ok, {_Location1, {?T_STRING, String}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_NEWLINE}} -> {ok, {Location, MkStmt(String)}};
        ScanRes -> badtok(ErrMsg, ScanRes)
      end;
    ScanRes -> badtok(ErrMsg, ScanRes)
  end.

dot_globl(ScanState, Location) ->
  case scan:token(ScanState) of
    {ok, {_Location1, {?T_SYMBOL, Name}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_NEWLINE}} -> {ok, {Location, #s_dot_globl{name = Name}}};
        ScanRes -> badtok("junk after .globl", ScanRes)
      end;
    ScanRes -> badtok("junk after .globl", ScanRes)
  end.

dot_hword(ScanState, Location) ->
  dot_short(ScanState, Location).

dot_long(ScanState, Location) ->
  integer_data_directive(ScanState, Location,
                         fun(Exprs) -> #s_dot_long{exprs = Exprs} end).

integer_data_directive(ScanState, Location, MkStmt) ->
  case expr_list(ScanState) of
    {ok, Exprs} -> {ok, {Location, MkStmt(Exprs)}};
    {error, _Reason} = Error -> Error
  end.

dot_popsection(ScanState, Location) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_NEWLINE}} -> {ok, {Location, #s_dot_popsection{}}};
    ScanRes -> badtok("junk after .popsection", ScanRes)
  end.

dot_previous(ScanState, Location) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_NEWLINE}} -> {ok, {Location, #s_dot_previous{}}};
    ScanRes -> badtok("junk after .previous", ScanRes)
  end.

dot_short(ScanState, Location) ->
  integer_data_directive(ScanState, Location,
                         fun(Exprs) -> #s_dot_short{exprs = Exprs} end).

%% For now only accepts ".size <sym>,.-<sym>".  TODO: extend
dot_size(ScanState, Location) ->
  case scan:token(ScanState) of
    {ok, {_Location1, {?T_SYMBOL, Name}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_COMMA}} ->
          case scan:token(ScanState) of
            {ok, {_Location3, ?T_DOT}} ->
              case scan:token(ScanState) of
                {ok, {_Location4, ?T_MINUS}} ->
                  case scan:token(ScanState) of
                    {ok, {_Location5, {?T_SYMBOL, Name}}} -> % same Name as above
                      case scan:token(ScanState) of
                        {ok, {_Location6, ?T_NEWLINE}} -> {ok, {Location, #s_dot_size{name = Name}}};
                        ScanRes -> badtok("junk after .size", ScanRes)
                      end;
                    ScanRes -> badtok("junk after .size", ScanRes)
                  end;
                ScanRes -> badtok("junk after .size", ScanRes)
              end;
            ScanRes -> badtok("junk after .size", ScanRes)
          end;
        ScanRes -> badtok("junk after .size", ScanRes)
      end;
    ScanRes -> badtok("junk after .size", ScanRes)
  end.

dot_subsection(ScanState, Location) ->
  case scan:token(ScanState) of
    {ok, {_Location1, {?T_UINTEGER, Nr}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_NEWLINE}} -> {ok, {Location, #s_dot_subsection{nr = Nr}}};
        ScanRes -> badtok("junk after .subsection <nr>", ScanRes)
      end;
    ScanRes -> badtok("junk after .subsection", ScanRes)
  end.

dot_text(ScanState, Location) ->
  case scan:token(ScanState) of
    {ok, {_Location1, ?T_NEWLINE}} -> {ok, {Location, #s_dot_text{nr = 0}}};
    {ok, {_Location1, {?T_UINTEGER, Nr}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_NEWLINE}} -> {ok, {Location, #s_dot_text{nr = Nr}}};
        ScanRes -> badtok("junk after .text <nr>", ScanRes)
      end;
    ScanRes -> badtok("junk after .text", ScanRes)
  end.

%% For now only accepts ".type <sym>,@function".  TODO: extend
dot_type(ScanState, Location) ->
  case scan:token(ScanState) of
    {ok, {_Location1, {?T_SYMBOL, Name}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_COMMA}} ->
          case scan:token(ScanState) of
            {ok, {_Location3, ?T_AT}} ->
              case scan:token(ScanState) of
                {ok, {_Location4, {?T_SYMBOL, "function"}}} ->
                  case scan:token(ScanState) of
                    {ok, {_Location5, ?T_NEWLINE}} -> {ok, {Location, #s_dot_type{name = Name}}};
                    ScanRes -> badtok("junk after .type", ScanRes)
                  end;
                ScanRes -> badtok("junk after .type", ScanRes)
              end;
            ScanRes -> badtok("junk after .type", ScanRes)
          end;
        ScanRes -> badtok("junk after .type", ScanRes)
      end;
    ScanRes -> badtok("junk after .type", ScanRes)
  end.

dot_word(ScanState, Location) ->
  dot_long(ScanState, Location).

%% .section/.pushsection directives --------------------------------------------
%%
%% .section <name> <sectionspec>
%% .pushsection <name> [, <nr>] <sectionspec>
%%
%% <sectionspec> ::= [, <flags> [, <type>]]
%%                 | , <flags>, <type>, <entsize>  (if <flags> contain M)
%%
%% <flags> ::= '"' <flagschar>* '"'
%% <flagschar> ::= [adexwMST0-9]
%%
%% <type> ::= "@progbits" | "@nobits" | "@note" | "@init_array" | "@fini_array"
%%          | "@preinit_array"
%%
%% TODO: add support for "G" and "?" flags, and <groupname> and <linkage> specs:
%%
%% <sectionspec> ::= , <flags>, <type>, <groupname> [, <linkage>]  (if <flags> contain G)
%%                 | , <flags>, <type>, <entsize>, <groupname> [, <linkage>]  (if <flags> contain both M and G)
%%
%% <linkage> ::= "comdat" | ".gnu.linkonce"

dot_section(ScanState, Location) ->
  dot_section_name(ScanState, Location, _IsPushsection = false).

dot_pushsection(ScanState, Location) ->
  dot_section_name(ScanState, Location, _IsPushsection = true).

dot_section_name(ScanState, Location, IsPushsection) ->
  case section_name(ScanState) of
    {ok, Name} -> dot_section_subsection(ScanState, Location, Name, IsPushsection);
    {error, _Reason} = Error -> Error
  end.

dot_section_subsection(ScanState, Location, Name, IsPushsection) ->
  case IsPushsection of
    true ->
      case scan:token(ScanState) of
        {ok, {_Location1, ?T_NEWLINE}} ->
          dot_section_finish(Location, Name, _Nr = 0, _ShFlags = 0, _ShType = 0, _ShEntSize = 0);
        {ok, {_Location1, ?T_COMMA}} ->
          case scan:token(ScanState) of
            {ok, {_Location2, {?T_UINTEGER, Nr}}} ->
              dot_section_flags(ScanState, Location, Name, Nr);
            ScanRes ->
              dot_section_flags(ScanState, Location, Name, _Nr = 0, ScanRes)
          end;
        ScanRes -> badtok("expected comma or newline", ScanRes)
      end;
    false -> dot_section_flags(ScanState, Location, Name, _Nr = false)
  end.

dot_section_flags(ScanState, Location, Name, Nr) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_NEWLINE}} ->
      dot_section_finish(Location, Name, Nr, _ShFlags = 0, _ShType = 0, _ShEntSize = 0);
    {ok, {_Location, ?T_COMMA}} ->
      dot_section_flags(ScanState, Location, Name, Nr, scan:token(ScanState));
    ScanRes -> badtok("expected comma or newline", ScanRes)
  end.

dot_section_flags(ScanState, Location, Name, Nr, ScanRes) ->
  case ScanRes of
    {ok, {_Location, {?T_STRING, String}}} ->
      case sh_flags(String) of
        {ok, ShFlags} -> dot_section_type(ScanState, Location, Name, Nr, ShFlags);
        false -> badtok("invalid <flags>", ScanRes)
      end;
    _ -> badtok("expected <string>", ScanRes)
  end.

sh_flags(String) -> sh_flags(String, 0).

sh_flags([], ShFlags) -> {ok, ShFlags};
sh_flags([C | Cs] = String, ShFlags) ->
  case sh_flag(C) of
    false ->
      case strtol:parse(String, _Base = 10) of
        {ok, {Mask, Rest}} -> sh_flags(Rest, ShFlags bor Mask);
        {error, _Reason} -> false
      end;
    Flag -> sh_flags(Cs, ShFlags bor Flag)
  end.

sh_flag(C) ->
  case C of
    $a -> ?SHF_ALLOC;
    $d -> ?SHF_GNU_MBIND;
    $e -> ?SHF_EXCLUDE;
    $w -> ?SHF_WRITE;
    $x -> ?SHF_EXECINSTR;
    $M -> ?SHF_MERGE;
    $S -> ?SHF_STRINGS;
    $T -> ?SHF_TLS;
    %% TODO: permit $G and $?
    _  -> false
  end.

dot_section_type(ScanState, Location, Name, Nr, ShFlags) ->
  case scan:token(ScanState) of
    {ok, {_Location1, ?T_NEWLINE}} = ScanRes ->
      case (ShFlags band (?SHF_MERGE bor ?SHF_GROUP)) =/= 0 of
        true -> badtok("expected ,@type", ScanRes);
         false ->
          dot_section_finish(Location, Name, Nr, ShFlags, _ShType = 0, _ShEntSize = 0)
      end;
    {ok, {_Location1, ?T_COMMA}} ->
      case sh_type(ScanState) of
        {ok, ShType} -> dot_section_entsize(ScanState, Location, Name, Nr, ShFlags, ShType);
        {error, _Reason} = Error -> Error
      end;
    ScanRes -> badtok("expected comma or newline", ScanRes)
  end.

sh_type(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location1, ?T_AT}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, {?T_SYMBOL, Name}}} = ScanRes ->
          case Name of
            "progbits"      -> {ok, ?SHT_PROGBITS};
            "nobits"        -> {ok, ?SHT_NOBITS};
            "note"          -> {ok, ?SHT_NOTE};
            "init_array"    -> {ok, ?SHT_INIT_ARRAY};
            "fini_array"    -> {ok, ?SHT_FINI_ARRAY};
            "preinit_array" -> {ok, ?SHT_PREINIT_ARRAY};
            _ -> badtok("invalid @type", ScanRes)
          end;
        {ok, {_Location2, {?T_UINTEGER, ShType}}} -> {ok, ShType};
        ScanRes -> badtok("expected <symbol> or <uinteger>", ScanRes)
      end;
    ScanRes -> badtok("expected @type", ScanRes)
  end.

dot_section_entsize(ScanState, Location, Name, Nr, ShFlags, ShType) ->
  case (ShFlags band ?SHF_MERGE) =/= 0 of
    true ->
      case scan:token(ScanState) of
        {ok, {_Location1, ?T_COMMA}} ->
          case scan:token(ScanState) of
            {ok, {_Location2, {?T_UINTEGER, ShEntSize}}} ->
              dot_section_newline(ScanState, Location, Name, Nr, ShFlags, ShType, ShEntSize);
            ScanRes -> badtok("expected <uinteger>", ScanRes)
          end;
        ScanRes -> badtok("expected ,<entsize>", ScanRes)
      end;
    false ->
      dot_section_newline(ScanState, Location, Name, Nr, ShFlags, ShType, _ShEntSize = 0)
  end.

dot_section_newline(ScanState, Location, Name, Nr, ShFlags, ShType, ShEntSize) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_NEWLINE}} ->
      dot_section_finish(Location, Name, Nr, ShFlags, ShType, ShEntSize);
    ScanRes -> badtok("expected <newline>", ScanRes)
  end.

dot_section_finish(Location, Name, Nr, ShFlags, ShType, ShEntSize) ->
  {ok, {Location, #s_dot_section{ name = Name
                                , nr = Nr
                                , sh_type = ShType
                                , sh_flags = ShFlags
                                , sh_entsize = ShEntSize
                                }}}.

section_name(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location, {?T_STRING, Name}}} -> {ok, Name};
    {ok, {_Location, {?T_SYMBOL, Name}}} -> {ok, Name};
    %% TODO: do we need a general mapping from reserved to plain symbols?
    {ok, {_Location, ?T_DOT_DATA}} -> {ok, ".data"};
    {ok, {_Location, ?T_DOT_TEXT}} -> {ok, ".text"};
    ScanRes -> badtok("invalid section name", ScanRes)
  end.

%% Expressions -----------------------------------------------------------------

%% <expr_list> ::= (<expr> ("," <expr>)*)? \n
expr_list(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_NEWLINE}} -> {ok, []};
    First ->
      case do_expr(First) of
        {ok, Expr} -> expr_list(ScanState, [Expr]);
        {error, _Reason} = Error -> Error
      end
  end.

expr_list(ScanState, Exprs) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_COMMA}} ->
      case expr(ScanState) of
        {ok, Expr} -> expr_list(ScanState, [Expr | Exprs]);
        {error, _Reason} = Error -> Error
      end;
    {ok, {_Location, ?T_NEWLINE}} -> {ok, lists:reverse(Exprs)};
    ScanRes -> badtok("expected comma or newline", ScanRes)
  end.

expr(ScanState) ->
  do_expr(scan:token(ScanState)).

do_expr(First) ->
  case First of
    {ok, {_Location, {?T_UINTEGER, UInt}}} ->
      {ok, mk_integer_expr(UInt)};
    {ok, {_Location, {?T_LOCAL_LABEL, Number, Direction}}} ->
      {ok, mk_local_label_expr(Number, Direction)};
    {ok, {_Location, {?T_SYMBOL, Symbol}}} ->
      {ok, mk_symbol_expr(Symbol)};
    _ ->
      badtok("invalid start of expr", First)
  end.

mk_integer_expr(Value) -> #e_integer{value = Value}.
mk_local_label_expr(Number, Direction) ->
  #e_local_label{number = Number, direction = Direction}.
mk_symbol_expr(Symbol) -> #e_symbol{name = Symbol}.

%% String Lists ----------------------------------------------------------------

%% <string_list> ::= (<string> ("," <string>)*)?
string_list(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location, {?T_STRING, String}}} -> string_list(ScanState, [String]);
    {ok, Follow} -> {ok, {[], Follow}};
    {error, _Reason} = Error -> Error
  end.

string_list(ScanState, Strings) ->
  case scan:token(ScanState) of
    {ok, {_Location1, ?T_COMMA}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, {?T_STRING, String}}} ->
          string_list(ScanState, [String | Strings]);
        {ok, First} -> badtok("expected string literal", {ok, First});
        {error, _Reason} = Error -> Error
      end;
    {ok, Follow} -> {ok, {lists:reverse(Strings), Follow}};
    {error, _Reason} = Error -> Error
  end.

%% Error reporting -------------------------------------------------------------

badtok(_ErrMsg, {error, _Reason} = Error) -> Error;
badtok(ErrMsg, {ok, {Location, Token}}) ->
  fmterr(Location, ErrMsg ++ "; current token is ~s", [token:format(Token)]).

fmterr({FileName, LineNr}, Fmt, Args) ->
  {error, {?MODULE, {FileName, LineNr, Fmt, Args}}}.

-spec format_error(term()) -> io_lib:chars().
format_error({FileName, LineNr, Fmt, Args}) ->
  io_lib:format("file ~s line ~p: " ++ Fmt, [FileName, LineNr | Args]).
