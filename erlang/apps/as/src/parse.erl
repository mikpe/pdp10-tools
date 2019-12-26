%%% -*- erlang-indent-level: 2 -*-
%%%
%%% parser for pdp10-elf as
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

-module(parse).

-export([ stmt/1
        , format_error/1
        ]).

-include("token.hrl").
-include("tunit.hrl").
-include_lib("lib/include/pdp10_opcodes.hrl").

-spec stmt(scan_state:scan_state())
      -> {ok, stmt()} | eof | {error, {module(), term()}}.
stmt(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_DOT_DATA}} -> dot_data(ScanState);
    {ok, {_Location, ?T_DOT_FILE}} -> dot_file(ScanState);
    {ok, {_Location, ?T_DOT_GLOBL}} -> dot_globl(ScanState);
    {ok, {_Location, ?T_DOT_IDENT}} -> dot_ident(ScanState);
    {ok, {_Location, ?T_DOT_POPSECTION}} -> dot_popsection(ScanState);
    {ok, {_Location, ?T_DOT_PREVIOUS}} -> dot_previous(ScanState);
    {ok, {_Location, ?T_DOT_PUSHSECTION}} -> dot_pushsection(ScanState);
    {ok, {_Location, ?T_DOT_SIZE}} -> dot_size(ScanState);
    {ok, {_Location, ?T_DOT_SUBSECTION}} -> dot_subsection(ScanState);
    {ok, {_Location, ?T_DOT_TEXT}} -> dot_text(ScanState);
    {ok, {_Location, ?T_DOT_TYPE}} -> dot_type(ScanState);
    {ok, {Location, {?T_SYMBOL, Name}}} -> stmt_after_symbol(ScanState, Location, Name);
    {ok, {_Location, {?T_UINTEGER, UInt}}} -> stmt_after_uinteger(ScanState, UInt);
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
    {ok, {_Location, ?T_COLON}} -> {ok, #s_label{name = Name}};
    {ok, {_Location, ?T_NEWLINE}} -> make_insn(Location, Name, false, false, false, false);
    {ok, {_Location, {?T_UINTEGER, UInt}}} -> insn_uint(ScanState, Location, Name, UInt);
    {ok, {_Location, {?T_SYMBOL, Symbol}}} -> insn_symbol(ScanState, Location, Name, Symbol);
    {ok, {_Location, {?T_LOCAL_LABEL, Number, Direction}}} ->
      insn_local_label(ScanState, Location, Name, Number, Direction);
    ScanRes -> badtok("junk after symbol", ScanRes)
  end.

%% <stmt> ::= <uinteger> . ":"
stmt_after_uinteger(ScanState, UInt) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_COLON}} -> {ok, #s_local_label{number = UInt}};
    ScanRes -> badtok("junk after symbol", ScanRes)
  end.

%% Seen "<symbol> <uinteger>".  The <uinteger> is the <accumulator> if followed
%% by ",", otherwise (the start of) the <displacement>.
insn_uint(ScanState, Location, Name, UInt) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_COMMA}} -> % the Uint is the Accumulator, parse EA next
      insn_ea(ScanState, Location, Name, _AccOrDev = UInt);
    {ok, {_Location, ?T_LPAREN}} -> % the Uint is the Displacement, parse Index next
      Displacement = #e_integer{value = UInt},
      insn_ea_index(ScanState, Location, Name, _AccOrDev = false, _At = false, Displacement);
    {ok, {_Location, ?T_NEWLINE}} -> % the Uint is the Displacement
      Displacement = #e_integer{value = UInt},
      make_insn(Location, Name, _AccOrDev = false, _At = false, Displacement, _Index = false);
    ScanRes -> badtok("junk after <symbol> <uinteger>", ScanRes)
  end.

%% Seen "<symbol> <symbol2>".  The <symbol2> is (the start of) the <displacement>.
%% TODO: permit <symbol2> to be the <accumulator> (named register or device).
insn_symbol(ScanState, Location, Name, Symbol2) ->
  Displacement = #e_symbol{name = Symbol2},
  insn_ea_disp(ScanState, Location, Name, _AccOrDev = false, _At = false, Displacement).

%% Seen "<symbol> <local label>".  The <local label> is (the start of) the <displacement>.
insn_local_label(ScanState, Location, Name, Number, Direction) ->
  Displacement = #e_local_label{number = Number, direction = Direction},
  insn_ea_disp(ScanState, Location, Name, _AccOrDev = false, _At = false, Displacement).

%% <symbol> <accordev> "," . [ ["@"] <displacement> ["(" <index> ")"] ] <newline>
insn_ea(ScanState, Location, Name, AccOrDev) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_NEWLINE}} ->
      make_insn(Location, Name, AccOrDev, _At = false, _Displacement = false, _Index = false);
    {ok, {_Location, ?T_AT}} -> insn_ea_at(ScanState, Location, Name, AccOrDev);
    {ok, {_Lcation, {?T_UINTEGER, UInt}}} ->
      Displacement = #e_integer{value = UInt},
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = false, Displacement);
    {ok, {_Location, {?T_SYMBOL, Symbol}}} ->
      Displacement = #e_symbol{name = Symbol},
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = false, Displacement);
    {ok, {_Location, {?T_LOCAL_LABEL, Number, Direction}}} ->
      Displacement = #e_local_label{number = Number, direction = Direction},
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = false, Displacement);
    ScanRes -> badtok("junk after comma", ScanRes)
  end.

%% <symbol> [<accordev> ","] "@" . <displacement> ["(" <index> ")"] <newline>
insn_ea_at(ScanState, Location, Name, AccOrDev) ->
  case scan:token(ScanState) of
    {ok, {_Location, {?T_UINTEGER, UInt}}} ->
      Displacement = #e_integer{value = UInt},
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = true, Displacement);
    {ok, {_Location, {?T_SYMBOL, Symbol}}} ->
      Displacement = #e_symbol{name = Symbol},
      insn_ea_disp(ScanState, Location, Name, AccOrDev, _At = true, Displacement);
    {ok, {_Location, {?T_LOCAL_LABEL, Number, Direction}}} ->
      Displacement = #e_local_label{number = Number, direction = Direction},
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
            ok -> {ok, #s_insn{ high13 = FinalHigh13
                              , at = At
                              , address = case Displacement of
                                            false -> #e_integer{value = 0};
                                            _ -> Displacement
                                          end
                              , index = if Index =:= false -> 0; true -> Index end
                              }}
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

dot_data(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location1, ?T_NEWLINE}} -> {ok, #s_dot_data{nr = 0}};
    {ok, {_Location1, {?T_UINTEGER, Nr}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_NEWLINE}} -> {ok, #s_dot_data{nr = Nr}};
        ScanRes -> badtok("junk after .data <nr>", ScanRes)
      end;
    ScanRes -> badtok("junk after .data", ScanRes)
  end.

dot_file(ScanState) ->
  dot_file_or_ident(ScanState, fun(String) -> #s_dot_file{string = String} end,
                   "junk after .file").

dot_ident(ScanState) ->
  dot_file_or_ident(ScanState, fun(String) -> #s_dot_ident{string = String} end,
                   "junk after .ident").

dot_file_or_ident(ScanState, MkStmt, ErrMsg) ->
  case scan:token(ScanState) of
    {ok, {_Location1, {?T_STRING, String}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_NEWLINE}} -> {ok, MkStmt(String)};
        ScanRes -> badtok(ErrMsg, ScanRes)
      end;
    ScanRes -> badtok(ErrMsg, ScanRes)
  end.

dot_globl(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location1, {?T_SYMBOL, Name}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_NEWLINE}} -> {ok, #s_dot_globl{name = Name}};
        ScanRes -> badtok("junk after .globl", ScanRes)
      end;
    ScanRes -> badtok("junk after .globl", ScanRes)
  end.

dot_popsection(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_NEWLINE}} -> {ok, #s_dot_popsection{}};
    ScanRes -> badtok("junk after .popsection", ScanRes)
  end.

dot_previous(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location, ?T_NEWLINE}} -> {ok, #s_dot_previous{}};
    ScanRes -> badtok("junk after .previous", ScanRes)
  end.

%% For now only accepts ".pushsection <name> [, <nr>]".  TODO: extend
dot_pushsection(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location, {?T_STRING, Name}}} -> dot_pushsection(ScanState, Name);
    {ok, {_Location, {?T_SYMBOL, Name}}} -> dot_pushsection(ScanState, Name);
    %% TODO: do we need a general mapping from reserved to plain symbols?
    {ok, {_Location, ?T_DOT_DATA}} -> dot_pushsection(ScanState, _Name = ".data");
    {ok, {_Location, ?T_DOT_TEXT}} -> dot_pushsection(ScanState, _Name = ".text");
    ScanRes -> badtok("junk after .pushsection", ScanRes)
  end.

%% Seen ".pushsection <name>", expects "[, <nr>]".
dot_pushsection(ScanState, Name) ->
  case scan:token(ScanState) of
    {ok, {_Location1, ?T_NEWLINE}} -> {ok, #s_dot_pushsection{name = Name, nr = 0}};
    {ok, {_Location1, ?T_COMMA}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, {?T_UINTEGER, Nr}}} ->
          case scan:token(ScanState) of
            {ok, {_Location3, ?T_NEWLINE}} -> {ok, #s_dot_pushsection{name = Name, nr = Nr}};
            ScanRes -> badtok("junk after .pushsection <name>, <nr>", ScanRes)
          end;
        ScanRes -> badtok("junk after .pushsection <name>,", ScanRes)
      end;
    ScanRes -> badtok("junk after .pushsection <name>", ScanRes)
  end.

%% For now only accepts ".size <sym>,.-<sym>".  TODO: extend
dot_size(ScanState) ->
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
                        {ok, {_Location6, ?T_NEWLINE}} -> {ok, #s_dot_size{name = Name}};
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

dot_subsection(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location1, {?T_UINTEGER, Nr}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_NEWLINE}} -> {ok, #s_dot_subsection{nr = Nr}};
        ScanRes -> badtok("junk after .subsection <nr>", ScanRes)
      end;
    ScanRes -> badtok("junk after .subsection", ScanRes)
  end.

dot_text(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location1, ?T_NEWLINE}} -> {ok, #s_dot_text{nr = 0}};
    {ok, {_Location1, {?T_UINTEGER, Nr}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_NEWLINE}} -> {ok, #s_dot_text{nr = Nr}};
        ScanRes -> badtok("junk after .text <nr>", ScanRes)
      end;
    ScanRes -> badtok("junk after .text", ScanRes)
  end.

%% For now only accepts ".type <sym>,@function".  TODO: extend
dot_type(ScanState) ->
  case scan:token(ScanState) of
    {ok, {_Location1, {?T_SYMBOL, Name}}} ->
      case scan:token(ScanState) of
        {ok, {_Location2, ?T_COMMA}} ->
          case scan:token(ScanState) of
            {ok, {_Location3, ?T_AT}} ->
              case scan:token(ScanState) of
                {ok, {_Location4, {?T_SYMBOL, "function"}}} ->
                  case scan:token(ScanState) of
                    {ok, {_Location5, ?T_NEWLINE}} -> {ok, #s_dot_type{name = Name}};
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

%% Error reporting -------------------------------------------------------------

badtok(_ErrMsg, {error, _Reason} = Error) -> Error;
badtok(ErrMsg, {ok, {Location, Token}}) ->
  fmterr(Location, ErrMsg ++ "; current token is ~s", [token:format(Token)]).

fmterr({FileName, LineNr}, Fmt, Args) ->
  {error, {?MODULE, {FileName, LineNr, Fmt, Args}}}.

-spec format_error(term()) -> io_lib:chars().
format_error({FileName, LineNr, Fmt, Args}) ->
  io_lib:format("file ~s line ~p: " ++ Fmt, [FileName, LineNr | Args]).
