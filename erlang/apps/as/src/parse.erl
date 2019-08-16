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
    {ok, ?T_DOT_FILE} -> dot_file(ScanState);
    {ok, ?T_DOT_GLOBL} -> dot_globl(ScanState);
    {ok, ?T_DOT_IDENT} -> dot_ident(ScanState);
    {ok, ?T_DOT_SIZE} -> dot_size(ScanState);
    {ok, ?T_DOT_TEXT} -> dot_text(ScanState);
    {ok, ?T_DOT_TYPE} -> dot_type(ScanState);
    {ok, {?T_SYMBOL, Name}} -> stmt_after_symbol(ScanState, Name);
    {ok, ?T_NEWLINE} -> stmt(ScanState);
    {ok, ?T_EOF} -> eof;
    ScanRes -> badtok(ScanState, "expected directive, label, or instruction", ScanRes)
  end.

%% Instructions and labels -----------------------------------------------------
%%
%% Recognize:
%%
%% <label> ::= <symbol> ":"
%%
%% <insn> ::= <symbol> (<accumulator> ",")? <address> <newline>
%%
%% <accumulator> ::= <uinteger> [uint <= 0xF]
%%
%% <address> ::= "@"? <displacement>? <index>?
%%
%% <displacement> ::= <uinteger>
%%
%% <index> ::= "(" <accumulator> ")"
%%
%% Examples:
%%
%% foo:
%% popj 17,
%% pushj 17,bar
%% movei 1,@fum(2)
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

stmt_after_symbol(ScanState, Name) ->
  case scan:token(ScanState) of
    {ok, ?T_COLON} -> {ok, #s_label{name = Name}};
    {ok, ?T_NEWLINE} -> make_insn(ScanState, Name, false, false, false, false);
    {ok, {?T_UINTEGER, UInt}} -> insn_uint(ScanState, Name, UInt);
    ScanRes -> badtok(ScanState, "junk after symbol", ScanRes)
  end.

%% Seen "<symbol> <uinteger>".  The <uinteger> is the <accumulator> if followed
%% by ",", otherwise (the start of) the <displacement>.
insn_uint(ScanState, Name, UInt) ->
  case scan:token(ScanState) of
    {ok, ?T_COMMA} -> % the Uint is the Accumulator, parse EA next
      insn_ea(ScanState, Name, _AccOrDev = UInt);
    {ok, ?T_LPAREN} -> % the Uint is the Displacement, parse Index next
      insn_ea_index(ScanState, Name, _AccOrDev = false, _At = false, _Displacement = UInt);
    {ok, ?T_NEWLINE} -> % the Uint is the Displacement
      make_insn(ScanState, Name, _AccOrDev = false, _At = false, _Displacement = UInt, _Index = false);
    ScanRes -> badtok(ScanState, "junk after <symbol> <uinteger>", ScanRes)
  end.

%% <symbol> <accordev> "," . [ ["@"] <displacement> ["(" <index> ")"] ] <newline>
insn_ea(ScanState, Name, AccOrDev) ->
  case scan:token(ScanState) of
    {ok, ?T_NEWLINE} ->
      make_insn(ScanState, Name, AccOrDev, _At = false, _Displacement = false, _Index = false);
    {ok, ?T_AT} -> insn_ea_at(ScanState, Name, AccOrDev);
    {ok, {?T_UINTEGER, Displacement}} ->
      insn_ea_disp(ScanState, Name, AccOrDev, _At = false, Displacement);
    ScanRes -> badtok(ScanState, "junk after comma", ScanRes)
  end.

%% <symbol> [<accordev> ","] "@" . <displacement> . ["(" <index> ")"] <newline>
insn_ea_at(ScanState, Name, AccOrDev) ->
  case scan:token(ScanState) of
    {ok, {?T_UINTEGER, Displacement}} ->
      insn_ea_disp(ScanState, Name, AccOrDev, _At = true, Displacement);
    ScanRes -> badtok(ScanState, "junk after @", ScanRes)
  end.

%% <symbol> [<accordev> ","] ["@"] <displacement> . ["(" <index> ")"] <newline>
insn_ea_disp(ScanState, Name, AccOrDev, At, Displacement) ->
  case scan:token(ScanState) of
    {ok, ?T_LPAREN} -> insn_ea_index(ScanState, Name, AccOrDev, At, Displacement);
    {ok, ?T_NEWLINE} -> make_insn(ScanState, Name, AccOrDev, At, Displacement, _Index = false);
    ScanRes -> badtok(ScanState, "junk after <displacement>", ScanRes)
  end.

%% <symbol> [<accordev> ","] ["@"] <displacement> "(" . <index> ")" <newline>
insn_ea_index(ScanState, Name, AccOrDev, At, Displacement) ->
  case scan:token(ScanState) of
    {ok, {?T_UINTEGER, Index}} when Index =< 8#17 ->
      case scan:token(ScanState) of
        {ok, ?T_RPAREN} ->
          case scan:token(ScanState) of
            {ok, ?T_NEWLINE} ->
              make_insn(ScanState, Name, AccOrDev, At, Displacement, Index);
            ScanRes -> badtok(ScanState, "junk after <index>", ScanRes)
          end;
        ScanRes -> badtok(ScanState, "junk in <index>", ScanRes)
      end;
    ScanRes -> badtok(ScanState, "junk in <index>", ScanRes)
  end.

make_insn(ScanState, Name, AccOrDev, At, Displacement, Index) ->
  Models = ?PDP10_KL10_271, % FIXME: make dynamic
  case pdp10_opcodes:insn_from_name(Models, Name, AccOrDev =/= false) of
    false -> badinsn(ScanState, "invalid mnemonic ~s", Name);
    #pdp10_insn_desc{ high13 = High13
                    , format = Format
                    , e_unused = EUnused
                    , extended = false % TODO: handle extended opcodes
                    } ->
      case make_high13(ScanState, Name, AccOrDev, High13, Format) of
        {error, _Reason} = Error -> Error;
        {ok, FinalHigh13} ->
          case check_e(ScanState, Name, At, Displacement, Index, EUnused) of
            {error, _Reason} = Error -> Error;
            ok -> {ok, #s_insn{ high13 = FinalHigh13
                              , at = At
                              , address = if Displacement =:= false -> 0; true -> Displacement end
                              , index = if Index =:= false -> 0; true -> Index end
                              }}
          end
      end
  end.

make_high13(ScanState, Name, AccOrDev, High13, Format) ->
  case {Format, AccOrDev} of
    {?PDP10_INSN_A_OPCODE, false} ->
      {ok, High13};
    {?PDP10_INSN_A_OPCODE, _} ->
      badinsn(ScanState, "~s: extraneous accumulator operand", Name);
    {?PDP10_INSN_IO, false} ->
      badinsn(ScanState, "~s: missing device operand", Name);
    {?PDP10_INSN_IO, _} ->
      make_high13_io(High13, AccOrDev);
    {?PDP10_INSN_BASIC, false} ->
      badinsn(ScanState, "~s: missing accumulator operand", Name);
    {?PDP10_INSN_BASIC, _} ->
      make_high13_basic(High13, AccOrDev);
    {?PDP10_INSN_A_NONZERO, false} ->
      badinsn(ScanState, "~s: missing accumulator operand", Name);
    {?PDP10_INSN_A_NONZERO, 0} ->
      badinsn(ScanState, "~s: accumulator must not be zero", Name);
    {?PDP10_INSN_A_NONZERO, _} ->
      make_high13_basic(High13, AccOrDev)
  end.

make_high13_basic(High13, Accumulator) ->
  Mask = ((1 bsl 4) - 1),
  {ok, (High13 band bnot Mask) bor (Accumulator band Mask)}.

make_high13_io(High13, Device) ->
  Mask = ((1 bsl 7) - 1),
  {ok, (High13 band bnot (Mask bsl 3)) bor ((Device band Mask) bsl 3)}.

check_e(ScanState, Name, At, Displacement, Index, EUnused) ->
  HaveE = At orelse Displacement =/= false orelse Index =/= false,
  case {EUnused, HaveE} of
    {true, false} -> ok;
    {true, true} -> badinsn(ScanState, "~s: extraneous address operand", Name);
    {false, false} -> badinsn(ScanState, "~s: missing address operand", Name);
    {false, true} -> ok
  end.

badinsn(ScanState, Fmt, Mnemonic) ->
  fmterr(ScanState, Fmt, [Mnemonic]).

%% Directives ------------------------------------------------------------------

dot_file(ScanState) ->
  dot_file_or_ident(ScanState, fun(String) -> #s_dot_file{string = String} end,
                   "junk after .file").

dot_ident(ScanState) ->
  dot_file_or_ident(ScanState, fun(String) -> #s_dot_ident{string = String} end,
                   "junk after .ident").

dot_file_or_ident(ScanState, MkStmt, ErrMsg) ->
  case scan:token(ScanState) of
    {ok, {?T_STRING, String}} ->
      case scan:token(ScanState) of
        {ok, ?T_NEWLINE} -> {ok, MkStmt(String)};
        ScanRes -> badtok(ScanState, ErrMsg, ScanRes)
      end;
    ScanRes -> badtok(ScanState, ErrMsg, ScanRes)
  end.

dot_globl(ScanState) ->
  case scan:token(ScanState) of
    {ok, {?T_SYMBOL, Name}} ->
      case scan:token(ScanState) of
        {ok, ?T_NEWLINE} -> {ok, #s_dot_globl{name = Name}};
        ScanRes -> badtok(ScanState, "junk after .globl", ScanRes)
      end;
    ScanRes -> badtok(ScanState, "junk after .globl", ScanRes)
  end.

%% For now only accepts ".size <sym>,.-<sym>".  TODO: extend
dot_size(ScanState) ->
  case scan:token(ScanState) of
    {ok, {?T_SYMBOL, Name}} ->
      case scan:token(ScanState) of
        {ok, ?T_COMMA} ->
          case scan:token(ScanState) of
            {ok, ?T_DOT} ->
              case scan:token(ScanState) of
                {ok, ?T_MINUS} ->
                  case scan:token(ScanState) of
                    {ok, {?T_SYMBOL, Name}} -> % same Name as above
                      case scan:token(ScanState) of
                        {ok, ?T_NEWLINE} -> {ok, #s_dot_size{name = Name}};
                        ScanRes -> badtok(ScanState, "junk after .size", ScanRes)
                      end;
                    ScanRes -> badtok(ScanState, "junk after .size", ScanRes)
                  end;
                ScanRes -> badtok(ScanState, "junk after .size", ScanRes)
              end;
            ScanRes -> badtok(ScanState, "junk after .size", ScanRes)
          end;
        ScanRes -> badtok(ScanState, "junk after .size", ScanRes)
      end;
    ScanRes -> badtok(ScanState, "junk after .size", ScanRes)
  end.

dot_text(ScanState) ->
  case scan:token(ScanState) of
    {ok, ?T_NEWLINE} -> {ok, #s_dot_text{}};
    ScanRes -> badtok(ScanState, "junk after .text", ScanRes)
  end.

%% For now only accepts ".type <sym>,@function".  TODO: extend
dot_type(ScanState) ->
  case scan:token(ScanState) of
    {ok, {?T_SYMBOL, Name}} ->
      case scan:token(ScanState) of
        {ok, ?T_COMMA} ->
          case scan:token(ScanState) of
            {ok, ?T_AT} ->
              case scan:token(ScanState) of
                {ok, {?T_SYMBOL, "function"}} ->
                  case scan:token(ScanState) of
                    {ok, ?T_NEWLINE} -> {ok, #s_dot_type{name = Name}};
                    ScanRes -> badtok(ScanState, "junk after .type", ScanRes)
                  end;
                ScanRes -> badtok(ScanState, "junk after .type", ScanRes)
              end;
            ScanRes -> badtok(ScanState, "junk after .type", ScanRes)
          end;
        ScanRes -> badtok(ScanState, "junk after .type", ScanRes)
      end;
    ScanRes -> badtok(ScanState, "junk after .type", ScanRes)
  end.

%% Error reporting -------------------------------------------------------------

badtok(_ScanState, _ErrMsg, {error, _Reason} = Error) -> Error;
badtok(ScanState, ErrMsg, {ok, Token}) ->
  fmterr(ScanState, ErrMsg ++ "; current token is ~s", [token:format(Token)]).

fmterr(ScanState, Fmt, Args) ->
  {ok, FileName} = scan_state:filename(ScanState),
  {ok, LineNr} = scan_state:linenr(ScanState),
  {error, {?MODULE, {FileName, LineNr, Fmt, Args}}}.

-spec format_error(term()) -> io_lib:chars().
format_error({FileName, LineNr, Fmt, Args}) ->
  io_lib:format("file ~s line ~p: " ++ Fmt, [FileName, LineNr | Args]).
