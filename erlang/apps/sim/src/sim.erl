%%% -*- erlang-indent-level: 2 -*-
%%%
%%% simulator for pdp10-elf
%%% Copyright (C) 2020  Mikael Pettersson
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

-module(sim).

-export([ main/1
        , format_error/1
        ]).

-record(options,
        { exe           = "" :: string()                % exe
        , argv          = [] :: [string()]              % argv for exe
        , stack_size    = 510*512 :: pos_integer()      % stack size
        , trace         = false :: boolean()            % trace mode
	}).

%% Command-line interface ======================================================

main(Argv) ->
  escript_runtime:start(fun main_/1, Argv).

-spec main_([string()]) -> no_return().
main_(Argv) ->
  case sim(Argv) of
    ok -> halt(0);
    {error, Reason} ->
      escript_runtime:fatal("~s\n", [error:format(Reason)])
  end.

%% Usage: pdp10-elf-sim [<simulator options>] <exe> [<arguments>]
%%
%% Simulates the execution of the given pdp10-elf <exe>.
%%
%% Options for the simulator:
%%
%% -s <size>
%% --stack-size=<size>
%%      Sets the stack size for main thread.
%%      Default: 261120 words (510 pages, one section with first and last
%%      page unmapped).
%%
%% -t [<mode>]
%% --trace[=<mode>]
%%      Enables (<mode> true, non-zero, or absent) or disables (<mode>
%%      zero or false) tracing.  Default: false.
%%
%% -v
%% --version
%%      Outputs the version of the simulator and exits.

sim(Argv) ->
  case getopt:parse(Argv, "+s:t::v",
                    [ { "stack-size", required, $s }
                    , { "trace", optional, $t }
                    , { "version", no, $v }
                    ]) of
    {ok, {Opts0, NonOpts0}} ->
      case parse_options(Opts0, NonOpts0) of
        {ok, Options} -> do_sim(Options);
        Else -> Else % ok or {error, _Reason}
      end;
    {error, _Reason} = Error -> Error
  end.

parse_options(Opts, NonOpts) ->
  parse_options(Opts, NonOpts, #options{}).

parse_options([Opt | Opts], NonOpts, Options) ->
  case parse_option(Opt, Options) of
    {ok, NewOptions} -> parse_options(Opts, NonOpts, NewOptions);
    Else -> Else % ok or {error, _Reason}
  end;
parse_options([], [Exe | Argv], Options) ->
  {ok, Options#options{exe = Exe, argv = Argv}};
parse_options([], [], _Options) ->
  {error, {?MODULE, no_exe}}.

parse_option(Opt, Options) ->
  case Opt of
    {$s, _}     -> handle_stack_size(Opt, Options);
    {$t, _}     -> handle_trace(Opt, Options);
    $t          -> handle_trace(Opt, Options);
    $v          -> handle_version(Opt, Options)
  end.

%% Option Handlers =============================================================

handle_stack_size({$s, Arg}, Options) ->
  case strtol:parse(Arg, _Base = 0) of
    {ok, {Number, Rest}} ->
      case Rest of
        [] -> do_handle_stack_size(Arg, Number, Options);
        [$p] -> do_handle_stack_size(Arg, Number*512, Options);
        _ -> invalid_stack_size(Arg)
      end;
    {error, _Reason} = Error -> Error
  end.

do_handle_stack_size(_Arg, Number, Options) when Number > 0 ->
  {ok, Options#options{stack_size = Number}};
do_handle_stack_size(Arg, _Number, _Options) ->
  invalid_stack_size(Arg).

invalid_stack_size(Arg) -> {error, {?MODULE, {invalid_stack_size, Arg}}}.

handle_trace({$t, Arg}, Options) ->
  case Arg of
    "false" -> do_handle_trace(false, Options);
    "true" -> do_handle_trace(true, Options);
    _ ->
      case strtol:parse(Arg, _Base = 0) of
        {ok, {Number, _Rest = []}} -> do_handle_trace(Number =/= 0, Options);
        {ok, {_Number, [_|_]}} -> {error, {?MODULE, {invalid_trace, Arg}}};
        {error, _Reason} = Error -> Error
      end
  end;
handle_trace($t, Options) -> do_handle_trace(true, Options).

do_handle_trace(Mode, Options) -> {ok, Options#options{trace = Mode}}.

handle_version($v, _Options) ->
  version(),
  ok. % causes main_/1 to halt(0)

version() ->
  io:format("pdp10-tools-sim version 0.1\n").

%% Simulation ==================================================================

do_sim(_Options) -> ok. % FIXME

%% Error Formatting ============================================================

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {invalid_stack_size, Arg} ->
      io_lib:format("invalid value for -s/--stack-size: ~s", [Arg]);
    {invalid_trace, Arg} ->
      io_lib:format("invalid value for -t/--trace: ~s", [Arg]);
    no_exe -> "no <exe> given"
  end.
