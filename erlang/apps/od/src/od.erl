%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'od' clone for files with 9-bit bytes
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

-module(od).
-export([main/1]).

-record(options, {
          %% user options
            address_radix       % -A/--address-radix=
          , skip_bytes          % -j/--skip-bytes=
          , read_bytes          % -N/--read-bytes=
          , output_type         % $c, $d, $o, $u, $x; $a and $f are NYI
          , output_z            % type has trailing $z
          , bytes_per_datum     % 1, 2, or 4
          , width               % -w/--width=
          %% compiled options
          , chars_per_datum
          , datums_per_line
          , bytes_per_line
         }).

%% Command-line interface ======================================================

-spec main([string()]) -> no_return().
main(Argv) ->
  case my_getopt:parse(Argv, "VbcdDiloOsxXA:j:N:t:w::",
                       [ {"version", no, $V}
                       , {"address-radix", required, $A}
                       , {"skip-bytes", required, $j}
                       , {"read-bytes", required, $N}
                       , {"format", required, $t}
                       , {"width", optional, $w}
                       ]) of
    {ok, {Options, Files}} ->
      od(scan_options(Options), Files);
    {error, Reason} ->
      escript_runtime:errmsg("~s\n", [error:format(Reason)]),
      usage()
  end.

usage() ->
  escript_runtime:fmterr(
    "Usage: ~s [-V] [-bcdDiloOsxX] [-t [bcdoux][1248][z]] [-A RADIX] [-j BYTES]"
    " [-N BYTES] [-w [BYTES]] [files..]\n",
    [escript_runtime:progname()]),
  halt(1).

scan_options(Options) ->
  Opts = #options{ address_radix = $o
                 , read_bytes = -1
                 , skip_bytes = 0
                 , output_type = $o
                 , output_z = false
                 , bytes_per_datum = 2
                 , width = 16
                 },
  compile_options(lists:foldl(fun scan_option/2, Opts, Options)).

scan_option($V, _Opts) -> % -V, non-standard alias for --version
  io:format(standard_io, "pdp10-tools od version 0.3\n", []),
  halt(0);
scan_option($b, Opts) -> % -b, same as -t o1
  Opts#options{ output_type = $o
              , bytes_per_datum = 1
              };
scan_option($c, Opts) -> % -c, same as -t c
  Opts#options{ output_type = $c
              , bytes_per_datum = 1
              };
scan_option($d, Opts) -> % -d, same as -t u2
  Opts#options{ output_type = $u
              , bytes_per_datum = 2
              };
scan_option($D, Opts) -> % -D, non-standard alias for -t u4
  Opts#options{ output_type = $u
              , bytes_per_datum = 4
              };
scan_option($o, Opts) -> % -o, same as -t o2
  Opts#options{ output_type = $o
              , bytes_per_datum = 2
              };
scan_option($O, Opts) -> % -O, non-standard alias for -t o4
  Opts#options{ output_type = $o
              , bytes_per_datum = 4
              };
scan_option($s, Opts) -> % -s, same as -t d2
  Opts#options{ output_type = $d
              , bytes_per_datum = 2
              };
scan_option(C, Opts) when C =:= $i; C =:= $l -> % -i, -l, same as -t d4
  Opts#options{ output_type = $d
              , bytes_per_datum = 4
              };
scan_option($x, Opts) -> % -x, same as -t x2
  Opts#options{ output_type = $x
              , bytes_per_datum = 2
              };
scan_option($X, Opts) -> % -X, non-standard alias for -t x4
  Opts#options{ output_type = $x
              , bytes_per_datum = 4
              };
scan_option({$t, Arg}, Opts) -> % -t TYPE, same as --format=TYPE
  parse_type(Arg, Opts);
scan_option({$A, Arg}, Opts) -> % -A RADIX, same as --address-radix=RADIX
  Opts#options{address_radix = parse_radix(Arg)};
scan_option({$j, Arg}, Opts) -> % -j BYTES, same as --skip-bytes=BYTES
  Opts#options{skip_bytes = parse_bytes(Arg)};
scan_option({$N, Arg}, Opts) -> % -N BYTES, same as --read-bytes=BYTES
  Opts#options{read_bytes = parse_bytes(Arg)};
scan_option($w, Opts) -> % -w, same as --width=32
  Opts#options{width = 32};
scan_option({$w, Arg}, Opts) -> % -w BYTES, same as --width=BYTES
  Opts#options{width = parse_bytes(Arg)}.

parse_radix(String) ->
  case String of
    [C] when C =:= $d; C =:= $o; C =:= $x; C =:= $n -> C;
    _ -> escript_runtime:fatal("invalid radix '~s'\n", [String])
  end.

parse_bytes(String) ->
  case strtol:parse(String, 0) of
    {ok, {Value, Rest}} ->
      Value * parse_multiplier(Rest);
    {error, Reason} ->
      escript_runtime:fatal("invalid number '~s': ~s\n",
                            [String, error:format(Reason)])
  end.

parse_multiplier(String) ->
  case String of
    ""   -> 1;
    "b"  -> 512;
    "KB" -> 1000;
    "K"  -> 1024;
    "MB" -> 1000*1000;
    "M"  -> 1024*1024;
    "GB" -> 1000*1000*1000;
    "G"  -> 1024*1024*1024;
    "TB" -> 1000*1000*1000*1000;
    "T"  -> 1024*1024*1024*1024;
    _    -> escript_runtime:fatal("invalid multiplier suffix '~s'\n", [String])
  end.

parse_type(String, Opts) ->
  case String of
    [$c | Rest] -> parse_z(Rest, 1, $c, Opts);
    [$d | Rest] -> parse_size(Rest, $d, Opts);
    [$o | Rest] -> parse_size(Rest, $o, Opts);
    [$u | Rest] -> parse_size(Rest, $u, Opts);
    [$x | Rest] -> parse_size(Rest, $x, Opts);
    _ -> escript_runtime:fatal("invalid type '~s'\n", [String])
  end.

parse_size(String, OutputType, Opts) ->
  case String of
    [$C | Rest] -> parse_z(Rest, 1, OutputType, Opts);
    [$S | Rest] -> parse_z(Rest, 2, OutputType, Opts);
    [$I | Rest] -> parse_z(Rest, 4, OutputType, Opts);
    [$L | Rest] -> parse_z(Rest, 4, OutputType, Opts);
    _ ->
      case strtol:parse(String, 10) of
        {ok, {Size, Rest}} when Size =:= 1; Size =:= 2; Size =:= 4 ->
          parse_z(Rest, Size, OutputType, Opts);
        _ -> escript_runtime:fatal("invalid type size '~s'\n", [String])
      end
  end.

parse_z(String, BytesPerDatum, OutputType, Opts0) ->
  Opts = Opts0#options{ output_type = OutputType
                      , bytes_per_datum = BytesPerDatum
                      },
  case String of
    "z" -> Opts#options{output_z = true};
    "" -> Opts;
    _ -> escript_runtime:fatal("trailing garbage in type '~s'\n", [String])
  end.

compile_options(Opts0) ->
  #options{ width = Width
          , bytes_per_datum = BytesPerDatum
          , output_type = OutputType
          } = Opts0,
  DatumsPerLine = Width div BytesPerDatum,
  BytesPerLine = BytesPerDatum * DatumsPerLine,
  Opts = Opts0#options{ datums_per_line = DatumsPerLine
                      , bytes_per_line = BytesPerLine
                      },
  if BytesPerLine =/= Width ->
          escript_runtime:fatal("line width ~p is not a multiple of the input"
                                " datum size ~p\n",
                                [Width, BytesPerDatum]);
     true -> ok
  end,
  case OutputType of
    $d -> compile_numfmt(10, $\s, Opts);
    $u -> compile_numfmt(10, $\s, Opts);
    $o -> compile_numfmt(8, $0, Opts);
    $x -> compile_numfmt(16, $0, Opts);
    $c -> Opts#options{chars_per_datum = 3}
  end.

compile_numfmt(Base, Pad, Opts) ->
  BytesPerDatum = Opts#options.bytes_per_datum,
  CharBit = 9, % for PDP10 nonet files
  MaxVal = (1 bsl (BytesPerDatum * CharBit)) - 1,
  TmpBuf = io_lib:format("~.*.*B", [Base, Pad, MaxVal]),
  CharsPerDatum = length(TmpBuf),
  Opts#options{chars_per_datum = CharsPerDatum}.

%% Od ==========================================================================

od(Opts, Files) ->
  #options{read_bytes = ReadBytes, skip_bytes = SkipBytes} = Opts,
  skip_bytes(SkipBytes, Opts, input_init(Files, ReadBytes), 0),
  halt(0).

skip_bytes(0, Opts, Input, Offset) -> output_lines(Opts, Input, Offset);
skip_bytes(SkipBytes, Opts, Input, Offset) when SkipBytes > 0 ->
  case input_fgetc_raw(Input) of
    {eof, _NewInput} ->
      escript_runtime:fatal("cannot skip past end of combined input\n", []);
    {_Ch, NewInput} ->
      skip_bytes(SkipBytes - 1, Opts, NewInput, Offset + 1)
  end.

output_lines(Opts, Input, Offset) ->
  print_offset(Opts, Offset),
  {NrBytes, LineBytes, NewInput} = input_line(Opts, Input),
  if NrBytes =:= 0 -> io:format("\n");
     true ->
          NrDatums = print_datums(Opts, NrBytes, LineBytes),
          print_padding(Opts, NrDatums),
          print_printable(Opts, LineBytes),
          io:format("\n"),
          output_lines(Opts, NewInput, Offset + NrBytes)
  end.

print_offset(Opts, Offset) ->
  case Opts#options.address_radix of
    $o -> io:format("~*.*.*B", [12, 8, $0, Offset]);
    $d -> io:format("~*.*.*B", [10, 10, $0, Offset]);
    $x -> io:format("~*.*.*B", [9, 16, $0, Offset]);
    $n -> ok
  end.

input_line(Opts, Input) ->
  input_line(Opts#options.bytes_per_line, Input, 0, []).

input_line(0, Input, NrBytes, LineBytes) ->
  {NrBytes, lists:reverse(LineBytes), Input};
input_line(N, Input, NrBytes, LineBytes) when N > 0 ->
  case input_fgetc_limited(Input) of
    {eof, NewInput} ->
      {NrBytes, lists:reverse(LineBytes), NewInput};
    {Ch, NewInput} ->
      input_line(N - 1, NewInput, NrBytes + 1, [Ch | LineBytes])
  end.

print_datums(Opts, NrBytes, LineBytes) ->
  print_datums(Opts, 0, NrBytes, LineBytes).

print_datums(_Opts, NrDatums, _NrBytes, []) ->
  NrDatums;
print_datums(Opts, NrDatums, NrBytes, LineBytes0) ->
  BytesPerDatum = Opts#options.bytes_per_datum,
  {Bytes, LineBytes} =
    if NrBytes >= BytesPerDatum ->
            lists:split(BytesPerDatum, LineBytes0);
       true ->
            {LineBytes0 ++ lists:duplicate(BytesPerDatum - NrBytes, 0), []}
    end,
  print_datum(Opts, Bytes),
  print_datums(Opts, NrDatums + 1, NrBytes - BytesPerDatum, LineBytes).

print_datum(Opts, Bytes) ->
  case {Opts#options.bytes_per_datum, Bytes} of
    {1, [B]} -> print_byte(Opts, B);
    {2, [B1, B2]} -> print_number(Opts, bytes_to_uint(B1, B2));
    {4, [B1, B2, B3, B4]} -> print_number(Opts, bytes_to_uint(B1, B2, B3, B4))
  end.

print_byte(Opts, Byte) ->
  case Opts#options.output_type of
    $c -> print_char(Byte);
    _ -> print_number(Opts, Byte)
  end.

print_char(C) ->
  case isprint(C) of
    true ->
      io:format("   ~c", [C]);
    false ->
      case C of
        $\0 -> io:format("  \\0");
        $\t -> io:format("  \\t");
        $\r -> io:format("  \\r");
        $\n -> io:format("  \\n");
        $\f -> io:format("  \\f");
        $\e -> io:format("  \\e");
        _ -> io:format(" ~3.8.0B", [C])
      end
  end.

print_number(Opts, UInt) ->
  OutputType = Opts#options.output_type,
  Number =
    case OutputType of
      $d -> sign_extend(UInt, 9 * Opts#options.bytes_per_datum);
      _ -> UInt
    end,
  {Base, Pad} =
    case OutputType of
      $d -> {10, $\s};
      $u -> {10, $\s};
      $o -> {8, $0};
      $x -> {16, $0}
    end,
  io:format(" ~*.*.*B", [Opts#options.chars_per_datum, Base, Pad, Number]).

sign_extend(UInt, NrBits) ->
  Max = (1 bsl NrBits) - 1,
  SignBit = 1 bsl (NrBits - 1),
  ((UInt band Max) bxor SignBit) - SignBit.

bytes_to_uint(B1, B2) -> % PDP10 has big-endian byte order
  ((B1 band 16#1FF) bsl 9) bor (B2 band 16#1FF).

bytes_to_uint(B1, B2, B3, B4) -> % PDP10 has big-endian byte order
  ((B1 band 16#1FF) bsl 27) bor
  ((B2 band 16#1FF) bsl 18) bor
  ((B3 band 16#1FF) bsl 9) bor
  (B4 band 16#1FF).

print_padding(Opts, NrDatums) ->
  #options{ chars_per_datum = CharsPerDatum
          , datums_per_line = DatumsPerLine
          } = Opts,
  if NrDatums < DatumsPerLine ->
          NrSpaces = (CharsPerDatum + 1) * (DatumsPerLine - NrDatums),
          io:format("~.*s", [NrSpaces, ""]);
     true ->
          ok
  end.

print_printable(#options{output_z = false}, _LineBytes) -> ok;
print_printable(#options{output_z = true}, LineBytes) ->
  io:format("  >"),
  lists:foreach(fun print_printable/1, LineBytes),
  io:format("<").

print_printable(C) ->
  OutC = case isprint(C) of true -> C; false -> $. end,
  io:format("~c", [OutC]).

isprint(C) ->
  io_lib:printable_list([C])
  andalso not lists:member(C, [$\n, $\r, $\t, $\v, $\b, $\f, $\e]).

%% Input iterator ==============================================================

-record(input, {
            read_bytes  % input limit, or -1 for unlimited
          , pdp10fp     % current file handle, or [] if none
          , file        % current file name
          , files       % remaining input files
        }).

input_init(Files, ReadBytes) ->
  {FP, File} =
    case Files of
      [] -> {ok, FP0} = pdp10_stdio:stdin(), {FP0, "<stdin>"};
      _ -> {[], []}
    end,
  #input{ read_bytes = ReadBytes
        , pdp10fp = FP
        , file = File
        , files = Files
        }.

input_fgetc_raw(Input) ->
  case Input#input.pdp10fp of
    [] ->
      case Input#input.files of
        [] ->
          {eof, Input};
        ["-" | Files] ->
          {ok, FP} = pdp10_stdio:stdin(),
          input_fgetc_raw(Input#input{ pdp10fp = FP
                                     , file = "<stdin>"
                                     , files = Files
                                     });
        [File | Files] ->
          case pdp10_stdio:fopen(File, [read]) of
            {ok, FP} ->
              input_fgetc_raw(Input#input{ pdp10fp = FP
                                         , file = File
                                         , files = Files
                                         });
            {error, Reason} ->
              escript_runtime:fatal("failed to open ~s: ~s\n",
                                    [File, error:format(Reason)])
          end
      end;
    FP ->
      case pdp10_stdio:fgetc(FP) of
        {ok, Ch} ->
          {Ch, Input};
        eof ->
          pdp10_stdio:fclose(FP),
          input_fgetc_raw(Input#input{pdp10fp = []});
        {error, Reason} ->
          File = Input#input.file,
          escript_runtime:fatal("error reading ~s: ~s\n",
                                [File, error:format(Reason)])
      end
  end.

input_fgetc_limited(Input0 = #input{read_bytes = ReadBytes}) ->
  case ReadBytes of
    0 -> {eof, Input0};
    _ ->
      case input_fgetc_raw(Input0) of
        {eof, Input} -> {eof, Input};
        {Ch, Input} ->
          if ReadBytes > 0 ->
                  {Ch, Input#input{read_bytes = ReadBytes - 1}};
             true ->
                  {Ch, Input}
          end
      end
  end.
