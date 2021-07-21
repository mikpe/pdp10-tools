%%% -*- erlang-indent-level: 2 -*-
%%%
%%% 'ld' clone for pdp10-elf
%%% Copyright (C) 2020-2021  Mikael Pettersson
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

-module(ld).

-export([ main/1
        , format_error/1
        ]).

-define(EMULATION, "elf36_pdp10").
-define(OBJFORMAT, "elf36-pdp10").

-record(options,
        { entry               = "_start" :: string() | non_neg_integer()
        , files               = []       :: [{file, string()} | {library, string()}]
        , library_path        = []       :: [string()]
        , output              = "a.out"  :: string()
        , page_align_sections = false    :: boolean()
        , rwtext              = false    :: boolean()
        , section_starts      = []       :: [{string(), integer()}]
        , trace               = 0        :: non_neg_integer()
        }).

%% Command-line interface ======================================================

main(Argv) ->
  escript_runtime:start(fun main_/1, Argv).

-spec main_([string()]) -> no_return().
main_(Argv) ->
  case ld(Argv) of
    ok -> halt(0);
    {error, Reason} ->
      escript_runtime:fatal("~s\n", [error:format(Reason)])
  end.

ld(Argv) ->
  %% NYI options:
  %% @file
  %% -a
  %% --audit
  %% -c / --mri-script
  %% -d / -dc / -dp
  %% -P / --depaudit
  %% --exclude-libs
  %% --exclude-modules-for-imlib
  %% -E / --[no-]export-dynamic
  %% -EL
  %% -f / --auxiliary
  %% -F / --filter
  %% -fini
  %% -g
  %% -G / --gpsize
  %% -init
  %% -h / -soname
  %% -M / --print-map
  %% --[no-]print-map-discarded
  %% -O
  %% -plugin
  %% --push-state / --pop-state
  %% -q / --emit-relocs
  %% --force-dynamic
  %% -r / --relocatable / -i
  %% -R / --just-symbols
  %% -s / --strip-all
  %% -S / --strip-debug
  %% --[no-]strip-discarded
  %% -T / --script
  %% -dT / --default-script
  %% -u / --undefined
  %% --require-defined
  %% -Ur
  %% --orphan-handling
  %% --unique
  %% -x / --discard-all
  %% -X / --discard-locals
  %% -y / --trace-symbol
  %% -Y
  %% -z
  %% -( / -) / --start-group / --end-group
  %% --[no-]accept-unknown-input-arch
  %% --[no-]as-needed
  %% -assert
  %% -Bdynamic / -dy / -call_shared
  %% -Bgroup
  %% -Bstatic / -dn / -non_shared / -static
  %% -Bsymbolic
  %% -Bsymbolic-functions
  %% --dynamic-list
  %% --dynamic-list-data
  %% --dynamic-list-cpp-new
  %% --dynamic-list-cpp-typeinfo
  %% --[no-]check-sections
  %% --[no-]copy-dt-needed-entries / --[no-]add-needed
  %% --cref
  %% --no-define-common
  %% --force-group-allocation
  %% --defsym
  %% --[no-]demangle
  %% -I / --dynamic-linker
  %% --no-dynamic-linker
  %% --embedded-relocs
  %% --disable-multiple-abs-defs
  %% --[no-]fatal-warnings
  %% --force-exe-suffix
  %% --[no-]gc-sections
  %% --[no-]print-gc-sections
  %% --gc-keep-exported
  %% --print-memory-usage
  %% -Map
  %% --no-keep-memory
  %% --no-undefined / -z defs
  %% --allow-multiple-definitions / -z muldefs
  %% --[no-]allow-shlib-undefined
  %% --no-undefined-version
  %% --default-symver
  %% --default-imported-symver
  %% --no-warn-mismatch
  %% --no-warn-search-mismatch
  %% --noinhibit-exec
  %% -nostdlib
  %% --out-implib
  %% -pie / --pic-executable
  %% -qmagic
  %% -Qy
  %% --[no-]relax
  %% --retain-symbols-file
  %% -rpath
  %% -rpath-link
  %% -shared / -Bshareable
  %% --sort-common
  %% --sort-section
  %% --spare-dynamic-tags
  %% --split-by-file
  %% --split-by-reloc
  %% --stats
  %% --sysroot
  %% --task-link
  %% --traditional-format
  %% -Tldata-segment
  %% --unresolved-symbols
  %% --dll-verbose / --verbose
  %% --version-script
  %% --warn-common
  %% --warn-constructors
  %% --warn-multiple-gp
  %% --warn-once
  %% --warn-section-align
  %% --warn-shared-textrel
  %% --warn-alternate-em
  %% --warn-unresolved-symbols / --error-unresolved-symbols
  %% --[no-]whole-archive
  %% --wrap
  %% --[no-]eh-frame-hdr
  %% --no-ld-generated-unwind-info
  %% --enable-new-dtags / --disable-new-dtags
  %% --hash-size
  %% --hash-style
  %% --compress-debug-sections
  %% --reduce-memory-overheads
  %% --build-id
  %% All other target-specific options.
  %%
  %% LD has quite archaic option syntax:
  %% - it takes some single-dash long options, e.g. -nostdlib
  %% - options and non-options must be processed in the order given,
  %%   e.g. due to --start-group <file.o>... --end-group
  case my_getopt:parse(Argv, "-b:e:l:L:m:nNo:tvV",
                       [ %% single-dash long options
                         { "-EB", no, 'EB' }
                       , { "-Tbss", required, 'Tbss' }
                       , { "-Tdata", required, 'Tdata' }
                       , { "-Trodata-segment", required, 'Trodata' }
                       , { "-Ttext", required, 'Ttext' }
                       , { "-Ttext-segment", required, 'Ttext' }
                         %% long-only options
                       , { "help", no, help }
                       , { "no-omagic", no, no_omagic }
                       , { "oformat", required, oformat }
                       , { "print-output-format", no, print_output_format }
                       , { "section-start", required, section_start }
                       , { "target-help", no, target_help }
                         %% long aliases for short options
                       , { "entry", required, $e }
                       , { "format", required, $b }
                       , { "library", required, $l }
                       , { "library-path", required, $L }
                       , { "nmagic", no, $n }
                       , { "omagic", no, $N }
                       , { "output", required, $o }
                       , { "trace", no, $t }
                       , { "version", no, $v }
                       ]) of
    {ok, {Opts, _NonOpts = []}} ->
      case process_options(Opts) of
        {ok, Options} ->
          case input(Options) of
            {ok, Inputs} ->
              %% TODO: receive ok | error
              {ok, Sections} = phase1(Options, Inputs),
              %% TODO: receive ok | error
              {ok, Segments0} = phase2(Options, Sections),
              Segments = assign(Options, Segments0),
              {GlobalMap, FileMap} = symtab_build(Options, Inputs, Segments),
              output(Options, Segments, GlobalMap, FileMap);
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

process_options(Opts) -> process_options(Opts, #options{}).

process_options([Opt | Opts], Options) ->
  case process_option(Opt, Options) of
    {ok, NewOptions} -> process_options(Opts, NewOptions);
    {error, _Reason} = Error -> Error
  end;
process_options([], Options) ->
  #options{ files = Files, library_path = LibraryPath } = Options,
  {ok, Options#options{ files = lists:reverse(Files)
                      , library_path = lists:reverse(LibraryPath)
                      }}.

process_option(Opt, Options) ->
  case Opt of
    'EB'                -> handle_EB(Opt, Options);
    {$m, _}             -> handle_emulation(Opt, Options);
    {$e, _}             -> handle_entry(Opt, Options);
    {1, _}              -> handle_file(Opt, Options);
    {$b, _}             -> handle_format(Opt, Options);
    help                -> handle_help(Opt, Options);
    {$l, _}             -> handle_library(Opt, Options);
    {$L, _}             -> handle_library_path(Opt, Options);
    $n                  -> handle_nmagic(Opt, Options);
    no_omagic           -> handle_no_omagic(Opt, Options);
    {oformat, _}        -> handle_oformat(Opt, Options);
    $N                  -> handle_omagic(Opt, Options);
    {$o, _}             -> handle_output(Opt, Options);
    print_output_format -> handle_print_output_format(Opt, Options);
    {section_start, _}  -> handle_section_start(Opt, Options);
    target_help         -> handle_target_help(Opt, Options);
    {'Tbss', _}         -> handle_Tbss(Opt, Options);
    {'Tdata', _}        -> handle_Tdata(Opt, Options);
    $t                  -> handle_trace(Opt, Options);
    {'Trodata', _}      -> handle_Trodata(Opt, Options);
    {'Ttext', _}        -> handle_Ttext(Opt, Options);
    $V                  -> handle_V(Opt, Options);
    $v                  -> handle_version(Opt, Options)
  end.

%% Option Handlers =============================================================

handle_EB('EB', Options) ->
  {ok, Options}.

handle_emulation({$m, Emulation}, Options) ->
  case Emulation of
    ?EMULATION -> {ok, Options};
    _ -> {error, {?MODULE, {invalid, emulation, Emulation}}}
  end.

handle_entry({$e, Entry}, Options) ->
  EntryVal =
    case strtol:parse(Entry, _Base = 0) of
      {ok, {Number, _Rest = []}} when Number >= 0 -> Number;
      _ -> Entry % assume it's a symbol
    end,
  {ok, Options#options{entry = EntryVal}}.

handle_file({1, File}, Options) ->
  {ok, Options#options{files = [{file, File} | Options#options.files]}}.

handle_format({$b, InputFormat}, Options) ->
  case InputFormat of
    ?OBJFORMAT -> {ok, Options};
    _ -> {error, {?MODULE, {invalid, "input format", InputFormat}}}
  end.

handle_help(help, _Options) ->
  io:format("--help: NYI\n"),
  exit(0).

handle_library({$l, Library}, Options) ->
  {ok, Options#options{files = [{library, Library} | Options#options.files]}}.

handle_library_path({$L, Path}, Options) ->
  {ok, Options#options{library_path = [Path | Options#options.library_path]}}.

handle_nmagic($n, Options) ->
  {ok, Options#options{page_align_sections = false}}.

handle_no_omagic(no_omagic, Options) ->
  {ok, Options#options{rwtext = false, page_align_sections = true}}.

handle_oformat({oformat, OutputFormat}, Options) ->
  case OutputFormat of
    ?OBJFORMAT -> {ok, Options};
    _ -> {error, {?MODULE, {invalid, "output format", OutputFormat}}}
  end.

handle_omagic($N, Options) ->
  {ok, Options#options{rwtext = true, page_align_sections = false}}.

handle_output({$o, Output}, Options) ->
  {ok, Options#options{output = Output}}.

handle_print_output_format(print_output_format, Options) ->
  io:format(?OBJFORMAT "\n"),
  {ok, Options}.

handle_section_start({section_start, Arg}, Options) ->
  case string:split(Arg, "=") of
    [Name, Org] -> section_start(Name, Org, Options);
    _ -> {error, {?MODULE, {invalid, "section-start", Arg}}}
  end.

handle_target_help(target_help, _Options) ->
  io:format("No target-specific options apply.\n"),
  exit(0).

handle_Tbss({'Tbss', Org}, Options) ->
  section_start(".bss", Org, Options).

handle_Tdata({'Tdata', Org}, Options) ->
  section_start(".data", Org, Options).

handle_trace($t, Options) ->
  {ok, Options#options{trace = Options#options.trace + 1}}.

handle_Trodata({'Trodata', Org}, Options) ->
  section_start(".rodata", Org, Options).

handle_Ttext({'Ttext', Org}, Options) ->
  section_start(".text", Org, Options).

handle_V($V, Options) ->
  version(),
  io:format("  Supported emulations:\n"),
  io:format("   " ?EMULATION "\n"),
  {ok, Options}.

handle_version($v, Options) ->
  version(),
  {ok, Options}.

section_start(Name, Org, Options) ->
  case strtol:parse(Org, _Base = 16) of
    {ok, {Value, _Rest = []}} ->
      SectionStarts = Options#options.section_starts,
      NewSectionStarts = lists:keystore(Name, 1, SectionStarts, {Name, Value}),
      {ok, Options#options{section_starts = NewSectionStarts}};
    _ -> {error, {?MODULE, {invalid, "org for section " ++ Name, Org}}}
  end.

version() ->
  io:format("pdp10-tools ld version 0.1\n").

%% Input Processing ============================================================

input(Options) ->
  Entry = Options#options.entry,
  UndefSyms =
    if is_list(Entry) -> [Entry];
       true -> []
    end,
  ld_input:input(Options#options.files, UndefSyms).

%% Linking Phase 1 =============================================================

phase1(_Options, Inputs) -> {ok, ld_phase1:phase1(Inputs)}.

%% Linking Phase 2 =============================================================

phase2(_Options, Sections) -> {ok, ld_phase2:phase2(Sections)}.

%% Assigning Addresses =========================================================

assign(_Options, Segments) -> ld_assign:assign(Segments).

%% Build symbol tables =========================================================

symtab_build(_Options, Inputs, Segments) -> ld_symtab:build(Inputs, Segments).

%% Output Generation ===========================================================

output(Options, Segments, GlobalMap, FileMap) ->
  #options{output = File, entry = Entry} = Options,
  ld_output:output(File, Entry, Segments, GlobalMap, FileMap).

%% Error Formatting ============================================================

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {invalid, What, Parameter} ->
      io_lib:format("invalid ~s: ~s", [What, Parameter])
  end.
