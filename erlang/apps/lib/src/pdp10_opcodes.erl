%%% -*- erlang-indent-level: 2 -*-
%%%
%%% pdp10_opcodes.erl
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

-module(pdp10_opcodes).

%% API
-export([ cpu_device_from_name/2
        , insn_from_name/3
        , insn_from_high13/4
        , models_from_name/1
        ]).

%% Internal export
-export([ own_lookup_table/0 ]).

-include_lib("lib/include/pdp10_opcodes.hrl").

-spec insn_from_name(pdp10_cpu_models(), string(), boolean()) -> #pdp10_insn_desc{} | false.
insn_from_name(Models, Name, HaveA) ->
  insn_from_name_1(descs_from_key(Name), Models, HaveA).

%% Find first matching descriptor.
insn_from_name_1([], _Models, _HaveA) -> false;
insn_from_name_1([Desc | Descs], Models, HaveA) ->
  case models_matches(Models, Desc) andalso havea_matches(HaveA, Desc) of
    true -> Desc;
    false -> insn_from_name_1(Descs, Models, HaveA)
  end.

models_matches(Models, #pdp10_insn_desc{models = Models2}) ->
  (Models2 band Models) =:= Models.

havea_matches(HaveA, #pdp10_insn_desc{format = Format}) ->
  case Format of
    ?PDP10_INSN_A_OPCODE -> not HaveA;
    _ -> true
  end.

-spec insn_from_high13(pdp10_cpu_models(), 0..((1 bsl 13) - 1), boolean(), boolean()) -> #pdp10_insn_desc{} | false.
insn_from_high13(Models, High13, Extended, Section0) ->
  insn_from_high13_1(descs_from_key(High13), Models, Extended, Section0, false).

%% Find highest-priority matching descriptor.
insn_from_high13_1([], _Models, _Extended, _Section0, Result) -> Result;
insn_from_high13_1([Desc | Descs], Models, Extended, Section0, Result) ->
  NewResult =
    case models_matches(Models, Desc) andalso
         extended_matches(Extended, Desc) andalso
         section0_matches(Section0, Desc) of
      true ->
        if Result =:= false -> Desc;
           Desc#pdp10_insn_desc.priority > Result#pdp10_insn_desc.priority -> Desc;
           true -> Result
        end;
      false -> Result
    end,
  insn_from_high13_1(Descs, Models, Extended, Section0, NewResult).

extended_matches(Extended, Desc) ->
  Extended =:= Desc#pdp10_insn_desc.extended.

section0_matches(Section0, #pdp10_insn_desc{section0 = RequiredSection0}) ->
  RequiredSection0 =:= undefined orelse RequiredSection0 =:= Section0.

%% Access lookup table, mapping names or opcode high13 bits,
%% to candidate descriptors.
%% TODO: _maybe_ use peristent_term instead
descs_from_key(NameOrHigh13) ->
  ensure_lookup_table(),
  case ets:lookup(?MODULE, NameOrHigh13) of
    [{_NameOrHigh13, Descs}] -> Descs;
    [] -> []
  end.

%% One-time initialization of the lookup table.
ensure_lookup_table() ->
  case ets:info(?MODULE, keypos) of
    undefined ->
      Self = self(),
      Pid = spawn_link(fun() -> build_lookup_table(Self) end),
      MonRef = monitor(process, Pid),
      Pid ! {Self, MonRef},
      unlink(Pid),
      receive
        {Pid, MonRef, Result} ->
          demonitor(MonRef, [flush]),
          case Result of
            ok -> ok;
            eagain -> ensure_lookup_table()
          end;
        {'DOWN', MonRef, _Type, _Pid, Reason} -> error(Reason)
      end;
    _KeyPos -> ok
  end.

build_lookup_table(Pid) ->
  receive {Pid, MonRef} -> ok end,
  try
    try register(?MODULE, self())
    catch error:badarg -> error(eagain)
    end,
    Map = build_lookup_map(),
    Records = maps:to_list(Map),
    try
      ets:new(?MODULE, [named_table]),
      ets:insert(?MODULE, Records)
    catch error:badarg -> error(eagain)
    end,
    Pid ! {self(), MonRef, ok}
  catch error:eagain ->
    Pid ! {self(), MonRef, eagain}
  end,
  own_lookup_table().

own_lookup_table() ->
  receive _ -> ok after 5*60*1000 -> ok end,
  ?MODULE:own_lookup_table().

build_lookup_map() ->
  add_descs(pdp10_extended_insns(), add_descs(pdp10_insns(), maps:new())).

add_descs([], Map) -> Map;
add_descs([Desc | Descs], Map) ->
  add_descs(Descs, add_keys(desc_keys(Desc), Desc, Map)).

add_keys([], _Desc, Map) -> Map;
add_keys([Key | Keys], Desc, Map) ->
  add_keys(Keys, Desc, add_key(Key, Desc, Map)).

add_key(Key, Desc, Map) ->
  Descs = maps:get(Key, Map, []),
  Models = Desc#pdp10_insn_desc.models,
  true = Models =/= 0,
  [check_descs(Key, Desc, Desc2) || Desc2 <- Descs],
  maps:put(Key, [Desc | Descs], Map).

check_descs(Key, Desc1, Desc2) ->
  case Desc1#pdp10_insn_desc.models band Desc2#pdp10_insn_desc.models of
    0 -> ok;
    _ ->
      case is_integer(Key) of
        true -> % High13, for disassembly
          case Desc1#pdp10_insn_desc.section0 =/= Desc2#pdp10_insn_desc.section0 orelse
               Desc1#pdp10_insn_desc.extended =/= Desc2#pdp10_insn_desc.extended of
            true -> ok;
            false ->
              case is_integer(Desc1#pdp10_insn_desc.priority) andalso
                   is_integer(Desc2#pdp10_insn_desc.priority) andalso
                   Desc1#pdp10_insn_desc.priority =/= Desc2#pdp10_insn_desc.priority of
                true -> ok;
                false -> error({check_models, Key, Desc1, Desc2})
              end
          end;
        false -> % Name, for assembly
          case (Desc1#pdp10_insn_desc.format =:= ?PDP10_INSN_A_OPCODE orelse
                Desc2#pdp10_insn_desc.format =:= ?PDP10_INSN_A_OPCODE) andalso
               Desc1#pdp10_insn_desc.format =/= Desc2#pdp10_insn_desc.format of
            true -> ok;
            false -> error({check_models, Key, Desc1, Desc2})
          end
      end
  end.

desc_keys(#pdp10_insn_desc{name = Name, high13 = High13, format = Format}) ->
  [Name | high13_keys(High13, Format)].

high13_keys(High13, Format) ->
  case Format of
    ?PDP10_INSN_BASIC ->
      %% add all values for the A field (low 4 bits)
      high13_keys(High13, _Shift = 0, _Width = 4);
    ?PDP10_INSN_A_OPCODE ->
      %% use given bits as-is
      [High13];
    ?PDP10_INSN_A_NONZERO ->
      %% add all values for the A field (low 4 bits)
      [_Key0 | Keys] = high13_keys(High13, _Shift = 0, _Width = 4),
      Keys;
    ?PDP10_INSN_IO ->
      %% add all values for the device field (middle 7 bits)
      high13_keys(High13, _Shift = 3, _Width = 7)
  end.

high13_keys(High13, Shift, Width) ->
  Value = (1 bsl Width) - 1,
  0 = High13 band (Value bsl Shift),
  high13_keys(High13, Shift, Value, []).

high13_keys(_High13, _Shift, -1, Keys) -> Keys;
high13_keys(High13, Shift, Value, Keys) ->
  Key = High13 bor (Value bsl Shift),
  high13_keys(High13, Shift, Value - 1, [Key | Keys]).

%% Macros to initialize both high13 and format in instruction descriptors.
%% Flags E_UNUSED and EXTENDED may be set separately.
%%
%% The convention in documentation is to list opcodes as three or five-digit
%% octal numbers, with zeros in IO device subfields.  Five-digit numbers have
%% two excess bits, which are removed by the macros to produce 13 bits.

-define(BASIC(OPCODE9), high13 = (OPCODE9) bsl 4, format = ?PDP10_INSN_BASIC).
-define(A_OPCODE(OPCODE15), high13 = (OPCODE15) bsr 2, format = ?PDP10_INSN_A_OPCODE).
-define(A_NONZERO(OPCODE9), high13 = (OPCODE9) bsl 4, format = ?PDP10_INSN_A_NONZERO).
-define(IO(OPCODE15), high13 = (OPCODE15) bsr 2, format = ?PDP10_INSN_IO).
-define(E_UNUSED(Opcode), Opcode, e_unused = true).
-define(SECTION_ZERO(Opcode), Opcode, section0 = true).
-define(SECTION_NONZERO(Opcode), Opcode, section0 = false).

%% A_UNUSED is shorthand for A_OPCODE with the opcode bits in A set to zero.
-define(A_UNUSED(OPCODE9), high13 = (OPCODE9) bsl 4, format = ?PDP10_INSN_A_OPCODE).

%% The .priority field is used to disambiguate lookups when a key maps to
%% multiple descriptors, and the models are not mutually exclusive.  This
%% matters mostly for disassembly, i.e. insn_from_high13/4.
-define(PRIO(Opcode, Prio), Opcode, priority = Prio).

-define(D(Name, Opcode, Models), #pdp10_insn_desc{name = Name, Opcode, models = Models}).

%% Much of the contents of these tables is based on Lars Brinkhoff's
%% pdp10-its-disassembler, but the code is completely rewritten.

pdp10_insns() ->
  [
    %% name,            high13, format, flags,          models

    %% 000: ILLEGAL
    %% 001-037: LUUOs
    %% ITS MUUOs
    ?D(".iot",          ?BASIC(8#040),                  ?PDP10_ITS)
  , ?D(".open",         ?BASIC(8#041),                  ?PDP10_ITS)
  , ?D(".oper",         ?BASIC(8#042),                  ?PDP10_ITS)
  , ?D(".call",         ?A_OPCODE(8#04300),             ?PDP10_ITS)
  , ?D(".dismis",       ?A_OPCODE(8#04304),             ?PDP10_ITS)
  , ?D(".lose",         ?A_OPCODE(8#04310),             ?PDP10_ITS)
  , ?D(".tranad",       ?A_OPCODE(8#04314),             ?PDP10_ITS)
  , ?D(".value",        ?A_OPCODE(8#04320),             ?PDP10_ITS)
  , ?D(".utran",        ?A_OPCODE(8#04324),             ?PDP10_ITS)
  , ?D(".core",         ?A_OPCODE(8#04330),             ?PDP10_ITS)
  , ?D(".trand",        ?A_OPCODE(8#04334),             ?PDP10_ITS)
  , ?D(".dstart",       ?A_OPCODE(8#04340),             ?PDP10_ITS)
  , ?D(".fdele",        ?A_OPCODE(8#04344),             ?PDP10_ITS)
  , ?D(".dstrtl",       ?A_OPCODE(8#04350),             ?PDP10_ITS)
  , ?D(".suset",        ?A_OPCODE(8#04354),             ?PDP10_ITS)
  , ?D(".ltpen",        ?A_OPCODE(8#04360),             ?PDP10_ITS)
  , ?D(".vscan",        ?A_OPCODE(8#04364),             ?PDP10_ITS)
  , ?D(".potset",       ?A_OPCODE(8#04370),             ?PDP10_ITS)
  , ?D(".uset",         ?BASIC(8#044),                  ?PDP10_ITS)
  , ?D(".break",        ?BASIC(8#045),                  ?PDP10_ITS)
  , ?D(".status",       ?BASIC(8#046),                  ?PDP10_ITS)
  , ?D(".access",       ?BASIC(8#047),                  ?PDP10_ITS)

    %% TOPS-10 MUUOs (formats and models guesstimates)
  , ?D(".call",         ?BASIC(8#040),                  ?PDP10_KA10up_not_ITS)
  , ?D(".init",         ?BASIC(8#041),                  ?PDP10_KA10up_not_ITS)
    %% 042-046: reserved MUUOs
  , ?D(".calli",        ?BASIC(8#047),                  ?PDP10_KA10up_not_ITS)
  , ?D(".open",         ?BASIC(8#050),                  ?PDP10_KA10up_not_ITS)
  , ?D(".ttcall",       ?BASIC(8#051),                  ?PDP10_KA10up_not_ITS)
  , ?D(".rename",       ?BASIC(8#055),                  ?PDP10_KA10up_not_ITS)
  , ?D(".in",           ?BASIC(8#056),                  ?PDP10_KA10up_not_ITS)
  , ?D(".out",          ?BASIC(8#057),                  ?PDP10_KA10up_not_ITS)
  , ?D(".setsts",       ?BASIC(8#060),                  ?PDP10_KA10up_not_ITS)
  , ?D(".stato",        ?BASIC(8#061),                  ?PDP10_KA10up_not_ITS)
  , ?D(".status",       ?PRIO(?BASIC(8#062), 0),        ?PDP10_KA10up_not_ITS)
  , ?D(".getsts",       ?PRIO(?BASIC(8#062), 1),        ?PDP10_KA10up_not_ITS) % FIXME: alias for .status?
  , ?D(".statz",        ?BASIC(8#063),                  ?PDP10_KA10up_not_ITS)
  , ?D(".inbuf",        ?BASIC(8#064),                  ?PDP10_KA10up_not_ITS)
  , ?D(".outbuf",       ?BASIC(8#065),                  ?PDP10_KA10up_not_ITS)
  , ?D(".input",        ?BASIC(8#066),                  ?PDP10_KA10up_not_ITS)
  , ?D(".output",       ?BASIC(8#067),                  ?PDP10_KA10up_not_ITS)
  , ?D(".close",        ?BASIC(8#070),                  ?PDP10_KA10up_not_ITS)
  , ?D(".releas",       ?BASIC(8#071),                  ?PDP10_KA10up_not_ITS)
  , ?D(".mtape",        ?BASIC(8#072),                  ?PDP10_KA10up_not_ITS)
  , ?D(".ugetf",        ?BASIC(8#073),                  ?PDP10_KA10up_not_ITS)
  , ?D(".useti",        ?BASIC(8#074),                  ?PDP10_KA10up_not_ITS)
  , ?D(".useto",        ?BASIC(8#075),                  ?PDP10_KA10up_not_ITS)
  , ?D(".lookup",       ?BASIC(8#076),                  ?PDP10_KA10up_not_ITS)
  , ?D(".enter",        ?BASIC(8#077),                  ?PDP10_KA10up_not_ITS)
  , ?D(".ujen",         ?BASIC(8#100),                  ?PDP10_KA10) % FIXME: or KI10up? + not ITS
    %% 101: unassigned
  , ?D("gfad",          ?BASIC(8#102),                  ?PDP10_KL10_271) % FIXME: or 271up?
  , ?D("gfsb",          ?BASIC(8#103),                  ?PDP10_KL10_271) % FIXME: or 271up?

    %% TOPS-20 MUUO
  , ?D("jsys",          ?BASIC(8#104),                  ?PDP10_KI10up band bnot ?PDP10_ITS)

  , ?D("adjsp",         ?BASIC(8#105),                  ?PDP10_KL10up)
  , ?D("gfmp",          ?BASIC(8#106),                  ?PDP10_KL10_271) % FIXME: or 271up?
  , ?D("gfdv",          ?BASIC(8#107),                  ?PDP10_KL10_271) % FIXME: or 271up?
  , ?D("dfad",          ?BASIC(8#110),                  ?PDP10_KI10up)
  , ?D("dfsb",          ?BASIC(8#111),                  ?PDP10_KI10up)
  , ?D("dfmp",          ?BASIC(8#112),                  ?PDP10_KI10up)
  , ?D("dfdv",          ?BASIC(8#113),                  ?PDP10_KI10up)
  , ?D("dadd",          ?BASIC(8#114),                  ?PDP10_KL10up)
  , ?D("dsub",          ?BASIC(8#115),                  ?PDP10_KL10up)
  , ?D("dmul",          ?BASIC(8#116),                  ?PDP10_KL10up)
  , ?D("ddiv",          ?BASIC(8#117),                  ?PDP10_KL10up)
  , ?D("dmove",         ?BASIC(8#120),                  ?PDP10_KI10up)
  , ?D("dmovn",         ?BASIC(8#121),                  ?PDP10_KI10up)
  , ?D("fix",           ?BASIC(8#122),                  ?PDP10_KI10up)
  , ?D("extend",        ?BASIC(8#123),                  ?PDP10_KL10up)
  , ?D("dmovem",        ?BASIC(8#124),                  ?PDP10_KI10up)
  , ?D("dmovnm",        ?BASIC(8#125),                  ?PDP10_KI10up)
  , ?D("fixr",          ?BASIC(8#126),                  ?PDP10_KI10up)
  , ?D("fltr",          ?BASIC(8#127),                  ?PDP10_KI10up)
  , ?D("ufa",           ?BASIC(8#130),                  ?PDP10_KA10_to_KI10) % FIXME: and TOPS-10 KL10
  , ?D("dfn",           ?BASIC(8#131),                  ?PDP10_KA10_to_KI10) % FIXME: and TOPS-10 KL10
  , ?D("fsc",           ?BASIC(8#132),                  ?PDP10_ALL)
  , ?D("ibp",           ?A_OPCODE(8#13300),             ?PDP10_ALL)
  , ?D("adjbp",         ?A_NONZERO(8#133),              ?PDP10_KL10up)
  , ?D("ildb",          ?BASIC(8#134),                  ?PDP10_ALL)
  , ?D("ldb",           ?BASIC(8#135),                  ?PDP10_ALL)
  , ?D("idpb",          ?BASIC(8#136),                  ?PDP10_ALL)
  , ?D("dpb",           ?BASIC(8#137),                  ?PDP10_ALL)
  , ?D("fad",           ?BASIC(8#140),                  ?PDP10_ALL)
  , ?D("fadl",          ?BASIC(8#141),                  ?PDP6_to_KI10) % FIXME: and TOPS-10 KL10?, not PDP6?
  , ?D("fadm",          ?BASIC(8#142),                  ?PDP10_ALL)
  , ?D("fadb",          ?BASIC(8#143),                  ?PDP10_ALL)
  , ?D("fadr",          ?BASIC(8#144),                  ?PDP10_ALL)
  , ?D("fadri",         ?BASIC(8#145),                  ?PDP10_KA10up)
  , ?D("fadrl",         ?BASIC(8#145),                  ?PDP6)
  , ?D("fadrm",         ?BASIC(8#146),                  ?PDP10_ALL)
  , ?D("fadrb",         ?BASIC(8#147),                  ?PDP10_ALL)
  , ?D("fsb",           ?BASIC(8#150),                  ?PDP10_ALL)
  , ?D("fsbl",          ?BASIC(8#151),                  ?PDP6_to_KI10) % FIXME: and TOPS-10 KL10
  , ?D("fsbm",          ?BASIC(8#152),                  ?PDP10_ALL)
  , ?D("fsbb",          ?BASIC(8#153),                  ?PDP10_ALL)
  , ?D("fsbr",          ?BASIC(8#154),                  ?PDP10_ALL)
  , ?D("fsbri",         ?BASIC(8#155),                  ?PDP10_KA10up)
  , ?D("fsbrl",         ?BASIC(8#155),                  ?PDP6)
  , ?D("fsbrm",         ?BASIC(8#156),                  ?PDP10_ALL)
  , ?D("fsbrb",         ?BASIC(8#157),                  ?PDP10_ALL)
  , ?D("fmp",           ?BASIC(8#160),                  ?PDP10_ALL)
  , ?D("fmpl",          ?BASIC(8#161),                  ?PDP6_to_KI10) % FIXME: and TOPS-10 KL10
  , ?D("fmpm",          ?BASIC(8#162),                  ?PDP10_ALL)
  , ?D("fmpb",          ?BASIC(8#163),                  ?PDP10_ALL)
  , ?D("fmpr",          ?BASIC(8#164),                  ?PDP10_ALL)
  , ?D("fmpri",         ?BASIC(8#165),                  ?PDP10_KA10up)
  , ?D("fmprl",         ?BASIC(8#165),                  ?PDP6)
  , ?D("fmprm",         ?BASIC(8#166),                  ?PDP10_ALL)
  , ?D("fmprb",         ?BASIC(8#167),                  ?PDP10_ALL)
  , ?D("fdv",           ?BASIC(8#170),                  ?PDP10_ALL)
  , ?D("fdvl",          ?BASIC(8#171),                  ?PDP6_to_KI10) % FIXME: and TOPS-10 KL10
  , ?D("fdvm",          ?BASIC(8#172),                  ?PDP10_ALL)
  , ?D("fdvb",          ?BASIC(8#173),                  ?PDP10_ALL)
  , ?D("fdvr",          ?BASIC(8#174),                  ?PDP10_ALL)
  , ?D("fdvri",         ?BASIC(8#175),                  ?PDP10_KA10up)
  , ?D("fdvrl",         ?BASIC(8#175),                  ?PDP6)
  , ?D("fdvrm",         ?BASIC(8#176),                  ?PDP10_ALL)
  , ?D("fdvrb",         ?BASIC(8#177),                  ?PDP10_ALL)
  , ?D("move",          ?BASIC(8#200),                  ?PDP10_ALL)
  , ?D("movei",         ?BASIC(8#201),                  ?PDP10_ALL)
  , ?D("movem",         ?BASIC(8#202),                  ?PDP10_ALL)
  , ?D("moves",         ?BASIC(8#203),                  ?PDP10_ALL)
  , ?D("movs",          ?BASIC(8#204),                  ?PDP10_ALL)
  , ?D("movsi",         ?BASIC(8#205),                  ?PDP10_ALL)
  , ?D("movsm",         ?BASIC(8#206),                  ?PDP10_ALL)
  , ?D("movss",         ?BASIC(8#207),                  ?PDP10_ALL)
  , ?D("movn",          ?BASIC(8#210),                  ?PDP10_ALL)
  , ?D("movni",         ?BASIC(8#211),                  ?PDP10_ALL)
  , ?D("movnm",         ?BASIC(8#212),                  ?PDP10_ALL)
  , ?D("movns",         ?BASIC(8#213),                  ?PDP10_ALL)
  , ?D("movm",          ?BASIC(8#214),                  ?PDP10_ALL)
  , ?D("movmi",         ?BASIC(8#215),                  ?PDP10_ALL)
  , ?D("movmm",         ?BASIC(8#216),                  ?PDP10_ALL)
  , ?D("movms",         ?BASIC(8#217),                  ?PDP10_ALL)
  , ?D("imul",          ?BASIC(8#220),                  ?PDP10_ALL)
  , ?D("imuli",         ?BASIC(8#221),                  ?PDP10_ALL)
  , ?D("imulm",         ?BASIC(8#222),                  ?PDP10_ALL)
  , ?D("imulb",         ?BASIC(8#223),                  ?PDP10_ALL)
  , ?D("mul",           ?BASIC(8#224),                  ?PDP10_ALL)
  , ?D("muli",          ?BASIC(8#225),                  ?PDP10_ALL)
  , ?D("mulm",          ?BASIC(8#226),                  ?PDP10_ALL)
  , ?D("mulb",          ?BASIC(8#227),                  ?PDP10_ALL)
  , ?D("idiv",          ?BASIC(8#230),                  ?PDP10_ALL)
  , ?D("idivi",         ?BASIC(8#231),                  ?PDP10_ALL)
  , ?D("idivm",         ?BASIC(8#232),                  ?PDP10_ALL)
  , ?D("idivb",         ?BASIC(8#233),                  ?PDP10_ALL)
  , ?D("div",           ?BASIC(8#234),                  ?PDP10_ALL)
  , ?D("divi",          ?BASIC(8#235),                  ?PDP10_ALL)
  , ?D("divm",          ?BASIC(8#236),                  ?PDP10_ALL)
  , ?D("divb",          ?BASIC(8#237),                  ?PDP10_ALL)
  , ?D("ash",           ?BASIC(8#240),                  ?PDP10_ALL)
  , ?D("rot",           ?BASIC(8#241),                  ?PDP10_ALL)
  , ?D("lsh",           ?BASIC(8#242),                  ?PDP10_ALL)
  , ?D("jffo",          ?BASIC(8#243),                  ?PDP10_KA10up)
  , ?D("ashc",          ?BASIC(8#244),                  ?PDP10_ALL)
  , ?D("rotc",          ?BASIC(8#245),                  ?PDP10_ALL)
  , ?D("lshc",          ?BASIC(8#246),                  ?PDP10_ALL)
    %% 247: MUUO (XKL-1, KD10, KC10), unassigned/trapping (KI10, KL10, KS10?), unassigned/nop (KA10, PDP-6?)
  , ?D("exch",          ?BASIC(8#250),                  ?PDP10_ALL)
  , ?D("blt",           ?BASIC(8#251),                  ?PDP10_ALL)
  , ?D("aobjp",         ?BASIC(8#252),                  ?PDP10_ALL)
  , ?D("aobjn",         ?BASIC(8#253),                  ?PDP10_ALL)

    %% 254: JRST instruction family.
    %% Special cases first, followed by the generic entry.
    %% 25414, 25440 (jrstil?), 25444, 25454, 25464, 25470, 25470: unassigned
  , ?D("portal",        ?A_OPCODE(8#25404),             ?PDP10_ALL)
  , ?D("jrstf",         ?A_OPCODE(8#25410),             ?PDP10_ALL)
  , ?D("halt",          ?A_OPCODE(8#25420),             ?PDP10_ALL)
  , ?D("xjrstf",        ?A_OPCODE(8#25424),             ?PDP10_KL10up)
  , ?D("xjen",          ?A_OPCODE(8#25430),             ?PDP10_KL10up)
  , ?D("xpcw",          ?A_OPCODE(8#25434),             ?PDP10_KL10up)
  , ?D("jen",           ?A_OPCODE(8#25450),             ?PDP10_ALL)
  , ?D("sfm",           ?A_OPCODE(8#25460),             ?PDP10_KL10up)
  , ?D("jrst",          ?A_UNUSED(8#254),               ?PDP10_ALL)

    %% 255: JFCL instruction family.
    %% Special cases first, followed by the generic entry.
    %% 25514, 25524, 25534, 25544, 25550, 25554, 25560, 25564, 25570, 25574: unassigned
  , ?D("nop",           ?PRIO(?E_UNUSED(?A_OPCODE(8#25500)), 1), ?PDP10_ALL)
  , ?D("jfov",          ?PRIO(?A_OPCODE(8#25504), 1),   ?PDP10_KA10up)
  , ?D("jpcch",         ?PRIO(?A_OPCODE(8#25504), 1),   ?PDP6)
  , ?D("jcry1",         ?PRIO(?A_OPCODE(8#25510), 1),   ?PDP10_ALL)
  , ?D("jcry0",         ?PRIO(?A_OPCODE(8#25520), 1),   ?PDP10_ALL)
  , ?D("jcry",          ?PRIO(?A_OPCODE(8#25530), 1),   ?PDP10_ALL)
  , ?D("jov",           ?PRIO(?A_OPCODE(8#25540), 1),   ?PDP10_ALL)
  , ?D("jfcl",          ?PRIO(?BASIC(8#255), 0),        ?PDP10_ALL)

  , ?D("xct",           ?A_UNUSED(8#256),               ?PDP10_ALL) % A zero, or in user mode, or is a KA10
  , ?D("pxct",          ?A_NONZERO(8#256),              ?PDP10_ALL) % A non-zero and in executive mode
  , ?D("map",           ?BASIC(8#257),                  ?PDP10_KA10_to_KI10) % FIXME: and TOPS-10 KL10, nop on KA10
  , ?D("pushj",         ?BASIC(8#260),                  ?PDP10_ALL)
  , ?D("push",          ?BASIC(8#261),                  ?PDP10_ALL)
  , ?D("pop",           ?BASIC(8#262),                  ?PDP10_ALL)
  , ?D("popj",          ?E_UNUSED(?BASIC(8#263)),       ?PDP10_ALL)
  , ?D("jsr",           ?A_UNUSED(8#264),               ?PDP10_ALL)
  , ?D("jsp",           ?BASIC(8#265),                  ?PDP10_ALL)
  , ?D("jsa",           ?BASIC(8#266),                  ?PDP10_ALL)
  , ?D("jra",           ?BASIC(8#267),                  ?PDP10_ALL)
  , ?D("add",           ?BASIC(8#270),                  ?PDP10_ALL)
  , ?D("addi",          ?BASIC(8#271),                  ?PDP10_ALL)
  , ?D("addm",          ?BASIC(8#272),                  ?PDP10_ALL)
  , ?D("addb",          ?BASIC(8#273),                  ?PDP10_ALL)
  , ?D("sub",           ?BASIC(8#274),                  ?PDP10_ALL)
  , ?D("subi",          ?BASIC(8#275),                  ?PDP10_ALL)
  , ?D("subm",          ?BASIC(8#276),                  ?PDP10_ALL)
  , ?D("subb",          ?BASIC(8#277),                  ?PDP10_ALL)
  , ?D("cai",           ?BASIC(8#300),                  ?PDP10_ALL)
  , ?D("cail",          ?BASIC(8#301),                  ?PDP10_ALL)
  , ?D("caie",          ?BASIC(8#302),                  ?PDP10_ALL)
  , ?D("caile",         ?BASIC(8#303),                  ?PDP10_ALL)
  , ?D("caia",          ?BASIC(8#304),                  ?PDP10_ALL)
  , ?D("caige",         ?BASIC(8#305),                  ?PDP10_ALL)
  , ?D("cain",          ?BASIC(8#306),                  ?PDP10_ALL)
  , ?D("caig",          ?BASIC(8#307),                  ?PDP10_ALL)
  , ?D("cam",           ?BASIC(8#310),                  ?PDP10_ALL)
  , ?D("caml",          ?BASIC(8#311),                  ?PDP10_ALL)
  , ?D("came",          ?BASIC(8#312),                  ?PDP10_ALL)
  , ?D("camle",         ?BASIC(8#313),                  ?PDP10_ALL)
  , ?D("cama",          ?BASIC(8#314),                  ?PDP10_ALL)
  , ?D("camge",         ?BASIC(8#315),                  ?PDP10_ALL)
  , ?D("camn",          ?BASIC(8#316),                  ?PDP10_ALL)
  , ?D("camg",          ?BASIC(8#317),                  ?PDP10_ALL)
  , ?D("jump",          ?BASIC(8#320),                  ?PDP10_ALL)
  , ?D("jumpl",         ?BASIC(8#321),                  ?PDP10_ALL)
  , ?D("jumpe",         ?BASIC(8#322),                  ?PDP10_ALL)
  , ?D("jumple",        ?BASIC(8#323),                  ?PDP10_ALL)
  , ?D("jumpa",         ?BASIC(8#324),                  ?PDP10_ALL)
  , ?D("jumpge",        ?BASIC(8#325),                  ?PDP10_ALL)
  , ?D("jumpn",         ?BASIC(8#326),                  ?PDP10_ALL)
  , ?D("jumpg",         ?BASIC(8#327),                  ?PDP10_ALL)
  , ?D("skip",          ?BASIC(8#330),                  ?PDP10_ALL)
  , ?D("skipl",         ?BASIC(8#331),                  ?PDP10_ALL)
  , ?D("skipe",         ?BASIC(8#332),                  ?PDP10_ALL)
  , ?D("skiple",        ?BASIC(8#333),                  ?PDP10_ALL)
  , ?D("skipa",         ?BASIC(8#334),                  ?PDP10_ALL)
  , ?D("skipge",        ?BASIC(8#335),                  ?PDP10_ALL)
  , ?D("skipn",         ?BASIC(8#336),                  ?PDP10_ALL)
  , ?D("skipg",         ?BASIC(8#337),                  ?PDP10_ALL)
  , ?D("aoj",           ?BASIC(8#340),                  ?PDP10_ALL)
  , ?D("aojl",          ?BASIC(8#341),                  ?PDP10_ALL)
  , ?D("aoje",          ?BASIC(8#342),                  ?PDP10_ALL)
  , ?D("aojle",         ?BASIC(8#343),                  ?PDP10_ALL)
  , ?D("aoja",          ?BASIC(8#344),                  ?PDP10_ALL)
  , ?D("aojge",         ?BASIC(8#345),                  ?PDP10_ALL)
  , ?D("aojn",          ?BASIC(8#346),                  ?PDP10_ALL)
  , ?D("aojg",          ?BASIC(8#347),                  ?PDP10_ALL)
  , ?D("aos",           ?BASIC(8#350),                  ?PDP10_ALL)
  , ?D("aosl",          ?BASIC(8#351),                  ?PDP10_ALL)
  , ?D("aose",          ?BASIC(8#352),                  ?PDP10_ALL)
  , ?D("aosle",         ?BASIC(8#353),                  ?PDP10_ALL)
  , ?D("aosa",          ?BASIC(8#354),                  ?PDP10_ALL)
  , ?D("aosge",         ?BASIC(8#355),                  ?PDP10_ALL)
  , ?D("aosn",          ?BASIC(8#356),                  ?PDP10_ALL)
  , ?D("aosg",          ?BASIC(8#357),                  ?PDP10_ALL)
  , ?D("soj",           ?BASIC(8#360),                  ?PDP10_ALL)
  , ?D("sojl",          ?BASIC(8#361),                  ?PDP10_ALL)
  , ?D("soje",          ?BASIC(8#362),                  ?PDP10_ALL)
  , ?D("sojle",         ?BASIC(8#363),                  ?PDP10_ALL)
  , ?D("soja",          ?BASIC(8#364),                  ?PDP10_ALL)
  , ?D("sojge",         ?BASIC(8#365),                  ?PDP10_ALL)
  , ?D("sojn",          ?BASIC(8#366),                  ?PDP10_ALL)
  , ?D("sojg",          ?BASIC(8#367),                  ?PDP10_ALL)
  , ?D("sos",           ?BASIC(8#370),                  ?PDP10_ALL)
  , ?D("sosl",          ?BASIC(8#371),                  ?PDP10_ALL)
  , ?D("sose",          ?BASIC(8#372),                  ?PDP10_ALL)
  , ?D("sosle",         ?BASIC(8#373),                  ?PDP10_ALL)
  , ?D("sosa",          ?BASIC(8#374),                  ?PDP10_ALL)
  , ?D("sosge",         ?BASIC(8#375),                  ?PDP10_ALL)
  , ?D("sosn",          ?BASIC(8#376),                  ?PDP10_ALL)
  , ?D("sosg",          ?BASIC(8#377),                  ?PDP10_ALL)
  , ?D("setz",          ?E_UNUSED(?BASIC(8#400)),       ?PDP10_ALL)
  , ?D("setzi",         ?E_UNUSED(?BASIC(8#401)),       ?PDP10_ALL)
  , ?D("setzm",         ?A_UNUSED(8#402),               ?PDP10_ALL)
  , ?D("setzb",         ?BASIC(8#403),                  ?PDP10_ALL)
  , ?D("and",           ?BASIC(8#404),                  ?PDP10_ALL)
  , ?D("andi",          ?BASIC(8#405),                  ?PDP10_ALL)
  , ?D("andm",          ?BASIC(8#406),                  ?PDP10_ALL)
  , ?D("andb",          ?BASIC(8#407),                  ?PDP10_ALL)
  , ?D("andca",         ?BASIC(8#410),                  ?PDP10_ALL)
  , ?D("andcai",        ?BASIC(8#411),                  ?PDP10_ALL)
  , ?D("andcam",        ?BASIC(8#412),                  ?PDP10_ALL)
  , ?D("andcab",        ?BASIC(8#413),                  ?PDP10_ALL)
  , ?D("setm",          ?BASIC(8#414),                  ?PDP10_ALL)
  , ?D("xmovei",        ?SECTION_NONZERO(?BASIC(8#415)),?PDP10_KL10up) % in non-zero section, setmi in zero section
  , ?D("setmi",         ?SECTION_ZERO(?BASIC(8#415)),   ?PDP10_ALL) % on KL10up depends on current section
  , ?D("setmm",         ?A_UNUSED(8#416),               ?PDP10_ALL)
  , ?D("setmb",         ?BASIC(8#417),                  ?PDP10_ALL)
  , ?D("andcm",         ?BASIC(8#420),                  ?PDP10_ALL)
  , ?D("andcmi",        ?BASIC(8#421),                  ?PDP10_ALL)
  , ?D("andcmm",        ?BASIC(8#422),                  ?PDP10_ALL)
  , ?D("andcmb",        ?BASIC(8#423),                  ?PDP10_ALL)
  , ?D("seta",          ?E_UNUSED(?BASIC(8#424)),       ?PDP10_ALL)
  , ?D("setai",         ?E_UNUSED(?BASIC(8#425)),       ?PDP10_ALL)
  , ?D("setam",         ?BASIC(8#426),                  ?PDP10_ALL)
  , ?D("setab",         ?BASIC(8#427),                  ?PDP10_ALL)
  , ?D("xor",           ?BASIC(8#430),                  ?PDP10_ALL)
  , ?D("xori",          ?BASIC(8#431),                  ?PDP10_ALL)
  , ?D("xorm",          ?BASIC(8#432),                  ?PDP10_ALL)
  , ?D("xorb",          ?BASIC(8#433),                  ?PDP10_ALL)
  , ?D("ior",           ?PRIO(?BASIC(8#434), 0),        ?PDP10_ALL)
  , ?D("or",            ?PRIO(?BASIC(8#434), 1),        ?PDP10_ALL) % alias for ior
  , ?D("iori",          ?PRIO(?BASIC(8#435), 0),        ?PDP10_ALL)
  , ?D("ori",           ?PRIO(?BASIC(8#435), 1),        ?PDP10_ALL) % alias for iori
  , ?D("iorm",          ?PRIO(?BASIC(8#436), 0),        ?PDP10_ALL)
  , ?D("orm",           ?PRIO(?BASIC(8#436), 1),        ?PDP10_ALL) % alias for iorm
  , ?D("iorb",          ?PRIO(?BASIC(8#437), 0),        ?PDP10_ALL)
  , ?D("orb",           ?PRIO(?BASIC(8#437), 1),        ?PDP10_ALL) % alias for iorb
  , ?D("andcb",         ?BASIC(8#440),                  ?PDP10_ALL)
  , ?D("andcbi",        ?BASIC(8#441),                  ?PDP10_ALL)
  , ?D("andcbm",        ?BASIC(8#442),                  ?PDP10_ALL)
  , ?D("andcbb",        ?BASIC(8#443),                  ?PDP10_ALL)
  , ?D("eqv",           ?BASIC(8#444),                  ?PDP10_ALL)
  , ?D("eqvi",          ?BASIC(8#445),                  ?PDP10_ALL)
  , ?D("eqvm",          ?BASIC(8#446),                  ?PDP10_ALL)
  , ?D("eqvb",          ?BASIC(8#447),                  ?PDP10_ALL)
  , ?D("setca",         ?E_UNUSED(?BASIC(8#450)),       ?PDP10_ALL)
  , ?D("setcai",        ?E_UNUSED(?BASIC(8#451)),       ?PDP10_ALL)
  , ?D("setcam",        ?BASIC(8#452),                  ?PDP10_ALL)
  , ?D("setcab",        ?BASIC(8#453),                  ?PDP10_ALL)
  , ?D("orca",          ?BASIC(8#454),                  ?PDP10_ALL)
  , ?D("orcai",         ?BASIC(8#455),                  ?PDP10_ALL)
  , ?D("orcam",         ?BASIC(8#456),                  ?PDP10_ALL)
  , ?D("orcab",         ?BASIC(8#457),                  ?PDP10_ALL)
  , ?D("setcm",         ?BASIC(8#460),                  ?PDP10_ALL)
  , ?D("setcmi",        ?BASIC(8#461),                  ?PDP10_ALL)
  , ?D("setcmm",        ?A_UNUSED(8#462),               ?PDP10_ALL)
  , ?D("setcmb",        ?BASIC(8#463),                  ?PDP10_ALL)
  , ?D("orcm",          ?BASIC(8#464),                  ?PDP10_ALL)
  , ?D("orcmi",         ?BASIC(8#465),                  ?PDP10_ALL)
  , ?D("orcmm",         ?BASIC(8#466),                  ?PDP10_ALL)
  , ?D("orcmb",         ?BASIC(8#467),                  ?PDP10_ALL)
  , ?D("orcb",          ?BASIC(8#470),                  ?PDP10_ALL)
  , ?D("orcbi",         ?BASIC(8#471),                  ?PDP10_ALL)
  , ?D("orcbm",         ?BASIC(8#472),                  ?PDP10_ALL)
  , ?D("orcbb",         ?BASIC(8#473),                  ?PDP10_ALL)
  , ?D("seto",          ?E_UNUSED(?BASIC(8#474)),       ?PDP10_ALL)
  , ?D("setoi",         ?E_UNUSED(?BASIC(8#475)),       ?PDP10_ALL)
  , ?D("setom",         ?A_UNUSED(8#476),               ?PDP10_ALL)
  , ?D("setob",         ?BASIC(8#477),                  ?PDP10_ALL)
  , ?D("hll",           ?BASIC(8#500),                  ?PDP10_ALL)
  , ?D("xhlli",         ?SECTION_NONZERO(?BASIC(8#501)),?PDP10_KL10up) % in non-zero section, hlli in zero section
  , ?D("hlli",          ?SECTION_ZERO(?BASIC(8#501)),   ?PDP10_ALL) % on KL10up depends on current section
  , ?D("hllm",          ?BASIC(8#502),                  ?PDP10_ALL)
  , ?D("hlls",          ?BASIC(8#503),                  ?PDP10_ALL)
  , ?D("hrl",           ?BASIC(8#504),                  ?PDP10_ALL)
  , ?D("hrli",          ?BASIC(8#505),                  ?PDP10_ALL)
  , ?D("hrlm",          ?BASIC(8#506),                  ?PDP10_ALL)
  , ?D("hrls",          ?BASIC(8#507),                  ?PDP10_ALL)
  , ?D("hllz",          ?BASIC(8#510),                  ?PDP10_ALL)
  , ?D("hllzi",         ?BASIC(8#511),                  ?PDP10_ALL)
  , ?D("hllzm",         ?BASIC(8#512),                  ?PDP10_ALL)
  , ?D("hllzs",         ?BASIC(8#513),                  ?PDP10_ALL)
  , ?D("hrlz",          ?BASIC(8#514),                  ?PDP10_ALL)
  , ?D("hrlzi",         ?BASIC(8#515),                  ?PDP10_ALL)
  , ?D("hrlzm",         ?BASIC(8#516),                  ?PDP10_ALL)
  , ?D("hrlzs",         ?BASIC(8#517),                  ?PDP10_ALL)
  , ?D("hllo",          ?BASIC(8#520),                  ?PDP10_ALL)
  , ?D("hlloi",         ?BASIC(8#521),                  ?PDP10_ALL)
  , ?D("hllom",         ?BASIC(8#522),                  ?PDP10_ALL)
  , ?D("hllos",         ?BASIC(8#523),                  ?PDP10_ALL)
  , ?D("hrlo",          ?BASIC(8#524),                  ?PDP10_ALL)
  , ?D("hrloi",         ?BASIC(8#525),                  ?PDP10_ALL)
  , ?D("hrlom",         ?BASIC(8#526),                  ?PDP10_ALL)
  , ?D("hrlos",         ?BASIC(8#527),                  ?PDP10_ALL)
  , ?D("hlle",          ?BASIC(8#530),                  ?PDP10_ALL)
  , ?D("hllei",         ?BASIC(8#531),                  ?PDP10_ALL)
  , ?D("hllem",         ?BASIC(8#532),                  ?PDP10_ALL)
  , ?D("hlles",         ?BASIC(8#533),                  ?PDP10_ALL)
  , ?D("hrle",          ?BASIC(8#534),                  ?PDP10_ALL)
  , ?D("hrlei",         ?BASIC(8#535),                  ?PDP10_ALL)
  , ?D("hrlem",         ?BASIC(8#536),                  ?PDP10_ALL)
  , ?D("hrles",         ?BASIC(8#537),                  ?PDP10_ALL)
  , ?D("hrr",           ?BASIC(8#540),                  ?PDP10_ALL)
  , ?D("hrri",          ?BASIC(8#541),                  ?PDP10_ALL)
  , ?D("hrrm",          ?BASIC(8#542),                  ?PDP10_ALL)
  , ?D("hrrs",          ?BASIC(8#543),                  ?PDP10_ALL)
  , ?D("hlr",           ?BASIC(8#544),                  ?PDP10_ALL)
  , ?D("hlri",          ?BASIC(8#545),                  ?PDP10_ALL)
  , ?D("hlrm",          ?BASIC(8#546),                  ?PDP10_ALL)
  , ?D("hlrs",          ?BASIC(8#547),                  ?PDP10_ALL)
  , ?D("hrrz",          ?BASIC(8#550),                  ?PDP10_ALL)
  , ?D("hrrzi",         ?BASIC(8#551),                  ?PDP10_ALL)
  , ?D("hrrzm",         ?BASIC(8#552),                  ?PDP10_ALL)
  , ?D("hrrzs",         ?BASIC(8#553),                  ?PDP10_ALL)
  , ?D("hlrz",          ?BASIC(8#554),                  ?PDP10_ALL)
  , ?D("hlrzi",         ?BASIC(8#555),                  ?PDP10_ALL)
  , ?D("hlrzm",         ?BASIC(8#556),                  ?PDP10_ALL)
  , ?D("hlrzs",         ?BASIC(8#557),                  ?PDP10_ALL)
  , ?D("hrro",          ?BASIC(8#560),                  ?PDP10_ALL)
  , ?D("hrroi",         ?BASIC(8#561),                  ?PDP10_ALL)
  , ?D("hrrom",         ?BASIC(8#562),                  ?PDP10_ALL)
  , ?D("hrros",         ?BASIC(8#563),                  ?PDP10_ALL)
  , ?D("hlro",          ?BASIC(8#564),                  ?PDP10_ALL)
  , ?D("hlroi",         ?BASIC(8#565),                  ?PDP10_ALL)
  , ?D("hlrom",         ?BASIC(8#566),                  ?PDP10_ALL)
  , ?D("hlros",         ?BASIC(8#567),                  ?PDP10_ALL)
  , ?D("hrre",          ?BASIC(8#570),                  ?PDP10_ALL)
  , ?D("hrrei",         ?BASIC(8#571),                  ?PDP10_ALL)
  , ?D("hrrem",         ?BASIC(8#572),                  ?PDP10_ALL)
  , ?D("hrres",         ?BASIC(8#573),                  ?PDP10_ALL)
  , ?D("hlre",          ?BASIC(8#574),                  ?PDP10_ALL)
  , ?D("hlrei",         ?BASIC(8#575),                  ?PDP10_ALL)
  , ?D("hlrem",         ?BASIC(8#576),                  ?PDP10_ALL)
  , ?D("hlres",         ?BASIC(8#577),                  ?PDP10_ALL)
  , ?D("trn",           ?BASIC(8#600),                  ?PDP10_ALL)
  , ?D("tln",           ?BASIC(8#601),                  ?PDP10_ALL)
  , ?D("trne",          ?BASIC(8#602),                  ?PDP10_ALL)
  , ?D("tlne",          ?BASIC(8#603),                  ?PDP10_ALL)
  , ?D("trna",          ?BASIC(8#604),                  ?PDP10_ALL)
  , ?D("tlna",          ?BASIC(8#605),                  ?PDP10_ALL)
  , ?D("trnn",          ?BASIC(8#606),                  ?PDP10_ALL)
  , ?D("tlnn",          ?BASIC(8#607),                  ?PDP10_ALL)
  , ?D("tdn",           ?BASIC(8#610),                  ?PDP10_ALL)
  , ?D("tsn",           ?BASIC(8#611),                  ?PDP10_ALL)
  , ?D("tdne",          ?BASIC(8#612),                  ?PDP10_ALL)
  , ?D("tsne",          ?BASIC(8#613),                  ?PDP10_ALL)
  , ?D("tdna",          ?BASIC(8#614),                  ?PDP10_ALL)
  , ?D("tsna",          ?BASIC(8#615),                  ?PDP10_ALL)
  , ?D("tdnn",          ?BASIC(8#616),                  ?PDP10_ALL)
  , ?D("tsnn",          ?BASIC(8#617),                  ?PDP10_ALL)
  , ?D("trz",           ?BASIC(8#620),                  ?PDP10_ALL)
  , ?D("tlz",           ?BASIC(8#621),                  ?PDP10_ALL)
  , ?D("trze",          ?BASIC(8#622),                  ?PDP10_ALL)
  , ?D("tlze",          ?BASIC(8#623),                  ?PDP10_ALL)
  , ?D("trza",          ?BASIC(8#624),                  ?PDP10_ALL)
  , ?D("tlza",          ?BASIC(8#625),                  ?PDP10_ALL)
  , ?D("trzn",          ?BASIC(8#626),                  ?PDP10_ALL)
  , ?D("tlzn",          ?BASIC(8#627),                  ?PDP10_ALL)
  , ?D("tdz",           ?BASIC(8#630),                  ?PDP10_ALL)
  , ?D("tsz",           ?BASIC(8#631),                  ?PDP10_ALL)
  , ?D("tdze",          ?BASIC(8#632),                  ?PDP10_ALL)
  , ?D("tsze",          ?BASIC(8#633),                  ?PDP10_ALL)
  , ?D("tdza",          ?BASIC(8#634),                  ?PDP10_ALL)
  , ?D("tsza",          ?BASIC(8#635),                  ?PDP10_ALL)
  , ?D("tdzn",          ?BASIC(8#636),                  ?PDP10_ALL)
  , ?D("tszn",          ?BASIC(8#637),                  ?PDP10_ALL)
  , ?D("trc",           ?BASIC(8#640),                  ?PDP10_ALL)
  , ?D("tlc",           ?BASIC(8#641),                  ?PDP10_ALL)
  , ?D("trce",          ?BASIC(8#642),                  ?PDP10_ALL)
  , ?D("tlce",          ?BASIC(8#643),                  ?PDP10_ALL)
  , ?D("trca",          ?BASIC(8#644),                  ?PDP10_ALL)
  , ?D("tlca",          ?BASIC(8#645),                  ?PDP10_ALL)
  , ?D("trcn",          ?BASIC(8#646),                  ?PDP10_ALL)
  , ?D("tlcn",          ?BASIC(8#647),                  ?PDP10_ALL)
  , ?D("tdc",           ?BASIC(8#650),                  ?PDP10_ALL)
  , ?D("tsc",           ?BASIC(8#651),                  ?PDP10_ALL)
  , ?D("tdce",          ?BASIC(8#652),                  ?PDP10_ALL)
  , ?D("tsce",          ?BASIC(8#653),                  ?PDP10_ALL)
  , ?D("tdca",          ?BASIC(8#654),                  ?PDP10_ALL)
  , ?D("tsca",          ?BASIC(8#655),                  ?PDP10_ALL)
  , ?D("tdcn",          ?BASIC(8#656),                  ?PDP10_ALL)
  , ?D("tscn",          ?BASIC(8#657),                  ?PDP10_ALL)
  , ?D("tro",           ?BASIC(8#660),                  ?PDP10_ALL)
  , ?D("tlo",           ?BASIC(8#661),                  ?PDP10_ALL)
  , ?D("troe",          ?BASIC(8#662),                  ?PDP10_ALL)
  , ?D("tloe",          ?BASIC(8#663),                  ?PDP10_ALL)
  , ?D("troa",          ?BASIC(8#664),                  ?PDP10_ALL)
  , ?D("tloa",          ?BASIC(8#665),                  ?PDP10_ALL)
  , ?D("tron",          ?BASIC(8#666),                  ?PDP10_ALL)
  , ?D("tlon",          ?BASIC(8#667),                  ?PDP10_ALL)
  , ?D("tdo",           ?BASIC(8#670),                  ?PDP10_ALL)
  , ?D("tso",           ?BASIC(8#671),                  ?PDP10_ALL)
  , ?D("tdoe",          ?BASIC(8#672),                  ?PDP10_ALL)
  , ?D("tsoe",          ?BASIC(8#673),                  ?PDP10_ALL)
  , ?D("tdoa",          ?BASIC(8#674),                  ?PDP10_ALL)
  , ?D("tsoa",          ?BASIC(8#675),                  ?PDP10_ALL)
  , ?D("tdon",          ?BASIC(8#676),                  ?PDP10_ALL)
  , ?D("tson",          ?BASIC(8#677),                  ?PDP10_ALL)

    %% IO and system instructions.  Lots of model-specifics here.
  , ?D("aprid",         ?PRIO(?A_OPCODE(8#70000), 1),   ?PDP10_KL10any bor ?PDP10_KS10 bor ?PDP10_XKL1) % aliases BLKI APR, and APR0
  , ?D("rsw",           ?PRIO(?A_OPCODE(8#70004), 1),   ?PDP10_KA10_to_KI10) % aliases DATAI APR,
  , ?D("wrfil",         ?PRIO(?A_OPCODE(8#70010), 1),   ?PDP10_KL10any) % aliases BLKO APR, and SYSID
  , ?D("wrapr",         ?PRIO(?A_OPCODE(8#70020), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases CONO APR, and APR0
  , ?D("rdapr",         ?PRIO(?A_OPCODE(8#70024), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases CONI APR, and APR0
  , ?D("rdera",         ?PRIO(?A_OPCODE(8#70040), 1),   ?PDP10_KL10any) % aliases BLKI PI, and WCTRLF
  , ?D("sbdiag",        ?PRIO(?A_OPCODE(8#70050), 1),   ?PDP10_KL10any) % aliases BLKO PI, and SIMIRD
  , ?D("wrpi",          ?PRIO(?A_OPCODE(8#70060), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases CONO PI, and APR0
  , ?D("rdpi",          ?PRIO(?A_OPCODE(8#70064), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases CONI PI, and APR0
  , ?D("rdubr",         ?PRIO(?A_OPCODE(8#70104), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases DATAI PAG, and APR1
  , ?D("clrpt",         ?PRIO(?A_OPCODE(8#70110), 1),   ?PDP10_KL10any bor ?PDP10_KS10 bor ?PDP10_XKL1) % aliases BLKO PAG, and APR1
  , ?D("wrubr",         ?PRIO(?A_OPCODE(8#70114), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases DATAO PAG, and APR1
  , ?D("wrebr",         ?A_OPCODE(8#70120),             ?PDP10_KS10) % aliases CONO PAG,
  , ?D("rdebr",         ?A_OPCODE(8#70124),             ?PDP10_KS10) % aliases CONI PAG,
  , ?D("swpia",         ?PRIO(?E_UNUSED(?A_OPCODE(8#70144)), 1), ?PDP10_KL10any bor ?PDP10_XKL1) % aliases DATAI CCA, and APR1
  , ?D("swpva",         ?PRIO(?E_UNUSED(?A_OPCODE(8#70150)), 1), ?PDP10_KL10any bor ?PDP10_XKL1) % aliases BLKO CCA, and APR1
  , ?D("swpua",         ?PRIO(?E_UNUSED(?A_OPCODE(8#70154)), 1), ?PDP10_KL10any bor ?PDP10_XKL1) % aliases DATAO CCA, and APR1
  , ?D("swpio",         ?PRIO(?A_OPCODE(8#70164), 1),   ?PDP10_KL10any bor ?PDP10_XKL1) % aliases CONI CCA, and APR1
  , ?D("swpvo",         ?PRIO(?A_OPCODE(8#70170), 1),   ?PDP10_KL10any bor ?PDP10_XKL1) % aliases CONSZ CCA, and APR1
  , ?D("swpuo",         ?PRIO(?A_OPCODE(8#70174), 1),   ?PDP10_KL10any bor ?PDP10_XKL1) % aliases CONSO CCA, and APR1
  , ?D("rdperf",        ?PRIO(?A_OPCODE(8#70200), 2),   ?PDP10_KL10any) % aliases BLKI TIM, APR2, and RDSPB
  , ?D("rdspb",         ?PRIO(?A_OPCODE(8#70200), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliasesr BLKI TIM, APR2, and RDPERF
  , ?D("rdtime",        ?PRIO(?A_OPCODE(8#70204), 1),   ?PDP10_KL10any band bnot ?PDP10_XKL1) % aliases DATAI TIM, (FIXME: was KL10any)
  , ?D("rdcsb",         ?PRIO(?A_OPCODE(8#70204), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases DATAI TIM, and APR2
  , ?D("wrpae",         ?PRIO(?A_OPCODE(8#70210), 2),   ?PDP10_KL10any) % aliases BLKO TIM, and APR2
  , ?D("rdpur",         ?PRIO(?A_OPCODE(8#70210), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases BLKO TIM, APR2, and WRPAE
  , ?D("rdcstm",        ?PRIO(?A_OPCODE(8#70214), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases APR2
  , ?D("rdtim",         ?A_OPCODE(8#70220),             ?PDP10_KS10) % aliases CONO TIM,
  , ?D("rdint",         ?A_OPCODE(8#70224),             ?PDP10_KS10) % aliases CONI TIM,
  , ?D("rdhsb",         ?A_OPCODE(8#70230),             ?PDP10_KS10) % aliases CONSZ
  , ?D("rdmact",        ?PRIO(?A_OPCODE(8#70240), 2),   ?PDP10_KL10any) % aliases BLKI MTR, APR2, and RDMACT
  , ?D("wrspb",         ?PRIO(?A_OPCODE(8#70240), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases BLKI MTR, APR2, and RDMACT
  , ?D("rdeact",        ?PRIO(?A_OPCODE(8#70244), 2),   ?PDP10_KL10any) % aliases DATAI MTR, APR2, and WRCSB
  , ?D("wrcsb",         ?PRIO(?A_OPCODE(8#70244), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases DATAI MTR, APR2, and RDEACT
  , ?D("wrpur",         ?PRIO(?A_OPCODE(8#70250), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases APR2
  , ?D("wrcstm",        ?PRIO(?A_OPCODE(8#70254), 1),   ?PDP10_KS10 bor ?PDP10_XKL1) % aliases APR2
% , ?D("wrtime",        ?A_OPCODE(8#70260),             ?PDP10_KL10any) % aliases CONO MTR (FIXME: ?)
  , ?D("wrtim",         ?A_OPCODE(8#70260),             ?PDP10_KS10)
  , ?D("wrint",         ?A_OPCODE(8#70264),             ?PDP10_KS10)
  , ?D("wrhsb",         ?A_OPCODE(8#70270),             ?PDP10_KS10)
  , ?D("umove",         ?BASIC(8#704),                  ?PDP10_KS10)
  , ?D("umovem",        ?BASIC(8#705),                  ?PDP10_KS10)
  , ?D("tioe",          ?BASIC(8#710),                  ?PDP10_KS10)
  , ?D("tion",          ?BASIC(8#711),                  ?PDP10_KS10)
  , ?D("rdio",          ?BASIC(8#712),                  ?PDP10_KS10)
  , ?D("wrio",          ?BASIC(8#713),                  ?PDP10_KS10)
  , ?D("bsio",          ?BASIC(8#714),                  ?PDP10_KS10)
  , ?D("bcio",          ?BASIC(8#715),                  ?PDP10_KS10)
  , ?D("tioeb",         ?BASIC(8#720),                  ?PDP10_KS10)
  , ?D("tionb",         ?BASIC(8#721),                  ?PDP10_KS10)
  , ?D("rdiob",         ?BASIC(8#722),                  ?PDP10_KS10)
  , ?D("wriob",         ?BASIC(8#723),                  ?PDP10_KS10)
  , ?D("bsiob",         ?BASIC(8#724),                  ?PDP10_KS10)
  , ?D("bciob",         ?BASIC(8#725),                  ?PDP10_KS10)

    %% KA10/KL10 ITS system instructions.
  , ?D("lpm",           ?A_OPCODE(8#10200),             ?PDP10_KA10_ITS bor ?PDP10_KL10_ITS)
  , ?D("spm",           ?A_OPCODE(8#10204),             ?PDP10_KA10_ITS bor ?PDP10_KL10_ITS)
  , ?D("lpmr",          ?A_OPCODE(8#10210),             ?PDP10_KA10_ITS bor ?PDP10_KL10_ITS)
  , ?D("lpmri",         ?A_OPCODE(8#10230),             ?PDP10_KA10_ITS bor ?PDP10_KL10_ITS)

    %% KA10 ITS system instructions.
  , ?D("xctr",          ?A_OPCODE(8#10300),             ?PDP10_KA10_ITS)
  , ?D("xctri",         ?A_OPCODE(8#10320),             ?PDP10_KA10_ITS)

    %% KL10 ITS system instructions.
  , ?D("xctr",          ?BASIC(8#074),                  ?PDP10_KL10_ITS)
  , ?D("xctri",         ?BASIC(8#075),                  ?PDP10_KL10_ITS)
  , ?D("lpmr",          ?BASIC(8#076),                  ?PDP10_KL10_ITS)
  , ?D("spm",           ?BASIC(8#077),                  ?PDP10_KL10_ITS)

    %% KS10 ITS system instructions.
  , ?D("xctr",          ?BASIC(8#102),                  ?PDP10_KS10_ITS)
  , ?D("xctri",         ?BASIC(8#103),                  ?PDP10_KS10_ITS)
  , ?D("aprid",         ?PRIO(?A_OPCODE(8#70000), 1),   ?PDP10_KS10_ITS) % aliases blki

    %% ITS appears to prefer CONO and CONI over these mnemonics
% , ?D("wrapr",         ?A_OPCODE(8#70020),             ?PDP10_KS10_ITS)
% , ?D("rdapr",         ?A_OPCODE(8#70024),             ?PDP10_KS10_ITS)
% , ?D("wrpi",          ?A_OPCODE(8#70060),             ?PDP10_KS10_ITS)
% , ?D("rdpi",          ?A_OPCODE(8#70064),             ?PDP10_KS10_ITS)

  , ?D("clrcsh",        ?PRIO(?A_OPCODE(8#70100), 1),   ?PDP10_KS10_ITS) % aliases blki
  , ?D("rdubr",         ?PRIO(?A_OPCODE(8#70104), 1),   ?PDP10_KS10_ITS) % aliases datai
  , ?D("clrpt",         ?PRIO(?A_OPCODE(8#70110), 1),   ?PDP10_KS10_ITS) % aliases blko
  , ?D("wrubr",         ?PRIO(?A_OPCODE(8#70114), 1),   ?PDP10_KS10_ITS) % aliases datao
  , ?D("wrebr",         ?PRIO(?A_OPCODE(8#70120), 1),   ?PDP10_KS10_ITS) % aliases cono
  , ?D("rdebr",         ?PRIO(?A_OPCODE(8#70124), 1),   ?PDP10_KS10_ITS) % aliases coni
  , ?D("sdbr1",         ?PRIO(?A_OPCODE(8#70200), 1),   ?PDP10_KS10_ITS) % aliases blki
  , ?D("sdbr2",         ?PRIO(?A_OPCODE(8#70204), 1),   ?PDP10_KS10_ITS) % aliases datai
  , ?D("sdbr3",         ?PRIO(?A_OPCODE(8#70210), 1),   ?PDP10_KS10_ITS) % aliases blko
  , ?D("sdbr4",         ?PRIO(?A_OPCODE(8#70214), 1),   ?PDP10_KS10_ITS) % aliases datao
  , ?D("rdtim",         ?PRIO(?A_OPCODE(8#70220), 1),   ?PDP10_KS10_ITS) % aliases cono
  , ?D("rdint",         ?PRIO(?A_OPCODE(8#70224), 1),   ?PDP10_KS10_ITS) % aliases coni
  , ?D("rdhsb",         ?PRIO(?A_OPCODE(8#70230), 1),   ?PDP10_KS10_ITS) % aliases consz
  , ?D("spm",           ?PRIO(?A_OPCODE(8#70234), 1),   ?PDP10_KS10_ITS) % aliases conso
  , ?D("ldbr1",         ?PRIO(?A_OPCODE(8#70240), 1),   ?PDP10_KS10_ITS) % aliases blki
  , ?D("ldbr2",         ?PRIO(?A_OPCODE(8#70244), 1),   ?PDP10_KS10_ITS) % aliases datai
  , ?D("ldbr3",         ?PRIO(?A_OPCODE(8#70250), 1),   ?PDP10_KS10_ITS) % aliases blko
  , ?D("ldbr4",         ?PRIO(?A_OPCODE(8#70254), 1),   ?PDP10_KS10_ITS) % aliases datao
  , ?D("wrtim",         ?PRIO(?A_OPCODE(8#70260), 1),   ?PDP10_KS10_ITS) % aliases cono
  , ?D("wrint",         ?PRIO(?A_OPCODE(8#70264), 1),   ?PDP10_KS10_ITS) % aliases coni
  , ?D("wrhsb",         ?PRIO(?A_OPCODE(8#70270), 1),   ?PDP10_KS10_ITS) % aliases consz
  , ?D("lpmr",          ?PRIO(?A_OPCODE(8#70274), 1),   ?PDP10_KS10_ITS) % aliases conso
  , ?D("umove",         ?PRIO(?BASIC(8#704), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("umovem",        ?PRIO(?BASIC(8#705), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iordi",         ?PRIO(?BASIC(8#710), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iordq",         ?PRIO(?BASIC(8#711), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iord",          ?PRIO(?BASIC(8#712), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iowr",          ?PRIO(?BASIC(8#713), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iowri",         ?PRIO(?BASIC(8#714), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iowrq",         ?PRIO(?BASIC(8#715), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("bltbu",         ?PRIO(?BASIC(8#716), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("bltub",         ?PRIO(?BASIC(8#717), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iordbi",        ?PRIO(?BASIC(8#720), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iordbq",        ?PRIO(?BASIC(8#721), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iordb",         ?PRIO(?BASIC(8#722), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iowrb",         ?PRIO(?BASIC(8#723), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iowrbi",        ?PRIO(?BASIC(8#724), 1),        ?PDP10_KS10_ITS) % aliases blki
  , ?D("iowrbq",        ?PRIO(?BASIC(8#725), 1),        ?PDP10_KS10_ITS) % aliases blki

    %% XKL-1 system instructions.
  , ?D("rdadb",         ?PRIO(?A_OPCODE(8#70004), 1),   ?PDP10_XKL1) % special cases of apr0
  , ?D("sysid",         ?PRIO(?A_OPCODE(8#70010), 2),   ?PDP10_XKL1) % aliases WRFIL
  , ?D("wradb",         ?PRIO(?A_OPCODE(8#70014), 1),   ?PDP10_XKL1)
  , ?D("szapr",         ?PRIO(?A_OPCODE(8#70030), 1),   ?PDP10_XKL1)
  , ?D("snapr",         ?PRIO(?A_OPCODE(8#70034), 1),   ?PDP10_XKL1)
  , ?D("wctrlf",        ?PRIO(?A_OPCODE(8#70040), 2),   ?PDP10_XKL1) % aliases RDERA
  , ?D("rctrlf",        ?PRIO(?A_OPCODE(8#70044), 1),   ?PDP10_XKL1)
  , ?D("simird",        ?PRIO(?A_OPCODE(8#70050), 2),   ?PDP10_XKL1) % aliases SBDIAG
  , ?D("wrkpa",         ?PRIO(?A_OPCODE(8#70054), 1),   ?PDP10_XKL1)
  , ?D("szpi",          ?PRIO(?A_OPCODE(8#70070), 1),   ?PDP10_XKL1)
  , ?D("snpi",          ?PRIO(?A_OPCODE(8#70074), 1),   ?PDP10_XKL1)
  , ?D("apr0",          ?PRIO(?BASIC(8#700), 0),        ?PDP10_XKL1)
  , ?D("wrerr",         ?PRIO(?A_OPCODE(8#70120), 1),   ?PDP10_XKL1) % special cases of apr1
  , ?D("rderr",         ?PRIO(?A_OPCODE(8#70124), 1),   ?PDP10_XKL1)
  , ?D("wrctx",         ?PRIO(?A_OPCODE(8#70130), 1),   ?PDP10_XKL1)
  , ?D("rdctx",         ?PRIO(?A_OPCODE(8#70134), 1),   ?PDP10_XKL1)
  , ?D("rddcsh",        ?PRIO(?A_OPCODE(8#70140), 1),   ?PDP10_XKL1)
  , ?D("dwrcsh",        ?PRIO(?A_OPCODE(8#70160), 1),   ?PDP10_XKL1)
  , ?D("apr1",          ?PRIO(?BASIC(8#701), 0),        ?PDP10_XKL1)
  , ?D("rditm",         ?PRIO(?A_OPCODE(8#70220), 1),   ?PDP10_XKL1) % special cases of apr2
  , ?D("rdtime",        ?PRIO(?A_OPCODE(8#70224), 1),   ?PDP10_XKL1)
  , ?D("drdptb",        ?PRIO(?A_OPCODE(8#70230), 1),   ?PDP10_XKL1)
  , ?D("wrtime",        ?PRIO(?A_OPCODE(8#70234), 1),   ?PDP10_XKL1)
  , ?D("writm",         ?PRIO(?A_OPCODE(8#70260), 1),   ?PDP10_XKL1)
  , ?D("dwrptb",        ?PRIO(?A_OPCODE(8#70270), 1),   ?PDP10_XKL1)
  , ?D("apr2",          ?PRIO(?BASIC(8#702), 0),        ?PDP10_XKL1)
  , ?D("rdcty",         ?PRIO(?A_OPCODE(8#70304), 1),   ?PDP10_XKL1) % special cases of apr3
  , ?D("wrcty",         ?PRIO(?A_OPCODE(8#70314), 1),   ?PDP10_XKL1)
  , ?D("wrctys",        ?PRIO(?A_OPCODE(8#70320), 1),   ?PDP10_XKL1)
  , ?D("rdctys",        ?PRIO(?A_OPCODE(8#70324), 1),   ?PDP10_XKL1)
  , ?D("szcty",         ?PRIO(?A_OPCODE(8#70330), 1),   ?PDP10_XKL1)
  , ?D("sncty",         ?PRIO(?A_OPCODE(8#70334), 1),   ?PDP10_XKL1)
  , ?D("apr3",          ?PRIO(?BASIC(8#703), 0),        ?PDP10_XKL1)
  , ?D("pmove",         ?BASIC(8#704),                  ?PDP10_XKL1)
  , ?D("pmovem",        ?BASIC(8#705),                  ?PDP10_XKL1)
  , ?D("nmove",         ?BASIC(8#706),                  ?PDP10_XKL1)
  , ?D("nmovem",        ?BASIC(8#707),                  ?PDP10_XKL1)
  , ?D("ldlpn",         ?BASIC(8#710),                  ?PDP10_XKL1)
  , ?D("rdcfg",         ?BASIC(8#711),                  ?PDP10_XKL1)
  , ?D("amove",         ?BASIC(8#714),                  ?PDP10_XKL1)
  , ?D("amovem",        ?BASIC(8#715),                  ?PDP10_XKL1)
  , ?D("umove",         ?BASIC(8#716),                  ?PDP10_XKL1)
  , ?D("umovem",        ?BASIC(8#717),                  ?PDP10_XKL1)

    %% PDP6 / KA10 / KI10 / KL10 IO instructions.
    %%
    %% These take an operand which determines which device to access.
    %% Most of the system instructions above are instances of these with
    %% pre-determined values for the device operands.
  , ?D("blki",          ?PRIO(?IO(8#70000), 0),         ?PDP10_not_KS10_or_XKL1)
  , ?D("datai",         ?PRIO(?IO(8#70004), 0),         ?PDP10_not_KS10_or_XKL1)
  , ?D("blko",          ?PRIO(?IO(8#70010), 0),         ?PDP10_not_KS10_or_XKL1)
  , ?D("datao",         ?PRIO(?IO(8#70014), 0),         ?PDP10_not_KS10_or_XKL1)
  , ?D("cono",          ?PRIO(?IO(8#70020), 0),         ?PDP10_not_KS10_or_XKL1)
  , ?D("coni",          ?PRIO(?IO(8#70024), 0),         ?PDP10_not_KS10_or_XKL1)
  , ?D("consz",         ?PRIO(?IO(8#70030), 0),         ?PDP10_not_KS10_or_XKL1)
  , ?D("conso",         ?PRIO(?IO(8#70034), 0),         ?PDP10_not_KS10_or_XKL1)
  ].

%% Extended instructions, second word.

-define(EXTENDED(OPCODE9), ?A_UNUSED(OPCODE9), extended = true).

pdp10_extended_insns() ->
  [
    %% name,            high13, format, flags,          models
    ?D("cmpsl",         ?E_UNUSED(?EXTENDED(8#001)),    ?PDP10_KL10up)
  , ?D("cmpse",         ?E_UNUSED(?EXTENDED(8#002)),    ?PDP10_KL10up)
  , ?D("cmpsle",        ?E_UNUSED(?EXTENDED(8#003)),    ?PDP10_KL10up)
  , ?D("edit",          ?E_UNUSED(?EXTENDED(8#004)),    ?PDP10_KL10up)
  , ?D("cmpsge",        ?E_UNUSED(?EXTENDED(8#005)),    ?PDP10_KL10up)
  , ?D("cmpsn",         ?E_UNUSED(?EXTENDED(8#006)),    ?PDP10_KL10up)
  , ?D("cmpsg",         ?E_UNUSED(?EXTENDED(8#007)),    ?PDP10_KL10up)
  , ?D("cvtdbo",        ?EXTENDED(8#010),               ?PDP10_KL10up)
  , ?D("cvtdbt",        ?EXTENDED(8#011),               ?PDP10_KL10up)
  , ?D("cvtbdo",        ?EXTENDED(8#012),               ?PDP10_KL10up)
  , ?D("cvtbdt",        ?EXTENDED(8#013),               ?PDP10_KL10up)
  , ?D("movso",         ?EXTENDED(8#014),               ?PDP10_KL10up)
  , ?D("movst",         ?EXTENDED(8#015),               ?PDP10_KL10up)
  , ?D("movslj",        ?E_UNUSED(?EXTENDED(8#016)),    ?PDP10_KL10up)
  , ?D("movsrj",        ?E_UNUSED(?EXTENDED(8#017)),    ?PDP10_KL10up)
  , ?D("xblt",          ?E_UNUSED(?EXTENDED(8#020)),    ?PDP10_KL10up)
  , ?D("gsngl",         ?EXTENDED(8#021),               ?PDP10_KL10_271)
  , ?D("gdble",         ?EXTENDED(8#022),               ?PDP10_KL10_271)
  , ?D("gdfix",         ?EXTENDED(8#023),               ?PDP10_KL10_271)
  , ?D("gdfixr",        ?EXTENDED(8#025),               ?PDP10_KL10_271)
  , ?D("gfix",          ?EXTENDED(8#024),               ?PDP10_KL10_271)
  , ?D("gfixr",         ?EXTENDED(8#026),               ?PDP10_KL10_271)
  , ?D("dgfltr",        ?EXTENDED(8#027),               ?PDP10_KL10_271)
  , ?D("gfltr",         ?EXTENDED(8#030),               ?PDP10_KL10_271)
  , ?D("gfsc",          ?EXTENDED(8#031),               ?PDP10_KL10_271)
  ].

%% Internal device names for IO instructions.
%%
%% The convention in documentation is to list 7-bit device codes as three-digit
%% octal numbers, with excess zeros in the low bits.  The DEVICE/1 macros
%% corrects those values back to 7 bits.

-spec cpu_device_from_name(pdp10_cpu_models(), string()) -> pdp10_cpu_device() | false.
cpu_device_from_name(Models, Name) ->
  %% This is a short and infrequently queried list, so just do a linear search.
  cpu_device_from_name(pdp10_cpu_devices(), Models, Name).

cpu_device_from_name([], _Models, _Name) -> false;
cpu_device_from_name([{Name2, Device, Models2} | Devices], Models, Name) ->
  if Name2 =:= Name, (Models2 band Models) =:= Models -> Device;
     true -> cpu_device_from_name(Devices, Models, Name)
  end.

-define(DEVICE(DEVICE9), ((DEVICE9) bsr 2)).

pdp10_cpu_devices() ->
  [
   %% name,     device,         models
    { "apr",    ?DEVICE(8#000), ?PDP10_KA10_to_KL10 }   % Arithmetic processor
  , { "pi",     ?DEVICE(8#004), ?PDP10_KA10_to_KL10 }   % Priority interrupt
  , { "pag",    ?DEVICE(8#010), ?PDP10_KI10_to_KL10 }   % Pager
  , { "cca",    ?DEVICE(8#014), ?PDP10_KL10any }        % Cache
  , { "tim",    ?DEVICE(8#020), ?PDP10_KL10any }        % Timer
  , { "mtr",    ?DEVICE(8#024), ?PDP10_KL10any }        % Meters
  ].

-spec models_from_name(string()) -> pdp10_cpu_models() | false.
models_from_name(Name) ->
  case Name of
    "pdp6" -> ?PDP6;
    "ka10" -> ?PDP10_KA10;
    "ki10" -> ?PDP10_KI10;
    "kl10" -> ?PDP10_KL10;
    "kl10b" -> ?PDP10_KL10_271;
    "ks10" -> ?PDP10_KS10;
    "xkl1" -> ?PDP10_XKL1;
    "all" -> ?PDP10_ALL;
    _ -> false
  end.
