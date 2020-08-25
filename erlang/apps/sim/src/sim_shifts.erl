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
%%%
%%%=============================================================================
%%%
%%% 2.5 Shift and Rotate

-module(sim_shifts).

-export([ handle_ASH/4
        , handle_ASHC/4
        , handle_LSH/4
        , handle_LSHC/4
        , handle_ROT/4
        , handle_ROTC/4
        ]).

-include("sim_core.hrl").

%% 2.5 Shift and Rotate ========================================================

%% LSH - Logical Shift

-spec handle_LSH(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_LSH(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = lsh(CA, EA#ea.offset),
  set_ac_next_pc(Core, Mem, AC, Word).

%% LSHC - Logical Shift Combined

-spec handle_LSHC(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_LSHC(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA0 = sim_core:get_ac(Core, AC),
  CA1 = sim_core:get_ac(Core, (AC + 1) band 8#17),
  {Word0, Word1} = lshc(CA0, CA1, EA#ea.offset),
  set_acs_next_pc(Core, Mem, AC, Word0, Word1).

%% ROT - Rotate

-spec handle_ROT(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ROT(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  Word = rot(CA, EA#ea.offset),
  set_ac_next_pc(Core, Mem, AC, Word).

%% ROTC - Rotate Combined

-spec handle_ROTC(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ROTC(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA0 = sim_core:get_ac(Core, AC),
  CA1 = sim_core:get_ac(Core, (AC + 1) band 8#17),
  {Word0, Word1} = rotc(CA0, CA1, EA#ea.offset),
  set_acs_next_pc(Core, Mem, AC, Word0, Word1).

%% ASH - Arithmetic Shift

-spec handle_ASH(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ASH(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA = sim_core:get_ac(Core, AC),
  {Word, Flags} = ash(CA, EA#ea.offset),
  set_ac_and_flags_next_pc(Core, Mem, AC, Word, Flags).

%% ASHC - Arithmetic Shift Combined

-spec handle_ASHC(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_ASHC(Core, Mem, IR, EA) ->
  AC = IR band 8#17,
  CA0 = sim_core:get_ac(Core, AC),
  CA1 = sim_core:get_ac(Core, (AC + 1) band 8#17),
  {Word0, Word1, Flags} = ashc(CA0, CA1, EA#ea.offset),
  set_acs_and_flags_next_pc(Core, Mem, AC, Word0, Word1, Flags).

%% Miscellaneous ===============================================================

ash(CA, Offset) ->
  case Offset band (1 bsl 17) of
    0 -> % left shift
      Count = Offset band ((1 bsl 8) - 1),
      case CA band (1 bsl 35) of
        0 -> % non-negative
          {Word, Lost} =
            if Count >= 35 ->
                 {_Word = 0, _Lost = CA =/= 0};
               true ->
                 LeftNrBits = Count,
                 RightNrBits = 35 - Count,
                 {_Word = (CA band ((1 bsl RightNrBits) - 1)) bsl LeftNrBits,
                  _Lost = (CA bsr RightNrBits) =/= 0}
            end,
          Flags =
            case Lost of
              false -> 0;
              true -> (1 bsl ?PDP10_PF_TRAP_1) bor (1 bsl ?PDP10_PF_OVERFLOW)
            end,
          {Word, Flags};
        _ -> % negative
          {Word, Lost} =
            if Count >= 35 ->
                 {_Word = 1 bsl 35,
                  _Lost = CA =/= ((1 bsl 36) - 1)};
               true ->
                 LeftNrBits = Count,
                 RightNrBits = 35 - Count,
                 {_Word = ((CA band ((1 bsl RightNrBits) - 1)) bsl LeftNrBits) bor (1 bsl 35),
                  _Lost = ((CA bsr RightNrBits) band ((1 bsl LeftNrBits) - 1)) =/= ((1 bsl LeftNrBits) - 1)}
            end,
          Flags =
            case Lost of
              false -> 0;
              true -> (1 bsl ?PDP10_PF_TRAP_1) bor (1 bsl ?PDP10_PF_OVERFLOW)
            end,
          {Word, Flags}
      end;
    _ -> % right shift
      Count = (-Offset) band ((1 bsl 8) - 1),
      case CA band (1 bsl 35) of
        0 -> % non-negative
          Word =
            if Count >= 35 -> 0;
               true -> CA bsr Count
            end,
          {Word, _Flags = 0};
        _ -> % negative
          Word =
            if Count >= 35 -> (1 bsl 36) - 1;
               true -> (((1 bsl Count) - 1) bsl (36 - Count)) bor (CA bsr Count)
            end,
          {Word, _Flags = 0}
      end
  end.

ashc(CA0, CA1, Offset) ->
  case Offset band (1 bsl 17) of
    0 -> % left shift
      Count = Offset band ((1 bsl 8) - 1),
      case CA0 band (1 bsl 35) of
        0 -> % non-negative
          {Word0, Word1, Lost} =
            if Count >= 70 ->
                 {_Word0 = 0, _Word1 = 0, _Lost = CA0 bor (CA1 band ((1 bsl 35) - 1))};
               Count >= 35 ->
                 Count1 = Count - 35,
                 W0 = (CA1 band ((1 bsl (35 - Count1)) - 1)) bsl Count1,
                 L1 = (CA1 bsr (35 - Count1)) band ((1 bsl Count1) - 1),
                 {_Word0 = W0, _Word1 = 0, _Lost = CA0 bor L1};
               Count > 0 ->
                 W0 = ((CA0 band ((1 bsl (35 - Count)) - 1)) bsl Count) bor (CA1 bsr (35 - Count)),
                 W1 = (CA1 band ((1 bsl (35 - Count)) - 1)) bsl Count,
                 L0 = CA0 bsr (35 - Count),
                 {_Word0 = W0, _Word1 = W1, _Lost = L0};
               true ->
                 {_Word0 = CA0, _Word1 = CA1, _Lost = 0}
            end,
          Flags =
            case Lost of
              0 -> 0;
              _ -> (1 bsl ?PDP10_PF_TRAP_1) bor (1 bsl ?PDP10_PF_OVERFLOW)
            end,
          {Word0, Word1, Flags};
        _ -> % negative
          {Word0, Word1, Lost} =
            if Count >= 70 ->
                 {_Word0 = 1 bsl 35, _Word1 = 1 bsl 35, _Lost = (CA0 bor CA1) band ((1 bsl 35) - 1)};
               Count >= 35 ->
                 Count1 = Count - 35,
                 W0 = (CA1 band ((1 bsl (35 - Count1)) - 1)) bsl Count1,
                 L1 = (CA1 bsr Count1) band ((1 bsl Count1) - 1),
                 {_Word0 = W0, _Word1 = 1 bsl 35, _Lost = (CA0 band ((1 bsl 35) - 1)) bor L1};
               true ->
                 W0 = ((CA1 band ((1 bsl (35 - Count)) - 1)) bsl Count) bor (1 bsl 35) bor ((CA1 bsr (35 - Count)) band ((1 bsl Count) - 1)),
                 W1 = ((CA1 band ((1 bsl (35 - Count)) - 1)) bsl Count) bor (1 bsl 35),
                 L0 = (CA0 bsr (35 - Count)) band ((1 bsl Count) - 1),
                 {_Word0 = W0, _Word1 = W1, _Lost = L0}
            end,
          Flags =
            case Lost of
              0 -> 0;
              _ -> (1 bsl ?PDP10_PF_TRAP_1) bor (1 bsl ?PDP10_PF_OVERFLOW)
            end,
          {Word0, Word1, Flags}
      end;
    _ -> % right shift
      Count = (-Offset) band ((1 bsl 8) - 1),
      case CA0 band (1 bsl 35) of
        0 -> % non-negative
          if Count >= 70 ->
               {_Word0 = 0, _Word1 = 0, _Flags = 0};
             Count >= 35 ->
               Word1 = CA0 bsr (Count - 35),
               {_Word0 = 0, Word1, _Flags = 0};
             true ->
               Word0 = CA0 bsr Count,
               Word1 = ((CA0 band ((1 bsl Count) - 1)) bsl (35 - Count)) bor (CA1 bsr Count),
               {Word0, Word1, _Flags = 0}
          end;
        _ -> % negative
          if Count >= 70 ->
               {_Word0 = (1 bsl 36) - 1, _Word1 = (1 bsl 36) - 1, _Flags = 0};
             Count >= 35 ->
               Count1 = Count - 35,
               W1 = (CA0 bsr Count1) bor (((1 bsl Count1) - 1) bsl (36 - Count1)),
               {_Word0 = (1 bsl 36) - 1, _Word1 = W1, _Flags = 0}
          end
      end
  end.

lsh(CA, Offset) ->
  case Offset band (1 bsl 17) of
    0 -> % left shift
      Count = Offset band ((1 bsl 8) - 1),
      if Count >= 36 -> 0;
         true -> (CA band ((1 bsl (36 - Count)) - 1)) bsl Count
      end;
    _ -> % right shift
      Count = (-Offset) band ((1 bsl 8) - 1),
      if Count >= 36 -> 0;
         true -> CA bsr Count
      end
  end.

lshc(CA0, CA1, Offset) ->
  case Offset band (1 bsl 17) of
    0 -> % left shift
      Count = Offset band ((1 bsl 8) - 1),
      if Count >= 72 ->
           {_Word0 = 0, _Word1 = 0};
         Count >= 36 ->
           Count1 = Count - 36,
           Word0 = (CA1 band ((1 bsl (36 - Count1)) - 1)) bsl Count1,
           {Word0, _Word1 = 0};
         true ->
           Word0 = ((CA0 band ((1 bsl (36 - Count)) - 1)) bsl Count) bor (CA1 bsr (36 - Count)),
           Word1 = ((CA1 band ((1 bsl (36 - Count)) - 1)) bsl Count),
           {Word0, Word1}
      end;
    _ -> % right shift
      Count = (-Offset) band ((1 bsl 8) - 1),
      if Count >= 72 ->
           {_Word0 = 0, _Word1 = 0};
         Count >= 36 ->
           Word1 = CA0 bsr (Count - 36),
           {_Word0 = 0, Word1};
         true ->
           Word0 = CA0 bsr Count,
           Word1 = ((CA0 band ((1 bsl Count) - 1)) bsl (36 - Count)) bor (CA1 bsr Count),
           {Word0, Word1}
      end
  end.

rot(CA, Offset) ->
  case Offset band (1 bsl 17) of
    0 -> % rotate left
      Count = (Offset band ((1 bsl 8) - 1)) rem 36,
      ((CA band ((1 bsl (36 - Count)) - 1)) bsl Count) bor (CA bsr (36 - Count));
    _ -> % rotate right
      Count = ((-Offset) band ((1 bsl 8) - 1)) rem 36,
      (CA bsr Count) bor ((CA band ((1 bsl Count) - 1)) bsl (36 - Count))
  end.

rotc(CA0, CA1, Offset) ->
  case Offset band (1 bsl 17) of
    0 -> % rotate left
      Count = (Offset band ((1 bsl 8) - 1)) rem 72,
      if Count >= 36 ->
           Count1 = Count - 36,
           Word0 = ((CA1 band ((1 bsl (36 - Count1)) - 1)) bsl Count1) bor (CA0 bsr (36 - Count1)),
           Word1 = ((CA0 band ((1 bsl (36 - Count1)) - 1)) bsl Count1) bor (CA1 bsr (36 - Count1)),
           {Word0, Word1};
         true ->
           Word0 = ((CA0 band ((1 bsl (36 - Count)) - 1)) bsl Count) bor (CA1 bsr (36 - Count)),
           Word1 = ((CA1 band ((1 bsl (36 - Count)) - 1)) bsl Count) bor (CA0 bsr (36 - Count)),
           {Word0, Word1}
      end;
    _ -> % rotate right
      Count = ((-Offset) band ((1 bsl 8) - 1)) rem 72,
      if Count >= 36 ->
           Count1 = Count - 36,
           Word0 = (CA1 bsr Count1) bor ((CA0 band ((1 bsl Count1) - 1)) bsl (36 - Count1)),
           Word1 = (CA0 bsr Count1) bor ((CA1 band ((1 bsl Count1) - 1)) bsl (36 - Count1)),
           {Word0, Word1};
         true ->
           Word0 = (CA0 bsr Count) bor ((CA1 band ((1 bsl Count) - 1)) bsl (36 - Count)),
           Word1 = (CA1 bsr Count) bor ((CA0 band ((1 bsl Count) - 1)) bsl (36 - Count)),
           {Word0, Word1}
      end
  end.

set_acs_next_pc(Core, Mem, AC, Word0, Word1) ->
  set_ac_next_pc(sim_core:set_ac(Core, AC, Word0), Mem, (AC + 1) band 8#17, Word1).

set_ac_next_pc(Core, Mem, AC, Word) ->
  sim_core:next_pc(sim_core:set_ac(Core, AC, Word), Mem).

set_acs_and_flags_next_pc(Core, Mem, AC, Word0, Word1, Flags) ->
  set_ac_and_flags_next_pc(sim_core:set_ac(Core, AC, Word0), Mem, (AC + 1) band 8#17, Word1, Flags).

set_ac_and_flags_next_pc(Core, Mem, AC, Word, Flags) ->
  Core1 = sim_core:set_ac(Core, AC, Word),
  Core2 = sim_core:set_flags(Core1, Flags),
  sim_core:next_pc(Core2, Mem).
