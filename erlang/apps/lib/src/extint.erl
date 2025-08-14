%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Converts between fixed-width integers and sequences of bytes.
%%% 16/32/64-bit integers are sequences of octets, while
%%% 18/36-bit integers are sequences of nonets.
%%%
%%% For now, all conversions are big-endian.
%%%
%%% Copyright (C) 2013-2025  Mikael Pettersson
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

-module(extint).

-export([ uint16_from_ext/1
        , uint16_to_ext/1
        , uint18_from_ext/1
        , uint18_to_ext/1
        , uint32_from_ext/1
        , uint32_to_ext/1
        , uint36_from_ext/1
        , uint36_from_s64/1
        , uint36_to_c36/1
        , uint36_to_ext/1
        , uint64_from_ext/1
        , uint64_to_ext/1
        ]).

-include_lib("lib/include/stdint.hrl").

-spec uint16_from_ext([uint8_t()]) -> uint16_t().
uint16_from_ext([B0, B1]) ->
  ((B0 band ?UINT8_MAX) bsl 8) bor (B1 band ?UINT8_MAX).

-spec uint16_to_ext(uint16_t()) -> [uint8_t()].
uint16_to_ext(U16) ->
  [_B0 = (U16 bsr 8) band ?UINT8_MAX, _B1 = U16 band ?UINT8_MAX].

-spec uint18_from_ext([uint9_t()]) -> uint18_t().
uint18_from_ext([B0, B1]) ->
  ((B0 band ?UINT9_MAX) bsl 9) bor (B1 band ?UINT9_MAX).

-spec uint18_to_ext(uint18_t()) -> [uint9_t()].
uint18_to_ext(U18) ->
  [_B0 = (U18 bsr 9) band ?UINT9_MAX, _B1 = U18 band ?UINT9_MAX].

-spec uint32_from_ext([uint8_t()]) -> uint32_t().
uint32_from_ext([B0, B1, B2, B3]) ->
  ((B0 band ?UINT8_MAX) bsl 24) bor
  ((B1 band ?UINT8_MAX) bsl 16) bor
  ((B2 band ?UINT8_MAX) bsl  8) bor
   (B3 band ?UINT8_MAX).

-spec uint32_to_ext(uint32_t()) -> [uint8_t()].
uint32_to_ext(U32) ->
  [_B0 = (U32 bsr 24) band ?UINT8_MAX,
   _B1 = (U32 bsr 16) band ?UINT8_MAX,
   _B2 = (U32 bsr  8) band ?UINT8_MAX,
   _B3 =  U32         band ?UINT8_MAX].

-spec uint36_from_ext([uint9_t()]) -> uint36_t().
uint36_from_ext([B0, B1, B2, B3]) ->
  ((B0 band ?UINT9_MAX) bsl 27) bor
  ((B1 band ?UINT9_MAX) bsl 18) bor
  ((B2 band ?UINT9_MAX) bsl  9) bor
   (B3 band ?UINT9_MAX).

%% S64 is the sparse representation of a 36-bit word in 64 bits where
%% each 9-bit field is zero-extended to 16 bits, i.e. two octets.
%% The PDP-10 port of GNU binutils uses this format.
%% This converts S64-encoded data back to a 36-bit word.
-spec uint36_from_s64([uint8_t()]) -> uint36_t().
uint36_from_s64([B0, B1, B2, B3, B4, B5, B6, B7]) ->
  0 = (B0 bor B2 bor B4 bor B6) band bnot 1, % assert
  N0 = (B0 bsl 8) bor B1,
  N1 = (B2 bsl 8) bor B3,
  N2 = (B4 bsl 8) bor B5,
  N3 = (B6 bsl 8) bor B7,
  uint36_from_ext([N0, N1, N2, N3]).

-spec uint36_to_ext(uint36_t()) -> [uint9_t()].
uint36_to_ext(U36) ->
  [_B0 = (U36 bsr 27) band ?UINT9_MAX,
   _B1 = (U36 bsr 18) band ?UINT9_MAX,
   _B2 = (U36 bsr  9) band ?UINT9_MAX,
   _B3 =  U36         band ?UINT9_MAX].

%% This converts a 36-bit word to 5 octets in KLH10's C36 format.
-spec uint36_to_c36(uint36_t()) -> [uint8_t()].
uint36_to_c36(U36) ->
  [_B0 = (U36 bsr 28) band 255,
   _B1 = (U36 bsr 20) band 255,
   _B2 = (U36 bsr 12) band 255,
   _B3 = (U36 bsr  4) band 255,
   _B4 =  U36         band  15].

-spec uint64_from_ext([uint8_t()]) -> uint64_t().
uint64_from_ext([B0, B1, B2, B3, B4, B5, B6, B7]) ->
  ((B0 band ?UINT8_MAX) bsl 56) bor
  ((B1 band ?UINT8_MAX) bsl 48) bor
  ((B2 band ?UINT8_MAX) bsl 40) bor
  ((B3 band ?UINT8_MAX) bsl 32) bor
  ((B4 band ?UINT8_MAX) bsl 24) bor
  ((B5 band ?UINT8_MAX) bsl 16) bor
  ((B6 band ?UINT8_MAX) bsl  8) bor
   (B7 band ?UINT8_MAX).

-spec uint64_to_ext(uint64_t()) -> [uint8_t()].
uint64_to_ext(U64) ->
  [_B0 = (U64 bsr 56) band ?UINT8_MAX,
   _B1 = (U64 bsr 48) band ?UINT8_MAX,
   _B2 = (U64 bsr 40) band ?UINT8_MAX,
   _B3 = (U64 bsr 32) band ?UINT8_MAX,
   _B4 = (U64 bsr 24) band ?UINT8_MAX,
   _B5 = (U64 bsr 16) band ?UINT8_MAX,
   _B6 = (U64 bsr  8) band ?UINT8_MAX,
   _B7 =  U64         band ?UINT8_MAX].
