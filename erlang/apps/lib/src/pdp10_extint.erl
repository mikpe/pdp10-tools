%%% -*- erlang-indent-level: 2 -*-
%%%
%%% converts between 18 and 36-bit integers and sequences of nonets
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

-module(pdp10_extint).

-export([ uint18_to_ext/1
        , uint18_from_ext/1
        , uint36_to_ext/1
        , uint36_from_ext/1
        ]).

-include_lib("lib/include/stdint.hrl").

%% The PDP10 is big-endian, so the conversions here are big-endian.
%% The bytes (nonets) are numbered in storage order.

-spec uint18_to_ext(uint18_t()) -> [uint9_t()].
uint18_to_ext(U18) ->
  [_B0 = (U18 bsr 9) band ?UINT9_MAX, _B1 = U18 band ?UINT9_MAX].

-spec uint18_from_ext([uint9_t()]) -> uint18_t().
uint18_from_ext([B0, B1]) ->
  ((B0 band ?UINT9_MAX) bsl 9) bor (B1 band ?UINT9_MAX).

-spec uint36_to_ext(uint36_t()) -> [uint9_t()].
uint36_to_ext(U36) ->
  [_B0 = (U36 bsr 27) band ?UINT9_MAX,
   _B1 = (U36 bsr 18) band ?UINT9_MAX,
   _B2 = (U36 bsr  9) band ?UINT9_MAX,
   _B3 =  U36         band ?UINT9_MAX].

-spec uint36_from_ext([uint9_t()]) -> uint36_t().
uint36_from_ext([B0, B1, B2, B3]) ->
  ((B0 band ?UINT9_MAX) bsl 27) bor
  ((B1 band ?UINT9_MAX) bsl 18) bor
  ((B2 band ?UINT9_MAX) bsl  9) bor
   (B3 band ?UINT9_MAX).
