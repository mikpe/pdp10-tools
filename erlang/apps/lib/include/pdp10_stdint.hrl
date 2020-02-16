%%% -*- erlang-indent-level: 2 -*-
%%%
%%% pdp10_stdint.hrl -- stdint.h clone for PDP10
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
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Provide stdint.h-like type names and macros for 9, 18, and 36-bit integers.
%%%
%%% Standard {u,}int<N>_t types must not contain any extraneous bits, but that
%%% cannot be guaranteed for these 9, 18, and 36-bit types embedded in Erlang
%%% integers of unbounded precision.  For arithmetic on these types, use the
%%% operations provided by pdp10_arith.
%%%
%%% Do not use these 18 or 36-bit types for file-level binary data structures,
%%% instead use the pdp10_extint and pdp10_stdio facilities to explicitly
%%% convert between file-level and host-level binary data structures.

-ifndef(PDP10_STDINT_HRL).
-define(PDP10_STDINT_HRL, 1).

-define(PDP10_UINT9_MAX, ((1 bsl 9) - 1)).
-type uint9_t() :: 0..?PDP10_UINT9_MAX.

-define(PDP10_INT9_MAX, ((1 bsl (9 - 1)) - 1)).
-define(PDP10_INT9_MIN, (-?PDP10_INT9_MAX - 1)).
-type int9_t() :: ?PDP10_INT9_MIN..?PDP10_INT9_MAX.

-define(PDP10_UINT18_MAX, ((1 bsl 18) - 1)).
-type uint18_t() :: 0..?PDP10_UINT18_MAX.

-define(PDP10_INT18_MAX, ((1 bsl (18 - 1)) - 1)).
-define(PDP10_INT18_MIN, (-?PDP10_INT18_MAX - 1)).
-type int18_t() :: ?PDP10_INT18_MIN..?PDP10_INT18_MAX.

-define(PDP10_UINT36_MAX, ((1 bsl 36) - 1)).
-type uint36_t() :: 0..?PDP10_UINT36_MAX.

-define(PDP10_INT36_MAX, ((1 bsl (36 - 1)) - 1)).
-define(PDP10_INT36_MIN, (-?PDP10_INT36_MAX - 1)).
-type int36_t() :: ?PDP10_INT36_MIN..?PDP10_INT36_MAX.

-endif. % PDP10_STDINT_HRL
