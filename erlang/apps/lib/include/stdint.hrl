%%% -*- erlang-indent-level: 2 -*-
%%%
%%% stdint.hrl -- stdint.h for Erlang
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
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Provide stdint.h-like type names and macros for 9, 18, and 36-bit integers.

-ifndef(STDINT_HRL).
-define(STDINT_HRL, 1).

-define(_STDINT_UMAX(N), ((1 bsl (N)) - 1)).
-define(_STDINT_IMAX(N), ((1 bsl ((N) - 1)) - 1)).
-define(_STDINT_IMIN(N), (-?_STDINT_IMAX((N)) - 1)).

-define(UINT9_MAX, ?_STDINT_UMAX(9)).
-type uint9_t() :: 0..?UINT9_MAX.

-define(INT9_MAX, ?_STDINT_IMAX(9)).
-define(INT9_MIN, ?_STDINT_IMIN(9)).
-type int9_t() :: ?INT9_MIN..?INT9_MAX.

-define(UINT18_MAX, ?_STDINT_UMAX(18)).
-type uint18_t() :: 0..?UINT18_MAX.

-define(INT18_MAX, ?_STDINT_IMAX(18)).
-define(INT18_MIN, ?_STDINT_IMIN(18)).
-type int18_t() :: ?INT18_MIN..?INT18_MAX.

-define(UINT36_MAX, ?_STDINT_UMAX(36)).
-type uint36_t() :: 0..?UINT36_MAX.

-define(INT36_MAX, ?_STDINT_IMAX(36)).
-define(INT36_MIN, ?_STDINT_IMIN(36)).
-type int36_t() :: ?INT36_MIN..?INT36_MAX.

-endif. % STDINT_HRL
