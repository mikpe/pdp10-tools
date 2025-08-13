%%% -*- erlang-indent-level: 2 -*-
%%%
%%% pdp10_relocs.hrl
%%% Copyright (C) 2020-2025  Mikael Pettersson
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

-ifndef(PDP10_RELOCS_HRL).
-define(PDP10_RELOCS_HRL, 1).

-define(R_PDP10_NONE,           0).     % no reloc
-define(R_PDP10_IFIW,           1).     % local address to global word (IFIW)
-define(R_PDP10_EFIW,           2).     % global word (EFIW)
-define(R_PDP10_LOCAL_W,        3).     % local word
-define(R_PDP10_LOCAL_B,        4).     % local byte pointer to 9-bit byte
-define(R_PDP10_LOCAL_H,        5).     % local byte pointer to 18-bit halfword
-define(R_PDP10_GLOBAL_B,       6).     % global byte pointer to 9-bit byte
-define(R_PDP10_GLOBAL_H,       7).     % global byte pointer to 18-bit halfword
-define(R_PDP10_LITERAL_W,      8).     % word-sized literal
-define(R_PDP10_LITERAL_H,      9).     % halfword-sized literal
-define(R_PDP10_LITERAL_B,      10).    % byte-sized literal
-define(R_PDP10_max,            10).    % last entry

-endif. % PDP10_STDINT_HRL
