%%% -*- erlang-indent-level: 2 -*-
%%%
%%% stdio for I/O with 9-bit bytes
%%% Copyright (C) 2025  Mikael Pettersson
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
%%% Wrapper around stdio9 providing the legacy pdp10_stdio module.

-module(pdp10_stdio).

-export([ fopen/2
        , fclose/1
        , fgetc/1
        , fread/3
        , fputc/2
        , fputs/2
        , fseek/2
        , ftell/1
        , stdin/0
        , stdout/0
        ]).

-type file() :: stdio9:file().
-type location() :: stdio9:location().

-export_type([ file/0
             , location/0
             ]).

%% API -------------------------------------------------------------------------

fopen(Path, Modes) -> stdio9:fopen(Path, Modes).

fclose(File) -> stdio9:fclose(File).

fgetc(File) -> stdio9:fgetc(File).

fread(Size, NMemb, File) -> stdio9:fread(Size * NMemb, File).

fputc(Nonet, File) -> stdio9:fputc(Nonet, File).

fputs(Nonets, File) -> stdio9:fputs(Nonets, File).

fseek(File, Location) -> stdio9:fseek(File, Location).

ftell(File) -> stdio9:ftell(File).

stdin() -> stdio9:stdin().

stdout() -> stdio9:stdout().
