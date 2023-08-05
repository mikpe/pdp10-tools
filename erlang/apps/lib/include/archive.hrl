%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Support for reading and writing 'ar' archive files for pdp10-elf
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

-ifndef(ARCHIVE_HRL).
-define(ARCHIVE_HRL, 1).

-record(arhdr,
        { ar_name       :: string() | non_neg_integer()
        , ar_date       :: non_neg_integer()
        , ar_uid        :: non_neg_integer()
        , ar_gid        :: non_neg_integer()
        , ar_mode       :: non_neg_integer()
        , ar_size       :: non_neg_integer()
        }).

-type offset() :: non_neg_integer(). % offset of ar header in archive file

-record(member,
        { arhdr         :: #arhdr{}
        , location      :: offset()          % at this offset in old archive
                         | string()          % in this external file
        }).

-record(archive,
        { symtab        :: [{string(), offset()}] | false
        , members       :: [#member{}] % sorted by offset in output archive
        }).

-endif. % ARCHIVE_HRL
