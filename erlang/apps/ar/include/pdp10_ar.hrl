%%% -*- erlang-indent-level: 2 -*-
%%%
%%% pdp10_elf36.hrl -- AR definitions for PDP10
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
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% This is essentially the standard SVR4/GNU AR format, with 'char'
%%% replaced by 'pdp10_uint9_t' throughout.
%%% Groups of 4 bytes in the symbol table denote pdp10_uint36_t
%%% values rather than plain int or uint32_t values.

-ifndef(PDP10_AR_HRL).
-define(PDP10_AR_HRL, 1).

-include_lib("lib/include/pdp10_stdint.hrl").                   % uint9_t()

-define(PDP10_ARMAG, [$!, $<, $a, $r, $c, $h, $>, $\n]).        % String that begins an archive file.
-define(PDP10_SARMAG, 8).                                       % Size of that string.

-define(PDP10_ARFMAG, [$`, $\n]).                               % String in ar_fmag at end of each header.

-record(pdp10_ar_hdr,
        { ar_name       :: [uint9_t()]  % [16] Member file name, sometimes / terminated.
        , ar_date       :: [uint9_t()]  % [12] File modification time, decimal seconds since Epoch.
        , ar_uid        :: [uint9_t()]  % [6] File user id, in decimal.
        , ar_gid        :: [uint9_t()]  % [6] File group id, in decimal.
        , ar_mode       :: [uint9_t()]  % [8] File mode, in octal.
        , ar_size       :: [uint9_t()]  % [10] File size, in decimal.
        , ar_fmag       :: [uint9_t()]  % [2] Always contains ARFMAG.
        }).

-define(PDP10_ARHDR_SIZEOF, 60).

%%
%% Additional information summarized from the FreeBSD ar(5) manual page
%% <https://www.freebsd.org/cgi/man.cgi?query=ar&sektion=5>.
%%
%% DESCRIPTION
%%
%% The archive file starts with an identifying byte sequence of the seven ASCII characters
%% '!<arch>' followed by an ASCII linefeed character (see the constant "ARMAG" in the header
%% file <ar.h>).
%% Archive members follow the initial identifying byte sequence.  Each archive member is
%% prefixed by a fixed size header describing the file attributes associated with the member.
%%
%% Archive Headers
%%
%% Archive headers are placed at an even offset in the archive file.  If the data for an
%% archive member ends at an odd byte offset, then a padding byte with value 0x0A is used
%% to position the next archive header on an even byte offset.
%%
%% Unused bytes in the fields of an archive header are set to the value 0x20.
%%
%% Representing File Names
%%
%% SVR4/GNU
%%      File names that are up to 15 characters long are stored directly in the ar_name
%%      field of the header, terminated by a "/" character.
%%      If the file name is larger than would fit in the space for the ar_name field,
%%      then the actual file name is kept in the archive string table, and the decimal
%%      offset of the file name in the string table is stored in the ar_name field,
%%      prefixed by a "/" character.
%%
%% Special Archive Members
%%
%% The following archive members are special.
%%
%% "/"
%%      In the SVR4/GNU variant of the archive format, the archive member with name "/"
%%      denotes an archive symbol table.  If present, this member will be the very first
%%      member of the archive.
%%
%% "//"
%%      In the SVR4/GNU variant of the archive format, the archive member with name "//"
%%      denotes the archive string table.  This special member is used to hold filenames
%%      that do not fit in the file name field of the header.  If present, this member
%%      immediately follows the archive symbol table if an archive symbol table is present,
%%      or is the first member otherwise.
%%
%% Archive String Tables
%%
%% An archive string table is used in the SVR4/GNU archive format to hold file names that
%% are too large to fit into the constraints of the ar_name field of the archive header.
%% An archive string table contains a sequence of file names.  Each file name in the archive
%% string table is terminated by the sequence 0x2F, 0x0A (the ASCII string "/\n").  No
%% padding is used to separate adjacent file names.
%%
%% Archive Symbol Tables
%%
%% Archive symbol tables are used to speed up link editing by providing a mapping between
%% the program symbols defined in the archive and the corresponding archive members.
%% Archive symbol tables are managed by the ranlib(1) utility.  The format of archive symbol
%% tables is as follows:
%%
%% SVR4/GNU
%%      In the SVR4/GNU archive format, the archive symbol table starts with a 4-byte binary
%%      value consisting of the number of entries contained in the archive symbol table.
%%      This count of entries is stored most significant byte first.  Next, there are 'count'
%%      4-byte numbers, each stored most significant byte first.  Each number is a binary
%%      offset to the archive header for the member in the archive file for the corresponding
%%      symbol table entry.  After the binary offset values, there are 'count' NUL-terminated
%%      strings in sequence, holding the symbol names for the corresponding symbol table entries.

%% Further references:
%% <http://www.linker-aliens.org/blogs/ali/entry/64_bit_archives_needed/>
%% <http://linux.die.net/man/1/llvm-ar>
%% "man -s 3HEAD ar.h" on Solaris 10

-endif. % PDP10_AR_HRL
