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
%%% Simulate traps to the kernel.

-module(sim_kernel).

-export([ handle_JSYS/4
        ]).

-include("sim_core.hrl").

-define(ENOSYS, 38).

%% JSYS ========================================================================
%%
%% System calls are made using JSYS with the system call number in the low 18
%% bits of EA, and the parameters in AC1 to AC7.
%% A successful call clears the Overflow flag and returns a value in AC1.
%% A failed call sets the Overflow flag and returns an error code in AC1.
%% [FIXME: Change to return success as 0 in AC0 and a value in AC1, and failure
%% as a non-zero errno in AC0 and -1 in AC1.  A more common approach on Linux
%% is to signal failure by returning -errno, detectable by being >= -4095U, but
%% implementing that comparison on the PDP10 is fairly expensive.]
%% System call numbers are from Linux' <asm-generic/unistd.h>, and error codes
%% are from Linux' <asm-generic/errno.h>.

-spec handle_JSYS(#core{}, sim_mem:mem(), IR :: word(), #ea{})
      -> {#core{}, sim_mem:mem(), {ok, integer()} | {error, {module(), term()}}}.
handle_JSYS(Core, Mem, _IR, #ea{offset = Nr}) ->
  case Nr of
    94 -> handle_exit_group(Core, Mem);
    _ -> return_errno(Core, Mem, ?ENOSYS)
  end.

handle_exit_group(Core, Mem) ->
  {Core, Mem, {ok, sim_core:get_ac(Core, 1)}}.

return_errno(Core0, Mem, Errno) ->
  Core1 = sim_core:set_ac(Core0, 1, Errno),
  Core2 = sim_core:set_flag(Core1, ?PDP10_PF_OVERFLOW),
  sim_core:next_pc(Core2, Mem).
