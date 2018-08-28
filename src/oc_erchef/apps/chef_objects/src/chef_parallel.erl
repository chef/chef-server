%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% Copyright 2013-2018 Chef Software, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc Utility functions for performing work in parallel.
-module(chef_parallel).

-export([
         parallelize_all_with_timeout/5
        ]).

%% @doc Parallelizes the map of `Fun' across `Items', with each
%% invocation subject to a `Timeout' (specified in ms).
%%
%% This function is intended for use with high-latency, IO-bound
%% operations (such as HTTP requests).  `Fanout' processes are
%% started, each of which will process a single element of `Items'.
%% If the call takes longer than `Timeout' ms to return,
%% `TimeoutHandler' is invoked with the offending item; the return
%% value will be the value present in the final list of results.
%% There will always be `Fanout' processes running at all points,
%% providing for more even throughput.
%%
%% `Fun' should be able to handle any errors or exceptions that are
%% thrown internally.  Note that every item in the list will be
%% processed; there is no short-circuiting if one invocation of `Fun'
%% fails or times out.
-spec parallelize_all_with_timeout(Items :: list(),
                                   Fun :: fun((any()) -> any()),
                                   Fanout :: pos_integer(),
                                   Timeout :: pos_integer(),
                                   TimeoutHandler :: fun((any()) -> any())) -> list().
parallelize_all_with_timeout(Items, Fun, Fanout, Timeout, TimeoutHandler) ->
    %% Create a wrapper function to map across `Items'.  It spawns an
    %% additional process to invoke `Fun' in order to control timeout
    %% situations.
    %%
    %% This is necessary because often we'd like to keep track of
    %% individual operation information like this, but erlware_commons
    %% now obscures timeout information.  Additionally,
    %% erlware_commons appears to only allow clients to specify a
    %% timeout on an entire list operation as a whole, instead of a
    %% timeout on each individual list item operation.
    %%
    %% By spawning a separate process and managing the timeout
    %% ourselves, we can once again capture this information.
    %% Additionally, we no longer need to specify a timeout via the
    %% ec_plist "malt" configuration, since that now basically takes
    %% care of itself.
    MapFun = fun(Item) ->
                     Me = self(),
                     %% This token is used below in the receive block
                     %% (just look!) to make absolutely certain that
                     %% we are only processing the exact message we
                     %% are expecting, as opposed to any other
                     %% messages the receiving process may be getting
                     %% from elsewhere in the system.
                     %%
                     %% It's admittedly a bit of paranoia, but should
                     %% help insulate from potential future changes in
                     %% erlware_commons (that involve message
                     %% passing).
                     %%
                     %% Also, paranoia.
                     Token = erlang:make_ref(),
                     Worker = proc_lib:spawn_link(fun() ->
                                                          Result = Fun(Item),
                                                          Me ! {Token, Result, self()}
                                                  end),
                     receive
                         {Token, Response, Worker} ->
                             Response
                     after Timeout ->
                             erlang:unlink(Worker),
                             erlang:exit(Worker, kill),
                             TimeoutHandler(Item)
                     end
             end,

    %% Have `Fanout' processes working on the list, with each process
    %% handling one list item at a time.
    %%
    %% See the documentation for erlware_commons' ec_plists module for
    %% more details.
    ParallelConfig = [1, {processes, Fanout}],

    %% Now we actually get to do the work!
    Results = ec_plists:ftmap(MapFun, Items, ParallelConfig),

    %% Get rid of the `{value, Term}' wrapping that ec_plists:ftmap/3
    %% introduces.
    [Value || {value, Value} <- Results].
