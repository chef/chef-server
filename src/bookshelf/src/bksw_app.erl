%% @copyright 2019 Opscode, Inc. All Rights Reserved
%% @author Tim Dysinger <dysinger@chef.io>
%%
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed
%% with this work for additional information regarding copyright
%% ownership.  The ASF licenses this file to you under the Apache
%% License, Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain a copy of
%% the License at http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.  See the License for the specific language governing
%% permissions and limitations under the License.
-module(bksw_app).

-behaviour(application).

-export([remsh_welcome/0]).
-export([start/2, stop/1]).

%% ===================================================================
%%                      Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% erlang 19.3.x SIGTERM changes caused runit failures.
    %% this reverts to previous working SIGTERM behavior.
    os:set_signal(sigterm, default),

    case os:getenv("DEVVM") of
        "1" ->
            application:start(sync);
        _ -> ok
    end,

    {ok, _OtherApps} = maybe_start_sqerl(bksw_conf:storage_type()),
    bksw_sup:start_link().

stop(_State) ->
    ok.

maybe_start_sqerl(filesystem) ->
    {ok, []};
maybe_start_sqerl(sql) ->
    application:ensure_all_started(sqerl, permanent).

%% @doc Print an informative message about how to use a remote shell
%% attached to a live bookshelf node. The idea is to call this from a
%% wrapper script used to start a remote shell, like:
%% `erl -name user1@127.0.0.7 -setcookie bookshelf \
%%    -remsh bookshelf@127.0.0.1 -s bksw_app remsh_welcome'.
%%
remsh_welcome() ->
    Msg =
        "~n~n==> Welcome to the bookshelf remote shell <==~n~n"
        "    TO EXIT: hit ctrl-g followed by q~n"
        "~n"
        "    DO NOT use q() or init:stop(), as these "
                    "will stop the bookshelf node~n~n",
    io:format(Msg),
    ok.
