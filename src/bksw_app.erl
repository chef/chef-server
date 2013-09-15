%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Tim Dysinger <dysinger@opscode.com>
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

-export([manual_start/0, manual_stop/0,
         remsh_welcome/0]).

-export([start/2, stop/1]).

%% ===================================================================
%%                          API functions
%% ===================================================================
-define(APPS,
        [crypto,
         public_key,
         ssl,
         inets,
         erlsom,
         mochiweb,
         webmachine,
         xmerl,
         ibrowse,
         mini_s3,
         bookshelf]).

manual_start() ->
    %% manual_* are used for testing. For common test, we don't want
    %% to stop sasl or else we end up not getting complete logs in the
    %% test reports. So we ensure sasl is started, but omit it from
    %% the ?APPS list so that we don't start/stop on each test.
    application:start(sasl),
    %% we start lager since we depend on it for the release. However,
    %% we want to keep error_logger on its own so that we continue to
    %% see messages in common test output.
    application:load(lager),
    application:set_env(lager, error_logger_redirect, false),
    application:set_env(lager, handlers, []),
    lager:start(),
    [ ensure_started(App) || App <- ?APPS ],
    ok.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        Error ->
            erlang:error({application_start_failed, Error})
    end.

manual_stop() ->
    [ application:stop(App) || App <- (lists:reverse(?APPS)) ],
    application:stop(lager).

%% ===================================================================
%%                      Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    bksw_sup:start_link().

stop(_State) ->
    ok.

%% @doc Print an informative message about how to use a remote shell
%% attached to a live bookshelf node. The idea is to call this from a
%% wrapper script used to start a remote shell, like:
%% `erl -name user1@127.0.0.7 -setcookie bookshelf -remsh bookshelf@127.0.0.1 -s bksw_app remsh_welcome'.
%%
remsh_welcome() ->
    Msg =
        "~n~n==> Welcome to the bookshelf remote shell <==~n~n"
        "    TO EXIT: hit ctrl-g followed by q~n"
        "~n"
        "    DO NOT use q() or init:stop(), as these will stop the bookshelf node~n~n",
    io:format(Msg),
    ok.
