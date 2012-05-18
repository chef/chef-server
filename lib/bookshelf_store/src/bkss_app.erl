%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Eric Merritt <ericbmerritt@gmail.com>
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

-module(bkss_app).

-behaviour(application).

-export([manual_start/0, manual_stop/0]).
-export([start/2, stop/1]).

-include("internal.hrl").

%%===================================================================
%% API functions
%%===================================================================

manual_start() ->
    application:start(kernel),
    application:start(stdlib),
    application:start(sasl),
    application:start(gen_leader),
    application:start(gproc),
    application:start(opset),
    application:start(inets),
    application:start(bookshelf_store).

manual_stop() ->
    application:stop(opset),
    application:stop(gproc),
    application:stop(gen_leader),
    application:stop(inets),
    application:stop(bookshelf_store).

start(_StartType, _StartArgs) ->
    opset:create(?BOOKSHELF_CONFIG,
                 [{disk_store, get_initial_config()}]),
    bkss_sup:start_link().

stop(_State) ->
    ok.

%%===================================================================
%% External
%%===================================================================
get_initial_config() ->
   Store1 =
        case application:get_env(bookshelf_store, disk_store) of
            undefined ->
                filename:join(code:priv_dir(bookshelf_store), "data");
            {ok, Store0} ->
                Store0
        end,
    filelib:ensure_dir(filename:join(Store1, "tmp")),
    Store1.
