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
-export([manual_start/0, manual_stop/0]).
-export([start/2, stop/1]).
-behaviour(application).


%%===================================================================
%% API functions
%%===================================================================

manual_start() ->
    application:start(kernel),
    application:start(stdlib),
    application:start(sasl),
    application:start(bookshelf_store).

manual_stop() ->
    application:stop(bookshelf_store).

%%===================================================================
%% Application callbacks
%%===================================================================

start(_StartType, _StartArgs) ->
    bkss_sup:start_link().

stop(_State) ->
    ok.
