%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Tim Dysinger <timd@opscode.com>
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

-module(bookshelf_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(cowboy),
    application:start(bookshelf).

start(_Type, _Args) ->
    Dispatch = [{'_', [{[], bookshelf_bucket, []}]},  %% service fqdn
                {'_', [{[], bookshelf_bucket, []}]}], %% bucket subdomain
    cowboy:start_listener(my_http_listener,
                          100,
                          cowboy_tcp_transport,
                          [{port, 8080}],
                          cowboy_http_protocol,
                          [{dispatch, Dispatch}]),
    bookshelf_sup:start_link().

stop(_State) ->
    ok.

%% TODO We have different behavior based on the request hostname
