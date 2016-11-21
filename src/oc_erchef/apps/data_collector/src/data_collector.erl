%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Ryan Cragun <ryan@chef.io>
%% @author John Keiser <jkeiser@chef.io.
%%
%% Copyright 2016 Chef Software, Inc. All Rights Reserved.
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
%%

-module(data_collector).
-export([
         ping/0,
         is_enabled/0,
         token/0
        ]).

-spec ping() -> pong | pang.
ping() ->
    case data_collector_http:get("/") of
        ok -> pong;
        _ -> pang
    end.

-spec is_enabled() -> boolean().
is_enabled() ->
    case application:get_env(data_collector, root_url) of
        {ok, _Value} ->
            true;
        undefined ->
            false
    end.

-spec token() -> list() | atom().
token() ->
    case application:get_env(data_collector, token) of
        {ok, Token} ->
            Token;
        undefined ->
            undefined
    end.
