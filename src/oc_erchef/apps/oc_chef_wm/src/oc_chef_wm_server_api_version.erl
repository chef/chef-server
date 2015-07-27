%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80. -*-
%% ex: ts=4 sw=4 et
%% Copyright 2015 Chef Software, Inc.
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

-module(oc_chef_wm_server_api_version).

-include("oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           validate_request/3,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           {allow_all/2, auth_info},
                           service_available/2]}]).


%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([init/1,
         init_resource_state/1,
         request_type/0,
         malformed_request_message/3
        ]).

%% Our implemented webmachine callbacks
-export([allowed_methods/2, to_json/2 ]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_) ->
    % We're as simple as it gets, no state here.
    {ok, undefined}.

request_type() ->
    "server_api_version".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

to_json(Req, State) ->
    {chef_json:encode(version_body()), Req, State}.

version_body() ->
    {[ {<<"min_api_version">>, ?API_MIN_VER},
       {<<"max_api_version">>, ?API_MAX_VER} ]}.

-spec malformed_request_message(
        term(), wm_req(), chef_wm:base_state()) -> no_return().
malformed_request_message(Any, Req, State) ->
    oc_chef_wm_base:default_malformed_request_message(Any, Req, State).
