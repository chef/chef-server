%% Copyright 2012-2018 Chef Software, Inc.
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

-module(chef_wm).

-include("oc_chef_wm.hrl").

-type error() :: {error, term()}.
-type http_verb() :: 'GET' | 'PUT' | 'POST' | 'DELETE' | 'HEAD' | 'OPTIONS'.
-type base_state() :: #base_state{}.

-export_type([http_verb/0, base_state/0, auth_info_return/0,container_name/0]).


-callback init(list()) ->
    {ok, base_state()} | error().

-callback init_resource_state(list()) ->
    {ok, resource_state()} | error().

-callback validate_request(http_verb(), wm_req(), base_state()) ->
    {wm_req(), base_state()} | no_return().

-callback malformed_request_message(term(), wm_req(), base_state()) ->
    term().                                     % return is really EJSON

-callback request_type() ->
    string().

-callback auth_info(wm_req(), base_state()) -> auth_info_return().

-type auth_info_return() ::
    {{halt, non_neg_integer()}, wm_req(), base_state()} |
    {{halt, non_neg_integer(), binary()}, wm_req(), base_state()} |
    {{create_in_container, container_name()}, wm_req(), base_state()} |
    {{container, container_name()}, wm_req(), base_state()} |
    {{object, object_id()}, wm_req(), base_state()} |
    {{actor, object_id()}, wm_req(), base_state()} |
    {{object, object_id(), atom()}, wm_req(), base_state()} |
    {{actor, object_id(), atom()}, wm_req(), base_state()} |
    {[auth_tuple()], wm_req(), base_state()} |
    {authorized, wm_req(), base_state()} |
    {superuser_only, wm_req(), base_state()} |
    {{container_id, object_id()}, wm_req(), base_state()} |
    {{group, object_id()}, wm_req(), base_state()} |
    {{group_id, object_id()}, wm_req(), base_state()}.
