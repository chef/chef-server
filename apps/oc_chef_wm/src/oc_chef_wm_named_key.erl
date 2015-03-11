%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Tyler Cloke <tyler@chef.io>
%% @author Marc Paradise <marc@chef.io>
%%
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_wm_named_key).

-include("../../include/oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-mixin([{oc_chef_wm_keys, [auth_info/2,
                           validate_request/3]}]).

%% chef_wm behavior callbacks
-behavior(chef_wm).
-export([init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0]).

-export([allowed_methods/2,
         delete_resource/2,
         auth_info/3,
         from_json/2,
         to_json/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #key_state{}}.

request_type() ->
    "keys".

allowed_methods(Req, State) ->
    {['GET', 'DELETE'], Req, State}.

% auth requirements are shared between this module and oc_chef_wm_key,
% but we additionally need to verify that the user isn't attempting to remove the
% key currently used for authentication, and that the requested key record exists.
% before we attempt any operation on it.
auth_info(Method, Req, #base_state{chef_db_context = Ctx,
                                   requestor = #chef_requestor{id = RequestorId, key_name = AuthenticatedKeyName},
                                   resource_state = #key_state{parent_id = ParentId} = ResourceState} = State) ->

    Name = chef_wm_util:object_name(key, Req),
    ct:pal("~p ~p ~p~n", [Name, RequestorId, Method]),
    case {Name, RequestorId, Method} of
        {AuthenticatedKeyName, ParentId, 'DELETE'} ->
            key_in_use_response(Name, Req, State);
        _ ->
            case chef_db:fetch(#chef_key{id = ParentId, key_name = Name}, Ctx) of
                not_found ->
                    key_not_found_response(Name, Req, State);
                #chef_key{} = Key ->
                    ResourceState1 = ResourceState#key_state{chef_key = Key},
                    oc_chef_wm_keys:auth_info(Method, Req, State#base_state{resource_state = ResourceState1})
            end
    end.

key_in_use_response(Name, Req, #base_state{resource_state = #key_state{parent_name = ParentName}} = State) ->
    % This is an action forbidden by policy, but the usual "you don't have permissions to X" doesn't apply -
    % use a custom message.
    Message = iolist_to_binary(["The key '", Name, "' was used to authenticate this request and cannot be modified or deleted."]),
    {{halt, 403, Message}, Req, State#base_state{log_msg={key_in_use_for_request, {ParentName, Name}}}}.

key_not_found_response(Name, Req, #base_state{resource_state = #key_state{parent_name = ParentName}} = State) ->
    Message = chef_wm_util:not_found_message(key, {ParentName, Name}),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 404}, Req1, State#base_state{log_msg={key_not_found, {ParentName, Name}}}}.

to_json(Req, #base_state{resource_state = #key_state{chef_key = Key}} = State) ->
    EJ = chef_key:ejson_from_key(Key),
    {chef_json:encode(EJ), Req, State}.

%% TODO: needed for PUT
from_json(_Req, #base_state{}) ->
    error(not_implemented).

delete_resource(Req, #base_state{requestor_id = RequestorId, chef_db_context = Ctx,
                                 resource_state = #key_state{chef_key = Key}} = State) ->
    ok = oc_chef_wm_base:delete_object(Ctx, Key, RequestorId),
    EJ = chef_key:ejson_from_key(Key),
    Req1 = chef_wm_util:set_json_body(Req, EJ),
    {true, Req1, State}.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).
