%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80. -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@chef.io>
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


%% @doc Resource module for Chef clients endpoint
%%
%% The REST API provided by this module is as follows:
%%
%% Create a new client with body of POST being client JSON
%% POST /clients/
%%
%% Fetch all client names
%% GET /clients/
%%
%%
-module(chef_wm_clients).

-include("oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           {list_objects_json/2, to_json},
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).


%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3 ]).

-export([allowed_methods/2,
         create_path/2,
         conflict_message/1,
         from_json/2,
         finalize_create_body/4]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #client_state{}}.

request_type() ->
    "clients".

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

%% @doc Currently we need to allow the pivotal user to create clients for pedant.
%% We set up the state such that the superuser avoids the ACL checks.
%% FIXME: This is a temporary fix until pedant uses the validator which has
%% permissions to create a new client
-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    %% Put a stub chef_client record into the resource_state. This allows us to use shared
    %% code for generating the map of name => URL returned for GET /clients.  OrgId is set via
    %% malformed_request.
    {Req, State#base_state{resource_state = #chef_client{org_id = OrgId}}};
validate_request('POST', Req, #base_state{server_api_version = ApiVersion} = State) ->
    case wrq:req_body(Req) of
        undefined ->
            throw({error, missing_body});
        Body ->
            {ok, Client} = chef_client:parse_binary_json(ApiVersion, Body, undefined),
            {Req, State#base_state{superuser_bypasses_checks = true,
                    resource_state = #client_state{client_data = Client}}}
    end.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('POST', Req, State) ->
    {{create_in_container, client}, Req, State};
auth_info('GET', Req, State) ->
    {{container, client}, Req, State}.

create_path(Req, #base_state{resource_state = #client_state{client_data = ClientData}} = State) ->
    Name = ej:get({<<"name">>}, ClientData),
    {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state = #client_state{client_data = ClientData, client_authz_id = AuthzId}} = State) ->
    KeyContext = #key_context{ object_name = ej:get({<<"name">>}, ClientData),
                               object_authz_id = AuthzId,
                               object_ej = ClientData,
                               type = client },
    oc_chef_wm_key_base:create_object_with_embedded_key_data(Req, State#base_state{key_context = KeyContext}).


% Callback from create_from_json, which allows us to customize our body response.
finalize_create_body(_Req, #base_state{ server_api_version = ?API_v0,
                                        key_context = #key_context{key_ej = KeyEJ}}, _Client, BodyEJ) ->
    PublicKey = ej:get({<<"public_key">>}, KeyEJ),
    PrivateKey = ej:get({<<"private_key">>}, KeyEJ),
    chef_key_base:set_key_pair(BodyEJ, {public_key, PublicKey}, {private_key, PrivateKey});
finalize_create_body(Req, State, _Client, BodyEJ) ->
    oc_chef_wm_key_base:finalize_create_body(Req, State, BodyEJ).

malformed_request_message(Any, Req, State) ->
    chef_wm_malformed:malformed_request_message(Any, Req, State).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Client already exists">>]}]}.
