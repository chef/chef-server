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
         from_json/2]).

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
validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    %% Put a stub chef_client record into the resource_state. This allows us to use shared
    %% code for generating the map of name => URL returned for GET /clients.  OrgId is set via
    %% malformed_request.
    {Req, State#base_state{resource_state = #chef_client{org_id = OrgId}}};
validate_request('POST', Req, State) ->
    case wrq:req_body(Req) of
        undefined ->
            throw({error, missing_body});
        Body ->
            {ok, Client} = chef_client:parse_binary_json(Body, undefined),
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

%% @doc We generate a new public/private key pair, insert the public key into the DB
%% and return the private key as part of the response
from_json(Req, #base_state{resource_state =
                               #client_state{client_data = ClientData}} = State) ->
    KeyData = case chef_object_base:cert_or_key(ClientData) of
                  {undefined, _} ->
                      chef_keygen_cache:get_key_pair();
                  {PubKey, _PubKeyVersion} ->
                      {PubKey, undefined}
              end,
    handle_client_create(KeyData, Req, State).

handle_client_create(keygen_timeout, Req, State) ->
    {{halt, 503}, Req, State#base_state{log_msg = keygen_timeout}};
handle_client_create({PublicKey, PrivateKey}, Req,
                     #base_state{resource_state =
                                     #client_state{client_data = ClientData,
                                                   client_authz_id = AuthzId}} = State) ->
    ClientData1 = chef_object_base:set_public_key(ClientData, PublicKey),
    case oc_chef_wm_base:create_from_json(Req, State, chef_client, {authz_id, AuthzId}, ClientData1) of
        {true, Req1, State1} ->
            %% create_from_json by default sets the response to a json body
            %% containing only a uri key. Here we want to add the generated key
            %% pair so we replace the response.
            Name = ej:get({<<"name">>}, ClientData),
            URI = oc_chef_wm_routes:route(client, Req1, [{name, Name}]),
            EJSON = chef_object_base:set_key_pair({[{<<"uri">>, URI}]},
                                                  {public_key, PublicKey},
                                                  {private_key, PrivateKey}),
            {true, chef_wm_util:set_json_body(Req1, EJSON), State1};
        Else ->
            Else
    end.

malformed_request_message(Any, Req, State) ->
    chef_wm_malformed:malformed_request_message(Any, Req, State).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Client already exists">>]}]}.
