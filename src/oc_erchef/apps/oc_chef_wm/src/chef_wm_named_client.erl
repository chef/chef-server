%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
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


-module(chef_wm_named_client).

-include("oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).

-export([
         allowed_methods/2,
         conflict_message/1,
         delete_resource/2,
         from_json/2,
         to_json/2
       ]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #client_state{}}.

request_type() ->
  "clients".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request(Method, Req, #base_state{chef_db_context = DbContext,
                                          organization_guid = OrgId,
                                          resource_state = ClientState} = State) ->
    Name = chef_wm_util:object_name(client, Req),
    Client = chef_db:fetch(#chef_client{org_id = OrgId, name = Name}, DbContext),
    NewState = State#base_state{resource_state = ClientState#client_state{chef_client = Client}},
    validate_data(Method, Req, NewState).

validate_data(_Any, Req, #base_state{resource_state = #client_state{chef_client = not_found}} = State) ->
    {Req, State}; % let auth_info fail on not_found
validate_data('PUT', Req, #base_state{server_api_version = ApiVersion,
                                      resource_state = #client_state{chef_client = OldClient} = ClientState} = State) ->
    Body = wrq:req_body(Req),
    {ok, ClientData} = chef_client:parse_binary_json(ApiVersion, Body,  OldClient),
    {Req, State#base_state{resource_state = ClientState#client_state{client_data = ClientData}}};
validate_data(_NotPut, Req, State) ->
    {Req, State}. % no data to validate for other request types.


auth_info(Req, #base_state{resource_state =
                               #client_state{chef_client = not_found}} = State) ->
    Name = chef_wm_util:object_name(client, Req),
    Message = chef_wm_util:not_found_message(client, Name),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 404}, Req1, State#base_state{log_msg = client_not_found}};
auth_info(Req, #base_state{resource_state =
                               #client_state{chef_client =
                                                 #chef_client{authz_id = AuthzId} =
                                                 Client} = ClientState} = State) ->
    ClientState1 = ClientState#client_state{chef_client = Client},
    State1 = State#base_state{resource_state = ClientState1},
    {{actor, AuthzId}, Req, State1}.

from_json(Req, #base_state{server_api_version = ?API_v0,
                           resource_state =
                               #client_state{chef_client = Client,
                                             client_data = ClientData}} = State) ->
    oc_chef_wm_key_base:update_object_embedded_key_data_v0(Req, State, Client, ClientData);
from_json(Req, #base_state{resource_state =
                               #client_state{chef_client = Client,
                                             client_data = ClientData}} = State) ->
    % We do not support key generation in named client from v1 onward -
    % those operations are to be performed via keys endpoints.
    oc_chef_wm_base:update_from_json(Req, State, Client, ClientData).

to_json(Req, #base_state{resource_state =
                             #client_state{chef_client = Client},
                         organization_name = OrgName} = State) ->
    EJson = chef_client:assemble_client_ejson(Client, OrgName),
    Json = chef_json:encode(EJson),
    {Json, Req, State}.

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #client_state{
                                   chef_client = Client},
                                 organization_name = OrgName} = State) ->
    ok = oc_chef_wm_base:delete_object(DbContext, Client, RequestorId),
    EJson = chef_client:assemble_client_ejson(Client, OrgName),
    Req1 = chef_wm_util:set_json_body(Req, EJson),
    {true, Req1, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Client already exists">>]}]}.
