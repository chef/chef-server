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

validate_any_request(Req, #base_state{chef_db_context = DbContext,
                                      organization_guid = OrgId,
                                      resource_state = ClientState} = State) ->
    Name = chef_wm_util:object_name(client, Req),
    Client = case chef_db:fetch(#chef_client{org_id = OrgId, name = Name}, DbContext) of
                 not_found ->
                     not_found;
                 #chef_client{} = Found ->
                     Found
             end,
    ClientState1 = ClientState#client_state{chef_client = Client},
    {Req, State#base_state{resource_state = ClientState1}}.

validate_request('PUT', Req, State) ->
    {Req1, State1} = validate_any_request(Req, State),
    #base_state{resource_state =
                    #client_state{chef_client = OldClient} = ClientState} = State1,
    case OldClient of
        not_found ->
            {Req1, State1};
        _ ->
            % FIXME: parse_binary_json can probably be simplified to NOT need a
            % name passed to it; the name extracted from the old client here is
            % the same as the request name, since the request name is used to pull
            % the old client from the database in the first place.
            #chef_client{name = Name} = OldClient,
            Body = wrq:req_body(Req),
            {ok, ClientData} = chef_client:parse_binary_json(Body, Name, OldClient),
            {Req1, State1#base_state{resource_state =
                                         ClientState#client_state{client_data =
                                                                      ClientData}}}
    end;
validate_request(_Other, Req, State) ->
    validate_any_request(Req, State).

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

from_json(Req, #base_state{resource_state =
                               #client_state{chef_client = Client,
                                             client_data = ClientData}} = State) ->
    case chef_wm_util:maybe_generate_key_pair(ClientData) of
        keygen_timeout ->
            {{halt, 503}, Req, State#base_state{log_msg = keygen_timeout}};
        ClientData1 ->
            oc_chef_wm_base:update_from_json(Req, State, Client, ClientData1)
    end.

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
