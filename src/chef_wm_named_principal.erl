%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_wm_named_principal).

-include("chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        ping/2]}]).

-mixin([{?BASE_RESOURCE, [forbidden/2,
                          service_available/2]}]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request/2,
         malformed_request_message/3,
         request_type/0,
         resource_exists/2,
         validate_request/3
        ]).

-export([
         allowed_methods/2,
         to_json/2
       ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #principal_state{}}.

request_type() ->
    "principal".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

principal_type(#chef_client{}) ->
    <<"client">>;
principal_type(#chef_user{}) ->
    <<"user">>.

% We need to override this instead of using the mixin; otherwise we reject unsigned
% requests, which is silly when we're not doing any authentication in the first place
malformed_request(Req, State) ->
    {false, Req, State}.

resource_exists(Req, #base_state{chef_db_context = DbContext,
                                 organization_name = OrgName,
                                 resource_state = ResourceState} = State) ->
    Name = chef_wm_util:object_name(principal, Req),
    case chef_db:fetch_requestor(DbContext, OrgName, Name) of
        {not_found, client} ->
            Message = chef_wm_util:not_found_message(client, Name),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {false, Req1, State#base_state{log_msg = client_not_found}};
        #chef_client{name = Name, public_key = PublicKey, authz_id = AuthzId} = Client ->
            State1 = ResourceState#principal_state{name = Name,
                                                   public_key = PublicKey,
                                                   type = principal_type(Client),
                                                   authz_id = AuthzId},
            {true, Req, State#base_state{resource_state = State1}};
        #chef_user{username = Name, public_key = PublicKey, authz_id = AuthzId} = User ->
            State1 = ResourceState#principal_state{name = Name,
                                                   public_key = PublicKey,
                                                   type = principal_type(User),
                                                   authz_id = AuthzId},
            {true, Req, State#base_state{resource_state = State1}}
    end.

validate_request(_Method, Req, State) ->
    {Req, State}.

auth_info(Req, State) ->
    {authorized, Req, State}.

assemble_principal_ejson(#principal_state{name = Name,
                                          public_key = PublicKey,
                                          type = Type,
                                          authz_id = AuthzId} = _Principal) ->
    {[{<<"name">>, Name},
      {<<"public_key">>, PublicKey},
      {<<"type">>, Type},
      {<<"authz_id">>, AuthzId}]}.
assemble_principal_ejson(#principal_state{name = Name,
                                          public_key = PublicKey,
                                          type = Type,
                                          authz_id = AuthzId} = _Principal,
                         OrgName, DbContext) ->
    Member = case Type of
                 <<"client">>  ->
                     true;
                 <<"user">> ->
                     chef_db:is_user_in_org(DbContext, Name, OrgName)
             end,                 
    {[{<<"name">>, Name},
      {<<"public_key">>, PublicKey},
      {<<"type">>, Type},
      {<<"authz_id">>, AuthzId},
      {<<"org_member">>, Member}]}.

to_json(Req, #base_state{resource_state = Principal, chef_db_context = DbContext,
                         organization_name = OrgName,
                         server_flavor = Flavor} = State) ->
    EJson = case Flavor of
                "osc" ->
                    assemble_principal_ejson(Principal);
                _other ->
                    assemble_principal_ejson(Principal, OrgName, DbContext)
            end,
    Json = ejson:encode(EJson),
    {Json, Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

