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


-module(chef_wm_named_principal).

-include("oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           ping/2,
                           forbidden/2,
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
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #principal_state{}}.

request_type() ->
    "principal".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

%% We need to be able to accept unsigned requests while still extracting the org id
%% If this kind of an unauthenticated request is used by more than one endpoint,
%% consider moving this into a mixin
malformed_request(Req, #base_state{}=State) ->
    try
        {OrgId, _OrgAuthzId} = chef_wm_util:fetch_org_metadata(State),
        {false, Req, State#base_state{organization_guid = OrgId}}
    catch
        throw:{org_not_found, Org} ->
            Req3 = chef_wm_util:set_json_body(Req, not_found_ejson(<<"org">>, Org)),
            {{halt, 404}, Req3, State#base_state{log_msg = org_not_found}};
        throw:Why ->
            Msg =  chef_wm_malformed:malformed_request_message(Why, Req, State),
            NewReq = wrq:set_resp_body(chef_json:encode(Msg), Req),
            {true, NewReq, State#base_state{log_msg = Why}}
    end.

resource_exists(Req, #base_state{server_api_version = ?API_v0,
                                 chef_db_context = DbContext,
                                 organization_guid = OrgId} = State) ->
    Name = chef_wm_util:object_name(principal, Req),
    case chef_db:fetch_requestor(DbContext, OrgId, Name) of
        {not_found, client} ->
            Req1 = chef_wm_util:set_json_body(Req, not_found_ejson(<<"principal">>, Name)),
            {false, Req1, State#base_state{log_msg = client_not_found}};
        Response->
            PrincipalState = make_principal_state(Response),
            {true, Req, State#base_state{resource_state = PrincipalState}}
    end;
resource_exists(Req, #base_state{chef_db_context = DbContext,
                                 organization_guid = OrgId } = State) ->
    Name = chef_wm_util:object_name(principal, Req),
    case chef_db:fetch_requestors(DbContext, OrgId, Name) of
        not_found ->
            Req1 = chef_wm_util:set_json_body(Req, not_found_ejson(<<"principal">>, Name)),
            {false, Req1, State#base_state{log_msg = principal_not_found}};
        Requestors ->
            PrincipalStates = [ make_principal_state(Requestor) || Requestor <- Requestors],
            {true, Req, State#base_state{resource_state = PrincipalStates}}
    end.


make_principal_state(#chef_client{name = Name, public_key = PublicKey, authz_id = AuthzId}) ->
  #principal_state{name = Name, public_key = PublicKey, type = <<"client">>, authz_id = AuthzId};
make_principal_state(#chef_user{username = Name, public_key = PublicKey, authz_id = AuthzId}) ->
  #principal_state{name = Name, public_key = PublicKey, type = <<"user">>, authz_id = AuthzId};
make_principal_state(#chef_requestor{name = Name, public_key = PublicKey, authz_id = AuthzId, type = Type}) ->
  #principal_state{name = Name, public_key = PublicKey, type = Type, authz_id = AuthzId}.

validate_request(_Method, Req, State) ->
    {Req, State}.

auth_info(Req, State) ->
    {authorized, Req, State}.

to_json(Req, #base_state{server_api_version = ?API_v0, resource_state = Principal,
                         chef_db_context = DbContext, organization_name = OrgName} = State) ->
    EJson = assemble_principal_ejson(Principal, OrgName, DbContext),
    Json = chef_json:encode(EJson),
    {Json, Req, State};
to_json(Req, #base_state{resource_state = Principals,
                         chef_db_context = DbContext, organization_name = OrgName} = State) ->
    EJ = [ assemble_principal_ejson(Principal, OrgName, DbContext) || Principal <- Principals ],
    Json = chef_json:encode({[{ <<"principals">>, EJ}]} ),
    {Json, Req, State}.

assemble_principal_ejson(#principal_state{name = Name,
                                          public_key = PublicKey,
                                          type = Type,
                                          authz_id = AuthzId} = _Principal,
                         OrgName, DbContext) ->
    Member = oc_chef_wm_base:is_user_in_org(Type, DbContext, Name, OrgName),
    {[{<<"name">>, Name},
      {<<"public_key">>, PublicKey},
      {<<"type">>, Type},
      {<<"authz_id">>, AuthzId},
      {<<"org_member">>, Member}]}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

not_found_ejson(Type, Name) when is_binary(Type), is_binary(Name) ->
    {[{<<"not_found">>, Type},
      {<<"error">>, iolist_to_binary(["Cannot find ", Type, " ", Name])}]}.
