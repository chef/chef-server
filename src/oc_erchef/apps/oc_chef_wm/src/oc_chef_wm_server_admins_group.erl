%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Lincoln Baker <lincoln.baker@progress.com>
%% Copyright Chef Software, Inc. All Rights Reserved.
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

%% @doc Webmachine resource for the global server-admins group endpoint
%% This endpoint provides read-only access to the server-admins group membership
%% for DSM user migration purposes. Only accessible by pivotal/superuser.
-module(oc_chef_wm_server_admins_group).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([allowed_methods/2,
         forbidden/2,
         resource_exists/2,
         to_json/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

%% GLOBAL_PLACEHOLDER_ORG_ID is defined in chef_types.hrl (included via oc_chef_wm.hrl)
-define(SERVER_ADMINS_GROUP_NAME, <<"server-admins">>).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #group_state{}}.

request_type() ->
    "server_admins_group".

%% Only allow GET - this is a read-only endpoint
allowed_methods(Req, State) ->
    {['GET'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('GET', Req, State) ->
    %% No validation needed for GET - group name and org_id are hardcoded
    {Req, State#base_state{superuser_bypasses_checks = true,
                           resource_state = #group_state{
                               oc_chef_group = #oc_chef_group{
                                   org_id = ?GLOBAL_PLACEHOLDER_ORG_ID,
                                   name = ?SERVER_ADMINS_GROUP_NAME
                               }
                           }}}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           resource_state = GroupState,
                           requestor_id = RequestorId} = State) ->
    lager:info("DEBUG: server-admins auth_info called with RequestorId=~p", [RequestorId]),
    %% Fetch the global server-admins group
    FetchRecord = #oc_chef_group{org_id = ?GLOBAL_PLACEHOLDER_ORG_ID,
                                 name = ?SERVER_ADMINS_GROUP_NAME,
                                 for_requestor_id = RequestorId},
    lager:info("DEBUG: Fetching server-admins with org_id=~p, name=~p", 
               [?GLOBAL_PLACEHOLDER_ORG_ID, ?SERVER_ADMINS_GROUP_NAME]),
    case chef_db:fetch(FetchRecord, DbContext) of
        not_found ->
            lager:warning("DEBUG: server-admins group not found in database"),
            Message = chef_wm_util:error_message_envelope(
                <<"Cannot load global server-admins group">>),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = server_admins_group_not_found}};
        #oc_chef_group{authz_id = AuthzId, clients = Clients, users = Users, groups = Groups} = Group ->
            lager:info("DEBUG: server-admins group fetched successfully"),
            lager:info("DEBUG: Group authz_id=~p", [AuthzId]),
            lager:info("DEBUG: Group clients=~p", [Clients]),
            lager:info("DEBUG: Group users=~p", [Users]),
            lager:info("DEBUG: Group groups=~p", [Groups]),
            GroupState1 = GroupState#group_state{oc_chef_group = Group},
            State1 = State#base_state{resource_state = GroupState1},
            {{group_id, AuthzId}, Req, State1}
    end.

%% Only allow pivotal/superuser to access this endpoint
forbidden(Req, State) ->
    case oc_chef_wm_base:is_superuser(Req) of
        true ->
            %% Superuser (pivotal) is allowed
            {false, Req, State};
        false ->
            %% All other users are forbidden - return simple error string not array
            Message = {[{<<"error">>, <<"Access to server-admins group is restricted to superuser only">>}]},
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 403}, Req1, State#base_state{log_msg = server_admins_forbidden}}
    end.

resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, #base_state{resource_state = #group_state{oc_chef_group = Group}} = State) ->
    lager:info("DEBUG: to_json called for server-admins"),
    lager:info("DEBUG: Group record before assemble: ~p", [Group]),
    %% Use null for OrgName since this is a global group
    Ejson = oc_chef_group:assemble_group_ejson(Group, null),
    lager:info("DEBUG: Assembled ejson: ~p", [Ejson]),
    Json = chef_json:encode(Ejson),
    lager:info("DEBUG: Encoded JSON: ~p", [Json]),
    {Json, Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
