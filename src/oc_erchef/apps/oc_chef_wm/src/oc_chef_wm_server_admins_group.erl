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
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([allowed_methods/2,
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
                           resource_state = GroupState} = State) ->
    %% Fetch the server-admins group authz_id from database
    %% Note: We don't use the full chef_db:fetch because that uses org-scoped name
    %% conversion which doesn't work for the global placeholder org_id.
    %% Instead, we follow the pattern from chef-server-ctl/plugins/server_admins.rb
    case fetch_server_admins_group_authz_id(DbContext) of
        {ok, GroupAuthzId} ->
            %% Build a minimal group record with the authz_id
            Group = #oc_chef_group{
                org_id = ?GLOBAL_PLACEHOLDER_ORG_ID,
                name = ?SERVER_ADMINS_GROUP_NAME,
                authz_id = GroupAuthzId
            },
            GroupState1 = GroupState#group_state{oc_chef_group = Group},
            State1 = State#base_state{resource_state = GroupState1},
            error_logger:info_msg("AUTH_INFO: Returning State1~n"),
            %% Return group_id so base forbidden/2 can check permissions
            %% This will enforce superuser-only access via group ACLs
            {{group_id, GroupAuthzId}, Req, State1};
        not_found ->
            Message = chef_wm_util:error_message_envelope(
                <<"Cannot load global server-admins group">>),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = server_admins_group_not_found}}
    end.

%% Only allow pivotal/superuser to access this endpoint
resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, #base_state{chef_db_context = DbContext,
                         requestor_id = RequestorId,
                         resource_state = #group_state{
                             oc_chef_group = #oc_chef_group{authz_id = GroupAuthzId}
                         }} = State) ->
    %% Fetch group members from bifrost and convert to names
    %% Following the pattern from chef-server-ctl/plugins/server_admins.rb
    %% which queries bifrost then users table directly (no org-scoping)
    {UserNames, ClientNames} = fetch_group_member_names(GroupAuthzId, RequestorId, DbContext),
    
    %% Build response JSON matching the format from oc_chef_wm_named_group
    %% The "actors" field must contain the concatenation of clients and users
    Ejson = {[
        {<<"actors">>, ClientNames ++ UserNames},
        {<<"users">>, UserNames},
        {<<"clients">>, ClientNames},
        {<<"groups">>, []},  %% Groups not supported in server-admins
        {<<"orgname">>, null},  %% null for global group
        {<<"name">>, ?SERVER_ADMINS_GROUP_NAME},
        {<<"groupname">>, ?SERVER_ADMINS_GROUP_NAME}
    ]},
    Json = chef_json:encode(Ejson),
    {Json, Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

%%
%% Internal helper functions
%%

%% @doc Fetch the server-admins group authz_id from the database
%% NOTE: We use direct SQL query instead of chef_db:fetch to avoid
%% org-scoped name conversion which fails for GLOBAL_PLACEHOLDER_ORG_ID
-spec fetch_server_admins_group_authz_id(chef_db:db_context()) -> {ok, binary()} | not_found.
fetch_server_admins_group_authz_id(_DbContext) ->
    case chef_sql:select_rows({find_group_by_orgid_name, 
                                [?GLOBAL_PLACEHOLDER_ORG_ID, ?SERVER_ADMINS_GROUP_NAME]}) of
        [GroupRow | _] ->
            case proplists:get_value(<<"authz_id">>, GroupRow) of
                undefined ->
                    not_found;
                AuthzId when is_binary(AuthzId) ->
                    {ok, AuthzId}
            end;
        [] ->
            not_found;
        {error, _Reason} ->
            not_found
    end.

%% @doc Fetch group members from bifrost and convert authz_ids to usernames/client names
%% This bypasses the org-scoped name conversion machinery and queries users/clients directly,
%% following the pattern from chef-server-ctl/plugins/server_admins.rb
-spec fetch_group_member_names(binary(), binary(), chef_db:db_context()) -> 
    {[binary()], [binary()]}.
fetch_group_member_names(GroupAuthzId, _RequestorId, _DbContext) ->
    %% Get bifrost superuser_id for authentication (required for global group access)
    {ok, SuperuserId} = chef_secrets:get(<<"oc_bifrost">>, <<"superuser_id">>),
    %% Query bifrost for group members using superuser_id
    %% Note: oc_chef_authz_http:request automatically adds X-Ops-Requesting-Actor-Id header
    case oc_chef_authz_http:request("/groups/" ++ binary_to_list(GroupAuthzId), 
                                    get, 
                                    [], 
                                    [], 
                                    SuperuserId) of
        {ok, GroupJson} ->
            %% Extract actor authz_ids from bifrost response
            ActorAuthzIds = ej:get({<<"actors">>}, GroupJson, []),
            
            %% Query users table directly (no org-scoping)
            UserNames = case ActorAuthzIds of
                [] -> [];
                _ -> fetch_user_names_by_authz_ids(ActorAuthzIds)
            end,
            
            %% Query clients table directly (no org-scoping)
            ClientNames = case ActorAuthzIds of
                [] -> [];
                _ -> fetch_client_names_by_authz_ids(ActorAuthzIds)
            end,
            
            {UserNames, ClientNames};
        _ ->
            %% On error, return empty lists
            {[], []}
    end.

%% @doc Query users table directly by authz_ids
-spec fetch_user_names_by_authz_ids([binary()]) -> [binary()].
fetch_user_names_by_authz_ids(AuthzIds) ->
    case chef_sql:select_rows({find_user_name_in_authz_ids, [AuthzIds]}) of
        Results when is_list(Results) ->
            %% Extract usernames from results
            [Username || [{<<"username">>, Username}, {<<"authz_id">>, _}] <- Results];
        _ ->
            []
    end.

%% @doc Query clients table directly by authz_ids
-spec fetch_client_names_by_authz_ids([binary()]) -> [binary()].
fetch_client_names_by_authz_ids(AuthzIds) ->
    case chef_sql:select_rows({find_client_name_in_authz_ids, [AuthzIds]}) of
        Results when is_list(Results) ->
            %% Extract client names from results
            [ClientName || [{<<"name">>, ClientName}, {<<"authz_id">>, _}] <- Results];
        _ ->
            []
    end.
