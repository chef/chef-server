%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Daniel DeLeo <dan@chef.io>
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

%% /policies/:policy_name/revisions
-module(oc_chef_wm_named_policy_revisions).

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
         post_is_create/2,
         resource_exists/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3,
         create_path/2,
         from_json/2]).


init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

post_is_create(Req, State) ->
    {true, Req, State}.

init_resource_state(_Config) ->
    {ok, #named_policy_revisions_state{}}.

request_type() ->
    "named_policy_revisions".

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request(_Verb, Req, #base_state{resource_state = NamedPolicyRevisionsState} = State) ->
    Name = wrq:path_info(policy_name, Req),
    Body = wrq:req_body(Req),
    {ok, Policy} = oc_chef_policy_revision:parse_binary_json(Body),
    validate_name(Name, Policy),
    UpdatedResourceState = NamedPolicyRevisionsState#named_policy_revisions_state{policy_name = Name, policy_revision_data = Policy},
    {Req, State#base_state{resource_state = UpdatedResourceState}}.

validate_name(NameFromReq, Policy) ->
    NameFromJson = erlang:binary_to_list(ej:get({<<"name">>}, Policy)),
    case ibrowse_lib:url_encode(NameFromJson) =:= ibrowse_lib:url_encode(NameFromReq) of
        true ->
            ok;
        false ->
            erlang:throw({mismatch, {<<"name">>, NameFromJson, NameFromReq}})
    end.


auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_guid = OrgId,
                           resource_state = NamedPolicyRevisionsState} = State) ->
    PolicyName = wrq:path_info(policy_name, Req),
    case chef_db:fetch(#oc_chef_policy{org_id = OrgId, name = PolicyName}, DbContext) of
        not_found ->
            StateWithFlag = NamedPolicyRevisionsState#named_policy_revisions_state{create_policy = true},
            BaseStateWithFlag = State#base_state{resource_state = StateWithFlag},
            {{create_in_container, policies}, Req, BaseStateWithFlag};
        #oc_chef_policy{authz_id = AuthzID} = Policy ->
            StateWithPolicy = NamedPolicyRevisionsState#named_policy_revisions_state{
                    policy_record = Policy, policy_authz_id = AuthzID},
            BaseStateWithPolicy = State#base_state{resource_state = StateWithPolicy},
            {{object, AuthzID}, Req, BaseStateWithPolicy}
    end.

%% This comes after auth_info/2 so if we made it here, it exists
resource_exists(Req, State) ->
    {true, Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

create_path(Req, #base_state{resource_state = NamedPolicyRevisionsState} = State) ->
    #named_policy_revisions_state{policy_name = PolicyName, policy_revision_data = PolicyData} = NamedPolicyRevisionsState,
    RevisionID = ej:get({<<"revision_id">>}, PolicyData),
    { "policies/" ++ PolicyName ++ "/revisions/" ++ binary_to_list(RevisionID), Req, State}.

from_json(Req, #base_state{chef_db_context = DbContext,
                           requestor_id = RequestorId,
                           organization_guid = OrgId,
                           api_version = ApiVersion,
                           resource_state = #named_policy_revisions_state{
                                policy_authz_id = PolicyAuthzID,
                                policy_name = PolicyName,
                                policy_revision_data = PolicyData}} = State) ->
    create_policy_if_needed(State),
    PolicyRevForInsert = oc_chef_policy_revision:new_record(ApiVersion, OrgId, PolicyAuthzID, PolicyData),
    case chef_db:create(PolicyRevForInsert, DbContext, RequestorId) of
        {conflict, _} ->
            LogMsg = {policy_revision, name_conflict, PolicyName},
            RevisionID = ej:get({"revision_id"}, PolicyData),
            ConflictMsg = "Policy '" ++ PolicyName ++ "' with revision_id '" ++ binary_to_list(RevisionID) ++ " already exists.",
            {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg),
             State#base_state{log_msg = LogMsg}};
        ok ->
            Uri = oc_chef_wm_routes:route(policy, Req, [{name, PolicyName}]),
            ReqWithURI = chef_wm_util:set_uri_of_created_resource(Uri, Req),
            ReqWithBody = chef_wm_util:set_json_body(ReqWithURI, PolicyData),
            {true, ReqWithBody, State};
        Error ->
            {{halt, 500}, Req, State#base_state{log_msg = Error}}
    end.

create_policy_if_needed(#base_state{resource_state = #named_policy_revisions_state{
                    create_policy = false,
                    policy_record = PolicyRecord}}) ->
    PolicyRecord;
create_policy_if_needed(#base_state{chef_db_context = DbContext,
                           requestor_id = RequestorId,
                           organization_guid = OrgId,
                           api_version = ApiVersion,
                           resource_state = #named_policy_revisions_state{
                                policy_authz_id = AuthzId,
                                policy_name = Name}}) ->
    PolicyRecord = oc_chef_policy:new_record(ApiVersion, OrgId, AuthzId, Name),
    ok = chef_db:create(PolicyRecord, DbContext, RequestorId),
    PolicyRecord.


