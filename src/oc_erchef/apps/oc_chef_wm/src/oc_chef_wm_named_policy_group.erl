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


-module(oc_chef_wm_named_policy_group).

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
         delete_resource/2,
         to_json/2]).


init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

post_is_create(Req, State) ->
    {false, Req, State}.

init_resource_state(_Config) ->
    {ok, #policy_group_state{}}.

request_type() ->
    "named_policy_group".

allowed_methods(Req, State) ->
    {['GET', 'DELETE'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request(_Verb, Req, State) ->
    {Req, State}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_guid = OrgId} = State) ->
    PolicyGroupName = wrq:path_info(policy_group, Req),
    case chef_db:fetch(#oc_chef_policy_group{org_id = OrgId, name = PolicyGroupName}, DbContext) of
        not_found ->
            Message= chef_wm_util:not_found_message(policy_group, PolicyGroupName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = policy_group_not_found}};
        #oc_chef_policy_group{authz_id = AuthzId} = PolicyGroup ->
            UpdatedPolicyState = #policy_group_state{policy_group = PolicyGroup},
            {{object, AuthzId}, Req, State#base_state{resource_state = UpdatedPolicyState}}
    end.

resource_exists(Req, State) ->
    %% This comes after auth_info/2 so if we made it here, it exists
    {true, Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 reqid = ReqId,
                                 organization_guid = OrgId,
                                 resource_state = #policy_group_state{
                                                     policy_group = PolicyGroup}
                                } = State) ->
    PolicyGroupName = PolicyGroup#oc_chef_policy_group.name,
    DeletedPolicyGroupJSON = build_policy_revisions_json(Req, ReqId, DbContext, OrgId, PolicyGroupName),
    %% CASCADE takes care of the policy group to policy revision association
    %% rows in the DB.
    ok = oc_chef_wm_base:delete_object(DbContext, PolicyGroup, RequestorId),
    {true, wrq:set_resp_body(DeletedPolicyGroupJSON, Req), State}.

-spec to_json(#wm_reqdata{}, #base_state{}) -> {binary(), #wm_reqdata{}, #base_state{}}.
to_json(Req, #base_state{chef_db_context = DbContext,
                         organization_guid = OrgId,
                         reqid = ReqId,
                         resource_state = #policy_group_state{
                            policy_group = PolicyGroup}} = State) ->
    PolicyGroupName = PolicyGroup#oc_chef_policy_group.name,
    ResponseBody = build_policy_revisions_json(Req, ReqId, DbContext, OrgId, PolicyGroupName),
    {ResponseBody, Req, State}.

build_policy_revisions_json(Req, ReqId, DbContext, OrgId, PolicyGroupName) ->
    case chef_db:find_all_policy_revisions_associated_to_group(DbContext, OrgId, PolicyGroupName) of
        {error, Why} ->
            Report = {find_all_policy_revisions_by_group_and_name, {Why, ReqId}},
            lager:error("~p", [Report]),
            error(Report);

        PolicyRevisionIDs ->
            URI = oc_chef_wm_routes:route(policy_group, Req, [{name, PolicyGroupName}]),
            BaseEJSON = ej:set_p({"uri"}, {[{<<"policies">>,{[]}}]}, URI),
            EJSONWithPolicies = build_nested_list_data(PolicyRevisionIDs, BaseEJSON),
            chef_json:encode(EJSONWithPolicies)
    end.

build_nested_list_data([Row|Rest], EJSON) ->
    {_PolicyGroupName, PolicyName, RevisionID} = Row,
    NewEJSON = ej:set_p({"policies", PolicyName, "revision_id"}, EJSON, RevisionID),
    build_nested_list_data(Rest, NewEJSON);
build_nested_list_data([], EJSON) ->
    EJSON.
