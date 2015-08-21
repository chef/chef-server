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

%% /policies/:policy_name
-module(oc_chef_wm_named_policy).

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

init_resource_state(_Config) ->
    {ok, #named_policy_revisions_state{}}.

request_type() ->
    "named_policy_revisions".

allowed_methods(Req, State) ->
    {['GET', 'DELETE'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request(_Verb, Req, #base_state{resource_state = NamedPolicyState} = State) ->
    Name = wrq:path_info(policy_name, Req),
    UpdatedResourceState = NamedPolicyState#named_policy_revisions_state{policy_name = Name},
    {Req, State#base_state{resource_state = UpdatedResourceState}}.


auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_guid = OrgId,
                           resource_state = NamedPolicyState} = State) ->
    PolicyName = wrq:path_info(policy_name, Req),
    case chef_db:fetch(#oc_chef_policy{org_id = OrgId, name = PolicyName}, DbContext) of
        not_found ->
            {{halt, 404}, Req, State#base_state{log_msg = policy_not_found}};
        #oc_chef_policy{authz_id = AuthzID} = Policy ->
            StateWithPolicy = State#base_state{
                    resource_state = NamedPolicyState#named_policy_revisions_state{
                        policy_record = Policy}},
            {{object, AuthzID}, Req, StateWithPolicy}
    end.

%% This comes after auth_info/2 so if we made it here, it exists
resource_exists(Req, State) ->
    {true, Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 organization_guid = OrgId,
                                 resource_state = #named_policy_revisions_state{
                                    policy_record = PolicyRecord,
                                    policy_name = PolicyName}} = State) ->
    Revisions = chef_db:list(#oc_chef_policy_revision{org_id = OrgId, name = PolicyName},
                              DbContext),
    RevisionsMap = build_revisions_map(Revisions, {[]}),
    EJSON = chef_json:encode(RevisionsMap),
    ok = oc_chef_wm_base:delete_object(DbContext, PolicyRecord, RequestorId),
    {true, wrq:set_resp_body(EJSON, Req), State}.

to_json(Req, #base_state{chef_db_context = DbContext,
                         organization_guid = OrgId,
                         resource_state = #named_policy_revisions_state{
                            policy_name = PolicyName}} = State) ->
    Revisions = chef_db:list(#oc_chef_policy_revision{org_id = OrgId, name = PolicyName},
                             DbContext),
    RevisionsMap = build_revisions_map(Revisions, {[]}),
    {chef_json:encode(RevisionsMap), Req, State}.

build_revisions_map([Revision|Rest],EJSON) ->
    Updated = ej:set_p({"revisions", Revision}, EJSON, {[]}),
    build_revisions_map(Rest, Updated);
build_revisions_map([], EJSON) ->
    EJSON.
