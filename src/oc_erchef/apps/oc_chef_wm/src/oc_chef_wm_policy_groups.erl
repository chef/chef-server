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


-module(oc_chef_wm_policy_groups).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
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
         to_json/2]).


init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #policy_state{}}.

request_type() ->
    "policy_groups".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    {Req, State#base_state{resource_state = #oc_chef_policy_group{org_id = OrgId}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('GET', Req, State) ->
    {{container, policy_group}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec to_json(#wm_reqdata{}, #base_state{}) -> {binary(), #wm_reqdata{}, #base_state{}}.
to_json(Req, #base_state{chef_db_context = DbContext,organization_guid = OrgId, reqid = ReqId, resource_state = StubRec} = State) ->
    %% we query for just the names first so we get groups that are empty,
    %% then we get the list of all policy revisions associated to a group and
    %% fill this in to the data structure from the first step.
    Names = chef_db:list(StubRec, DbContext),
    BaseEJSON = make_base_ejson(Names, Req),
    case chef_db:find_all_policy_revisions_by_group_and_name(DbContext, OrgId) of
        {error, Why} ->
            Report = {find_all_policy_revisions_by_group_and_name, {Why, ReqId}},
            lager:error("~p", [Report]),
            error(Report);
        PolicyGroupRevisionIDs ->
            EJSON = build_nested_list_data(PolicyGroupRevisionIDs, BaseEJSON),
            {chef_json:encode(EJSON), Req, State}
    end.

make_base_ejson(Names, Req) ->
    RouteFun = oc_chef_wm_routes:bulk_route_fun(policy_group, Req),
    UriMap= [{Name, RouteFun(Name)} || Name <- Names],
    build_nested_base_ejson(UriMap, {[]}).

build_nested_base_ejson([{[PolicyGroupName], URI}|Rest], EJSON) ->
    NewEJSON = ej:set_p({PolicyGroupName, "uri"}, EJSON, URI),
    build_nested_base_ejson(Rest, NewEJSON);
build_nested_base_ejson([], EJSON) ->
    EJSON.

build_nested_list_data([Row|Rest], EJSON) ->
    {PolicyGroupName, PolicyName, RevisionID} = Row,
    NewEJSON = ej:set_p({PolicyGroupName, "policies", PolicyName, "revision_id"}, EJSON, RevisionID),
    build_nested_list_data(Rest, NewEJSON);
build_nested_list_data([], EJSON) ->
    EJSON.
