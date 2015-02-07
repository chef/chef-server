%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Oliver Ferrigni <oliver@chef.io>
%% @author Jean Rouge <jean@chef.io>
%% Copyright 2013-2014 Chef Software, Inc. All Rights Reserved.

-module(oc_chef_wm_policies).

-include("../../include/oc_chef_wm.hrl").

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
         conflict_message/1,
         create_path/2,
         from_json/2,
         resource_exists/2 ]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3,
         to_json/2]).

%% Shared functions
-export([ policy_name_invalid/2 ]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #policy_state{}}.

request_type() ->
    "policies".

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    {Req, State#base_state{superuser_bypasses_checks = true,
                           resource_state = #oc_chef_policy{org_id = OrgId}}};
validate_request('POST', Req, #base_state{organization_guid = OrgId,
                                          resource_state = ResourceState}
                 = State) ->
    Body = wrq:req_body(Req),
    {ok, Json} = oc_chef_policy:parse_binary_json(Body),
    {Req, State#base_state{superuser_bypasses_checks = true,
                           resource_state = ResourceState#policy_state{
                                              oc_chef_policy =
                                                  #oc_chef_policy{
                                                     org_id = OrgId},
                                              policy_data = Json}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('GET', Req, State ) ->
    {{container, policies}, Req, State};
auth_info('POST', Req, State = #base_state{resource_state = #policy_state{policy_data = Json}}) ->
    {{create_in_container, policies}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #policy_state{policy_data = PolicyData}} = State) ->
    Name = fetch_id_name_from_json(PolicyData),
    {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state = #policy_state{policy_data = PolicyData,
                                                             policy_authz_id = AuthzId}} = State) ->
    oc_chef_wm_base:create_from_json(Req, State, oc_chef_policy, {authz_id, AuthzId}, PolicyData).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Policy already exists">>]}]}.

policy_name_invalid(Req, State) ->
    Msg = <<"Invalid policy name.">>,
    JsonError = {[{<<"error">>, [Msg]}]},
    {{halt, 400}, chef_wm_util:set_json_body(Req, JsonError), State}.

%% We allow either id or policyname to be set for a policy.  Precedence is
%% given to id over policyname if both set.
fetch_id_name_from_json(PolicyJson) ->
    ej:get({<<"id">>}, PolicyJson, ej:get({<<"name">>}, PolicyJson)).

to_json(Req, #base_state{chef_db_context = DbContext,
                         resource_state = StubRec
                        } = State) ->
    Names = chef_db:list(StubRec, DbContext),
    {chef_json:encode({[ {Name, Group} || [Name, Group] <- Names]}), Req, State}.
