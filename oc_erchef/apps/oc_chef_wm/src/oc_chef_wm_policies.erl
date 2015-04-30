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

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #policy_state{}}.

request_type() ->
    "policies".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    {Req, State#base_state{superuser_bypasses_checks = true,
                           resource_state = #oc_chef_policy{org_id = OrgId}}}.

-spec auth_info(wm_req(), chef_wm:base_state()) ->
                       chef_wm:auth_info_return().
auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

-spec auth_info(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                       chef_wm:auth_info_return().
auth_info('GET', Req, State ) ->
    {{container, policies}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

to_json(Req, #base_state{chef_db_context = DbContext,
                         resource_state = StubRec
                        } = State) ->
    Names = chef_db:list(StubRec, DbContext),
    {chef_json:encode(lists:sort([ hd(NameAsList) ||NameAsList <- Names])), Req, State}.
