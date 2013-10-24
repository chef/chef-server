%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_groups).

-include_lib("chef_wm/include/chef_wm.hrl").
%%-include_lib("oc_chef_authz/include/oc_chef_types.hrl").
-include_lib("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

-mixin([{chef_wm_base, [
                        content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2,
                        post_is_create/2,
                        {list_objects_json/2, to_json}
                       ]}]).

-mixin([{oc_chef_wm_base, [forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-behaviour(chef_wm).

-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).

-export([
         allowed_methods/2,
         conflict_message/1,
         create_path/2,
         from_json/2,
         resource_exists/2
        ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #group_state{}}.

request_type() ->
    "groups".

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    {Req, State#base_state{superuser_bypasses_checks = true, resource_state = #oc_chef_group{org_id = OrgId}}};
validate_request('POST', Req, #base_state{organization_guid = OrgId, resource_state = ResourceState} = State) ->
    Body = wrq:req_body(Req),
    {ok, Json} = oc_chef_group:parse_binary_json(Body),
    {Req, State#base_state{superuser_bypasses_checks = true,resource_state = ResourceState#group_state{oc_chef_group = #oc_chef_group{org_id = OrgId}, group_data = Json}}}. 

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

%% TODO: Figure out where these hook in
auth_info('GET', Req, State = #base_state{ resource_state = ResourceState}) ->
    error_logger:info_msg(ResourceState),
    {{container, group}, Req, State};
auth_info('POST', Req, State) ->
    {{create_in_container, group}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #group_state{group_data = GroupData}} = State) ->
    Name = ej:get({<<"groupname">>}, GroupData),
    {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state = #group_state{group_data = GroupData,
                                                             group_authz_id = AuthzId}} = State) ->
    chef_wm_base:create_from_json(Req, State, oc_chef_group, {authz_id, AuthzId}, GroupData).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Group already exists">>]}]}.
