%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013-2018 Chef Software, Inc.

-module(oc_chef_wm_containers).

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
                           service_available/2,
                           {list_objects_json/2, to_json}]}]).

-export([allowed_methods/2,
         conflict_message/1,
         create_path/2,
         from_json/2,
         resource_exists/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).

-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #container_state{}}.

request_type() ->
    "containers".

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    {Req, State#base_state{resource_state = #oc_chef_container{org_id = OrgId}}};
validate_request('POST', Req, #base_state{resource_state = ContainerState}= State) ->
    Body = wrq:req_body(Req),
    {ok, EJson} = oc_chef_container:parse_binary_json(Body),
    {Req, State#base_state{resource_state = ContainerState#container_state{container_data = EJson}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('GET', Req, State) ->
    {{container, container}, Req, State};
auth_info('POST', Req, State) ->
    {{create_in_container, container}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #container_state{container_data = ContainerData}} = State) ->
    Name = ej:get({<<"containername">>}, ContainerData),
    {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state = #container_state{container_data = ContainerData,
                                                             container_authz_id = AuthzId}} = State) ->
    oc_chef_wm_base:create_from_json(Req, State, oc_chef_container, {authz_id, AuthzId}, ContainerData).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Container already exists">>]}]}.
