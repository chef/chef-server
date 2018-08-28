%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013-2018 Chef Software, Inc.

-module(oc_chef_wm_groups).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           {list_objects_json/2, to_json},
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
         validate_request/3 ]).

%% Shared functions
-export([validate_group_name/1,
         group_name_invalid/2 ]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #group_state{}}.

request_type() ->
    "groups".

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    {Req, State#base_state{superuser_bypasses_checks = true,
                           resource_state = #oc_chef_group{org_id = OrgId}}};
validate_request('POST', Req, #base_state{organization_guid = OrgId,
                                          resource_state = ResourceState}
                 = State) ->
    Body = wrq:req_body(Req),
    {ok, Json} = oc_chef_group:parse_binary_json(Body),
    {Req, State#base_state{superuser_bypasses_checks = true,
                           resource_state = ResourceState#group_state{
                                              oc_chef_group =
                                                  #oc_chef_group{
                                                     org_id = OrgId},
                                              group_data = Json}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('GET', Req, State ) ->
    {{container, group}, Req, State};
auth_info('POST', Req, State = #base_state{resource_state = #group_state{group_data = Json}}) ->
    case validate_group_name(fetch_id_name_from_json(Json)) of
        valid ->
            {{create_in_container, group}, Req, State};
        _ ->
            group_name_invalid(Req, State)
    end.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #group_state{group_data = GroupData}} = State) ->
    Name = fetch_id_name_from_json(GroupData),
    {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state = #group_state{group_data = GroupData,
                                                             group_authz_id = AuthzId}} = State) ->
    oc_chef_wm_base:create_from_json(Req, State, oc_chef_group, {authz_id, AuthzId}, GroupData).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Group already exists">>]}]}.

-define(VALID_NAME_REGEX, "^[a-z0-9\-_]+$").

validate_group_name(undefined) ->
    missing;
validate_group_name(Name) ->
    {ok, CompiledRegex} = re:compile(?VALID_NAME_REGEX),
    case re:run(Name, CompiledRegex) of
        {match, _} ->
            valid;
        nomatch  ->
            invalid
    end.

group_name_invalid(Req, State) ->
    Msg = <<"Invalid group name.">>,
    JsonError = {[{<<"error">>, [Msg]}]},
    {{halt, 400}, chef_wm_util:set_json_body(Req, JsonError), State}.

%% We allow either id or groupname to be set for a group.  Precedence is
%% given to id over groupname if both set.
fetch_id_name_from_json(GroupJson) ->
    ej:get({<<"id">>}, GroupJson, ej:get({<<"groupname">>}, GroupJson)).
