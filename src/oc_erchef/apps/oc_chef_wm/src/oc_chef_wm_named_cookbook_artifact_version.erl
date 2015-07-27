%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Jean Rouge <jean@chef.io>
%% Copyright 2013-2015 Chef Software, Inc. All Rights Reserved.

-module(oc_chef_wm_named_cookbook_artifact_version).

-include("oc_chef_wm.hrl").

-behaviour(chef_wm).

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([validate_request/3,
         auth_info/2,
         init_resource_state/1,
         init/1,
         request_type/0,
         allowed_methods/2,
         from_json/2,
         resource_exists/2,
         to_json/2,
         create_path/2,
         malformed_request_message/3,
         conflict_message/1,
         delete_resource/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #cookbook_artifact_version_state{}}.

request_type() ->
    "cookbook_artifact_versions".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

create_path(Req, State) ->
    Identifier = wrq:path_info(identifier, Req),
    {Identifier, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('PUT', Req, #base_state{resource_state = CAVState} = State) ->
    CAVData = validate_json(Req),
    NewResourceState = CAVState#cookbook_artifact_version_state{cookbook_artifact_version_data = CAVData},
    valid_request(Req, State#base_state{resource_state = NewResourceState});
validate_request(_GetOrDelete, Req, State) ->
    valid_request(Req, State).

validate_json(Req) ->
    Body = wrq:req_body(Req),
    Ejson = oc_chef_cookbook_artifact_version:parse_binary_json(Body),
    [validate_url_param(Req, Ejson, Param) || Param <- [name, identifier]],
    Ejson.

validate_url_param(Req, Ejson, Param) ->
    FromReq = erlang:list_to_binary(wrq:path_info(Param, Req)),
    ParamBin = erlang:atom_to_binary(Param, utf8),
    FromEjson = ej:get({ParamBin}, Ejson),
    case FromReq =:= FromEjson of
        true ->
            ok;
        false ->
            erlang:throw({mismatch, {ParamBin, FromEjson, FromReq}})
    end.

valid_request(Req, #base_state{organization_guid = OrgId,
                               resource_state = ResourceState} = State) ->
    Name = wrq:path_info(name, Req),
    Identifier = wrq:path_info(identifier, Req),
    CAVRec = #oc_chef_cookbook_artifact_version{org_id = OrgId,
                                                name = Name,
                                                identifier = Identifier},
    NewResourceState = ResourceState#cookbook_artifact_version_state{oc_chef_cookbook_artifact_version = CAVRec},
    NewState = State#base_state{superuser_bypasses_checks = true,
                                resource_state = NewResourceState},
    {Req, NewState}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           resource_state = #cookbook_artifact_version_state{
                               oc_chef_cookbook_artifact_version = CAVRec
                           } = ResourceState} = State) ->
    Method = wrq:method(Req),
    auth_info(Req, State, ResourceState, Method, DbContext, CAVRec).

auth_info(Req, State, ResourceState, Method, DbContext, CAVRec) ->
    case {chef_db:fetch(CAVRec, DbContext), Method} of
        {not_found, 'PUT'} ->
            {{create_in_container, cookbook_artifact}, Req, State};
        {not_found, GetOrDelete} when GetOrDelete =:= 'GET'; GetOrDelete =:= 'DELETE' ->
            Message = chef_wm_util:error_message_envelope(<<"not_found">>),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = cookbook_artifact_version_not_found}};
        {#oc_chef_cookbook_artifact_version{authz_id = AuthzId} = CAV, _} ->
            NewResourceState = ResourceState#cookbook_artifact_version_state{oc_chef_cookbook_artifact_version = CAV},
            State1 = State#base_state{resource_state = NewResourceState},
            {{object, AuthzId}, Req, State1}
    end.

to_json(Req, #base_state{resource_state = #cookbook_artifact_version_state{
                             oc_chef_cookbook_artifact_version = CAVRec
                         }} = State) ->
    ExternalUrl = chef_wm_util:base_uri(Req),
    Ejson = oc_chef_cookbook_artifact_version:to_json(CAVRec, ExternalUrl),
    {jiffy:encode(Ejson), Req, State}.

from_json(Req, #base_state{resource_state = #cookbook_artifact_version_state{
                               authz_id = AuthzId,
                               cookbook_artifact_version_data = CAVData
                         }} = State) ->
    case oc_chef_wm_base:create_from_json(Req, State,
                                          oc_chef_cookbook_artifact_version,
                                          {authz_id, AuthzId},
                                          CAVData) of
        {_, Req1, #base_state{log_msg = {error, invalid_checksum}} = State1} ->
            %% that's a bit ugly, but looks like the only way to catch this
            %% case without massive changes
            Msg = <<"Manifest has a checksum that hasn't been uploaded.">>,
            JsonError = chef_wm_util:error_message_envelope(Msg),
            {{halt, 400}, chef_wm_util:set_json_body(Req1, JsonError), State1};
        Else ->
            Else
    end.

delete_resource(Req, #base_state{
                        chef_db_context = DbContext,
                        requestor_id = RequestorId,
                        resource_state = #cookbook_artifact_version_state{
                          oc_chef_cookbook_artifact_version = CAVRec
                        }} = State) ->
    ok = oc_chef_wm_base:delete_object(DbContext, CAVRec, RequestorId),
    ExternalUrl = chef_wm_util:base_uri(Req),
    Ejson = oc_chef_cookbook_artifact_version:to_json(CAVRec, ExternalUrl),
    {true, chef_wm_util:set_json_body(Req, Ejson), State}.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).

conflict_message(_Name) ->
    {[{<<"error">>, <<"Cookbook artifact already exists">>}]}.
