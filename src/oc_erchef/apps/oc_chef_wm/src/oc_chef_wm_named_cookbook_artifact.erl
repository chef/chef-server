%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Jean Rouge <jean@chef.io>
%% Copyright 2013-2015 Chef Software, Inc. All Rights Reserved.

-module(oc_chef_wm_named_cookbook_artifact).

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
                           service_available/2,
                           validate_request/3]}]).

-export([auth_info/2,
         init_resource_state/1,
         init/1,
         request_type/0,
         allowed_methods/2,
         to_json/2,
         malformed_request_message/3]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #cookbook_artifacts_state{}}.

request_type() ->
    "named_cookbook_artifact".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

-spec auth_info(wm_req(), chef_wm:base_state()) ->
                       chef_wm:auth_info_return().
auth_info(Req, #base_state{organization_guid = OrgId,
                           chef_db_context = DbContext,
                           resource_args = single_artifact} = State) ->
    Name = wrq:path_info(name, Req),
    BaseRec = #oc_chef_cookbook_artifact{org_id = OrgId,
                                         name = Name},
    case chef_db:fetch(BaseRec, DbContext) of
        not_found ->
            Message = chef_wm_util:error_message_envelope(<<"not_found">>),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = cookbook_artifact_not_found}};
        #oc_chef_cookbook_artifact{} = CookbookArtifactRec ->
            auth_info_succeeded([CookbookArtifactRec], Req, State)
    end;
auth_info(Req, #base_state{organization_guid = OrgId,
                           chef_db_context = DbContext,
                           resource_args = list_artifacts} = State) ->
    case chef_db:fetch_multi(oc_chef_cookbook_artifact,
                             DbContext,
                             list_cookbook_artifacts_by_org_id,
                             [OrgId]) of
        not_found ->
            auth_info_succeeded([], Req, State);
        CookbookArtifactRecs when erlang:is_list(CookbookArtifactRecs) ->
            auth_info_succeeded(CookbookArtifactRecs, Req, State)
    end.

auth_info_succeeded(Recs, Req, #base_state{resource_state = ResourceState} = State) ->
    NewResourceState = ResourceState#cookbook_artifacts_state{oc_chef_cookbook_artifacts = Recs},
    State1 = State#base_state{resource_state = NewResourceState},
    {{container, cookbook_artifact}, Req, State1}.

to_json(Req, #base_state{resource_state = #cookbook_artifacts_state{
                             oc_chef_cookbook_artifacts = CookbookArtifactRecs
                         }} = State) ->
    CAFun = oc_chef_wm_routes:bulk_route_fun(cookbook_artifact, Req),
    ArtifactList = lists:map(
        fun(#oc_chef_cookbook_artifact{version_identifiers = Identifiers,
                                       name = CAVName}) ->
            CAVFun = oc_chef_wm_routes:bulk_route_fun(cookbook_artifact_version, CAVName, Req),
            VersionList = lists:map(
                fun(Identifier) ->
                    {[{<<"url">>, CAVFun(Identifier)},
                      {<<"identifier">>, Identifier}]}
                end,
                Identifiers
            ),
            ArtifactEjson = {[{<<"url">>, CAFun(CAVName)},
                              {<<"versions">>, VersionList}]},
            {CAVName, ArtifactEjson}
        end,
        CookbookArtifactRecs
    ),
    {jiffy:encode({ArtifactList}), Req, State}.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).
