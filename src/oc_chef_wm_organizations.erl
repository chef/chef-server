%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_organizations).

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
    {ok, #organization_state{}}.

request_type() ->
    "organizations".

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    {Req, State#base_state{resource_state = #oc_chef_organization{id = OrgId}}};
validate_request('POST', Req, #base_state{resource_state = OrganizationState}= State) ->
    Body = wrq:req_body(Req),
    {ok, EJson} = oc_chef_organization:parse_binary_json(Body),
    {Req, State#base_state{resource_state = OrganizationState#organization_state{organization_data = EJson}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('GET', Req, State) ->
    {{container, organization}, Req, State};
auth_info('POST', Req, State) ->
    {{create_in_container, organization}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #organization_state{organization_data = OrganizationData}} = State) ->
    Name = ej:get({<<"name">>}, OrganizationData),
    {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state = #organization_state{organization_data = OrganizationData,
                                                                organization_authz_id = AuthzId}
                          } = State) ->
    Result = create_from_json(Req, State, oc_chef_organization, {authz_id, AuthzId}, OrganizationData),
    maybe_create_org(Result).

%%
%% This was extracted from chef_wm base for a few reasons
%% 1) We don't need to index organizations
%% 2) We do want the created org record back; all the special casing for the various resource
%%    state records makes that hard.
create_from_json(#wm_reqdata{} = Req,
                 #base_state{chef_db_context = DbContext,
                             organization_guid = OrgId, % remove this
                             requestor_id = ActorId,
                             resource_mod = ResourceMod,
                             resource_state = ResourceState
                             } = State,
                 RecType, {authz_id, AuthzId}, ObjectEjson) ->
    %% ObjectEjson should already be normalized. Record creation does minimal work and does
    %% not add or update any fields.
    ObjectRec = chef_object:new_record(RecType, OrgId, AuthzId, ObjectEjson),
    ResourceState1 = ResourceState#organization_state{organization_data = ObjectEjson,
                                                      organization_authz_id = AuthzId,
                                                      oc_chef_organization = ObjectRec},
    Name = chef_object:name(ObjectRec),
    TypeName = chef_object:type_name(ObjectRec),

    %% Perform any additional platform-specific work on the object
    %% This is strange; apparently it can't change anything?
    ObjectRec = ?BASE_RESOURCE:object_creation_hook(ObjectRec, State),

    case chef_db:create(ObjectRec, DbContext, ActorId) of
        {conflict, _} ->
            ?BASE_RESOURCE:object_creation_error_hook(ObjectRec, ActorId),
            %% FIXME: created authz_id is leaked for this case, cleanup?
            LogMsg = {RecType, name_conflict, Name},
            ConflictMsg = ResourceMod:conflict_message(Name),
            {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg),
             State#base_state{log_msg = LogMsg}};
        ok ->
            LogMsg = {created, Name},
            Uri = ?BASE_ROUTES:route(TypeName, Req, [{name, Name}]),
            {true,
             chef_wm_util:set_uri_of_created_resource(Uri, Req),
             State#base_state{log_msg = LogMsg, resource_state = ResourceState1}};
        What ->
            %% FIXME: created authz_id is leaked for this case, cleanup?
            ?BASE_RESOURCE:object_creation_error_hook(ObjectRec, ActorId),
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.


maybe_create_org({true, Req,
                  #base_state{
                     resource_state = #organization_state{
                                         oc_chef_organization = #oc_chef_organization{
                                                                   id = OrgId,
                                                                   name = OrgName
                                                                  } = OrganizationData},
                     requestor = User,
                     chef_authz_context = AuthzCtx} = State}) ->
    %% TODO error check here?
    ok = oc_chef_authz_org_creator:create_org(OrganizationData, User),

    %% create the authzid for the for the validator client
    %% TODO: handle failures for this (ala chef_wm_clients:handle_client_create)
    {PubKey, PrivKey} = chef_keygen_cache:get_key_pair(),

    {ok, ClientAuthzId} = oc_chef_authz:create_entity_if_authorized(AuthzCtx, OrgId, superuser, client),
    ClientName = <<OrgName/binary,"-validator">>,
    ClientEJson = chef_object_base:set_public_key({[{<<"name">>, ClientName},
                                                    {<<"validator">>, true}]},
                                                  PubKey),
    {ok, EnvAuthzId} = oc_chef_authz:create_entity_if_authorized(AuthzCtx, OrgId, superuser, environment),
    EnvEJson = chef_environment:set_default_values({[{<<"name">>, <<"_default">>},
                                                     {<<"description">>, <<"The default Chef environment">>}]}),

    EnvState = State#base_state{resource_mod = chef_wm_environments,
                                organization_guid = OrgId,
                                organization_name = OrgName},
    ClientState = State#base_state{resource_mod = chef_wm_clients,
                                   organization_guid = OrgId,
                                   organization_name = OrgName},

    case chef_wm_base:create_from_json(Req, EnvState, chef_environment, {authz_id, EnvAuthzId}, EnvEJson) of
        {{halt, _Code}, _Req, _State} = Response -> Response;
        {true, _Req, _State} ->
            case chef_wm_base:create_from_json(Req, ClientState, chef_client, {authz_id, ClientAuthzId}, ClientEJson) of
                {{halt, _Code1}, _Req1, _State1} = Response1 -> Response1;
                {true, _Req1, _State1} ->
                    URI = ?BASE_ROUTES:route(organization, Req, [{name, OrgName}]),
                    EJson = {[{<<"uri">>, URI},
                              {<<"clientname">>, ClientName},
                              {<<"private_key">>, PrivKey}]},
                    {true, chef_wm_util:set_json_body(Req, EJson), State}
            end
    end;
maybe_create_org(Other) ->
    Other.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Organization already exists">>]}]}.
