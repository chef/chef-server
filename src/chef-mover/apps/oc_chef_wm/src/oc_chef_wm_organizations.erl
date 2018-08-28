%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013-2018 Chef Software, Inc.

-module(oc_chef_wm_organizations).

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

%%
%% Org creation is a multi-step process, where we create
%%
%% * organization record in SQL
%% * organization policy (groups, containers) which can be a bit time consuming
%% * a default environment
%% * a validator client, with a keypair
%%
%% We then return the client name and private key.
%%
%% This process can fail at any point, and we need to be able to clean up and recover when it happens
%% The key thing is to be able to allow the user to retry and not be blocked, so we must
%% delete the global admins group and the org record, because otherwise they'd have trouble using the same
%% name.
%%
%% We don't have to clean up org local data (clients/environments/containers/groups) because
%% that's tied to the guid and won't block re-creation.
%%
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
    ObjectRec = oc_chef_wm_base:object_creation_hook(ObjectRec, State),

    case chef_db:create(ObjectRec, DbContext, ActorId) of
        {conflict, _} ->
            oc_chef_wm_base:object_creation_error_hook(ObjectRec, ActorId),
            %% FIXME: created authz_id is leaked for this case, cleanup?
            LogMsg = {RecType, name_conflict, Name},
            ConflictMsg = ResourceMod:conflict_message(Name),
            {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg),
             State#base_state{log_msg = LogMsg, resource_state = ResourceState1}};
        ok ->
            LogMsg = {created, Name},
            Uri = oc_chef_wm_routes:route(TypeName, Req, [{name, Name}]),
            {true,
             chef_wm_util:set_uri_of_created_resource(Uri, Req),
             State#base_state{log_msg = LogMsg, resource_state = ResourceState1}};
        What ->
            %% FIXME: created authz_id is leaked for this case, cleanup?
            oc_chef_wm_base:object_creation_error_hook(ObjectRec, ActorId),
            {{halt, 500}, Req, State#base_state{log_msg = What, resource_state = ResourceState1}}
    end.


maybe_create_org({true, Req,
                  #base_state{
                     resource_state = #organization_state{
                                         oc_chef_organization = OrganizationData},
                     requestor = User} = State}) ->
    Result = oc_chef_authz_org_creator:create_org(OrganizationData, User),
    maybe_create_environment(Result, Req, State);
maybe_create_org({_Error, _Body, #base_state{}} = Result) ->
    %% Note: We don't handle the case where the org create failed yet somehow created an org
    %% record. The most common way for this to fail is for there already to be a org with
    %% that name, and we don't want to blow that away.
    Result.

maybe_create_environment({error, Error}, Req, State) ->
    %% TODO: Verify we provide useful logging for this.
    cleanup_org({{error, Error}, Req, State});
maybe_create_environment(ok, Req,
                         #base_state{
                            resource_state = #organization_state{
                                                oc_chef_organization = #oc_chef_organization{
                                                                          id = OrgId,
                                                                          name = OrgName
                                                                         }}} = State) ->
    EnvEJson = chef_environment:set_default_values(
                 {[{<<"name">>, <<"_default">>},
                   {<<"description">>, <<"The default Chef environment">>}]}),

    %% We have to fake up a a base state record for the environment create, but don't want
    %% it when we are done
    EnvState = State#base_state{resource_mod = chef_wm_environments,
                                organization_guid = OrgId,
                                organization_name = OrgName},
    AfterEnvCreate = fun({Status, Rec, _EnvState}) ->
                             %% The state record we give to create_object_with_acl can be
                             %% thrown away, since it only is updated on errors
                             maybe_create_client_key({Status, Rec, State})
                     end,
    create_object_with_acl(EnvEJson, environment, Req, EnvState, AfterEnvCreate).

maybe_create_client_key({true, Req, #base_state{} = State}) ->
    %% create the authzid for the for the validator client
    KeyPair = chef_keygen_cache:get_key_pair(),
    maybe_create_client(KeyPair, Req, State);
maybe_create_client_key({_Error, _Req, _State}=R) ->
    cleanup_org(R).

maybe_create_client(keygen_timeout, Req, State) ->
    cleanup_org({{halt, 503}, Req, State#base_state{log_msg = keygen_timeout}});
maybe_create_client({PublicKey, PrivateKey}, Req,
                    #base_state{
                       resource_state = #organization_state{
                                           oc_chef_organization = #oc_chef_organization{
                                                                     id = OrgId,
                                                                     name = OrgName
                                                                    }} = ResourceState} = State) ->
    ClientName = <<OrgName/binary,"-validator">>,
    ClientEJson = chef_object_base:set_public_key({[{<<"name">>, ClientName},
                                                    {<<"validator">>, true}]},
                                                  PublicKey),

    %% Update the return state
    OrgEJson =  {[{<<"uri">>, oc_chef_wm_routes:route(organization, Req, [{name, OrgName}])},
                  {<<"clientname">>, ClientName},
                  {<<"private_key">>, PrivateKey}]},

    State1 = State#base_state{resource_state= ResourceState#organization_state{organization_data = OrgEJson}},

    %% We have to fake up a a base state record for the client create, but don't want
    %% it when we are done
    ClientState = State#base_state{resource_mod = chef_wm_clients,
                                   organization_guid = OrgId,
                                   organization_name = OrgName},
    AfterClientCreate = fun(X) -> finish_org_create(X, OrgEJson, State1) end,
    create_object_with_acl(ClientEJson, client, Req, ClientState, AfterClientCreate).

%%
%% TODO: This maybe should be combined/refactored with oc_chef_wm_base:create_in_container
%%
-spec create_object_with_acl(ObjectJson :: {[tuple()]}, Type :: atom(),
                             Req :: wm_req(), State :: #base_state{},
                             ContinuationFn :: fun((tuple()) -> tuple()) ) ->
                                    tuple().
create_object_with_acl(ObjectJson, Type, Req,
                       #base_state{
                       resource_state = #organization_state{
                                           oc_chef_organization = #oc_chef_organization{
                                                                     id = OrgId,
                                                                     name = OrgName
                                                                    } },
                       chef_authz_context = AuthzCtx} = State,
                       ContinuationFn) ->
    {ChefType, ResourceMod} = create_type_helper(Type),
    ObjState = State#base_state{resource_mod = ResourceMod,
                                organization_guid = OrgId,
                                organization_name = OrgName},
    case oc_chef_authz:create_entity_if_authorized(AuthzCtx, OrgId, superuser, Type) of
        {ok, AuthzId} ->
            Result = oc_chef_wm_base:create_from_json(Req, ObjState, ChefType, {authz_id, AuthzId}, ObjectJson),
            ContinuationFn(Result);
        {error, forbidden} ->
            {Req1, State1} = oc_chef_wm_base:set_forbidden_msg(create, Req, State),
            {{halt, 503}, Req1, State1};
        {error, Error} ->
            {Req1, State1} = set_error_msg(Error, Req, State),
            {{halt, 503}, Req1, State1}
    end.

finish_org_create({true, Req, _ClientState}, OrgEJson, State) ->
    %% The state record we give to create_object_with_acl can be
    %% thrown away, since it only is updated on errors
    {true, chef_wm_util:set_json_body(Req, OrgEJson), State};
finish_org_create(Result, _, _) ->
    cleanup_org(Result).

create_type_helper(environment) ->
    {chef_environment, chef_wm_environments};
create_type_helper(client) ->
    {chef_client, chef_wm_clients}.


%%
%% We at mininimum need to clean up the org and the global_admins group
%% We will orphan some org-local information (groups, containers, validator client, and default environment)
%% But that's no worse than any org deletion right now, and is sufficient to allow retry of org creation.
%%
cleanup_org({_Error, _Body,
             #base_state{
                chef_db_context = DbContext,
                chef_authz_context=AuthzContext,
                requestor_id = RequestorId,
                organization_name = OrgName,
                resource_state = #organization_state{oc_chef_organization = Organization}}} = Result) ->
    oc_chef_wm_named_organization:delete_global_admins(DbContext, AuthzContext, OrgName, RequestorId),
    oc_chef_wm_base:delete_object(DbContext, Organization, RequestorId),
    Result.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, <<"Organization already exists">>}]}.

set_error_msg(Error, Req, State) ->
    JsonMsg = chef_json:encode({[{<<"error">>, [Error]}]}),
    Req1 = wrq:set_resp_body(JsonMsg, Req),
    {Req1, State#base_state{log_msg = Error } }.
