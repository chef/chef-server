%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2014-2018 Chef Software, Inc.

-module(oc_chef_wm_named_organization).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         resource_exists/2,
         to_json/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).


-export([delete_global_admins/4]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #organization_state{}}.

request_type() ->
    "organizations".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request(Method, Req, State = #base_state{organization_guid = OrgId}) when Method == 'GET';
                                                                                   Method == 'DELETE' ->
    {Req, State#base_state{superuser_bypasses_checks = true, resource_state = #organization_state{oc_chef_organization = #oc_chef_organization{id = OrgId}}}};
validate_request('PUT', Req, #base_state{organization_name = OrgName,
                                         organization_guid = OrgId,
                                         resource_state = OrgState} = State) ->
    Body = wrq:req_body(Req),
    {ok, Org} = oc_chef_organization:parse_binary_json({Body, OrgName}),
    {Req, State#base_state{
            superuser_bypasses_checks = true,
            resource_state = OrgState#organization_state{
                               oc_chef_organization = #oc_chef_organization{id = OrgId},
                               organization_data = Org}}}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           resource_state = OrgState,
                           organization_guid = OrgId,
                           organization_name = OrgName} = State) ->
    %% TODO: This fetch might be sensibly merged into the org GUID fetch code, but it is
    %% simpler now to just get the org fresh, fewer yaks to shave

    case chef_db:fetch(#oc_chef_organization{id = OrgId, name = OrgName}, DbContext) of
        not_found ->
            Message = chef_wm_util:error_message_envelope(iolist_to_binary(["Cannot load Organization ",
                                                                            OrgName])),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = organization_not_found}};
        #oc_chef_organization{authz_id = AuthzId} = Org ->
            %% TODO: If we are putting to an org and changing anything other than
            %% full_name, we might want to fail here instead, unless we are superuser
            OrgState1 = OrgState#organization_state{oc_chef_organization = Org},
            State1 = State#base_state{resource_state = OrgState1},
            {{object, AuthzId}, Req, State1}
    end.

resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, #base_state{resource_state = #organization_state{
                                             oc_chef_organization = Org
                                            }} = State) ->
    Ejson = oc_chef_organization:assemble_organization_ejson(Org),
    Json = chef_json:encode(Ejson),
    {Json, Req, State}.

from_json(Req, #base_state{resource_state = #organization_state{
                                               oc_chef_organization = Organization,
                                               organization_data = OrganizationData
                                              }
                          } = State) ->
    oc_chef_wm_base:update_from_json(Req, State, Organization, OrganizationData).

%% NOTE: This is really only partially implemented
%% Org deletion needs to:
%% 1) Disassociate all users
%%    We don't directly disassociate users. Instead, we rely on two consistiency properties
%%    * Deleting the org deletes the associations and invites via foreign key constraints.
%%    * Deleting the global admins group removes it from the users read ACE.
%%
%%    This has the advantage of removing users last, since we delete the org record as the
%%    last step. If we disasociated users sooner a failure during the delete could leave us
%%    in a state where where there's no one left with the privs to retry the delete.
%%
%%    We will need to be cleverer if we move to a world where more cleanup is required for
%%    associations.
%%
%% 2) Delete <ORGNAME>_global_admins
%%    Bifrost will remove that any ACLs and groups where it appears.
%%
%% 3) TODO: Clean up all org objects in sql and authz.
%%    We leave a lot of records behind in sql and authz. (nodes, etc) This is likely to be
%%    expensive to remove, as there are a lot of objects to delete in authz. One possiblity
%%    would be to do that part of the cleanup offline
%%
%% TODO: verify we have tests for double delete, permissions, and org membership.
%%
delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #organization_state{
                                                     oc_chef_organization = Organization}
                                } = State) ->
    delete_global_admins(Req, State),
    ok = oc_chef_wm_base:delete_object(DbContext, Organization, RequestorId),
    Ejson = oc_chef_organization:assemble_organization_ejson(Organization),
    {true, chef_wm_util:set_json_body(Req, Ejson), State}.

%% Delete global admins
%% We delete both the global admins record in SQL, and the authz object.
%% We do not directly remove the global_admins group from each users ACE; instead
%% we rely on bifrost to maintain consistiency when the group is deleted.
%%
%% TODO: Make sure tests explicitly test for global admins removal on org deletion
delete_global_admins(_Req, #base_state{chef_db_context = DbContext,
                                       organization_name = OrgName,
                                       chef_authz_context = AuthzContext,
                                       requestor_id = RequestorId} = _State) ->
    delete_global_admins(DbContext, AuthzContext, OrgName, RequestorId).

delete_global_admins(DbContext, AuthzContext, OrgName, RequestorId) ->
    %% TODO
    %% Refactor the fetch of global admins, along with that in oc_chef_wm_association
    %% we use this low-level api because we don't want to expand the users/groups from authz.
    case oc_chef_authz_db:fetch_global_admins(AuthzContext, OrgName) of
        #oc_chef_group{} = GlobalAdmins ->
            %% This could be done as superuser, because we've already checked our org's
            %% permissions, and those should take precedence over those on global_admins.
            oc_chef_object_db:safe_delete(DbContext, GlobalAdmins, RequestorId);
        {not_found, authz_group} ->
            %% Ignoring this error lets us retry the whole deletion process if it fails part
            %% of the way through
            lager:error("Could not find global admins when deleting org ~s", [OrgName]),
            ok
    end.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).
