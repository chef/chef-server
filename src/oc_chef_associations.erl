%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@getchef.com>
%% Copyright Chef Software, Inc. All Rights Reserved.
%%
%% Common functions for managing user-org associations.
%%

-module(oc_chef_associations).

-include_lib("chef_wm/include/chef_wm.hrl").
-include_lib("oc_chef_wm/include/oc_chef_wm.hrl").
-include_lib("oc_chef_authz/include/oc_chef_authz.hrl").

-export([deprovision_removed_user/3,
         provision_associated_user/3,
         % Turns out thes are common enough too.
         wm_associate_user/3,
         wm_conflict_response/4]).

-type deprovision_error() :: error_fetching_usag | error_fetching_org_users_group |
                              error_removing_from_org_user_group.
-type deprovision_warning() :: usag_record_delete_failed | org_admin_group_fetch_failed |
                              org_admin_ace_removal_failed.
-type deprovision_error_tuple() :: {error, { deprovision_error(),term()}}.
-type deprovision_warning_msg() ::  { deprovision_warning(), term() }.
-type deprovision_warning_tuple() :: {warning, [ deprovision_warning_msg(), ... ] }.
-type deprovision_response() ::  ok | deprovision_warning_tuple() | deprovision_error_tuple().

-type provision_error() :: usag_authz_creation_failed | usag_creation_failed |fetch_org_users_group_failed |
                           usag_update_failed | add_usag_to_org_users_group_failed | fetch_org_users_group_by_name_failed.
-type provision_warning() :: fetch_org_admins_failed | add_read_ace_for_admins_failed.
-type provision_error_tuple() :: {error, { provision_error(), term()}}.
-type provision_warning_msg() :: { provision_warning(), term() }.
-type provision_warning_tuple() :: { warning, [ provision_warning_msg(), ... ] }.
-type provision_response() :: ok | provision_warning_tuple() | provision_error_tuple().

% Internal use
-record(context, { authz_context,
                   db_context,
                   org_name,
                   org_id,
                   user_authz_id,
                   user_name,
                   user_id,
                   requestor_authz_id,
                   real_requestor_authz_id,
                   org_users,
                   msg = [],
                   usag} ).

wm_associate_user(Req, #base_state{organization_guid = OrgId,
                                   chef_db_context = DbContext,
                                   resource_state = #association_state{user = #chef_user{id = UserId,
                                                                                         username = UserName},
                                                                       data = ReqData}} = State,
                  RequestorId) ->
    ObjectRec = chef_object:new_record(oc_chef_org_user_association, OrgId, {authz_id, UserId}, ReqData),
    case chef_db:create(ObjectRec, DbContext, RequestorId) of
        {conflict, _} ->
            wm_conflict_response(Req, State, user_already_in_org, UserName);
        ok ->
            add_user_to_org(Req, State, ObjectRec, RequestorId);
        What ->
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

add_user_to_org(Req, #base_state{organization_guid = OrgId,
                                          organization_name = OrgName,
                                          chef_db_context = DbContext,
                                          resource_state = #association_state{user = #chef_user{id = UserId,
                                                                                                username = UserName} = User }} = State,
                         ObjectRec, RequestorId) ->
    TypeName = chef_object:type_name(ObjectRec),
    Name = chef_object:name(ObjectRec),
    Uri = ?BASE_ROUTES:route(TypeName, Req, [{name, Name}]),
    DeleteRecord = #oc_chef_org_user_association{org_id = OrgId, user_id = UserId},
    case provision_associated_user(State, User, RequestorId) of
        {error, {Step, Detail}} ->
            oc_chef_object_db:safe_delete(DbContext, DeleteRecord, RequestorId),
            wm_provision_failed(Req, State, {Step, Detail});
        {warning, Warnings} ->
            LogMsg = [{added, UserName, to, OrgName}, {warnings, Warnings}],
            {true,  Req, State#base_state{log_msg = LogMsg}};
        ok ->
            LogMsg = {added, UserName, to, OrgName},
            Uri = ?BASE_ROUTES:route(TypeName, Req, [{name, Name}]),
            {true, Req, State#base_state{log_msg = LogMsg}}
    end.

% A special case in which the requestor provided does not have permissions
% to create the USAG - this would happen if the inviting user had been removed from
% org admins group.
wm_provision_failed(Req, State, {usag_authz_creation_failed, {error, forbidden}}) ->
    {{halt, 403}, chef_wm_util:set_json_body(Req, invite_invalid_message()),
     State#base_state{log_msg = {inviting_admin_not_permitted}}};
wm_provision_failed(Req, State, {Step, Detail}) ->
    {{halt, 500}, Req, State#base_state{log_msg = {Step, Detail}}}.

%% Conflict responses are common/similar across invite creation,
%% invite acceptance, and association creation
wm_conflict_response(Req, State, ConflictType, UserName) ->
    LogMsg = {oc_chef_associations, ConflictType, UserName},
    ConflictMsg = conflict_message(ConflictType),
    {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg), State#base_state{log_msg = LogMsg}}.

%% Given a user whose record has been removed from an organization,
%% remove all permissions related to that org, for that user.
%% 1. fetch usag
%% 2. fetch org users
%% 3. remove USAG from org users
%% 4. delete USAG
%% 5. remove admin from user read ACE
-spec deprovision_removed_user(#base_state{}, #chef_user{}, oc_authz_id()) ->
    deprovision_response().
deprovision_removed_user(State, User, RequestorAuthzId) ->
    Context = association_context(State, User, RequestorAuthzId),
    Result = chef_db:fetch(#oc_chef_group{org_id = Context#context.org_id,
                                          name = Context#context.user_id,
                                          for_requestor_id = RequestorAuthzId},
                           Context#context.db_context),
    deprovision_fetch_users_group(Result, Context).

deprovision_fetch_users_group(#oc_chef_group{} = USAG, #context{ db_context = DbContext,
                                                            org_id = OrgId,
                                                            requestor_authz_id = RequestorAuthzId } = Context) ->
    Result = chef_db:fetch(#oc_chef_group{org_id = OrgId, name = "users",
                                          for_requestor_id = RequestorAuthzId }, DbContext),
    deprovision_remove_usag_from_users(Result, Context#context{usag = USAG});
deprovision_fetch_users_group(Error, _Context) ->
    {error, {error_fetching_usag,Error}}.

deprovision_remove_usag_from_users(#oc_chef_group{} = OrgUsersGroup,
                                   #context{usag = USAG,
                                            db_context = DbContext,
                                            requestor_authz_id = RequestorAuthzId} = Context) ->
    OrgUsersGroup1 = oc_chef_group:remove_group_member(OrgUsersGroup, USAG#oc_chef_group.name),
    Result = chef_db:update(OrgUsersGroup1, DbContext, RequestorAuthzId),
    deprovision_delete_usag(Result, Context#context{org_users = OrgUsersGroup1});
deprovision_remove_usag_from_users(Error, _Context) ->
    {error, {error_fetching_org_users_group, Error}}.

deprovision_delete_usag(ok, #context{usag = USAG,
                                     db_context = DbContext,
                                     requestor_authz_id = RequestorID } = Context) ->
    % This will delete both the USAG and its authz entry.
    Result = oc_chef_object_db:safe_delete(DbContext, USAG, RequestorID),
    deprovision_fetch_org_global_admins(Result, Context);
deprovision_delete_usag(Error, _Context) ->
    {error, {error_removing_from_org_user_group, Error}}.

deprovision_fetch_org_global_admins(ok, #context{authz_context = AuthzContext,
                                                 org_name = OrgName} = Context) ->
    Result = oc_chef_authz_db:fetch_global_group_authz_id(AuthzContext, OrgName, "global_admins"),
    deprovision_remove_global_org_admin_ace(Result, Context) ;
deprovision_fetch_org_global_admins(Error, Context) ->
    % We don't care if we failed to delete the actual USAG record from
    % the DB. If previous step to remove USAG from org users was successful, we can consider this a
    % successful deletion.
    % This will change when we start doing proper cleanup of auth entity as part of USAG deletion.
    deprovision_remove_global_org_admin_ace({ok, ok}, Context = #context{msg = [{usag_record_delete_failed, Error}]}).

deprovision_remove_global_org_admin_ace(OrgGlobalAdminsAuthzId,
                                        #context{ user_authz_id = UserAuthzId,
                                                  requestor_authz_id = RequestorAuthzId } = Context)  when is_binary(OrgGlobalAdminsAuthzId) ->
    %We're spoofing the requesting actor for this next operation to be the actual user
    % who is being removed.  This is because the actor will need to have update access
    % to that user's record - and the originator of this request may not.
    Result = oc_chef_authz:remove_ace_for_entity(RequestorAuthzId,
                                                 group, OrgGlobalAdminsAuthzId,
                                                 actor, UserAuthzId,
                                                 read),
    deprovision_removed_user_done(Result, Context);
deprovision_remove_global_org_admin_ace(Error, #context{msg = Msg}) ->
    % Here, we have deleted user from the org, etc - but we can't remove permissions.
    % This shouldn't fail the delete request which has already succeeded.
    {warning, [{org_admin_group_fetch_failed, Error}] ++ Msg}.

deprovision_removed_user_done(ok, #context{msg = []}) ->
    ok;
deprovision_removed_user_done(ok, #context{msg = Msg}) ->
    {warning, Msg};
deprovision_removed_user_done(Error, #context{msg = Msg} = Context) ->
    deprovision_removed_user_done(ok, Context#context{msg = [{org_admin_ace_removal_failed, Error}] ++ Msg}).



%% Given a user who has a record within an organization,
%% provision that user with proper permissions. These steps are broken out
%% below as follows:
%% 1. create USAG authzid
%% 2. create and save USAG record (in 'groups')
%% 3. update USAG to contain user
%% 4. add USAG to org
%% 5. set org admin read ACL on user (not usag)
-spec provision_associated_user(#base_state{}, #chef_user{}, binary()) -> provision_response().
provision_associated_user(State, #chef_user{id = UserId} = User, RequestorAuthzId) ->
    Requestor2 = case oc_chef_authz:superuser_id() of
                     RequestorAuthzId ->
                         superuser;
                     _ ->
                         RequestorAuthzId
                 end,
    Context = association_context(State, User, RequestorAuthzId),
    OrgId = Context#context.org_id,
    USAG0 = oc_chef_group:create_record(OrgId, UserId, RequestorAuthzId),
    % By forcing superuser here, this means we expect that the
    % calling resource has verified that the caller does have update org permissions
    % before invoking this.
    Result = oc_chef_authz:create_entity_if_authorized(Context#context.authz_context,
                                                       OrgId,
                                                       Requestor2,
                                                       group),
    provision_create_usag(Result, Context#context{usag = USAG0}).

provision_create_usag({ok, AuthzId}, #context{usag = USAG,
                                              requestor_authz_id = RequestorAuthzId,
                                              db_context = DbContext} = Context) ->
    USAG0 = USAG#oc_chef_group{authz_id = AuthzId},
    Result = chef_db:create(USAG0, DbContext, RequestorAuthzId),
    provision_set_usag_members(Result, Context#context{usag = USAG0});
provision_create_usag(Error, _Context) ->
    {error, {usag_authz_creation_failed, Error}}.

provision_set_usag_members(ok, #context{usag = USAG,
                                        user_name = UserName,
                                        requestor_authz_id = RequestorAuthzId,
                                        db_context = DbContext} = Context) ->
    USAG0 = oc_chef_group:add_user_member(USAG, UserName),
    Result = chef_db:update(USAG0, DbContext, RequestorAuthzId),
    provision_fetch_org_users_group(Result, Context#context{usag = USAG0});
provision_set_usag_members(Error, _Context) ->
    {error, {usag_creation_failed, Error}}.

provision_fetch_org_users_group(ok, #context{org_id = OrgId,
                                             requestor_authz_id = RequestorAuthzId,
                                             db_context = DbContext} = Context) ->
    Result = chef_db:fetch(#oc_chef_group{org_id = OrgId, name = "users",
                                          for_requestor_id = RequestorAuthzId}, DbContext),
    provision_add_usag_to_org_users(Result, Context);
provision_fetch_org_users_group(Error, _Context) ->
    {error, {update_usag_members_failed, Error}}.

provision_add_usag_to_org_users(#oc_chef_group{} = OrgUsersGroup,
                                #context{usag = USAG,
                                         requestor_authz_id = RequestorAuthzId,
                                         db_context = DbContext} = Context) ->
    OrgUsersGroup0 = oc_chef_group:add_group_member(OrgUsersGroup, USAG#oc_chef_group.name),
    Result = chef_db:update(OrgUsersGroup0, DbContext, RequestorAuthzId),
    provision_fetch_org_global_admins(Result, Context);
provision_add_usag_to_org_users(Error, _Context) ->
    {error, {fetch_org_users_group_by_name_failed, Error}}.

provision_fetch_org_global_admins(ok, #context{org_name = OrgName,
                                        authz_context = AuthzContext } = Context) ->
    Result = oc_chef_authz_db:fetch_global_group_authz_id(AuthzContext, OrgName, "global_admins"),
    provision_add_user_ace_to_global_admins(Result, Context);
provision_fetch_org_global_admins(Error, _Context) ->
    {error, {add_usag_to_org_users_group_failed, Error}}.

provision_add_user_ace_to_global_admins(OrgGlobalAdminsAuthzId,
                                        #context{user_authz_id = UserAuthzId,
                                                 real_requestor_authz_id = RequestorId } = Context) when is_binary(OrgGlobalAdminsAuthzId) ->
    % Spoofing to user as requestor, so that we have necessary access to update
    Result = oc_chef_authz:add_ace_for_entity(RequestorId,
                                              group, OrgGlobalAdminsAuthzId,
                                              actor, UserAuthzId,
                                              read),
    provision_associated_user_done(Result, Context);
provision_add_user_ace_to_global_admins(Error, _) ->
    % No need to continue - if we can't get the admin group, we can't add
    % permissions. However, the USAG is part fo the group, so even
    % though an admin cannot view this user in the context of the org,
    % they wll have permissions within the org.
    {warning, [{fetch_org_admins_failed, Error}]}.

provision_associated_user_done(ok, _Context) ->
    ok;
provision_associated_user_done(Error, _Context) ->
    {warning, [{add_read_ace_for_admins_failed, Error}]}.


% Consolidate our disparate inputs into one structure for ease of reference
association_context(#base_state{ organization_name = OrgName,
                                 organization_guid = OrgId,
                                 chef_authz_context = AuthzContext,
                                 chef_db_context = DbContext,
                                 requestor_id = RealRequestor},
                    #chef_user{ authz_id = UserAuthzId,
                                username = UserName,
                                id = UserId },
                    RequestorAuthzId) ->
    #context{ authz_context = AuthzContext,
              db_context = DbContext,
              org_name = OrgName,
              org_id = OrgId,
              user_authz_id = UserAuthzId,
              user_name = UserName,
              user_id = UserId,
              real_requestor_authz_id = RealRequestor,
              requestor_authz_id = RequestorAuthzId}.

invite_invalid_message() ->
    {[{<<"error">>, <<"This invitation is no longer valid. Please notify an administrator and request to be re-invited to the organization.">>}]}.

conflict_message(user_already_invited) ->
    {[{<<"error">>, <<"The invite already exists.">>}]};
conflict_message(user_already_in_org) ->
    {[{<<"error">>, <<"The association already exists.">>}]}.
