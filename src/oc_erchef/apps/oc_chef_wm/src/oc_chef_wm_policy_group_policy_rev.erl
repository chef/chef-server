%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Oliver Ferrigni <oliver@chef.io>
%% @author Jean Rouge <jean@chef.io>
%% Copyright 2013-2015 Chef Software, Inc. All Rights Reserved.

-module(oc_chef_wm_policy_group_policy_rev).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([allowed_methods/2,
         forbidden/2,
         delete_resource/2,
         from_json/2,
         resource_exists/2,
         to_json/2,
         create_path/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3,
         validate_json/2,
         policy_permissions_objects/4,
         conflict_message/1]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #policy_state{}}.

request_type() ->
    "policies".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

create_path(Req, State) ->
    Name = wrq:path_info(policy_name, Req),
    {Name, Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request(Method, Req,
                 State = #base_state{organization_guid = OrgId})
  when Method == 'GET'; Method == 'DELETE' ->
    Name = wrq:path_info(policy_name, Req),
    Group = wrq:path_info(policy_group, Req),
    {Req, State#base_state{superuser_bypasses_checks = true,
               resource_state = #policy_state{
                     oc_chef_policy_group_revision_association = create_input_pgr_assoc_record(OrgId, Name, Group)
                     }
              }
    };
validate_request('PUT', Req, #base_state{organization_guid = OrgId,
                                              resource_state = PolicyState
                                             } = State) ->
    Name = wrq:path_info(policy_name, Req),
    Group = wrq:path_info(policy_group, Req),
    Body = wrq:req_body(Req),
    PolicyRevision = validate_json(Body, Name),
    {Req, State#base_state{
            superuser_bypasses_checks = true,
            resource_state = PolicyState#policy_state{
                oc_chef_policy_group_revision_association = create_input_pgr_assoc_record(OrgId, Name, Group),
                policy_data = PolicyRevision}}}.

create_input_pgr_assoc_record(OrgID, PolicyName, GroupName) ->
    oc_chef_policy_group_revision_association:record_for_find(OrgID, PolicyName, GroupName).

validate_json(Body, NameFromReq) ->
    {ok, Policy} = oc_chef_policy_revision:parse_binary_json(Body),
    ok = validate_name(NameFromReq, Policy),
    Policy.

validate_name(NameFromReq, Policy) ->
    NameFromJson = erlang:binary_to_list(ej:get({<<"name">>}, Policy)),
    case ibrowse_lib:url_encode(NameFromJson) =:= ibrowse_lib:url_encode(NameFromReq) of
        true ->
            ok;
        false ->
            erlang:throw({mismatch, {<<"name">>, NameFromJson, NameFromReq}})
    end.

forbidden(Req, State) ->
    case auth_info(Req, State) of
        {{halt, Code}, Req1, State1} ->
            {{halt, Code}, Req1, State1};
        {AuthTuples, Req1, State1} ->
            MultiAuthResult = oc_chef_wm_base:multi_auth_check(AuthTuples, Req1, State1),
            case MultiAuthResult of
                {true, _Req, _State} ->
                    oc_chef_wm_base:multi_auth_check_to_wm_response(MultiAuthResult);
                {_Error, _AuthCheck, _Req, #base_state{resource_state = PolicyState, requestor_id = RequestorId}} ->
                    cleanup_orphaned_authz_ids(PolicyState#policy_state.created_policy, RequestorId, PolicyState#policy_state.policy_authz_id),
                    cleanup_orphaned_authz_ids(PolicyState#policy_state.created_policy_group, RequestorId, PolicyState#policy_state.policy_group_authz_id),
                    oc_chef_wm_base:multi_auth_check_to_wm_response(MultiAuthResult)
            end
    end.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           resource_state = #policy_state{oc_chef_policy_group_revision_association = QueryRecord} = PolicyState
                           } = State) ->
    %% for GET and DELETE we want to save the serialized object in here.
    PolicyAssoc = oc_chef_policy_group_revision_association:find_policy_revision_by_orgid_name_group_name(QueryRecord, DbContext),
    StateWithResponse = case PolicyAssoc of
        #oc_chef_policy_group_revision_association{id = ID, serialized_object = Object} ->
            PolicyStateWithResponseData = PolicyState#policy_state{
                    policy_assoc_exists = ID,
                    policy_data_for_response = Object},
            State#base_state{resource_state = PolicyStateWithResponseData};
        _Any -> State
    end,

    PermissionsObjects = policy_permissions_objects(wrq:method(Req), PolicyAssoc, QueryRecord, DbContext),
    StateWithAuthzIDs = stash_permissions_objects_authz_ids(PermissionsObjects, StateWithResponse),
    PermissionsListOrHalt = permissions_with_actions(PermissionsObjects, Req),

    case PermissionsListOrHalt of
        {halt, 404, Message} ->
            ReqWithBody = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, ReqWithBody, StateWithAuthzIDs#base_state{log_msg = policy_not_found}};
        PermissionsList ->
            {PermissionsList, Req, StateWithAuthzIDs}
    end.


stash_permissions_objects_authz_ids(Halt, State) when is_tuple(Halt) ->
    State;
stash_permissions_objects_authz_ids([], State) ->
    State;
stash_permissions_objects_authz_ids([{create_in_container, _C}|Rest], State) ->
    stash_permissions_objects_authz_ids(Rest, State);
stash_permissions_objects_authz_ids([{policy_group,AzID}|Rest], #base_state{resource_state = PolicyState} = State) ->
    UpdatedPolicyState = PolicyState#policy_state{policy_group_authz_id = AzID},
    UpdatedBaseState = State#base_state{resource_state = UpdatedPolicyState},
    stash_permissions_objects_authz_ids(Rest, UpdatedBaseState);
stash_permissions_objects_authz_ids([{policy,AzID}|Rest], #base_state{resource_state = PolicyState} = State) ->
    UpdatedPolicyState = PolicyState#policy_state{policy_authz_id = AzID},
    UpdatedBaseState = State#base_state{resource_state = UpdatedPolicyState},
    stash_permissions_objects_authz_ids(Rest, UpdatedBaseState).

cleanup_orphaned_authz_ids(false, _RequestorId, _AuthzID) ->
    ok;
cleanup_orphaned_authz_ids(true, RequestorId, AuthzId) ->
    oc_chef_authz:delete_resource(RequestorId, object, AuthzId),
    ok.

permissions_with_actions(Halt, _Req) when is_tuple(Halt) ->
    Halt;
permissions_with_actions(PermissionsList, Req) when is_list(PermissionsList) ->
    [ permission_with_action_for_object(Permission, Req) || Permission <- PermissionsList].

permission_with_action_for_object({create_in_container, Container}, _Req) ->
    {create_in_container, Container};
permission_with_action_for_object({_ObjectName, AuthzID}, Req) ->
    {object, AuthzID, oc_chef_wm_base:http_method_to_authz_perm(Req)}.

%% Returns a list items to be consulted to run authz checks for the request,
%% depending on whether the policy_group and policy referenced in the request
%% exist and the HTTP method. When the prerequsite objects exist (and are
%% needed to authz the request), they are returned as 2-tuples of
%% {policy|policy_group, AuthzID}, so they can be stored in the request state
%% for later. Note that oc_chef_wm_base:multi_auth expects tuples like
%% {object, AuthzID}, use format_permissions_for_multi_auth/1 to convert.
policy_permissions_objects(_AnyMethod,
                   #oc_chef_policy_group_revision_association{
                        policy_authz_id = PolicyAuthzId, policy_group_authz_id = PolicyGroupAuthzID},
                   _QueryRecord, _DbContext) ->
    %% This means that delete will need delete on the policy and group, even
    %% though you're not deleting anything.
    [{policy, PolicyAuthzId}, {policy_group, PolicyGroupAuthzID}];
policy_permissions_objects('PUT', not_found, QueryRecord, DbContext) ->
    PrereqObjects = oc_chef_policy_group_revision_association:fetch_prereq_objects(QueryRecord, DbContext),
    prereq_objects_to_permissions(PrereqObjects);
policy_permissions_objects(_GetOrDelete, not_found,
                   #oc_chef_policy_group_revision_association{
                        policy_revision_name = PolicyName,
                        policy_group_name = PolicyGroupName},
                   _DbContext) ->
    Message = chef_wm_util:error_message_envelope(
                iolist_to_binary(["Cannot load policy ", PolicyName, " in policy group ", PolicyGroupName])),
    {halt, 404, Message}.

prereq_objects_to_permissions(PrereqObjects) ->
    prereq_objects_to_permissions(PrereqObjects, []).

prereq_objects_to_permissions([], PermissionsList) ->
    PermissionsList;
prereq_objects_to_permissions([PrereqObject|Rest], PermissionsList) ->
    RequiredPermission = prereq_object_permission(PrereqObject),
    UpdatedPermissionList = [RequiredPermission | PermissionsList ],
    prereq_objects_to_permissions(Rest, UpdatedPermissionList).

prereq_object_permission({policy, not_found}) ->
    {create_in_container, policies};
prereq_object_permission({policy, #oc_chef_policy{authz_id = AuthzID}}) ->
    {policy, AuthzID};
prereq_object_permission({policy_group, not_found}) ->
    {create_in_container, policy_group};
prereq_object_permission({policy_group, #oc_chef_policy_group{authz_id = AuthzID}}) ->
    {policy_group, AuthzID}.

resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, #base_state{resource_state = #policy_state{policy_data_for_response = PolicyData}} = State) ->
    {jiffy:encode(PolicyData), Req, State}.

from_json(Req, #base_state{organization_guid = OrgID,
                           chef_db_context = DbContext,
                           requestor_id = RequestorId,
                           server_api_version = ApiVersion,
                           resource_state = #policy_state{policy_data = PolicyData,
                                                          policy_authz_id = PolicyAuthzID,
                                                          policy_group_authz_id = PolicyGroupAuthzID,
                                                          policy_assoc_exists = MaybeAssocID
                                                         }} = State) ->

    PolicyName = iolist_to_binary(wrq:path_info(policy_name, Req)),
    PolicyGroupName = iolist_to_binary(wrq:path_info(policy_group, Req)),

    case MaybeAssocID of
        false ->
            CreateRecord =  oc_chef_policy_group_revision_association:new_record(ApiVersion,
                                                                                 OrgID,
                                                                                 PolicyName,
                                                                                 PolicyAuthzID,
                                                                                 PolicyGroupName,
                                                                                 PolicyGroupAuthzID,
                                                                                 PolicyData),
            R = oc_chef_policy_group_revision_association:insert_association(CreateRecord, DbContext, RequestorId),
            handle_create_result(R, PolicyName, PolicyData, Req, State);
        AssocId ->
            UpdateRecord = oc_chef_policy_group_revision_association:update_record(ApiVersion,
                                                                                   AssocId,
                                                                                   OrgID,
                                                                                   PolicyName,
                                                                                   PolicyAuthzID,
                                                                                   PolicyGroupName,
                                                                                   PolicyGroupAuthzID,
                                                                                   PolicyData),
            R = oc_chef_policy_group_revision_association:update_association(UpdateRecord, DbContext, RequestorId),
            handle_update_result(R, PolicyName, PolicyData, Req, State)
    end.

handle_create_result(ok, PolicyName, PolicyData, Req, State) ->
    LogMsg = {created, PolicyName},
    Uri = oc_chef_wm_routes:route(policy, Req, [{name, PolicyName}]),
    ReqWithURI = chef_wm_util:set_uri_of_created_resource(Uri, Req),
    ReqWithBody = chef_wm_util:set_json_body(ReqWithURI, PolicyData),
    {true, ReqWithBody, State#base_state{log_msg = LogMsg}};
handle_create_result(ERROR, _PolicyName, _PolicyData, Req, State) ->
    {{halt, 500}, Req, State#base_state{log_msg = ERROR}}.

handle_update_result(ok, PolicyName, PolicyData, Req, State) ->
    LogMsg = {updated, PolicyName},
    ReqWithBody = chef_wm_util:set_json_body(Req, PolicyData),
    {true, ReqWithBody, State#base_state{log_msg = LogMsg}};
handle_update_result(ERROR, _PolicyName, _PolicyData, Req, State) ->
    {{halt, 500}, Req, State#base_state{log_msg = ERROR}}.


conflict_message(Name) ->
    {[{<<"error">>, list_to_binary("Policy already exists " ++ Name)}]}.



delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 resource_state = #policy_state{policy_assoc_exists = AssocID,
                                                                policy_data_for_response = PolicyData}} = State) ->
    AssocToDelete = #oc_chef_policy_group_revision_association{ id = AssocID },
    DeleteResult = oc_chef_policy_group_revision_association:delete_association(AssocToDelete, DbContext),
    case DeleteResult of
        % Delete returns integer
        1 ->
            {true, chef_wm_util:set_json_body(Req, PolicyData), State};
        ERROR ->
            {{halt, 500}, Req, State#base_state{log_msg = ERROR}}
    end.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).
