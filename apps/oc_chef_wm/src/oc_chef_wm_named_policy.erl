%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Oliver Ferrigni <oliver@chef.io>
%% @author Jean Rouge <jean@chef.io>
%% Copyright 2013-2015 Chef Software, Inc. All Rights Reserved.

-module(oc_chef_wm_named_policy).

-include("../../include/oc_chef_wm.hrl").

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
         conflict_message/1,
         post_is_create/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #policy_state{}}.

request_type() ->
    "policies".

%% TODO: we shouldn't accept POST
allowed_methods(Req, State) ->
    {['GET', 'PUT', 'POST', 'DELETE'], Req, State}.

%% TODO: we shouldn't accept POST
post_is_create(Req, State) ->
    {true, Req, State}.

create_path(Req, State) ->
    Name = wrq:path_info(policy_name, Req),
    {Name, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO!!! set back to real validate_request functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_request(_Method, Req, State) ->
    {Req, State}.

%%%% validate_request(Method, Req,
%%%%                  State = #base_state{organization_guid = OrgId})
%%%%   when Method == 'GET'; Method == 'DELETE' ->
%%%%     Name = wrq:path_info(policy_name, Req),
%%%%     Group = wrq:path_info(policy_group, Req),
%%%%     {Req, State#base_state{superuser_bypasses_checks = true,
%%%%                resource_state = #policy_state{
%%%%                      oc_chef_policy = #oc_chef_policy{
%%%%                          org_id = OrgId,
%%%%                          name = Name,
%%%%                          policy_group = Group}
%%%%                      }
%%%%               }
%%%%     };
%%%% validate_request(_PostOrPut, Req, #base_state{organization_guid = OrgId,
%%%%                                               resource_state = PolicyState
%%%%                                              } = State) ->
%%%%     Policy = validate_json(Req),
%%%%     Group = wrq:path_info(policy_group, Req),
%%%%     {Req, State#base_state{
%%%%             superuser_bypasses_checks = true,
%%%%             resource_state = PolicyState#policy_state{
%%%%                                oc_chef_policy = #oc_chef_policy{org_id = OrgId,
%%%%                                                                 policy_group = Group
%%%%                                                                },
%%%%                                policy_data = Policy}}}.
%%%%

%%%% validate_json(Req) ->
%%%%     Body = wrq:req_body(Req),
%%%%     {ok, Policy} = oc_chef_policy:parse_binary_json(Body),
%%%%     ok = validate_name(Req, Policy),
%%%% 		Policy.
%%%% 
%%%% validate_name(Req, Policy) ->
%%%%     NameFromReq = wrq:path_info(policy_name, Req),
%%%%     NameFromJson = erlang:binary_to_list(
%%%%                      ej:get({<<"name">>}, Policy, list_to_binary(
%%%%                                                     wrq:path_info(policy_name, Req)
%%%%                                                    )
%%%%                            )
%%%%                     ),
%%%%     case ibrowse_lib:url_encode(NameFromJson) =:= ibrowse_lib:url_encode(NameFromReq) of
%%%%         true ->
%%%%             ok;
%%%%         false ->
%%%%             erlang:throw({mismatch, {<<"name">>, NameFromJson, NameFromReq}})
%%%%     end.

% TODO: this is a callback from oc_chef_wm_base:forbidden/2. Reading the code
% there, we see that it does not check more than one container or object at a
% time. Creating a policy revision (via this endpoint) requires create in two
% containers, policy_groups and policies (if those don't exist), or update on
% two objects, the policy_group and policy. Note that one object might exist
% and the other not, so, for example, you could need CREATE on the groups
% container and UPDATE on a policy object if the policy exists but the group
% doesn't. Likewise, for read, you need read on those two objects.
%
auth_info(Req, #base_state{chef_db_context = DbContext,
                           resource_state = PolicyState = #policy_state{},
                           organization_guid = OrgId} =State) ->
    PolicyName = chef_wm_util:extract_from_path(policy_name, Req),
    case wrq:method(Req) of
        PostOrPut when PostOrPut =:= 'POST'; PostOrPut =:= 'PUT' ->
            policy_permissions(true, OrgId, PolicyName, DbContext, Req, State, PolicyState);
        _DeleteOrGet ->
            policy_permissions(false, OrgId, PolicyName, DbContext, Req, State, PolicyState)
    end.

%% TODO: figure out if we should prefer 403 to 404 to prevent leakage of
%% object names. If not, we can return 404 more efficiently for GET if any of
%% the policy or policy_group or group-revision association don't exist.
policy_permissions(CanCreate, _OrgId, PolicyName, DbContext, Req,
                #base_state{} = State, PolicyState = #policy_state{
                                                        oc_chef_policy = InputPolicy
                                                       }) ->
    %% This should be #oc_chef_policy_group_revision_association
    PolicyWithName = InputPolicy#oc_chef_policy{name = PolicyName},
    %% TODO: access the set of objects that this request pertains to via
    %% oc_chef_policy_group_revision_association. That module needs to be
    %% have a function which returns all the authz info we need for the
    %% request.
    case {chef_db:fetch(PolicyWithName, DbContext), CanCreate} of
        {not_found, true} ->
            {{create_in_container, policies}, Req,
             State#base_state{resource_state =
                                  PolicyState#policy_state{create_policy = true}}};
        {not_found, false} ->
            Message = chef_wm_util:error_message_envelope(
                        iolist_to_binary(["Cannot load policy ", PolicyName])),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = policy_not_found}};
        {forbidden, _} ->
            Message = chef_wm_util:error_message_envelope(
                        iolist_to_binary(["No permission for policy ", PolicyName])),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 403}, Req1, State#base_state{log_msg = policy_not_found}};
        {#oc_chef_policy{authz_id = AuthzId} = Policy, _} ->
            PolicyState1 = PolicyState#policy_state{oc_chef_policy = Policy},
            State1 = State#base_state{resource_state = PolicyState1},
            {{object, AuthzId}, Req, State1}
    end.

%% TODO: If we always do this because we determine 404 in the forbidden/2 callback,
%% why is it not a mixin?
resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, #base_state{
                organization_name = OrgName,
                resource_state = #policy_state{
                                    oc_chef_policy = Policy
                                   }} = State) ->
    % TODO: return document is actually a policy_revision need to implement any
    % required JSON munging in that module.
    Ejson = oc_chef_policy:assemble_policy_ejson(Policy, OrgName),

    {jiffy:encode(Ejson), Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO!!! set back to real functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

from_json(Req, State) ->
    {Req, State}.


%%%% from_json(Req, #base_state{resource_state = #policy_state{
%%%%                                                create_policy = false,
%%%%                                                oc_chef_policy = Policy,
%%%%                                                policy_data = PolicyData
%%%%                                               }
%%%%                           } = State) ->
%%%%     Group = wrq:path_info(policy_group, Req),
%%%%     UpdatedPolicy = Policy#oc_chef_policy{policy_group = list_to_binary(Group)},
%%%%     oc_chef_wm_base:update_from_json(Req, State, UpdatedPolicy, PolicyData);
%%%% from_json(Req, #base_state{resource_state = #policy_state{
%%%%                                                create_policy = true,
%%%%                                                policy_authz_id = AuthzId,
%%%%                                                policy_data = PolicyData
%%%%                                               }
%%%%                           } = State) ->
%%%%     Group = wrq:path_info(policy_group, Req),
%%%%     UpdatedPolicyData = ej:set({<<"policy_group">>}, PolicyData, Group),
%%%%     oc_chef_wm_base:create_from_json(Req, State, oc_chef_policy, {authz_id, AuthzId},
%%%%                                      UpdatedPolicyData).

conflict_message(Name) ->
    {[{<<"error">>, list_to_binary("Policy already exists " ++ Name)}]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO!!! set back to real functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete_resource(Req, State) ->
    {Req, State}.

%%%% delete_resource(Req, #base_state{
%%%%                         organization_name = OrgName,
%%%%                         chef_db_context = DbContext,
%%%%                         requestor_id = RequestorId,
%%%%                         resource_state = #policy_state{
%%%%                                             oc_chef_policy = InputPolicy }
%%%%                        } = State) ->
%%%%     Group = wrq:path_info(policy_group, Req),
%%%%     Policy = InputPolicy#oc_chef_policy{
%%%%                last_updated_by = RequestorId,
%%%%                policy_group = Group
%%%%               },
%%%%     ok = oc_chef_wm_base:delete_object(DbContext, Policy, RequestorId),
%%%%     Ejson = oc_chef_policy:assemble_policy_ejson(Policy, OrgName),
%%%%     {true, chef_wm_util:set_json_body(Req, Ejson), State}.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).
