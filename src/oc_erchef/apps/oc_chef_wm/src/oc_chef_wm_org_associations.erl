%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@chef.io>
%% Copyright 2014-2018 Chef Software, Inc.
%%
%% This module handles the following
%%
% GET   /organizations/:orgname/users
%        - list of org users.
% POST  /organizations/:orgname/users -
%        - immediately associate a user to an org. Superuser only.
% GET   /organizations/:orgname/users/:username
%        - get user details for an associated user. Must have read on user and org.
% DELETE /organizations/:orgname/users/:username
%        - disassociate a user from an org. Org Admin.
%
%

-module(oc_chef_wm_org_associations).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           post_is_create/2,
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
         create_path/2,
         request_type/0,
         validate_request/3]).


init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #association_state{}}.

request_type() ->
    "associations".

allowed_methods(Req, State) ->
    Allowed = case chef_wm_util:object_name(user, Req) of
        undefined ->
            ['GET', 'POST'];
        _ ->
            ['GET', 'DELETE']
    end,
    {Allowed, Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('POST', Req, #base_state{chef_db_context = DbContext} = State) ->
    {ok, EJ} = oc_chef_org_user_association:parse_binary_json(wrq:req_body(Req)),
    UserName = ej:get({<<"username">>}, EJ),
    User = chef_db:fetch(#chef_user{username = UserName}, DbContext),
    {Req, State#base_state{resource_state = #association_state{user = User,
                                                               user_name = UserName,
                                                               data = EJ}}};
validate_request(_Method, Req, #base_state{organization_guid = OrgId, chef_db_context = DbContext} = State) ->
    case chef_wm_util:object_name(user, Req) of
        undefined ->
            {Req, State};
        UserName ->
            % GET or DELETE for a named user has to verify that the user exists  and that requestor
            % has appopriate access to that actor - so let's load up both.
            {User, Association} = user_and_association(OrgId, DbContext, UserName),
            {Req, State#base_state{resource_state = #association_state{user = User,
                                                                       org_user_association = Association,
                                                                       user_name = UserName }}}
    end.

user_and_association(OrgId, DbContext, UserName) when is_binary(UserName) ->
    user_and_association(OrgId, DbContext, chef_db:fetch(#chef_user{username = UserName}, DbContext));
user_and_association(_, _, not_found) ->
    {not_found, not_found};
user_and_association(OrgId, DbContext, #chef_user{id = UserId} = User) ->
    Association = chef_db:fetch(#oc_chef_org_user_association{user_id = UserId, org_id = OrgId}, DbContext),
    {User, Association}.


%% If we reach this point and required a user object, no matter the method, if the user
%% doesn't exist we'll bail. (If a user is not required, it will be 'undefined')
auth_info(Req, #base_state{resource_state = #association_state{user = not_found,
                                                               user_name = UserName}} = State) ->
    Message = chef_wm_util:not_found_message(user, UserName),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 404}, Req1, State#base_state{log_msg = user_not_found}};
auth_info(Req, #base_state{organization_name = OrgName,
                           resource_state = #association_state{user = #chef_user{},
                                                               org_user_association = not_found,
                                                               user_name = UserName} } = State) ->
    %% If attempting an operation on a valid user within an org, but the user does not exist in the org,
    %% the answer is always not found
    Message = chef_wm_util:not_found_message(association, {UserName, OrgName}),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 404}, Req1, State#base_state{log_msg = user_not_found}};
auth_info(Req, #base_state{requestor_id = RequestorAuthzId,
                           organization_authz_id = OrgAuthzId,
                           resource_state = #association_state{user = User} } = State) ->
    case wrq:method(Req) of
        'POST' ->
            % Only the superuser can force-create an org-user association
            {superuser_only, Req, State};
        Method ->
            {auth_type_for_method(Method, User, OrgAuthzId, RequestorAuthzId), Req, State}
    end.

auth_type_for_method('DELETE', #chef_user{ authz_id = UserAuthzId }, _OrgAuthzId, UserAuthzId) ->
    %% permissions-wise, user can always disassociate his or her own org association
    %% though we'll have additional safety checks below as well.
    authorized;
auth_type_for_method('DELETE', #chef_user{authz_id = UserAuthzId }, OrgAuthzId, _RequestorAuthzId) ->
    % Because we're still returning the user's data on succesful delete, ensure
    % that the requestor is allowed to see it, in addition to updating the org.
    [{object, OrgAuthzId, update}, {actor, UserAuthzId, read}];
auth_type_for_method('GET', #chef_user{ authz_id = UserAuthzId }, OrgAuthzId, _RequestorAuthzId) ->
    [{object, OrgAuthzId, read}, {actor, UserAuthzId, read}];
auth_type_for_method('GET', undefined, OrgAuthzId, _RequestorAuthzId) ->
    {object, OrgAuthzId, read}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #association_state{user = #chef_user{username = Name}}} = State) ->
    {binary_to_list(Name), Req, State}.

to_json(Req, #base_state{organization_guid = OrgId,
                         chef_db_context = DbContext,
                         resource_state = #association_state{user = undefined}} = State ) ->
    % Because we're not using the standard form of "[a,b,c]"  in our response in order to
    % keep compatible, using oc_chef_wm_base:list_object_json does more than we can use -
    % instead capture the result directly here.
    case chef_db:list(#oc_chef_org_user_association{org_id = OrgId}, DbContext) of
        Names when is_list(Names) ->
            EJson = oc_chef_org_user_association:ejson_from_list(Names),
            {chef_json:encode(EJson), Req, State};
        Error ->
            {{halt, 500}, Req, State#base_state{log_msg = Error }}
    end;
to_json(Req, #base_state{organization_name = OrgName,
                         chef_db_context = DbContext,
                         resource_state = #association_state{user = #chef_user{username = UserName} = User}} = State ) ->
    % Verify that the user is actually in the org, then send the results back.
    case chef_db:is_user_in_org(DbContext, UserName, OrgName) of
        true ->
            EJson = chef_user:assemble_user_ejson(User, OrgName),
            Json = chef_json:encode(EJson),
            {Json, Req, State};
        false ->
            Message = chef_wm_util:not_found_message(association, {UserName, OrgName}),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = user_not_found}}
    end.

from_json(Req, State) ->
    oc_chef_associations:wm_associate_user(Req, State, oc_chef_authz:superuser_id()).

delete_resource(Req, #base_state{ resource_state = #association_state{user = #chef_user{username = UserName }}} = State ) ->
    % POLICY NOTE: we explicitly prevent a user who is an admin from self-deletion from the org.
    % If the user wants to disassociate from the org, it is a two step process: remove self from
    % admins group, then remove self from the org. Error response reflects this.
    case oc_chef_wm_base:user_in_group(State, UserName, <<"admins">>) of
        true ->
            Text = iolist_to_binary(["Please remove ", UserName, " from this organization's admins group before removing him or her from the organization."]),
            Message = {[{<<"error">>, Text}]},
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 403}, Req1, State#base_state{log_msg = admin_user_removal_not_permitted}};
        false ->
            remove_user_from_org(Req, State)
    end.

remove_user_from_org(Req, #base_state{organization_guid = OrgId,
                                      chef_db_context = DbContext,
                                      requestor_id = RequestorId,
                                      resource_state = #association_state{user = #chef_user{id = UserId}}} = State ) ->
    % From this point forward the user is not a member of the org - even if subsequent
    % steps in remove_user fail.
    case oc_chef_object_db:safe_delete(DbContext,
                                       #oc_chef_org_user_association{org_id = OrgId, user_id = UserId},
                                       RequestorId) of
        ok ->
            deprovision_user(Req, State);
        not_found ->
            % Because we pre-checked membership, this would only occur in a race condition.
            {{halt, 404}, Req, State#base_state{log_msg = association_not_found}};
        {error, What} ->
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

deprovision_user(Req, #base_state{organization_name = OrgName,
                             resource_state = #association_state{ user = #chef_user{username = UserName} = User}  } = State ) ->

    RequestorId = oc_chef_authz:superuser_id(),
    case oc_chef_associations:deprovision_removed_user(State, User, RequestorId) of
        ok ->
            EJ = chef_user:assemble_user_ejson(User, OrgName),
            {true, chef_wm_util:set_json_body(Req, EJ), State#base_state{log_msg = {removed, UserName, from, OrgName}}};
        {warning, Warnings} ->
            lager:error("Warnings in deprovision of ~p from ~p: ~p", [UserName, OrgName, Warnings]),
            EJ = chef_user:assemble_user_ejson(User, OrgName),
            {true, chef_wm_util:set_json_body(Req, EJ), State#base_state{log_msg = {warning_in_deprovision, Warnings}}};
        {error, Error} ->
            lager:error("Error in deprovision of ~p from ~p: ~p", [UserName, OrgName, Error]),
            {{halt, 500}, Req, State#base_state{log_msg = {error_in_deprovision, Error}}}
     end.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).
