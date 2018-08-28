%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Marc Paradise <marc@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%


-module(oc_chef_wm_named_user).

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
                           service_available/2]}]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         to_json/2 ]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         conflict_message/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3,
         finalize_update_body/3 ]).


init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).
init_resource_state(_Config) ->
    {ok, #user_state{}}.

request_type() ->
  "users".

allowed_methods(Req, #base_state{resource_args = Args} = State) when Args == invitations;
                                                                     Args == invitation_count;
                                                                     Args == org_list ->
    {['GET'], Req, State};
allowed_methods(Req, #base_state{resource_args = invitation_response} = State) ->
    {['PUT'], Req, State};
allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request('PUT', Req, #base_state{resource_args = invitation_response} = State) ->
    validate_put_request_response(fun(Body, _User) ->
                                          oc_chef_org_user_invite:parse_binary_json(Body, response)
                                  end,
                                  Req, State);
validate_request('PUT', Req, #base_state{server_api_version = ApiVersion} = State) ->
    validate_put_request_response(fun(Body, User) ->
                                          chef_user:parse_binary_json(ApiVersion, Body, update, User)
                                  end,
                                  Req, State);
validate_request(_Method, Req, #base_state{chef_db_context = DbContext,
                                          resource_state = UserState} = State) ->
    User = fetch_user_data(DbContext, Req),
    UserState1 = UserState#user_state{chef_user = User},
    {Req, State#base_state{resource_state = UserState1}}.

validate_put_request_response(ParseBinaryJSON, Req, #base_state{chef_db_context = DbContext,
                                                                resource_state = UserState} = State) ->
    Body = wrq:req_body(Req),
    User = fetch_user_data(DbContext, Req),
    {ok, EJ} = ParseBinaryJSON(Body, User),
    UserState1 = UserState#user_state{chef_user = User, user_data = EJ},
    {Req, State#base_state{resource_state = UserState1}}.

fetch_user_data(DbContext, Req) ->
    UserName = chef_wm_util:object_name(user, Req),
    chef_db:fetch(#chef_user{username = UserName}, DbContext).

% No matter what we're doing next, we must have a user since we're the
% 'named user' endpoint - fail early if we don't.
auth_info(Req, #base_state{resource_state = #user_state{chef_user = not_found}} = State) ->
    UserName = chef_wm_util:object_name(user, Req),
    Message = chef_wm_util:not_found_message(user, UserName),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 404}, Req1, State#base_state{log_msg = user_not_found}};
auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info(_, Req, #base_state{resource_args = Args,
                              requestor_id = RequestorAuthzId,
                              resource_state = #user_state{chef_user = User}} = State) when Args == invitations;
                                                                                            Args == invitation_response;
                                                                                            Args == invitation_count ->
    #chef_user{authz_id = AuthzId} = User,
    Auth  = case RequestorAuthzId of
        AuthzId ->
            % In case permissions are in any way incorrect, don't require that the user
            % have any permissions to her own object - her identity is sufficent
            authorized;
        _ ->
            invite_access_for(Args, AuthzId)
    end,
    {Auth, Req, State};
auth_info(Method, Req, #base_state{resource_state = #user_state{chef_user = User} = UserState} = State) ->
    #chef_user{authz_id = AuthzId} =   User,
    {auth_type(Method, AuthzId, UserState), Req, State}.

invite_access_for(invitations, _) -> superuser_only;
invite_access_for(invitation_count, _) -> superuser_only;
invite_access_for(invitation_response, AuthzId) -> {actor, AuthzId, grant}.

auth_type('PUT', AuthzId, #user_state{user_data = UserData}) ->
    ExtId = ej:get({<<"external_authentication_uid">>}, UserData),
    Recovery = ej:get({<<"recovery_authentication_enabled">>}, UserData),
    case {ExtId, Recovery} of
        {undefined, undefined} ->
            {actor, AuthzId};
        _ ->
            superuser_only
    end;
auth_type(_, AuthzId, _State) ->
    {actor, AuthzId}.

from_json(Req, #base_state{resource_args = invitation_response,
                           chef_db_context = DbContext,
                           requestor_id = RequestorId,
                           resource_state = #user_state{ chef_user = User, user_data = EJ}} = State) ->
    Id = chef_wm_util:object_name(invitation, Req),
    case chef_db:fetch(#oc_chef_org_user_invite{id = Id}, DbContext) of
        not_found ->
            Message = chef_wm_util:not_found_message(invitation, Id),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = invite_not_found}};
        #oc_chef_org_user_invite{id = Id, org_id = OrgId, org_name = OrgName,
                                 last_updated_by = OriginalRequestorAuthzId} = Invitation ->
            Result = case ej:get({<<"response">>}, EJ) of
                <<"accept">> ->
                    NewState = State#base_state{organization_name = OrgName,
                                                organization_guid = OrgId,
                                                resource_state = #association_state{user = User, data = EJ}},
                    oc_chef_associations:wm_associate_user(Req, NewState, OriginalRequestorAuthzId);
                <<"reject">> ->
                    {true, Req, State#base_state{log_msg = {invite_rejected, Id}}}
            end,
            case Result of
                {{halt, 500}, _, _} ->
                    Result;
                {{halt, _}, _, _} ->
                    % Any other error means this invite is no good, so nuke it and preserve the response
                    % we've been given.
                    oc_chef_object_db:safe_delete(DbContext, Invitation,  RequestorId),
                    Result;
                {true, Req1, State1} ->
                    % A successful result means we give back our serialized invite record.
                    oc_chef_object_db:safe_delete(DbContext, Invitation,  RequestorId),
                    EJResponse = oc_chef_org_user_invite:to_ejson(Invitation),
                    {true, chef_wm_util:set_json_body(Req1, EJResponse), State1}
            end
    end;
from_json(Req, #base_state{server_api_version = ?API_v0,
                           resource_state = #user_state{ chef_user = User, user_data = UserData}} = State) ->
    oc_chef_wm_key_base:update_object_embedded_key_data_v0(Req, State, User, UserData);
from_json(Req, #base_state{resource_state = #user_state{ chef_user = User, user_data = UserData}} = State) ->
    % in v1+ keys may only be updated via the keys endpoint.
    oc_chef_wm_base:update_from_json(Req, State, User, UserData).

finalize_update_body(Req, _State, BodyEJ) ->
    %% Custom json body needed to maintain compatibility with opscode-account behavior.
    %% oc_chef_wm_base:update_from_json will reply with the complete object, but
    %% clients currently expect only a URI, and a private key if the key is new.
    NewName = chef_user:username_from_ejson(BodyEJ),
    Uri = oc_chef_wm_routes:route(user, Req, [{name, NewName}]),
    EJ1 = {[{<<"uri">>, Uri}]},

    % private_key will be set if we generated a new private_key, in which case
    % we need to supply it to the caller.
    case ej:get({<<"private_key">>}, BodyEJ) of
        undefined ->
            EJ1;
        Key ->
            ej:set({<<"private_key">>}, EJ1, Key)
     end.

to_json(Req, #base_state{resource_args = undefined,
                         resource_state = #user_state{chef_user = User},
                         organization_name = OrgName} = State) ->
    EJson = chef_user:assemble_user_ejson(User, OrgName),
    Json = chef_json:encode(EJson),
    {Json, Req, State};
to_json(Req, #base_state{ resource_args = Args,
                          resource_state = #user_state{chef_user = User },
                          chef_db_context = DbContext } = State) when Args == invitations;
                                                                      Args == invitation_count ->

    case chef_db:list(#oc_chef_org_user_invite{user_id = User#chef_user.id}, DbContext) of
        Invitations when is_list(Invitations) ->
            EJson = case Args of
                invitation_count ->
                    {[{<<"value">>, length(Invitations)}]};
                invitations ->
                    oc_chef_org_user_invite:ejson_from_list(Invitations, <<"orgname">>)
            end,
            {chef_json:encode(EJson), Req, State};
        Error ->
            {{halt, 500}, Req, State#base_state{log_msg = Error }}
    end;
to_json(Req, #base_state{ resource_args = org_list,
                          requestor = #chef_requestor{id = RequestorID},
                          resource_state = #user_state{chef_user = #chef_user{id = TargetUserID} },
                          chef_db_context = DbContext } = State) ->
    Result = case superuser_or_self(TargetUserID, RequestorID, Req) of
               true ->
                     chef_db:list(#oc_chef_org_user_association{user_id = TargetUserID}, DbContext);
               false ->
                     chef_db:list_common_orgs(TargetUserID, RequestorID, DbContext)
             end,
    case Result of
        Orgs when is_list(Orgs) ->
            {chef_json:encode(orgs_to_ej(Orgs)), Req, State};
        Error ->
            {{halt, 500}, Req, State#base_state{log_msg = Error}}
    end.

superuser_or_self(UserId, UserId, _Req) ->
    true;
superuser_or_self(_UserId, _RequestorID, Req) ->
    %% The is_superuser function only knows how to deal with usernames and how to
    %% pull them from request objects, so we use that instead of the RequestorID
    oc_chef_wm_base:is_superuser(Req).

orgs_to_ej(Orgs) ->
    [{[{<<"organization">>,
        {[{<<"name">>, ShortName},
          {<<"full_name">>, FullName},
          {<<"guid">>, Guid}]}}]} || [ShortName, FullName, Guid] <- Orgs ].

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #user_state{ chef_user = User},
                                 organization_name = OrgName } = State) ->
    ok = oc_chef_wm_base:delete_object(DbContext, User, RequestorId),
    EJson = chef_user:assemble_user_ejson(User, OrgName),
    Req1 = chef_wm_util:set_json_body(Req, EJson),
    {true, Req1, State}.



conflict_message(Name) ->
    Msg = iolist_to_binary([<<"User '">>, Name, <<"' already exists">>]),
    {[{<<"error">>, [Msg]}]}.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).
