%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@chef.io>
%% Copyright 2014-2018 Chef Software, Inc.
%%
%% This module handles the following
%%
%% GET    /organizations/:orgname/association_requests
%% POST   /organizations/:orgname/association_requests
%% DELETE /organizations/:orgname/association_requests/:id

-module(oc_chef_wm_org_invites).

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

%% chef_wm behavior callbacks
-behaviour(chef_wm).

-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         create_path/2,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         resource_exists/2,
         to_json/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #association_state{}}.

request_type() ->
    "invite".

allowed_methods(Req, #base_state{resource_args = invitations} = State) ->
    {['GET', 'POST'], Req, State};
allowed_methods(Req, State) ->
    {['DELETE'], Req, State}.

validate_request('DELETE', Req, #base_state{chef_db_context = DbContext} = State) ->
    Id = chef_wm_util:object_name(invitation, Req),
    Invitation = chef_db:fetch(#oc_chef_org_user_invite{id = Id}, DbContext),
    {Req, State#base_state{resource_state = #association_state{ org_user_invite = Invitation } }};
validate_request('GET', Req, State) ->
    {Req, State};
validate_request('POST', Req, #base_state{chef_db_context = DbContext} = State) ->
    case wrq:req_body(Req) of
        undefined ->
          throw({error, missing_body});
    Body ->
        {ok, EJ} =   oc_chef_org_user_invite:parse_binary_json(Body, create),
        UserName = ej:get({<<"user">>}, EJ),
        User = chef_db:fetch(#chef_user{username = UserName}, DbContext),
        {Req, State#base_state{resource_state = #association_state{user = User,
                                                                   user_name = UserName,
                                                                   data = EJ}}}
    end.

-spec auth_info(wm_req(), chef_wm:base_state()) ->
                       chef_wm:auth_info_return().
auth_info(Req, #base_state{
                  resource_state = #association_state{
                                      org_user_invite = not_found
                                     }
                 } = State) ->
    Id = chef_wm_util:object_name(invitation, Req),
    Message = chef_wm_util:not_found_message(invitation, Id),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 404}, Req1, State#base_state{log_msg = user_not_found}};
auth_info(Req, #base_state{organization_name = OrgName,
                           organization_authz_id = OrgAuthzId,
                           resource_state = #association_state{org_user_invite = #oc_chef_org_user_invite{org_name = OrgName}}} = State) ->
    %% org name matches - any valid op on a specific existing invite requires org update
    {{object, OrgAuthzId, update}, Req, State};
auth_info(Req, #base_state{organization_name = OrgName,
                           resource_state = #association_state{org_user_invite = #oc_chef_org_user_invite{org_name = BadOrgName}}} = State) ->
    Message = org_name_mismatch_message(OrgName, BadOrgName),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 400}, Req1, State#base_state{log_msg = {org_name_mismatch, OrgName, BadOrgName}}};
auth_info(Req, #base_state{resource_state = #association_state{user = not_found,
                                                               user_name = UserName}} = State) ->
    Message = chef_wm_util:not_found_message(user, UserName),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 404}, Req1, State#base_state{log_msg = user_not_found}};
auth_info(Req, #base_state{organization_authz_id = OrgAuthzId} = State) ->
    case wrq:method(Req) of
        'GET' ->
            {{object, OrgAuthzId}, Req, State};
         _ ->
            %% Any other method will change org attributes
            %% and requires update.
            {{object, OrgAuthzId, update}, Req, State}
    end.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, State) ->
    {"invalid_path", Req, State}.


to_json(Req, #base_state{ organization_guid = OrgId, chef_db_context = DbContext } = State) ->
    case chef_db:list(#oc_chef_org_user_invite{org_id = OrgId}, DbContext) of
        Invitations when is_list(Invitations)->
            EJson = oc_chef_org_user_invite:ejson_from_list(Invitations, <<"username">>),
            {chef_json:encode(EJson), Req, State};
        Error ->
            {{halt, 500}, Req, State#base_state{log_msg = Error }}
    end.

from_json(Req, #base_state{organization_name = OrgName,
                           chef_db_context = DbContext,
                           resource_state = #association_state{user = #chef_user{username = UserName} }} = State) ->
    case chef_db:is_user_in_org(DbContext, UserName, OrgName) of
        true ->
            oc_chef_associations:wm_conflict_response(Req, State, user_already_in_org, UserName);
        false ->
            % But the invitation itself may also exist, we'll capture that via the normal route...
            create_association(Req, State)
    end.

create_association(Req, #base_state{organization_guid = OrgId,
                                    chef_db_context = DbContext,
                                    requestor_id = RequestorId,
                                    server_api_version = ApiVersion,
                                    resource_state = #association_state{user = #chef_user{id = UserId,
                                                                                          username = UserName},
                                                                        data = ReqData}} = State) ->
    ObjectRec = chef_object:new_record(oc_chef_org_user_invite, ApiVersion, OrgId,
                                       UserId, ReqData),
    case chef_db:create(ObjectRec, DbContext, RequestorId) of
        %% TODO further research - is this response possible and testable?
        %% If the user is in the org, we can't issue the invite. Perhaps
        %% via race condition involving a POST to force-add and POST to creat invite?
        {conflict, _} ->
            oc_chef_associations:wm_conflict_response(Req, State, user_already_invited, UserName);
        ok ->
            invitation_response(Req, State, ObjectRec);
        What ->
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

delete_resource(Req, #base_state{ chef_db_context = DbContext,
                                  requestor_id = RequestorId,
                                  resource_state = #association_state{ org_user_invite = Invite} }= State) ->
    case oc_chef_object_db:safe_delete(DbContext, Invite, RequestorId) of
        ok ->
            EJ= oc_chef_org_user_invite:to_ejson(Invite),
            {true, chef_wm_util:set_json_body(Req, EJ), State#base_state{log_msg = {invite_deleted, Invite#oc_chef_org_user_invite.id}}};
        not_found ->
            % Note that not_found is not possible except in race
            % condition during concurrent delete
            {{halt, 404}, Req, State#base_state{log_msg = invitation_not_found}};
        {error, What} ->
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

% This function will construct the following document and update location url:
% {
%     "uri" : "...",
%     "organization_user" : { "username" : RequestorUserName},
%     "organization" : { "name" , OrgName }
%     "user" : { "email" : Email, "first_name" : FirstName  }
% }
% Note: it is technically possible for a #chef_client to make the request which would fail on
% match of #chef_user - but only really screwed up permissions would let them get this far...
% so let's let that crash
invitation_response(Req, #base_state{ organization_name = OrgName,
                           requestor = #chef_requestor{type = <<"user">>, name = RequestorUserName},
                           resource_state = #association_state{user = User}} = State, ObjectRec) ->
    TypeName = chef_object:type_name(ObjectRec),
    Uri = oc_chef_wm_routes:route(TypeName, Req, [{name, ObjectRec#oc_chef_org_user_invite.id}]),
    OrgUser = {<<"organization_user">>,  {[{<<"username">>, RequestorUserName}]} },
    Org = {<<"organization">>,  {[{<<"name">>, OrgName}]} },
    UserOut = {<<"user">>,  {[{<<"email">>, User#chef_user.email},
                                {<<"first_name">>, chef_user:serialized_field_value(first_name, User)}]} },

    LogMsg = {invited, User#chef_user.username, into, OrgName},
    Req1 = chef_wm_util:set_uri_of_created_resource(Uri, Req),
    Req2 = chef_wm_util:set_json_body(Req1, {[{<<"uri">>, Uri}, OrgUser, Org, UserOut]}),
    {true, Req2, State#base_state{log_msg = LogMsg}}.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).

org_name_mismatch_message(OrgName, BadOrgName) ->
    {[{<<"error">>, iolist_to_binary(["Organization ", OrgName, " does not match ", BadOrgName, " in association request"])}]}.
