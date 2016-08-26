%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@chef.io>
%% Copyright 2014 Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_wm_acl).

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

-export([
         allowed_methods/2,
         to_json/2,
         check_acl_auth/2
        ]).



%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).

%% Other
-export([% Also used by oc_chef_wm_acl_permission:
         validate_authz_id/6]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(Config) ->
    AclType = ?gv(acl_object_type, Config),
    {ok, #acl_state{type = AclType}}.

request_type() ->
    "acl".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('GET', Req, #base_state{chef_db_context = DbContext,
                                         organization_guid = OrgId,
                                         resource_state = #acl_state{type = Type} =
                                             AclState} = State) ->
    validate_authz_id(Req, State, AclState, Type, OrgId, DbContext).

auth_info(Req, State) ->
    check_acl_auth(Req, State).

to_json(Req, #base_state{resource_state = #acl_state{type = Type, authz_id = AuthzId}} = State) ->
    Granular = case wrq:get_qs_value("detail", Req) of
                   "granular" -> granular;
                   _ -> undefined
               end,
    case oc_chef_authz_acl:fetch(Type, AuthzId, Granular) of
        forbidden ->
            {{halt, 403}, Req, State#base_state{log_msg = requestor_access_failed}};
        Ejson ->
            Json = chef_json:encode(Ejson),
            {Json, Req, State}
    end.

%% Also used by oc_chef_wm_acl_permission
validate_authz_id(Req,
                  #base_state{organization_authz_id = AuthzId} = State,
                  AclState, organization, _OrgId, _DbContext) ->
    AclState1 = AclState#acl_state{authz_id = AuthzId},
    {Req, State#base_state{resource_state = AclState1, superuser_bypasses_checks = true}};
validate_authz_id(Req, State, AclState, Type, OrgId, DbContext) ->
    Name = chef_wm_util:object_name(Type, Req),
    AuthzId = case Type of
                  cookbook ->
                      oc_chef_authz_acl:fetch_cookbook_id(DbContext, Name, OrgId);
                  NotCookbook ->
                      oc_chef_authz_acl:fetch_id(NotCookbook, DbContext, Name, OrgId)
              end,
    AclState1 = AclState#acl_state{authz_id = AuthzId},
    % We're doing our own checks, so don't fail on superuser (also, not
    % doing this results in spurious auth failures):
    {Req, State#base_state{resource_state = AclState1, superuser_bypasses_checks = true}}.

check_acl_auth(Req, #base_state{requestor_id = RequestorId,
                                resource_state = #acl_state{type = Type,
                                                            authz_id = AuthzId}} = State) ->
    case AuthzId of
        not_found ->
            Name = chef_wm_util:object_name(Type, Req),
            Message = chef_wm_util:not_found_message(Type, Name),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = acl_not_found}};
        Id ->
            Result = oc_chef_authz_acl:has_grant_on(Type, Id, RequestorId),
            case Result of
                true ->
                    {authorized, Req, State};
                false ->
                    {{halt, 403}, Req, State};
                Other ->
                    Other
            end
    end.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
