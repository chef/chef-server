%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@getchef.com>
%% Copyright 2014 Chef, Inc. All Rights Reserved.
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

-include_lib("chef_wm/include/chef_wm.hrl").
-include_lib("oc_chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2,
                        post_is_create/2]}]).

-mixin([{?BASE_RESOURCE, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

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

-export([
         allowed_methods/2,
         from_json/2,
         to_json/2
        ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(Config) ->
    io:format("~n---init~n", []),
    AclType = ?gv(acl_object_type, Config),
    {ok, #acl_state{type = AclType}}.

request_type() ->
    "acl".

allowed_methods(Req, State) ->
    {['GET', 'PUT'], Req, State}.

validate_request(Method, Req, #base_state{chef_db_context = DbContext,
                                          requestor_id = RequestorId,
                                          organization_guid = OrgId,
                                          organization_name = OrgName,
                                          resource_state = #acl_state{type = Type} = 
                                              AclState} = State) when Method == 'GET';
                                                                      Method == 'PUT' ->
    io:format("---val ~p~n", [Method]),
    Name = chef_wm_util:object_name(Type, Req),
    io:format("---val got name: ~p~n", [Name]),
    try
        AuthzId = case Type of
                      group ->
                          fetch_group_id(DbContext, Name, OrgId, RequestorId);
                      cookbook ->
                          fetch_cookbook_id(DbContext, Name, OrgName);
                      Other ->
                          fetch_id(Other, DbContext, Name, OrgId)
                  end,
        io:format("---val got authzId: ~p~n", [AuthzId]),
        AclState1 = AclState#acl_state{authz_id = AuthzId},
        {Req, State#base_state{resource_state = AclState1}}
    catch
        throw:{not_found, Name} ->
            Message = chef_wm_util:not_found_message(Type, Name),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = user_not_found}};
        throw:{forbidden, Name} ->
            % TODO: do a proper forbidden error message
            {{halt, 403}, Req, State}
    end.

% TODO: don't like this; we only need the authz id, so grabbing complete objects
% is wastefull (particularly in the case of groups, where I believe all sorts of
% extra work is done for the subgroups and actors).  Also, this might be more
% suited to be moved to oc_chef_wm_util or something
fetch_id(user, DbContext, Name, _OrgId) ->
    case chef_db:fetch(#chef_user{username = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_user{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(client, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_client{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_client{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(container, DbContext, Name, OrgId) ->
    case chef_db:fetch(#oc_chef_container{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #oc_chef_container{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(data_bag, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_data_bag{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_data_bag{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(node, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_node{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_node{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(role, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_node{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_node{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(environment, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_environment{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_environment{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(organization, _DbContext, _Name, _OrgId) ->
    % TODO: implement; this is different than all the others
    error(not_implemented).

% TODO: Don't like this, either, bespoke code
fetch_group_id(DbContext, Name, OrgId, RequestorId) ->
    % TODO: either pass the requestor, or do this somewhere else
    % nothing else will work until this does: needed for setting up the tests in pedant
    % I think this will fail for some cases, pretty sure superuser can be removed
    % from group ACL, but doing this for the moment until decision made
    case chef_db:fetch(#oc_chef_group{org_id = OrgId, name = Name,
                                      for_requestor_id = RequestorId}, DbContext) of
        not_found ->
            throw({not_found, Name});
        forbidden ->
            throw({forbidden, Name});
        #oc_chef_group{authz_id = AuthzId} ->
            AuthzId
    end.

% TODO: and more bespoke code; cookbooks retrieval is by orgname instead of ID
fetch_cookbook_id(DbContext, Name, OrgName) ->
    % cookbook endpoint pattern is utterly different from the others, generic
    % doen not handle cookbooks (and, well, versioning)
    case chef_db:fetch_latest_cookbook_version(DbContext, OrgName, Name) of
        not_found ->
            throw({not_found, Name});
        {cookbook_exists, AuthzId} ->
            % unclear when this can happen; I assume for wrong version but won't
            % happen with 'latest' version?   But still checking for it here.
            AuthzId;
        #chef_cookbook_version{authz_id = AuthzId} ->
            AuthzId
    end.

auth_info(Req, State) ->
    io:format("---auth info~n", []),
    % Don't fail yet; we only want to fetch the information once, so we'll check for
    % permission when we return/update the data
    % TODO: do we want to do it that way, or do we want to fetch the data now, check
    % and save it in the state instead?
    {authorized, Req, State}.

from_json(Req, State) ->
    io:format("---from json~n", []),
    % TODO parse and update
    {true, Req, State}.

to_json(Req, State) ->
    io:format("---to json~n", []),
    % TODO return data
    {chef_json:encode({[]}), Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
