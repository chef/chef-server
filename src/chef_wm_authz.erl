%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


%% Authz logic and helpers for chef_wm endpoints
%%

-module(chef_wm_authz).

-export([
         all_but_validators/1,
         allow_admin/1,
         allow_admin_or_requesting_client/2,
         allow_admin_or_requesting_node/2,
         is_admin/1,
         is_requesting_client/2,
         is_requesting_node/2,
         is_validator/1,
         use_custom_acls/4]).

-include("chef_wm.hrl").

%% @doc Reject validator clients, but allow all other requestors.
all_but_validators(#chef_client{validator=true}) -> forbidden;
all_but_validators(#chef_client{})               -> authorized;
all_but_validators(#chef_user{})                 -> authorized.

%% @doc Only authorize admin requestors.
%%
%% Technically, it is possible for a client to be both validator and admin... this should
%% probably be disallowed.
allow_admin(#chef_client{validator = true}) -> forbidden;
allow_admin(#chef_client{admin = true})     -> authorized;
allow_admin(#chef_client{})                 -> forbidden;
allow_admin(#chef_user{admin = true})       -> authorized;
allow_admin(#chef_user{})                   -> forbidden.

%% @doc Admins can do what they wish, but other requestors can only proceed if they are
%% operating on themselves.
%%
%% Validators can only create clients, get info on itself, and update itself. It cannot do
%% anything else, including operating on node resources.
-spec allow_admin_or_requesting_node(#chef_client{} | #chef_user{}, binary()) -> authorized | forbidden.
allow_admin_or_requesting_node(#chef_client{validator = true}, _Name) -> forbidden;
allow_admin_or_requesting_node(#chef_client{name = Name}, Name)       -> authorized;
allow_admin_or_requesting_node(#chef_client{} = Client, _Name)        -> allow_admin(Client);
allow_admin_or_requesting_node(#chef_user{username = Name}, Name)     -> authorized;
allow_admin_or_requesting_node(#chef_user{} = User, _Name)            -> allow_admin(User).

%% @doc Admins can do what they wish, but other requestors can only proceed if they are
%% operating on themselves.
%%
%% Validators can only create clients, get info on itself, and update itself. It cannot do
%% anything else, including operating on node resources.
%%
%% Forbids non-admin users with the same name as the client
-spec allow_admin_or_requesting_client(#chef_client{} | #chef_user{}, binary()) -> authorized | forbidden.
allow_admin_or_requesting_client(#chef_client{name = Name}, Name)       -> authorized;
allow_admin_or_requesting_client(#chef_client{validator = true}, _Name) -> forbidden;
allow_admin_or_requesting_client(#chef_client{} = Client, _Name)        -> allow_admin(Client);
allow_admin_or_requesting_client(#chef_user{} = User, _Name)            -> allow_admin(User).

is_admin(#chef_client{admin = true}) -> true;
is_admin(#chef_client{})             -> false;
is_admin(#chef_user{admin = true})   -> true;
is_admin(#chef_user{})               -> false.

%% @doc Is the requestor operating on itself?
-spec is_requesting_node(#chef_client{} | #chef_user{}, binary()) -> true | false.
is_requesting_node(#chef_client{name = Name}, Name)   -> true;
is_requesting_node(#chef_client{}, _Name)             -> false;
is_requesting_node(#chef_user{username = Name}, Name) -> true;
is_requesting_node(#chef_user{}, _Name)               -> false.

%% Specifically does not allow users with the same name as the client
-spec is_requesting_client(#chef_client{} | #chef_user{}, binary()) -> true | false.
is_requesting_client(#chef_client{name = Name}, Name)   -> true;
is_requesting_client(_, _)                              -> false.

is_validator(#chef_client{validator = true}) -> true;
is_validator(#chef_client{})                 -> false;
is_validator(#chef_user{})                   -> false.

-spec use_custom_acls(Endpoint :: atom(),
                      Auth :: {object, object_id()} |
                              {container, container_name()} | [auth_tuple()],
                      Req :: wrq:req(),
                      State :: #base_state{})
    -> {_, _, #base_state{}}.

%% Should customized ACLs be consulted to authorize this request, or should we just assume
%% Chef Server default ACLs?
%%
%% Custom ACLs refers to any system ACLs that you have modified in any way.  If you have
%% kept the default ACLs of a Chef Server intact (and intend to keep it that way), you can
%% opt for a bit of a performance boost by effectively "baking in" these defaults into
%% erchef, such that it will not interact with the authorization system to check the
%% permissions.
%%
%% If you wish to take advantage of this performance boost, there are a few configuration
%% parameters to be aware of.  Note that all are for the oc_chef_wm application.
%%
%%   custom_acls_cookbooks
%%   custom_acls_roles
%%   custom_acls_data
%%   custom_acls_depsolver
%%
%% as well as
%%
%%  custom_acls_always_for_modification
%%
%% For each of the first four that are either undefined OR set to true, the Chef Server will
%% issue requests to the authorization system for each request that is serviced by the
%% specified endpoint.
%%
%% If any of the first four parameters are set to FALSE, however, this indicates that the
%% default ACL rules should be assumed to be in effect for requests to that endpoint;
%% therefore, the authorization system will NOT be consulted, and you'll save an HTTP
%% request.  That is to say, you must declare explicitly to Chef that you will NOT be using
%% customized ACLs for the given endpoint.
%%
%% This can further be tuned using the custom_acls_always_for_modification parameter.  When
%% this is set to false, both read and write operations to a given endpoint will be governed
%% by the default ACL rules.  However, if it is set to true (or is undefined), then only
%% read operations are assumed to be governed by the defaults; all write operations will
%% still be actively checked against the authorization system.
%%
%% In summary, if you want your read operations to go a little faster, and you haven't
%% customized your ACLs, set custom_acls_ENDPOINT to false.  If you want to go further and
%% have your write operations bypass the authorization check as well, you must additionally
%% set custom_acls_always_for_modification to false.  Remember that you must have set
%% custom_acls_ENDPOINT to false for custom_acls_always_for_modification to have any effect.
%%
%% We differentiate between read and write operations because read operations dominate a
%% typical Chef Server workload.
%%
%% Remember, this does not disable authorization; it merely assumes that the default ACLs
%% are in place and responds accordingly without the additional overhead of making
%% additional HTTP requests to the internal authorization service.  If you modify your
%% system's ACLs from the default IN ANY WAY, you should NOT use these configuration
%% parameters, as your ACL customizations will be ignored.
%%
%% (Of course, if you have modified the ACLs relevant to one endpoint, you can just not set
%% the corresponding parameter; the authorization system will still be checked for all
%% requests to that endpoint, but can still be bypassed for others.  This is pretty advanced
%% stuff, though, so only even think about doing it if you know what you're doing.)
%%
%% Also, remember that all requests made by users, as opposed to clients, ALWAYS go through
%% the authorization system.  These configuration parameters ONLY affect requests made by
%% clients.  This is because users are not inherently associated with an organization like
%% clients are; the organization-association check (performed earlier) is what allows us to
%% take this shortcut.
%%
%% Note that the inputs (and outputs) of this function correspond to the valid return values
%% for the chef_wm:auth_info/2 callback
use_custom_acls(_Endpoint, Auth, Req, #base_state{requestor = #chef_user{} } = State) ->
    {Auth, Req, State};
use_custom_acls(Endpoint, Auth, Req, #base_state{requestor = #chef_client{} } = State) ->
    case application:get_env(oc_chef_wm, config_for(Endpoint)) of
        {ok, false} ->
            %% The server is configured as though we have NOT customized any ACLs relevant
            %% for the current endpoint (i.e. they remain unchanged from the defaults).  Now
            %% we just need to determine if the request is a read or write operation, and
            %% (if it is write) if we are still consulting custom ACLs.
            {customize_for_modification_maybe(Endpoint, wrq:method(Req), Auth), Req, State};
        _Else -> %% consult the authorization system
            {Auth, Req, State}
    end.

%% Allow the option to contact opscode-authz with custom acls for all create,update,delete
%% requests. When 'custom_acls_always_for_modification' is set to true the
%% 'custom_acls_FOO' flags only apply to the GET method.
%%
%% Controlled by the config variable {oc_chef_wm, custom_acls_always_for_modification}
customize_for_modification_maybe(Endpoint, Method, Auth) ->
    %% If the current request is modifying the system, and we are configured to always use
    %% customized ACLs when servicing modification requests, defer to the authorization
    %% system to determine if this request should be authorized (i.e., allow the original
    %% `Auth' value to be used).  Otherwise, short-circuit and declare unilaterally that the
    %% request is authorized.
    case is_modification(Endpoint, Method) of
        true ->
            case auth_for_modification() of
                true ->
                    Auth;
                false ->
                    authorized
            end;
        false ->
            %% It's not a modification request; allow it.
            authorized
    end.

%% @doc Determine the configuration parmeter that corresponds to the given endpoint.
config_for(cookbooks) -> custom_acls_cookbooks;
config_for(roles)     -> custom_acls_roles;
config_for(data)      -> custom_acls_data;
config_for(depsolver) -> custom_acls_depsolver.

%% @doc Is the current request one that modifies the system, as determined by the endpoint
%% and HTTP verb being used?
is_modification(depsolver, 'POST')  -> false;
is_modification(_Endpoint, 'GET')   -> false;
is_modification(_Endpoint, 'HEAD')  -> false;
is_modification(_Endpoint, _Method) -> true.

%% @doc Determine if we should use the authorization system for modification requests, or if
%% we should just follow the default ACL rules.  We only skip the check if [oc_chef_wm
%% custom_acls_always_for_modification] is explicitly set to false; undefined is the same as
%% true.
auth_for_modification() ->
    case application:get_env(oc_chef_wm, custom_acls_always_for_modification) of
        {ok, Value} ->
            Value =:= true;
        _Else ->
           true
    end.
