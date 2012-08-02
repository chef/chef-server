%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(chef_wm_roles).

-include("chef_wm.hrl").

-mixin([{?BASE_RESOURCE, [malformed_request/2,
                          service_available/2,
                          is_authorized/2,
                          content_types_provided/2,
                          ping/2,
                          finish_request/2,
                          forbidden/2]}]).

%% chef_wm behavior callbacks
-export([request_type/0,
         validate_request/3,
         malformed_request_message/3,
         auth_info/2]).

-export([
         init/1,
         allowed_methods/2,
         content_types_accepted/2,
         create_path/2,
         from_json/2,
         post_is_create/2,
         resource_exists/2,
         to_json/2
        ]).

-behaviour(chef_wm).

init(Config) ->
    ?BASE_RESOURCE:init(?MODULE, Config).

request_type() ->
    "roles".

%% Memoize the container id so we don't hammer the database
auth_info(Req, #base_state{chef_authz_context = AuthzContext,
                            resource_state = RoleState,
                            organization_guid=OrgId}=State) ->
    ContainerId = chef_authz:get_container_aid_for_object(AuthzContext, OrgId, role),
    RoleState1 = RoleState#role_state{role_container_id=ContainerId},
    {container, ContainerId, Req, State#base_state{resource_state=RoleState1}}.

allowed_methods(Req, State) ->
    {['GET','POST'], Req, State}.

validate_request('GET', Req, State) ->
    {Req, State#base_state{resource_state = #role_state{}}};
validate_request('POST', Req, State) ->
    Body = wrq:req_body(Req),
    {ok, Role} = chef_role:parse_binary_json(Body, create),
    {Req, State#base_state{resource_state = #role_state{role_data = Role}}}.

resource_exists(Req, State) ->
    {true, Req, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #role_state{role_data = RoleData}}=State) ->
    Name = ej:get({<<"name">>}, RoleData),
    {binary_to_list(Name), Req, State}.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

from_json(Req, #base_state{resource_state =
                               #role_state{role_data = RoleData,
                                           role_container_id = ContainerId}} = State) ->
    ?BASE_RESOURCE:create_from_json(Req, State, chef_role, ContainerId, RoleData).

to_json(Req, State) ->
    {all_roles_json(Req, State), Req, State}.

%% @doc Generate a JSON string for a hash of rolename => role URI
%% pairs.
%% @end
%%
%% TODO: try to extract this to a common function, as this pattern
%% pops up with a few other endpoints, too
all_roles_json(Req, #base_state{chef_db_context = DbContext,
                                organization_name = OrgName}) ->
    RoleNames = chef_db:fetch_roles(DbContext, OrgName),
    RouteFun = oc_chef_wm_routes:bulk_route_fun(role, Req),
    UriMap= [{Name, RouteFun(Name)} || Name <- RoleNames],
    ejson:encode({UriMap}).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
