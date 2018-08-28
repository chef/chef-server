%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013-2018 Chef Software, Inc.

-module(oc_chef_wm_named_container).

-include("oc_chef_wm.hrl").

%% Webmachine callbacks
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
         to_json/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).


init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #container_state{}}.

request_type() ->
    "containers".

allowed_methods(Req, State) ->
    {['GET', 'DELETE'], Req, State}.

validate_request(Method, Req, State) when Method == 'GET';
                                          Method == 'DELETE' ->
    {Req, State}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           resource_state = ContainerState,
                           organization_guid = OrgId} =State) ->
    ContainerName = chef_wm_util:extract_from_path(container_name, Req),
    case chef_db:fetch(#oc_chef_container{org_id = OrgId, name = ContainerName}, DbContext) of
        not_found ->
            Message = chef_wm_util:error_message_envelope(iolist_to_binary(["Cannot load container ",
                                                                            ContainerName])),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = container_not_found}};
        #oc_chef_container{authz_id = AuthzId} = Container ->
            ContainerState1 = ContainerState#container_state{oc_chef_container = Container},
            State1 = State#base_state{resource_state = ContainerState1},
            {{container_id, AuthzId}, Req, State1}
    end.

resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, #base_state{resource_state = #container_state{
                                             oc_chef_container = Container
                                            }} = State) ->
    Ejson = oc_chef_container:assemble_container_ejson(Container),
    Json = chef_json:encode(Ejson),
    {Json, Req, State}.

from_json(Req, #base_state{resource_state = #container_state{
                                               oc_chef_container = Container,
                                               container_data = ContainerData
                                              }
                          } = State) ->
    oc_chef_wm_base:update_from_json(Req, State, Container, ContainerData).

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #container_state{
                                                     oc_chef_container = Container}
                                } = State) ->
    ok = oc_chef_wm_base:delete_object(DbContext, Container, RequestorId),
    Ejson = oc_chef_container:assemble_container_ejson(Container),
    {true, chef_wm_util:set_json_body(Req, Ejson), State}.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).
