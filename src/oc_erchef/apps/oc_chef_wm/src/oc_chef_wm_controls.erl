%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Dave Parfitt <dparfitt@chef.io>
%% Copyright 2014 Chef Software, Inc. All Rights Reserved.

-module(oc_chef_wm_controls).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           {list_objects_json/2, to_json},
                           forbidden/2,
                           is_authorized/2]}]).

-export([allowed_methods/2,
         create_path/2,
         from_json/2,
         resource_exists/2,
         service_available/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).


-define(CONTROL_GROUP_SCHEMA,
        {[ {<<"control_groups">>, array},
           {<<"node_name">>, string},
           {<<"run_id">>, string}
         ]}).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #control_state{}}.

service_available(Req, State) ->
    case oc_chef_wm_base:service_available(Req, State) of
        {true, Req1, State1} ->
            ActionsEnabled = envy:get(oc_chef_wm, enable_actions, false, boolean),
            case ActionsEnabled of
                true -> {true, Req1, State1};
                false ->
                    Msg = <<"Resource gone, analytics unavailable in the current server configuration">>,
                    Req2 = wrq:set_resp_header("Content-type", "application/json", Req1),
                    Req3 = wrq:set_resp_body(chef_json:encode({[{<<"error">>, Msg}]}), Req2),
                    {{halt, 410}, Req3, State1#base_state{log_msg = rabbitmq_disabled}}
            end;
        Other ->
          Other
    end.

request_type() ->
    "controls".

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('POST', Req, #base_state{resource_state = ControlState} = State) ->
    Body = wrq:req_body(Req),
    Data = chef_json:decode(Body),
    case ej:valid(?CONTROL_GROUP_SCHEMA, Data) of
        ok ->
            % message is valid, stash it in resource_state/control_data
            {Req, State#base_state{resource_state =
                                     ControlState#control_state{control_data = Data}}};
        #ej_invalid{} = Error ->
             lager:error(": ~p", [Error]),
             throw(Error)
    end.


auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('POST', Req, #base_state{resource_state = #control_state{control_data = ControlData},
                                   requestor=#chef_requestor{type = <<"client">>, name = ClientName}} = State) ->
    % schema validates that node_name exist
    case ej:get({"node_name"}, ControlData) of
    ClientName ->
        {authorized, Req, State};
    _Else ->
        {{halt, 403}, Req, State}
    end;
auth_info('POST', Req, State) ->
    {{halt, 403}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #control_state{} = ControlGroupState}
                                                            = State) ->
    %% create the ID and stash it in the state
    %% This path isn't very useful, as we don't provide a way to GET a
    %% control_group once it's posted
    Id = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
    ControlGroupState1 = ControlGroupState#control_state{control_group_id=Id},
    {binary_to_list(Id), Req, State#base_state{resource_state=ControlGroupState1}}.

from_json(Req, #base_state{resource_state = #control_state{control_data = ControlData,
                                                           control_group_id=Id}} = State) ->
    RoutingKey = routing_key(),
    Msg = construct_payload(ControlData, Id, Req, State),
    publish(RoutingKey, Msg),
    %% return an empty response so knife raw doesn't throw an exception
    Out = chef_json:encode(""),
    Req2 = wrq:set_resp_body(Out, Req),
    {true, Req2, State}.

-spec construct_payload(FullControlGroupPayload :: {[{binary(), binary()}]},
                        Id   :: binary(),
                        Req :: wm_req(),
                        State :: #base_state{}) -> binary().
construct_payload(FullControlGroupPayload,
                  Id,
                  Req, #base_state{reqid = RequestId,
                                   organization_name = OrgName}
                                   ) ->
    MsgVersion = req_header("x-ops-audit-report-protocol-version", Req),
    {FullControlGroupNode} = FullControlGroupPayload,
    Msg = {[{<<"message_type">>, <<"control_groups">>},
            {<<"message_version">>, MsgVersion},
            {<<"organization_name">>, OrgName},
            {<<"chef_server_fqdn">>, hostname()},
            {<<"recorded_at">>, req_header("x-ops-timestamp", Req)},
            {<<"remote_hostname">>, req_header("x-forwarded-for", Req)},
            {<<"request_id">>, RequestId},
            {<<"id">>, Id}
              | FullControlGroupNode]},
    Msg1 = maybe_add_remote_request_id(Msg, req_header("x-remote-request-id", Req)),
    iolist_to_binary(chef_json:encode(Msg1)).

maybe_add_remote_request_id(Msg, undefined) ->
    Msg;
maybe_add_remote_request_id(Msg, RemoteRequestId) ->
    ej:set({<<"remote_request_id">>}, Msg, RemoteRequestId).

-spec hostname() -> binary().
hostname() ->
    envy:get(oc_chef_wm, actions_fqdn, binary).

req_header(Name, Req) ->
    case wrq:get_req_header(Name, Req) of
        undefined ->
            undefined;
        Header ->
            iolist_to_binary(Header)
    end.

-spec publish(RoutingKey :: binary(),
              Msg :: binary()) -> ok.
publish(RoutingKey, Msg)->
    oc_chef_action_queue:publish(RoutingKey, Msg).

-spec routing_key() -> binary().
routing_key() ->
    iolist_to_binary([<<"control_group">>]).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
