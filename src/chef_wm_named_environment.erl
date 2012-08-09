%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author John Keiser <jkeiser@opscode.com>
%% @author Douglas Triggs <doug@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(chef_wm_named_environment).

-include("chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2]}]).

-mixin([{?BASE_RESOURCE, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         resource_exists/2,
         to_json/2]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

request_type() ->
    "environments".

allowed_methods(Req, State) ->
    Methods = case chef_wm_util:object_name(environment, Req) of
                  <<"_default">> ->
                      ['GET'];
                  _EnvName ->
                      ['GET', 'PUT', 'DELETE']
              end,
    Msg = <<"The '_default' environment cannot be modified.">>,
    Req1 = chef_wm_util:set_json_body(Req, {[{<<"error">>, [Msg]}]}),
    {Methods, Req1, State#base_state{resource_state = #environment_state{}}}.

validate_request('PUT', Req, State) ->
    Body = wrq:req_body(Req),
    {ok, Environment} = chef_environment:parse_binary_json(Body),
    {Req, State#base_state{resource_state =
                               #environment_state{environment_data =
                                                      Environment}}};
validate_request(_Other, Req, State) ->
    {Req, State#base_state{resource_state = #environment_state{}}}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_name = OrgName,
                           resource_state = EnvironmentState} = State) ->
    Name = chef_wm_util:object_name(environment, Req),
    case chef_db:fetch_environment(DbContext, OrgName, Name) of
        not_found ->
            Message = chef_wm_util:not_found_message(environment, Name),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = environment_not_found}};
        #chef_environment{authz_id = AuthzId} = Environment ->
            %% check authz here
            EnvironmentState1 = EnvironmentState#environment_state{chef_environment =
                                                                       Environment},
            State1 = State#base_state{resource_state = EnvironmentState1},
            {{object, AuthzId}, Req, State1}
    end.

resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, #base_state{resource_state = #environment_state{
                           chef_environment = #chef_environment{
                             serialized_object = JSON}}} = State) ->
    {chef_db_compression:decompress(JSON), Req, State}.

from_json(Req, #base_state{resource_state =
                               #environment_state{chef_environment = Environment,
                                                  environment_data = EnvironmentData}} =
              State) ->
    chef_wm_base:update_from_json(Req, State, Environment, EnvironmentData).

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 resource_state = #environment_state{
                                     chef_environment = Environment},
                                 requestor = #chef_requestor{
                                     authz_id = RequestorId}} = State) ->

    ok = ?BASE_RESOURCE:delete_object(DbContext, Environment, RequestorId),
    Json = chef_db_compression:decompress(Environment#chef_environment.serialized_object),
    {true, wrq:set_resp_body(Json, Req), State}.

% TODO: this needs to be refactored; repeated from chef_wm_environments
error_message(Msg) when is_list(Msg) ->
    error_message(iolist_to_binary(Msg));
error_message(Msg) when is_binary(Msg) ->
    {[{<<"error">>, [Msg]}]}.

malformed_request_message(#ej_invalid{type = json_type, key = Key}, _Req, _State) ->
    case Key of
        undefined -> error_message([<<"Incorrect JSON type for request body">>]);
        _ ->error_message([<<"Incorrect JSON type for ">>, Key])
    end;
malformed_request_message(#ej_invalid{type = missing, key = Key}, _Req, _State) ->
    error_message([<<"Required value for ">>, Key, <<" is missing">>]);
malformed_request_message({invalid_key, Key}, _Req, _State) ->
    error_message([<<"Invalid key ">>, Key, <<" in request body">>]);
malformed_request_message(invalid_json_body, _Req, _State) ->
    error_message([<<"Incorrect JSON type for request body">>]);
malformed_request_message(#ej_invalid{type = exact, key = Key, msg = Expected},
                          _Req, _State) ->
    error_message([Key, <<" must equal ">>, Expected]);
malformed_request_message(#ej_invalid{type = string_match, msg = Error},
                          _Req, _State) ->
    error_message([Error]);
malformed_request_message(#ej_invalid{type = object_key, key = Object, found = Key},
                          _Req, _State) ->
    error_message([<<"Invalid key '">>, Key, <<"' for ">>, Object]);
% TODO: next two tests can get merged (hopefully) when object_map is extended not
% to swallow keys
malformed_request_message(#ej_invalid{type = object_value, key = Object, found = Val},
                          _Req, _State) when is_binary(Val) ->
    error_message([<<"Invalid value '">>, Val, <<"' for ">>, Object]);
malformed_request_message(#ej_invalid{type = object_value, key = Object, found = Val},
                          _Req, _State) ->
    error_message([<<"Invalid value '">>, io_lib:format("~p", [Val]),
                   <<"' for ">>, Object]);
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
