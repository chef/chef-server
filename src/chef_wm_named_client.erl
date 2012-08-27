%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(chef_wm_named_client).

-include("chef_wm.hrl").

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
         delete_resource/2,
         from_json/2,
         to_json/2
       ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #client_state{}}.

request_type() ->
  "data".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request(Method, Req, #base_state{chef_db_context = DbContext,
                                         organization_name = OrgName,
                                         resource_state = ClientState} = State) ->
    Name = chef_wm_util:object_name(client, Req),
    OldClient = chef_db:fetch_client(DbContext, OrgName, Name),
    case OldClient of
        not_found ->
            throw({client_not_found, Name});
        _ ->
            case Method of
                'PUT' ->
                    Body = wrq:req_body(Req),
                    {ok, Client} = chef_client:parse_binary_json(Body, Name, OldClient),
                    {Req,
                     State#base_state{resource_state =
                                          ClientState#client_state{client_data = Client,
                                                                   chef_client =
                                                                       OldClient}}};
                _ ->
                    {Req, State#base_state{resource_state =
                                               ClientState#client_state{chef_client =
                                                                            OldClient}}}
            end
    end.

auth_info(Req, #base_state{resource_state =
                               #client_state{chef_client =
                                                 #chef_client{authz_id = AuthzId} =
                                                 Client} = ClientState} = State) ->
    ClientState1 = ClientState#client_state{chef_client = Client},
    State1 = State#base_state{resource_state = ClientState1},
    {{object, AuthzId}, Req, State1}.

from_json(Req, #base_state{reqid = RequestId,
                           resource_state =
                               #client_state{chef_client =
                                                 #chef_client{name = ReqName} = Client,
                                             client_data = ClientData}} =
              State) ->
    Name = ej:get({<<"name">>}, ClientData),
    % Check to see if we need to generate a new key
    ClientData1 = case ej:get({<<"private_key">>}, ClientData) of
                      true ->
                          {PublicKey, PrivateKey} = chef_wm_util:generate_keypair(Name, RequestId),
                          chef_client:set_public_key(ClientData, PublicKey);
                      _ ->
                          PrivateKey = undefined,
                          ClientData
                  end,
    {Result, Req1, State1} = chef_wm_base:update_from_json(Req, State,
                                                           Client, ClientData1),
    case Result of
        {halt, _} ->
            % There was a problem with the update; abort! abort!
            {Result, Req1, State1};
        _ ->
            % This is for returning the private key, but needs to happen after update
            Req2 = case PrivateKey of
                       undefined ->
                           Req1;
                       _ ->
                           chef_wm_util:append_field_to_json_body(Req1,
                                                                  <<"private_key">>,
                                                                  PrivateKey)
                   end,
            % Need to return uri (in case request is a rename?)
            Uri = ?BASE_ROUTES:route(client, Req1, [{name, Name}]),
            Req3 = chef_wm_util:append_field_to_json_body(Req2, <<"uri">>, Uri),
            FinalReq = set_http_response_code(Name, ReqName, Uri, Req3),
            {Result, FinalReq, State1}
    end.

set_http_response_code(Name, ReqName, Uri, Request) ->
    % If this is a rename, we need to add the Location header so it returns 201
    case ReqName of
        Name ->
            Request;
        _ ->
            wrq:set_resp_header("Location", binary_to_list(Uri), Request)
    end.

to_json(Req, #base_state{resource_state =
                             #client_state{chef_client = Client},
                         organization_name = OrgName} = State) ->
    EJson = chef_client:assemble_client_ejson(Client, OrgName),
    Json = ejson:encode(EJson),
    {Json, Req, State}.

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #client_state{
                                   chef_client = Client},
                                 organization_name = OrgName} = State) ->
    ok = chef_object_db:delete(DbContext, Client, RequestorId),
    EJson = chef_client:assemble_client_ejson(Client, OrgName),
    Req1 = chef_wm_util:set_json_body(Req, EJson),
    {true, Req1, State}.

% TODO: this could stand refactoring: I'm sure there is stuff re-used by other
% endpoints and possibly unused code here
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

