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
    {ok, #data_state{}}.

request_type() ->
  "data".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request('PUT', Req, #base_state{chef_db_context = DbContext,
                                         organization_name = OrgName} = State) ->
    Body = wrq:req_body(Req),
    Name = chef_rest_util:object_name(client, Req),
    OldClient = chef_db:fetch_client(DbContext, OrgName, Name),
    case OldClient of
        not_found ->
            {ok, Client} = chef_client:parse_binary_json(Body, Name);
        _ ->
            {Previous} = chef_client:assemble_client_ejson(OldClient, OrgName),
            {ok, Client} = chef_client:parse_binary_json(Body, Name, Previous)
    end,
    {Req, State#base_state{resource_state = #client_state{client_data = Client}}};
validate_request(_Other, Req, State) ->
    {Req, State#base_state{resource_state = #client_state{}}}.

auth_info(Req, State) ->
    {{create_in_container, client}, Req, State}.

from_json(Req, #base_state{reqid = RequestId,
                           resource_state =
                               #client_state{chef_client =
                                                 #chef_client{name = Name} = Client,
                                             client_data = ClientData}} =
              State) ->
    % Check to see if we need to generate a new key
    ClientData1 = case ej:get({<<"private_key">>}, ClientData) of
                      true ->
                          {Cert, PrivateKey} = chef_cert_http:gen_cert(Name, RequestId),
                          ej:set({<<"certificate">>}, ClientData, Cert);
                      _ ->
                          PrivateKey = undefined,
                          ClientData
    end,
    {Result, Req1, State1} = chef_rest_wm:update_from_json(Req, State,
                                                           Client, ClientData1),
    % This is for returning the private key, but needs to happen after update
    Req2 = case PrivateKey of
               undefined ->
                   Req1;
               _ ->
                   chef_rest_util:append_field_to_json_body(Req1, <<"private_key">>,
                                                            PrivateKey)
           end,
    % Need to return uri (for no good reason except ruby endpoint does)
    Uri = chef_rest_routes:route(client, Req1, [{name, Name}]),
    FinalReq = chef_rest_util:append_field_to_json_body(Req2, <<"uri">>, Uri),
    {Result, FinalReq, State1}.

to_json(Req, #base_state{resource_state =
                             #client_state{chef_client = Client},
                         organization_name = OrgName} = State) ->
    EJson = chef_client:assemble_client_ejson(Client, OrgName),
    Json = ejson:encode(EJson),
    {Json, Req, State}.

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 resource_state = #client_state{
                                   chef_client = Client},
                                 requestor = #chef_requestor{authz_id = RequestorId},
                                 organization_name = OrgName} = State) ->
    ok = chef_object_db:delete(DbContext, Client, RequestorId),
    EJson = chef_client:assemble_client_ejson(Client, OrgName),
    Req1 = chef_rest_util:set_json_body(Req, EJson),
    {true, Req1, State}.

%% Internal Functions
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
