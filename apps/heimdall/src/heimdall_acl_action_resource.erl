-module(heimdall_acl_action_resource).

-include("heimdall_wm_rest_endpoint.hrl").

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         init/1,
         to_json/2]).

init(Config) ->
    heimdall_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request(Req, State) ->
    heimdall_wm_base:validate_requestor(Req, State).

auth_info('GET') ->
    {any};
auth_info('PUT') ->
    {grant};
auth_info('DELETE') ->
    {grant}.

to_json(Req, State) ->
    % TODO: output the relevant part of the ACL populated by forbidden
    {<<"{}">>, Req, State}.

from_json(Req, State) ->
    % TODO: parse the request body
    % TODO: update the relevant part of the ACL in the DB
    {ok, Req, State}.

delete_resource(Req, State) ->
    % TODO: delete the relevant part of the ACL from DB
    {ok, Req, State}.
