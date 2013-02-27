-module(heimdall_wm_acl_action_resource).

-include("heimdall_wm_rest_endpoint.hrl").

-export([delete_resource/2,
         from_json/2,
         to_json/2]).

init(Config) ->
    heimdall_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request(Req, State) ->
    heimdall_wm_base:validate_requestor(Req, State).

auth_info('GET') ->
    any;
auth_info(Verb) when Verb =:= 'PUT';
                     Verb =:= 'DELETE' ->
    grant.

to_json(Req, State) ->
    % TODO: output the relevant part of the ACL populated by forbidden
    {<<"{}">>, Req, State}.

from_json(Req, State) ->
    % TODO: parse the request body
    % TODO: update the relevant part of the ACL in the DB
    {true, Req, State}.

delete_resource(Req, State) ->
    % TODO: delete the relevant part of the ACL from DB
    {true, Req, State}.
