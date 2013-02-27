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

to_json(Req, #base_state{authz_id = AuthzId, request_type = RequestType,
                         action = Action} = State) ->
    try
        Ejson = heimdall_acl:make_ejson_action(Action, RequestType, AuthzId),
        {heimdall_wm_util:encode(Ejson), Req, State}
    catch
        throw:{db_error, Error} ->
            heimdall_wm_error:set_db_exception(Req, State, {error, Error})
    end.    

from_json(Req, State) ->
    % TODO: move this to a postgres function so that it can all happen
    % in a transaction
    % TODO: parse the request body
    % TODO: update the relevant part of the ACL in the DB
    {true, Req, State}.

delete_resource(Req, #base_state{authz_id = AuthzId, request_type = RequestType,
                                 action = Action} = State) ->
    try
        heimdall_acl:clear_access(RequestType, AuthzId, Action),
        {true, Req, State}
    catch
        throw:{db_error, Error} ->
            heimdall_wm_util:set_db_exception(Req, State, {error, Error})
    end.
