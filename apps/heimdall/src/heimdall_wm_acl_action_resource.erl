-module(bifrost_wm_acl_action_resource).

-include("bifrost_wm_rest_endpoint.hrl").

-export([delete_resource/2,
         from_json/2,
         to_json/2]).

init(Config) ->
    bifrost_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request(Req, State) ->
    bifrost_wm_base:validate_requestor(Req, State).

auth_info('GET') ->
    any;
auth_info(Verb) when Verb =:= 'PUT';
                     Verb =:= 'DELETE' ->
    grant.

to_json(Req, #base_state{reqid = ReqId,
                         authz_id = AuthzId,
                         request_type = RequestType,
                         action = Action} = State) ->
    try
        Ejson = bifrost_acl:make_ejson_action(ReqId, Action, RequestType, AuthzId),
        {bifrost_wm_util:encode(Ejson), Req, State}
    catch
        throw:{db_error, Error} ->
            bifrost_wm_error:set_db_exception(Req, State, Error)
    end.

from_json(Req, #base_state{reqid = ReqId,
                           authz_id = AuthzId,
                           request_type = RequestType,
                           action = Action} = State) ->
    try
        Body = wrq:req_body(Req),
        {Actors, Groups} = bifrost_acl:parse_acl_json(Body),
        bifrost_acl:update_acl(ReqId, RequestType, AuthzId, Action, Actors, Groups),
        {true, wrq:set_resp_body(<<"{}">>, Req), State}
    catch
        throw:{error, invalid_json} ->
            {_Return, Req0, _State} =
                bifrost_wm_error:set_malformed_request(Req, State, invalid_json),
            {{halt, 400}, Req0, State};
        throw:{db_error, Error} ->
            bifrost_wm_error:set_db_exception(Req, State, Error)
    end.



delete_resource(Req, #base_state{reqid = ReqId,
                                 authz_id = AuthzId,
                                 request_type = RequestType,
                                 action = Action} = State) ->
    try
        bifrost_acl:clear_access(ReqId, RequestType, AuthzId, Action),
        {true, wrq:set_resp_body(<<"{}">>, Req), State}
    catch
        throw:{db_error, Error} ->
            bifrost_wm_error:set_db_exception(Req, State, Error)
    end.
