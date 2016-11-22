-module(bifrost_wm_acl_resource).

-include("bifrost_wm_rest_endpoint.hrl").

-export([to_json/2]).

init(Config) ->
    bifrost_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

validate_request(Req, State) ->
    bifrost_wm_base:validate_requestor(Req, State).

auth_info('GET') ->
    any.

to_json(Req, #base_state{reqid = ReqId,
                         authz_id = AuthzId,
                         request_type = RequestType} = State) ->
    try
        Ejson = bifrost_acl:make_ejson_acl(ReqId, RequestType, AuthzId),
        {bifrost_wm_util:encode(Ejson), Req, State}
    catch
        throw:{db_error, Error} ->
            bifrost_wm_error:set_db_exception(Req, State, Error)
    end.
