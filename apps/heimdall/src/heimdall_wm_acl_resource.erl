-module(heimdall_wm_acl_resource).

-include("heimdall_wm_rest_endpoint.hrl").

-export([to_json/2]).

init(Config) ->
    heimdall_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

validate_request(Req, State) ->
    heimdall_wm_base:validate_requestor(Req, State).

auth_info('GET') ->
    any.

to_json(Req, #base_state{authz_id = AuthzId, request_type = RequestType} = State) ->
    try
        Ejson = heimdall_acl_util:make_ejson_acl(RequestType, AuthzId, Req, State),
        {heimdall_wm_util:encode(Ejson), Req, State}
    catch
        throw:{wm_db_error_tuple, Error} ->
            Error
    end.
