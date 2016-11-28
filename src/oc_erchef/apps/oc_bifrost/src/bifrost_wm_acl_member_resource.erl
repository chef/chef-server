-module(bifrost_wm_acl_member_resource).

-include("bifrost_wm_rest_endpoint.hrl").

-export([to_json/2]).

init(Config) ->
    bifrost_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

validate_request(Req, State) ->
    bifrost_wm_base:validate_requestor(Req, State).

auth_info('GET') ->
    ignore.

to_json(Req, #base_state{reqid = ReqId,
                         authz_id = AuthzId,
                         request_type = RequestType,
                         action = Action,
                         member_id = MemberId} = State) ->
    try
        case bifrost_acl:check_access(ReqId, RequestType, AuthzId, MemberId, Action) of
            true ->
                {<<"{}">>, Req, State};
            false ->
                {{halt, 404}, Req, State};
            {error, {invalid_actor, _}} ->
                {{halt, 404}, Req, State}
        end
    catch
        throw:{db_error, Error} ->
            bifrost_wm_error:set_db_exception(Req, State, Error)
    end.
