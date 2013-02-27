-module(heimdall_wm_acl_member_resource).

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

to_json(Req, #base_state{authz_id = AuthzId, request_type = RequestType,
                         action = Action, member_id = MemberId} = State) ->
    try
        case heimdall_acl:check_access(RequestType, AuthzId, MemberId, Action) of
            true ->
                {<<"{}">>, Req, State};
            false ->
                {{halt, 404}, Req, State}
        end
    catch
        throw:{db_error, Error} ->
            heimdall_wm_error:set_db_exception(Req, State, Error)
    end.
