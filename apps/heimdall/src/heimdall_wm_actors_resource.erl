-module(heimdall_wm_actors_resource).

-include("heimdall_wm_rest_endpoint.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-mixin([{heimdall_wm_base, [create_path/2]}]).

-export([from_json/2]).

init(Config) ->
    heimdall_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

validate_request(Req, State) ->
    % We really don't care if there's a requestor or not, so we just take whatever
    % comes back from get_requestor without raising an error, even if the
    % requestor_id remains undefined (yes, the old Authz and V1 of Heimdall do not
    % require that a requestor be supplied.  This will probably change in V2).
    try
        State0 = heimdall_wm_util:get_requestor(Req, State),
        {false, Req, State0}
    catch
        throw:{bad_requestor, Id} ->
            heimdall_wm_error:set_malformed_request(Req, State, {bad_requestor, Id})
    end.

auth_info(_Method) ->
    ignore.

from_json(Req, #base_state{reqid = ReqId,
                           authz_id = AuthzId,
                           requestor_id = RequestorId} = State) ->
    case ?SH_TIME(ReqId, heimdall_db, create, (actor, AuthzId, RequestorId)) of
        ok ->
            Req0 = heimdall_wm_util:set_created_response(Req, AuthzId),
            {ok, Req0, State};
        {error, Error} ->
            heimdall_wm_error:set_db_exception(Req, State, Error)
    end.
