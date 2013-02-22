-module(heimdall_groups_resource).

-include("heimdall_wm_rest_endpoint.hrl").

-export([allowed_methods/2,
         create_path/2,
         from_json/2,
         init/1]).

init(Config) ->
    heimdall_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

create_path(Req, State) ->
    {"/groups", Req, State}.

validate_request(Req, State) ->
    State0 = heimdall_wm_util:get_requestor(Req, State),
    case State0#base_state.requestor_id of
        undefined ->
            heimdall_wm_error:set_malformed_request(Req, State, missing_actor);
        _ ->
            {false, Req, State0}
    end.

auth_info(_Method) ->
    {any}.

from_json(Req, State) ->
    AuthzId = heimdall_wm_util:generate_authz_id(),
    % Attempt to store group in DB
    % Attempt to add requestor to ACL
    Req0 = heimdall_wm_util:set_created_response(Req, AuthzId),
    {ok, Req0, State}.
