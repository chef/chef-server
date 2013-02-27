-module(heimdall_wm_actors_resource).

-include("heimdall_wm_rest_endpoint.hrl").

-export([create_path/2,
         from_json/2]).

init(Config) ->
    heimdall_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

create_path(Req, State) ->
    AuthzId = heimdall_wm_util:generate_authz_id(),
    {AuthzId, Req, State#base_state{authz_id = AuthzId}}.

validate_request(Req, State) ->
    % We really don't care if there's a requestor or not, so we just take whatever
    % comes back, even if the requestor_id remains undefined.
    try
        State0 = heimdall_wm_util:get_requestor(Req, State),
        {false, Req, State0}
    catch
        throw:{bad_requestor, Id} ->
            heimdall_wm_error:set_malformed_request(Req, State, {bad_requestor, Id})
    end.

auth_info(_Method) ->
    ignore.

from_json(Req, #base_state{authz_id = AuthzId,
                           requestor_id = RequestorId} = State) ->
    case heimdall_db:create(actor, AuthzId) of
        ok ->
            try
                heimdall_acl:add_full_access(actor, AuthzId, actor, AuthzId),
                heimdall_acl:add_full_access(actor, AuthzId, actor, RequestorId),
                Req0 = heimdall_wm_util:set_created_response(Req, AuthzId),
                {ok, Req0, State}
            catch
                throw:{db_error, Error} ->
                    heimdall_wm_error:set_db_exception(Req, State, Error)
            end;
        {error, Error} ->
            heimdall_wm_error:set_db_exception(Req, State, Error)
    end.
