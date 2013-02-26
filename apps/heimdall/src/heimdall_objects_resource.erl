-module(heimdall_objects_resource).

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
    heimdall_wm_base:validate_requestor(Req, State).

auth_info(_Method) ->
    ignore.

from_json(Req, #base_state{authz_id = AuthzId,
                           requestor_id = RequestorId} = State) ->
    case heimdall_db:create(object, AuthzId) of
        ok ->
            try
                heimdall_acl_util:add_full_access(object, AuthzId,
                                                  actor, RequestorId),
                Req0 = heimdall_wm_util:set_created_response(Req, AuthzId),
                {ok, Req0, State}
            catch
                throw:Error ->
                    heimdall_wm_error:set_db_exception(Req, State, Error)
            end;
        Error ->
            heimdall_wm_error:set_db_exception(Req, State, Error)
    end.
