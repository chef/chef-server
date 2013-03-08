-module(heimdall_wm_unnamed_resources).

-include("heimdall_wm_rest_endpoint.hrl").

-mixin([{heimdall_wm_base, [create_path/2]}]).

-export([create_path/2,
         from_json/2]).

init(Config) ->
    heimdall_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

validate_request(Req, State) ->
    heimdall_wm_base:validate_requestor(Req, State).

auth_info(_Method) ->
    ignore.

from_json(Req, #base_state{authz_id = AuthzId, request_type = Type,
                           requestor_id = RequestorId} = State) ->
    case heimdall_db:create(Type, AuthzId) of
        ok ->
            try
                heimdall_acl:add_full_access(Type, AuthzId, actor, RequestorId),
                Req0 = heimdall_wm_util:set_created_response(Req, AuthzId),
                {ok, Req0, State}
            catch
                throw:{db_error, Error} ->
                    heimdall_wm_error:set_db_exception(Req, State, Error)
            end;
        {error, Error} ->
            heimdall_wm_error:set_db_exception(Req, State, Error)
    end.
