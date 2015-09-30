-module(bifrost_wm_unnamed_resources).

-include("bifrost_wm_rest_endpoint.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-mixin([{bifrost_wm_base, [create_path/2,
                           post_is_create/2]}]).

-export([from_json/2]).

init(Config) ->
    bifrost_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

validate_request(Req, State) ->
    bifrost_wm_base:validate_requestor(Req, State).

auth_info(_Method) ->
    ignore.

from_json(Req, #base_state{reqid = ReqId,
                           authz_id = AuthzId,
                           request_type = Type,
                           requestor_id = RequestorId} = State) ->
    case ?SH_TIME(ReqId, bifrost_db, create, (Type, AuthzId, RequestorId)) of
        ok ->
            Req0 = bifrost_wm_util:set_created_response(Req, AuthzId),
            {ok, Req0, State};
        {error, Error} ->
            bifrost_wm_error:set_db_exception(Req, State, Error)
    end.
