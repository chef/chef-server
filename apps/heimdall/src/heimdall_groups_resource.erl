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
    heimdall_wm_base:validate_requestor(Req, State).

auth_info(_Method) ->
    ignore.

from_json(Req, State) ->
    AuthzId = heimdall_wm_util:generate_authz_id(),
    % TODO: Attempt to store group in DB
    % TODO: Attempt to add requestor to ACL
    Req0 = heimdall_wm_util:set_created_response(Req, AuthzId),
    {ok, Req0, State}.
