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

to_json(Req, State) ->
    % TODO: check if member has permission on entity; {halt 404} if not
    {<<"{}">>, Req, State}.
