-module(heimdall_named_resources).

-include("heimdall_wm_rest_endpoint.hrl").

-export([delete_resource/2,
         to_json/2]).

init(Config) ->
    heimdall_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['GET', 'DELETE'], Req, State}.

validate_request(Req, State) ->
    heimdall_wm_base:validate_requestor(Req, State).

auth_info('GET') ->
    {read};
auth_info('DELETE') ->
    {delete}.

to_json(Req, #base_state{request_type = RequestType} = State) ->
    case RequestType of
        group ->
            % TODO: get group membership
            % TODO: return group membership instead
            {<<"{}">>, Req, State};
        _ ->
            {<<"{}">>, Req, State}
    end.

delete_resource(Req, State) ->
    % TODO: delete the entity
    {ok, Req, State}.
