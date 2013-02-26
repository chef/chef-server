-module(heimdall_wm_group_member_resource).

-include("heimdall_wm_rest_endpoint.hrl").

-export([delete_resource/2,
         from_json/2]).

init(Config) ->
    heimdall_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['PUT', 'DELETE'], Req, State}.

validate_request(Req, State) ->
    heimdall_wm_base:validate_requestor(Req, State).

auth_info('PUT') ->
    {grant};
auth_info('DELETE') ->
    {grant}.

from_json(Req, State) ->
    % TODO: add the object to the group
    {true, Req, State}.

delete_resource(Req, State) ->
    % TODO: delete the object from the group
    {true, Req, State}.
