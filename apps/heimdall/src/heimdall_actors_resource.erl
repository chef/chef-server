-module(heimdall_actors_resource).

-include("heimdall_wm.hrl").

-mixin([{heimdall_wm_base, [content_types_accepted/2,
                            content_types_provided/2,
                            finish_request/2,
                            ping/2,
                            post_is_create/2,
                            service_available/2]}]).

-behavior(heimdall_wm).
-export([auth_info/1,
         forbidden/2,
         malformed_request/2]).

-export([allowed_methods/2,
         create_path/2,
         from_json/2,
         init/1]).

init(_Config) ->
    {ok, #base_state{}}.

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

create_path(Req, State) ->
    {"/actors", Req, State}.

malformed_request(Req, State) ->
    % Figures out the requestor (does NOT error out if not there, this endpoint
    % only)
    {false, Req, State}.

forbidden(Req, State) ->
    {false, Req, State}.

auth_info(_Method) ->
    {any}.

from_json(Req, State) ->
    AuthzId = heimdall_wm_util:generate_authz_id(),
    % Attempt to store actor in DB
    % Attempt to add actor to own ACL
    % Attempt to add requestor to ACL
    Req0 = heimdall_wm_util:set_created_response(Req, AuthzId),
    {ok, Req0, State}.
