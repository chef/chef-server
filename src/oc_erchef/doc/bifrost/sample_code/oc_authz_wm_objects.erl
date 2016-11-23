% Pattern for /objects

% There would be one of these for actors, groups, containers, and objects

-module(oc_authz_wm_objects).

-include("oc_authz_wm.hrl").

-mixin([{oc_authz_wm_base, [content_types_accepted/2,
                            content_types_provided/2,
                            finish_request/2,
                            ping/2,
                            post_is_create/2,
                            service_available/2]}]).

-behavior(oc_authz_wm).
-export([auth_info/2,
         forbidden/2,
         malformed_request/2]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         init/1,
         to_json/2]).

init(_Config) ->
    {ok, #object_base_state{}}.

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

malformed_request(Req, State) ->
    % Figures out the requestor (errors out if not there)
    ...

forbidden(Req, State) ->
    {false, State}.

auth_info(_Method) ->
    {any}.

from_json(Req, State) ->
    % Attempts to create the object, adds requestor to ACL (actor
    % endpoint would also add actor to own ACL)
    ...
