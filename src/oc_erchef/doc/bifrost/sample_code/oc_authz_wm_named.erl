% Pattern for /objects/<id>

-module(oc_authz_wm_named).

-include("oc_authz_wm.hrl").

-mixin([{oc_authz_wm_base, [content_types_accepted/2,
                            content_types_provided/2,
                            finish_request/2,
                            forbidden/2,
                            malformed_request/2,
                            ping/2,
                            service_available/2]}]).

-behavior(oc_authz_wm).
-export([auth_info/2]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         init/1,
         to_json/2]).

init(_Config) ->
    {ok, #acl_base_state{}}.

allowed_methods(Req, State) ->
    {['GET', 'DELETE'], Req, State}.

auth_info('GET') ->
    {read};
auth_info('DELETE') ->
    {delete}.

to_json(Req, State) ->
    % Currently returns empty body (or members if this was the group endpoint)
    ...

delete_resource(Req, State) ->
    % Deletes the object (attempts to clear from DB)
    ...

% Key steps in web machine:

% malformed_request: figures out the type, pulls the ID from the URL, determines if
%   if it exists in DB, puts current object ACL in state
% forbidden: calls auth_info, forbids based on permission supplied against state ACL
