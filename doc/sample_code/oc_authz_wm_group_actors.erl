% Pattern for /groups/<id>/actors/<actor_id>

% There would be one of these for /groups/<id>/groups/<group_id>

-module(oc_authz_wm_group_actors).

-include("oc_authz_wm.hrl").

-mixin([{oc_authz_wm_base, [content_types_accepted/2,
                            content_types_provided/2,
                            finish_request/2,
                            forbidden/2,
                            malformed_request/2
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
    {['PUT', 'DELETE'], Req, State}.

auth_info('PUT') ->
    {grant};
auth_info('DELETE') ->
    {grant}.

from_json(Req, State) ->
    % Add actor to group (attempt to save in DB/fail on validation errors)
    ...

delete_resource(Req, State) ->
    % Remove actor from group (attempt to remove in DB/fail on validation errors)
    ...

% Key steps in web machine:

% malformed_request: figures out the type (always group), pulls the ID
%   from the URL, determines if if it exists in DB, puts current object
%   ACL in state
% forbidden: calls auth_info, forbids based on permission supplied against state ACL
