% Pattern for /.../acl (+ extended URL) requests [this one is /.../acl/<action>/]

% There would be three of these:
%  oc_authz_wm_acl           : /<type>/<id>/acl
%  oc_authz_wm_acl_action    : /<type>/<id>/acl/<action>
%  oc_authz_wm_acl_member    : /<type>/<id>/acl/<action/<member_type>/<member_id>
% (Last one might actually be two: actors + groups)

-module(oc_authz_wm_acl_action).

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
    {['GET', 'PUT', 'DELETE'], Req, State}.

auth_info('GET') ->
    {any};
auth_info('PUT') ->
    {grant};
auth_info('DELETE') ->
    {grant}.

to_json(Req, State) ->
    % GET ACL for requested entity (available in state)
    ...

from_json(Req, State) ->
    % SET ACL for requested entity (attempt to save in DB/fail on validation errors)
    ...

delete_resource(Req, State) ->
    % CLEAR ACL for requested entity (attempt to clear DB for resource)
    ...

% Key steps in web machine:

% malformed_request: figures out the type, pulls the ID from the URL, determines if
%   if it exists in DB, puts current object ACL in state
% forbidden: calls auth_info, forbids based on permission supplied against state ACL
