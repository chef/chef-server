%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% Authz logic and helpers for chef_wm endpoints
%%

-module(chef_wm_authz).

-export([allow_admin/1,
         allow_admin_or_requesting_node/2,
         allow_validator/1,
         is_admin/1,
         is_validator/1]).

-include("chef_wm.hrl").

%%
%% TODO implement is_FOO functions for #chef_user{} when the record
%% has an 'admin' field
%%

-spec allow_admin(#chef_client{}) -> authorized | forbidden.
allow_admin(#chef_client{admin = true}) ->
    authorized;
allow_admin(#chef_client{}) ->
    forbidden.

-spec allow_admin_or_requesting_node(#chef_client{}, binary()) -> authorized | forbidden.
allow_admin_or_requesting_node(#chef_client{name = Name}, Name) ->
    authorized;
allow_admin_or_requesting_node(#chef_client{} = Client, _Name) ->
    allow_admin(Client).

-spec allow_validator(#chef_client{}) -> authorized | forbidden.
allow_validator(#chef_client{validator = true}) ->
    authorized;
allow_validator(#chef_client{}) ->
    forbidden.

-spec is_admin(#chef_client{}) -> true | false.
is_admin(#chef_client{admin = true}) ->
    true;
is_admin(#chef_client{}) ->
    false.

-spec is_validator(#chef_client{}) -> true | false.
is_validator(#chef_client{validator = true}) ->
    true;
is_validator(#chef_client{}) ->
    false.


