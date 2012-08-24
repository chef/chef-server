%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% Authz logic and helpers for chef_wm endpoints
%%

-module(chef_wm_authz).

-export([is_admin/1,
         is_validator/1,
         is_admin_or_validator/1,
         is_admin_or_requesting_node/2]).

-include("chef_wm.hrl").

%%
%% TODO implement is_FOO functions for #chef_user{} when the record
%% has an 'admin' field
%%

-spec is_admin(#chef_client{}) -> boolean().
is_admin(#chef_client{admin = true}) ->
    true;
is_admin(#chef_client{}) ->
    false.

-spec is_validator(#chef_client{}) -> boolean().
is_validator(#chef_client{validator = true}) ->
    true;
is_validator(#chef_client{}) ->
    false.

-spec is_admin_or_validator(#chef_client{}) -> boolean().
is_admin_or_validator(#chef_client{validator = true}) ->
    true;
is_admin_or_validator(#chef_client{} = Client) ->
    is_admin(Client).

-spec is_admin_or_requesting_node(#chef_client{}, binary()) -> boolean().
is_admin_or_requesting_node(#chef_client{name = Name} = Client, NodeName) ->
    case NodeName of
        N when N =:= Name ->
            true;
        _Else ->
            is_admin(Client)
    end.
