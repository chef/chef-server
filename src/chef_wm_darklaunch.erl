%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
-module(chef_wm_darklaunch).

-export([is_enabled/1,
         is_enabled/2]).

-ifndef(CHEF_WM_DARKLAUNCH).
is_enabled(<<"add_type_and_bag_to_items">>) ->
    true;
is_enabled(<<"couchdb_", _Rest/binary>>) ->
    false.

is_enabled(Feature, _Org) ->
    is_enabled(Feature).
-else.
is_enabled(Feature) ->
    ?CHEF_WM_DARKLAUNCH:is_enabled(Feature).

is_enabled(Feature, Org) ->
    ?CHEF_WM_DARKLAUNCH:is_enabled(Feature, Org).
-endif.
