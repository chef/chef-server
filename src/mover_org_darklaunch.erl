%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%

%% Placeholder module for darklaunch redis functions.
%%
-module(mover_org_darklaunch).

-export([disable_org/1,
         enable_org/1,
         org_to_sql/2]).

disable_org(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    eredis:q(mover_eredis_client, ["HSET", OrgKey, "503_mode", "true"]),
    ok.

enable_org(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    eredis:q(mover_eredis_client, ["HSET", OrgKey, "503_mode", "false"]),
    ok.

org_to_sql(OrgName, Components) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    PropKVs = lists:foldl(fun(X, Accum) -> ["couchdb_" ++ atom_to_list(X), "false" | Accum] end, [], Components),
    eredis:q(mover_eredis_client, ["HMSET", OrgKey] ++ PropKVs),
    ok.
