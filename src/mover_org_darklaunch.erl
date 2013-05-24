%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%

%% Placeholder module for darklaunch redis functions.
%%
-module(mover_org_darklaunch).
-compile([{parse_transform, lager_transform}]).

-export([disable_org/1,
         enable_org/1,
         org_to_couch/2,
         org_to_sql/2]).

disable_org(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    send_eredis_q(["HSET", OrgKey, "503_mode", "true"]).

enable_org(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    send_eredis_q(["HSET", OrgKey, "503_mode", "false"]).

org_to_couch(OrgName, Components) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    PropKVs = lists:foldl(fun(X, Accum) -> ["couchdb_" ++ atom_to_list(X), "true" | Accum] end, [], Components),
    send_eredis_q(["HMSET", OrgKey] ++ PropKVs).

org_to_sql(OrgName, Components) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    PropKVs = lists:foldl(fun(X, Accum) -> ["couchdb_" ++ atom_to_list(X), "false" | Accum] end, [], Components),
    send_eredis_q(["HMSET", OrgKey] ++ PropKVs).

send_eredis_q(Command) ->
    %% if we're configured for- dry_run mode, don't send the commands to redis
    send_eredis_q(envy:get(mover, dry_run, boolean), Command).

send_eredis_q(true, _) ->
	lager:info("Redis dry-run enabled"),
    ok;
send_eredis_q(false, Command) ->
    case eredis:q(mover_eredis_client, Command) of
        {ok, _} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

