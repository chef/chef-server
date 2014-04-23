%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%

%% Placeholder module for darklaunch redis functions.
%%
-module(mover_org_darklaunch).

-export([
         disable_org_creation/0,
         enable_org_creation/0,
         disable_org/1,
         enable_org/1,
         init_org_to_couch/2,
         org_to_couch/2,
         org_to_sql/2,
         enable_solr4/0,
         enable_solr4/1,
         enable_both_solrs/1,
         enable_solr1/1]).

disable_org_creation() ->
    send_eredis_q(["HMSET", "dl_org__OC_INTERNAL_NO_ORG", "disable_new_orgs", "true"]).

enable_org_creation() ->
    send_eredis_q(["HDEL", "dl_org__OC_INTERNAL_NO_ORG", "disable_new_orgs"]).

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

init_org_to_couch(OrgName, Components) ->
    %% Use HSETNX so that couchdb_* flags are set only if it does not already exist
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    [send_eredis_q(["HSETNX", OrgKey, "couchdb_" ++ atom_to_list(X), "true"]) || X <- Components].

org_to_sql(OrgName, Components) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    PropKVs = lists:foldl(fun(X, Accum) -> ["couchdb_" ++ atom_to_list(X), "false" | Accum] end, [], Components),
    send_eredis_q(["HMSET", OrgKey] ++ PropKVs).

enable_solr4() ->
    send_eredis_q(["HSET", "dl_default", "solr4", "true"]).

%% Enables solr4 and disables the paired sending to solr1.4 and solr4.
enable_solr4(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    send_eredis_q(["HSET", OrgKey, "solr4", "true"]),
    send_eredis_q(["HDEL", OrgKey, "query_aux_solr","rabbit_aux_vhost"]).

%% Enables the paired sending to solr1.4 and solr4 and disables sending only to sol4.
enable_both_solrs(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    send_eredis_q(["HMSET", OrgKey, "query_aux_solr", "true", "rabbit_aux_vhost", "true"]),
    send_eredis_q(["HDEL", OrgKey, "solr4"]).

enable_solr1(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    send_eredis_q(["HDEL", OrgKey, "solr4", "query_aux_solr", "rabbit_aux_vhost"]).

send_eredis_q(Command) ->
    %% if we're configured for- dry_run mode, don't send the commands to redis
    send_eredis_q(envy:get(mover, dry_run, boolean), Command).

send_eredis_q(true, _) ->
    ok;
send_eredis_q(false, Command) ->
    case eredis:q(mover_eredis_client, Command) of
        {ok, _} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

