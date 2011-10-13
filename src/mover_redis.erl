%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011 Opscode, Inc.

-module(mover_redis).

-export([connect/0,
         disconnect/1,
         delete_tracking/2,
         inflight_requests_for_org/2]).

-define(MIGRATION_DB, 5).

connect() ->
    case is_dry_run() of
        true -> dry_run_redis_client;
        false ->
            {ok, RedisHost} = application:get_env(mover, redis_host),
            {ok, RedisPort} = application:get_env(mover, redis_port),
            {ok, RedisDb} = application:get_env(mover, redis_db),
            {ok, Client} = erldis:connect(RedisHost, RedisPort),
            ok = erldis:select(Client, RedisDb),
            Client
    end.

disconnect(dry_run_redis_client) ->
    ok;
disconnect(Client) ->
    erldis:quit(Client).

inflight_requests_for_org(dry_run_redis_client, _) ->
    [];
inflight_requests_for_org(Client, OrgName) ->
    [ begin
          [Host, Port] = binary:split(M, <<",">>),
          {Host, Port}
      end
      || M <- erldis:smembers(Client, OrgName) ].

delete_tracking(dry_run_redis_client, _) ->
    ok;
delete_tracking(Client, OrgName) ->
    erldis:del(Client, OrgName).

is_dry_run() ->
    {ok, DryRun} = application:get_env(mover, dry_run),
    DryRun.
