%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011 Opscode, Inc.

-module(mover_redis).

-export([connect/0,
         disconnect/1,
         inflight_requests_for_org/2]).

-define(MIGRATION_DB, 5).

connect() ->
    {ok, RedisHost} = application:get_env(mover, redis_host),
    {ok, RedisPort} = application:get_env(mover, redis_port),
    {ok, RedisDb} = application:get_env(mover, redis_db),
    {ok, Client} = erldis:connect(RedisHost, RedisPort),
    ok = erldis:select(Client, RedisDb),
    Client.

disconnect(Client) ->
    erldis:quit(Client).

inflight_requests_for_org(Client, OrgName) ->    
    [ begin
          [Host, Port] = binary:split(M, <<",">>),
          {Host, Port}
      end
      || M <- erldis:smembers(Client, OrgName) ].
