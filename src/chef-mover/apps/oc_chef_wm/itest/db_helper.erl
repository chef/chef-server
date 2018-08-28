%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013-2018 Chef Software, Inc.

-module(db_helper).

-include_lib("common_test/include/ct.hrl").

-compile([{parse_transform, lager_transform}]).

-export([
         start_db/1,
         stop_db/1
        ]).

start_db(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),

    DbName = "oc_chef_wm_itests",
    DbDataDir = filename:join([PrivDir, "db_data"]),
    DbLog = filename:join([PrivDir, "db.log"]),
    ECSchema = filename:join([DataDir, "deps", "enterprise-chef-server-schema"]),
    OSCSchema = filename:join([ECSchema, "deps", "chef-server-schema"]),
    DbPort = random_bogus_port(),
    PortString = integer_to_list(DbPort),

    CMDS = [
            ["initdb -D", DbDataDir],
            ["pg_ctl -D", DbDataDir, "-l", DbLog, "-o \"-p", PortString, "\" start"],
            ["sleep 1"],
            ["createdb -p", PortString, DbName],
            ["cd", OSCSchema, "&& sqitch --engine pg --db-name", DbName,
             "--db-port", PortString, "deploy"],
            ["cd", ECSchema, "&& sqitch --engine pg --db-name", DbName,
             "--db-port", PortString, "deploy"]
           ],
    CmdsResult = run_cmds(CMDS),

    lager:info("db_start:~n~s~n", [CmdsResult]),

    [{db_name, DbName},
     {db_port, DbPort},
     {db_data, DbDataDir},
     {db_user, os:getenv("USER")},
     {db_pass, "pass-ignored"}].

stop_db(Config) ->
    DbDataDir = ?config(db_data, Config),
    CMDS = [
            ["pg_ctl -D", DbDataDir, "-m fast", "stop"]
           ],
    CmdsResult = run_cmds(CMDS),
    lager:info("db_stop:~n~s~n", [CmdsResult]),
    ok.

%%
%% helper funs
%%

%% @doc If lucky, return an unused port. This is a cheat that opens a
%% UDP port letting the OS pick the port, captures that port, and then
%% closes the socket returning the port number. While not reliable,
%% this seems to work to obtain an "unused" port for setting up
%% services needed for testing.
random_bogus_port() ->
    {ok, S} = gen_udp:open(0, [binary, {active, once}]),
    {ok, Port} = inet:port(S),
    gen_udp:close(S),
    Port.

run_cmds(CMDS) ->
    [ begin
          CC = space_join(C),
          [CC, "\n", os:cmd(CC), "\n\n"]
      end || C <- CMDS ].

space_join(L) ->
    [ [Elt, " "] || Elt <- L ].
