%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2013-2015 Chef, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(chef_test_db_helper).

-include_lib("common_test/include/ct.hrl").

-export([
         start_db/2,
         stop_db/1,
         delete_all_from_table/1
        ]).


%%
%% We expect an external instance of postgres set up and the following environment variables
%% This makes it easy to use something like pg_virtualenv
%% PGHOST:     default localhost
%% PGDATABASE: default bookshelf
%% PGUSER:     default bookshelf
%% PGPASSWORD: default test_sql_password
get_db_config(DbName) ->
    Host =     os:getenv("PGHOST", "localhost"),
    Db =       os:getenv("PGDATABASE", DbName),
    User =     os:getenv("PGUSER", os:getenv("USER")),
    Password = os:getenv("PGPASSWORD", "test_sql_password"),
    #{host => Host,
      db => Db,
      user => User,
      password => Password,
      port => 5432
     }.


%%
%%
%%
maybe_init_db(false, _, DbConfig) ->
    DbConfig;
maybe_init_db(_, Config, DbConfig) ->
    DataDir = ?config(data_dir, Config),
    PgData = filename:join(DataDir, "pg_data"),
    PgLog = filename:join(DataDir, "pg.log"),
    Port = chef_test_suite_helper:random_bogus_port(),

    PgHost = maps:get(host, DbConfig),
    PortStr = integer_to_list(Port),
    DbConfig1 = DbConfig#{port=> port},

    CMDS = [
            ["rm -rf", ?config(data_dir, Config)],
            ["mkdir -p", DataDir],
            ["initdb -D", PgData],
            ["pg_ctl -D", PgData, "-l", PgLog, "-o \"-h ",  PgHost, " -p ", PortStr,   "-k '' \" start"],
            %% db start is async, sleep? :(
            ["sleep 1"]],
    CmdOut = chef_test_suite_helper:run_cmds(CMDS),
    ct:pal("Cmd~n~p", [CmdOut]),
    DbConfig1.


%% Set statements field on Config to overide default prepared_statements for sqerl.
%% You should set it to the function that can be called by sqerl to define the statements.
%% I.e. NewConfig = [{statements, {chef_db_SUITE, statements, []}} | Config].
%% They default to oc_chef_sql:statements(pgsql) like everything.
start_db(Config, DbName) ->
    DbConfig = get_db_config(DbName),
    DbConfig1 = maybe_init_db(os:getenv("TEST_SETUP_DB"), Config, DbConfig),

    DataDir = ?config(data_dir, Config),

    %% Will the real root dir please stand up!
    RootDir = the_real_root_dir(DataDir),

    App = ?config(app, Config),
    AppDir = filename:join([RootDir, "apps", App]),

    %%CommonDir = filename:join([DataDir, "../common"]),
    CommonDir = filename:join([AppDir, "itest", "common"]),

    Schema = filename:join([CommonDir, "schema.sql"]),
    ECSchema = filename:join([RootDir, "schema"]),
    OSCSchema = filename:join([ECSchema, "baseline"]),
    PgData = filename:join(DataDir, "pg_data"),
    PgLog = filename:join(DataDir, "pg.log"),

    PgHost = maps:get(host, DbConfig1),
    Port = maps:get(port, DbConfig1),
    PortStr = integer_to_list(Port),

    CMDS = [
            ["dropdb -h ", PgHost, " -p ", PortStr, " ", DbName],
            ["createdb -h ", PgHost, " -p ", PortStr, " ", DbName],
            ["cd", OSCSchema, ["&& sqitch db:pg:", DbName],
             "--db-port", PortStr, "--db-host", PgHost, "deploy"],
            ["cd", ECSchema, ["&& sqitch db:pg:", DbName],
             "--db-port", PortStr, "--db-host", PgHost, "deploy"],
            ["psql -p", PortStr, DbName, "-h", PgHost, " <", Schema]
           ],

    %% FIXME: we should fail explicitely here if any of the commands above
    %% fail
    CmdOut = chef_test_suite_helper:run_cmds(CMDS),
    ct:pal("Cmd\n~p", [CmdOut]),
    % make sure it's seen in output, don't use lager.
    %CmdsResult = chef_test_suite_helper:run_cmds(CMDS),
    %io:format(user, "db_start: ~n~p~n", [CmdsResult]),

    Statements = case ?config(statements, Config) of
                     undefined ->
                         {oc_chef_sql, statements, [pgsql]};
                      _ ->
                         ?config(statements, Config)
                 end,

    chef_test_suite_helper:set_env(sqerl,
            [{db_host, "localhost"},
             {db_port, Port},
             {db_user, maps:get(user, DbConfig1)},
             {db_pass, maps:get(password, DbConfig1)},
             {db_name, DbName },
             {idle_check, 10000},
             {prepared_statements, Statements},
             {column_transforms,
              [{<<"created_at">>, {sqerl_transformers, convert_YMDHMS_tuple_to_datetime}},
               {<<"updated_at">>, {sqerl_transformers, convert_YMDHMS_tuple_to_datetime}}]}]),

    [{pg_port, Port}, {pg_data, PgData}, {pg_log, PgLog}, {pg_name, DbName}, {pg_map, DbConfig1} | Config].

the_real_root_dir("") ->
    {error, not_found};
the_real_root_dir(Dir) ->
    AllFiles = maybe_find_files(Dir),
    case {lists:member("_build", AllFiles), lists:member("apps", AllFiles)} of
        {true, true} ->
            Dir;
        _ ->
            Tokens = filename:split(Dir),
            NewDir = filename:join(lists:droplast(Tokens)),
            the_real_root_dir(NewDir)
    end.

maybe_find_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, AllFiles} ->
            AllFiles;
        _ ->
            []
    end.

stop_db(Config) ->
    PgMap = ?config(pg_map, Config),

    DbName  = maps:get(name, PgMap),
    PgHost  = maps:get(host, PgMap),
    PortStr = integer_to_list(maps:get(port, PgMap)),

    CMDS = [["dropdb -h ", PgHost, " -p ", PortStr, " ", DbName]],

    CmdOut = chef_test_suite_helper:run_cmds(CMDS),
    ct:pal("stop_db results:~n~p~n", [CmdOut]),

    maybe_stop_db(os:getenv("TEST_SETUP_DB"), Config),
    ok.

maybe_stop_db(false, _Config) ->
    ok;
maybe_stop_db(_, Config) ->
    PgData = ?config(pg_data, Config),
    CMDS = [
            ["pg_ctl -D", PgData, "-m fast", "stop"]
           ],
    CmdsResult = chef_test_suite_helper:run_cmds(CMDS),
    ct:pal("maybe_stop_db results:~n~s~n", [CmdsResult]),
    ok.

delete_all_from_table(TableName) ->
    case sqerl:adhoc_delete(TableName, all) of
        {ok, Count} ->
            Count;
        Error ->
            throw(Error)
    end,
    ok.
