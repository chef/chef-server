-module(oc_chef_authz_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("oc_chef_authz/include/oc_chef_authz.hrl").

-compile([export_all]).

all() -> [fetch_container_sql, fetch_group_sql].

init_per_suite(Config) ->
    ct:pal("hi from init~n"),
    init_app_config(init_pg_db(Config)).

end_per_suite(Config) ->
    ct:pal("bye from init~n"),
    %% Note: We see a noisy error message of the form below when terminating.
    %% This trains us to ignore errors, which is a bad thing.
    %% Silencing the error message is a short term fix, we need a better one.
    %%
    %% ** State machine <0.133.0> terminating
    %% ** Last message in was {'EXIT',<0.132.0>,killed}
    %% ** When State == ready
    %% **      Data  == {state,undefined,<0.134.0>,5000,
    %%                         [{<<"application_name">>,<<>>},
    %%                          {<<"client_encoding">>,<<"UTF8">>},
    %%                          {<<"DateStyle">>,<<"ISO, MDY">>},
    %%                          {<<"integer_datetimes">>,<<"on">>},
    %%                          {<<"IntervalStyle">>,<<"postgres">>},
    %%                          {<<"is_superuser">>,<<"on">>},
    %%                          {<<"server_encoding">>,<<"UTF8">>},
    %%                          {<<"server_version">>,<<"9.2.3">>},
    %%                          {<<"session_authorization">>,<<"mark">>},
    %%                          {<<"standard_conforming_strings">>,<<"on">>},
    %%                          {<<"TimeZone">>,<<"US/Pacific">>}],
    %%                         undefined,
    %%                         {<0.132.0>,#Ref<0.0.0.1089>},
    %%                         undefined,false,[],
    %%                         {90852,579277529},
    %%                         undefined,73}
    error_logger:tty(false),
    stop_apps(),
    error_logger:tty(true),
    ct:pal("~s", [stop_pg_db(Config)]),
    ok.

fetch_container_sql(_Config) ->
    OrgId = <<"33330000000000000000000000000000">>,
    ContainerName = <<"nodes">>,
    ReqId = <<"test-1-req-id">>,
    Ctx = oc_chef_authz:make_context(ReqId, darklaunch_stub),
    case oc_chef_authz_db:fetch_container_sql(Ctx, OrgId, ContainerName) of
        #chef_container{name = <<"nodes">>} = C ->
            ct:pal("Found container: ~p", [C]),
            ok;
        Bad ->
            erlang:error({unexpected_result, Bad})
    end.

fetch_group_sql(_Config) ->
    OrgId = <<"77770000000000000000000000000000">>,
    Name = <<"admins">>,
    ReqId = <<"test-2-req-id">>,
    Ctx = oc_chef_authz:make_context(ReqId, darklaunch_stub),
    case oc_chef_authz_db:fetch_group_authz_id_sql(Ctx, OrgId, Name) of
        <<"66660000000000000000000000000000">> = V ->
            ct:pal("Found group with authz_id: ~p", [V]),
            ok;
        Bad ->
            erlang:error({unexpected_result, Bad})
    end.



random_bogus_port() ->
    {ok, S} = gen_udp:open(0, [binary, {active, once}]),
    {ok, Port} = inet:port(S),
    gen_udp:close(S),
    Port.

init_pg_db(Config) ->
    Dir = ?config(priv_dir, Config),
    Schema = filename:join([?config(data_dir, Config), "schema.sql"]),
    PgData = filename:join(Dir, "pg_data"),
    PgLog = filename:join(Dir, "pg.log"),
    Port = random_bogus_port(),
    PortStr = integer_to_list(Port),
    CMDS = [
            ["initdb -D", PgData],
            ["pg_ctl -D", PgData, "-l", PgLog, "-o \"-p", PortStr, "\" start"],
            %% db start is async, sleep? :(
            ["sleep 1 && createdb -p", PortStr, "testdb"],
            ["psql -p", PortStr, "testdb", "<", Schema]
           ],
    ct:pal("~s", [run_cmds(CMDS)]),
    [{pg_port, Port}, {pg_data, PgData}, {pg_log, PgLog} | Config ].

stop_pg_db(Config) ->
    PgData = ?config(pg_data, Config),
    CMDS = [
            ["pg_ctl -D", PgData, "-m fast", "stop"]
           ],
    run_cmds(CMDS).

run_cmds(CMDS) ->
    [ begin
          CC = space_join(C),
          [CC, "\n", os:cmd(CC), "\n\n"]
      end || C <- CMDS ].

space_join(L) ->
    [ [Elt, " "] || Elt <- L ].

init_app_config(Config) ->
    set_env(stats_hero, [{estatsd_host, "localhost"},
                         {estatsd_port, random_bogus_port()},
                         {udp_socket_pool_size, 1}]),

    application:set_env(pooler, pools,
                        [[{name, sqerl},
                          {max_count, 2},
                          {init_count, 2},
                          {start_mfa, {sqerl_client, start_link, []}}]]),

    set_env(oc_chef_authz,
            [{authz_service, [{root_url, "http://test-authz-service:2323"},
                              {timeout, 2000},
                              {init_count, 5},
                              {max_count, 50},
                              {ibrowse_options, [{connect_timeout, 5000}]}]},
             {couchdb_host, "localhost"},
             {couchdb_port, 6984},
             {authz_superuser_id, <<"beefbeefbeef">>}]),

    set_env(sqerl,
            [{db_host, "localhost"},
             {db_port, ?config(pg_port, Config)},
             {db_user, os:getenv("USER")},
             {db_pass, "sesame1-ignored"},
             {db_name, "testdb" },
             {idle_check, 10000},
             {prepared_statements, {oc_chef_authz_db, statements, [pgsql]}},
             {column_transforms,
              [{<<"created_at">>, {sqerl_transformers, convert_YMDHMS_tuple_to_datetime}},
               {<<"updated_at">>, {sqerl_transformers, convert_YMDHMS_tuple_to_datetime}}]}]),

    [ ok = ensure_started(A) || A <- needed_apps() ],
    Config.

set_env(App, AppConfig) ->
    [ application:set_env(App, Key, Value) || {Key, Value} <- AppConfig ].

needed_apps() ->
    [ibrowse, pooler, stats_hero, crypto, public_key, ssl, epgsql, sqerl, oc_chef_authz].

stop_apps() ->
    [ application:stop(A) || A <- lists:reverse(needed_apps()) ],
    ok.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        E ->
            E
    end.
