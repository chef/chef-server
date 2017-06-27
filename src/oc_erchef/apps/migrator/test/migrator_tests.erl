-module(migrator_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% These are to be IN DVM's /host/src/oc_erchef via:
%%   veil-env-helper --use-file -s postgresql.db_superuser_password -- ./rebar3 eunit -s migrator_tests

compare_tables_test_() ->
    application:set_env(chef_secrets, provider, chef_secrets_fd),
    application:ensure_all_started(chef_secrets),

    [{ lists:flatten(io_lib:format("table ~s has the same content", [Table])),
       fun() ->
           RowsFromSource = fetch_all("opscode_chef", Table),
           RowsFromSink = fetch_all("opscode_chef_target", Table),
           ?assertEqual(RowsFromSource, RowsFromSink)
       end
     } || Table <- [ users, keys, clients ]
    ].

fetch_all(Database, Table) ->
    {ok, Password} = chef_secrets:get(<<"postgresql">>, <<"db_superuser_password">>),
    Query = <<"SELECT * FROM ", (erlang:atom_to_binary(Table, latin1))/binary, " ORDER BY id">>,
    {ok, Conn} = epgsql:connect("127.0.0.1", "opscode-pgsql", Password, [{database, Database}]),

    {ok, _Fields, Data} = epgsql:squery(Conn, Query),
    Data.
