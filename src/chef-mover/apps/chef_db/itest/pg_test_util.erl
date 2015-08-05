-module(pg_test_util).

-export([
         random_bogus_port/0,
         init_pg_db/4,
         stop_pg_db/1
        ]).

%% @doc Start a new postgres "cluster" with data directory `Dir',
%% initialized with schema found in the SQL file located at
%% `Schema'. The postgres db will listen on port `Port'.
init_pg_db(Dir, DbName, Schema, Port) ->
    PgData = filename:join(Dir, "pg_data"),
    PgLog = filename:join(Dir, "pg.log"),
    PortStr = integer_to_list(Port),
    CMDS = [
            ["initdb -D", PgData],
            ["pg_ctl -D", PgData, "-l", PgLog, "-o \"-p", PortStr, "\" start"],
            %% db start is async, sleep! :(
            ["sleep 1 && createdb -p", PortStr, DbName],
            ["psql -p", PortStr, DbName, "<", Schema]
           ],
    error_logger:info_msg("init_pg_db:~n~s~n", [run_cmds(CMDS)]),
    [{pg_name, DbName}, {pg_port, Port}, {pg_data, PgData}, {pg_log, PgLog}].

%% @doc Fast stop the posgres cluster running on the data directory
%% `Dir'.
stop_pg_db(Dir) ->
    CMDS = [
            ["pg_ctl -D", Dir, "-m fast", "stop"]
           ],
    error_logger:info_msg("stop_pg_db:~n~s~n", [run_cmds(CMDS)]),
    ok.

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
