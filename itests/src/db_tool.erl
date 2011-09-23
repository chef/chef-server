-module(db_tool).

-export([truncate_nodes_table/0, connect/0]).

-include_lib("emysql/include/emysql.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(gv(X, L), proplists:get_value(X, L)).

connect() ->
    %% XXX: assume we get run from rebar eunit.
    application:start(emysql),
    {ok, Config} = file:consult("../../rel/vars.config"),
    Host = ?gv(mysql_host, Config),
    Port = ?gv(mysql_port, Config),
    DbUser = ?gv(mysql_user, Config),
    DbPass = ?gv(mysql_pass, Config),
    DbName = ?gv(mysql_db_name, Config),
    error_logger:info_msg("db_tool connecting to MySQL ~s:~b ~s~n",
                          [Host, Port, DbName]),
    emysql:add_pool(db_tool_pool, 10, DbUser, DbPass,
                    Host, Port, DbName, utf8).

truncate_nodes_table() ->
    #ok_packet{} = emysql:execute(db_tool_pool, <<"DELETE FROM nodes">>),
    ok.
