-module(db_tool).

-export([truncate_nodes_table/0,
         connect/0,
         metadata_for_node/1]).

-include_lib("emysql/include/emysql.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(gv(X, L), proplists:get_value(X, L)).

-define(fetch_node_metadata,
        <<"SELECT `last_updated_by`, `created_at`, `updated_at` FROM nodes "
          "WHERE (`name` = ?) LIMIT 1">>).

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
                    Host, Port, DbName, utf8),
    emysql:prepare(fetch_node_metadata, ?fetch_node_metadata).

truncate_nodes_table() ->
    #ok_packet{} = emysql:execute(db_tool_pool, <<"DELETE FROM nodes">>),
    ok.

%% XXX: find just by node name to avoid having to obtain org id.
metadata_for_node(Name) ->
    case emysql:execute(db_tool_pool, fetch_node_metadata, [Name]) of
        #result_packet{rows = []} -> not_found;
        #result_packet{rows = [[Id, {datetime, Created}, {datetime, Updated}]]} ->
            [{last_updated_by, Id},
             {created_at, Created},
             {updated_at, Updated}];
        Else ->
            {error, Else}
    end.
