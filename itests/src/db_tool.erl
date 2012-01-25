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
    {ok, DbConfig} = file:consult("../../rel/db_vars.config"),
    {ok, Config} = file:consult("../../rel/vars.config"),
    Host = ?gv(db_host, Config),
    Port = ?gv(db_port, DbConfig),
    DbUser = ?gv(db_user, DbConfig),
    DbPass = ?gv(db_pass, DbConfig),
    DbName = ?gv(db_name, DbConfig),
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
