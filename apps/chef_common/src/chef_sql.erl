-module(chef_sql).

-export([fetch_user/1,
         connect/0]).

-include("chef_sql.hrl").
-include_lib("emysql/include/emysql.hrl").

-define(fetch_user_sql,
        iolist_to_binary(["SELECT `id`, `authz_id`, `username`, ",
                          "`pubkey_version`, `public_key` ",
                          "FROM `users` WHERE (`username` = ?) LIMIT 1"])).

%% @doc Establish a connection pool to the database.
%% 
%% Uses database connection parameters set in app.config
%% 
connect() ->
    {ok, Host} = application:get_env(chef_common, mysql_host),
    {ok, Port} = application:get_env(chef_common, mysql_port),
    {ok, DbUser} = application:get_env(chef_common, mysql_user),
    {ok, DbPass} = application:get_env(chef_common, mysql_pass),
    {ok, DbName} = application:get_env(chef_common, mysql_db_name),
    {ok, PoolSize} = application:get_env(chef_common, mysql_pool_size),
    emysql:add_pool(erchef_pool, PoolSize, DbUser, DbPass,
                    Host, Port, DbName, utf8).

-spec fetch_user(string()) -> #chef_user{} | not_found | {error, term()}.
%% @doc Return user data for the given username
%% 
%%
fetch_user(UserName) ->
    emysql:prepare(fetch_user_stmt, ?fetch_user_sql),
    case emysql:execute(erchef_pool, fetch_user_stmt, [UserName]) of
        Result = #result_packet{rows = Rows} ->
            Recs = emysql_util:as_record(Result, chef_user,
                                         record_info(fields, chef_user)),
            case Recs of
                [] ->
                    not_found;
                [User] -> User
            end;
        Else ->
            {error, Else}
    end.
