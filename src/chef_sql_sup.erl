%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright 2011 Opscode, Inc.
-module(chef_sql_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         new_connection/5]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

new_connection(Host, Port, User, Pass, Database) ->
    supervisor:start_child(?SERVER, [Host, Port, User, Pass, Database]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, DbFlavor} = application:get_env(chef_common, db_type),
    ConnSpec = case DbFlavor of
                   mysql ->
                       {undefined, {chef_mysql_client, start_link, []},
                        temporary, 10000, worker, [chef_mysql_client]};
                   pgsql ->
                       {undefined, {chef_pgsql_client, start_link, []},
                        temporary, 10000, worker, [chef_pgsql_client]}
               end,
    {ok, {{simple_one_for_one, 10, 10}, [ConnSpec]}}.
