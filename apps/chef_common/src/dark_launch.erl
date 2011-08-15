-module(dark_launch).

-export([is_enabled/1]).

is_enabled("sql_users") ->
    {ok, SQLUsers} = application:get_env(chef_common, dark_launch_sql_users),
    SQLUsers.
