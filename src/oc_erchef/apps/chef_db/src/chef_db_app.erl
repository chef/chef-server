%%%-------------------------------------------------------------------
%%% @copyright (C) 2017 Chef Software, Inc
%%%-------------------------------------------------------------------
-module(chef_db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% make this "start properly" for eunit tests, without starting any sqerl pools.
-ifndef(TEST).
start(_StartType, _StartArgs) ->
    set_sqerl_environment(),
    pooler:new_pool(sqerl_pool_config()).
-else.
start(_StartType, _StartArgs) ->
    {ok, self()}.
-endif.

sqerl_pool_config() ->
    MaxCount  = envy:get(chef_db, sql_pool_max_count, pos_integer),
    InitCount = envy:get(chef_db, sql_pool_init_count, pos_integer),
    QueueMax  = envy:get(chef_db, sql_pool_queue_max, pos_integer),
    [{name, sqerl},
     {max_count, MaxCount},
     {init_count, InitCount},
     {start_mfa, {sqerl_client, start_link, []}},
     {queue_max, QueueMax}].

set_sqerl_environment() ->
    {ok, Password} = chef_secrets:get(<<"opscode_erchef">>, <<"sql_password">>),
    application:set_env(sqerl, db_pass, erlang:binary_to_list(Password)).

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
