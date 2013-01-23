%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @doc Supervisor for eredis clients.
-module(oc_chef_wm_eredis_sup).

-behaviour(supervisor).

-export([start_link/0,
         eredis_start_wrapper/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, ClientCount} = application:get_env(oc_chef_wm, eredis_client_pool_size),
    error_logger:info_msg("starting oc_chef_wm_eredis_sup with ~B senders~n", [ClientCount]),
    ok = pg2:create(redis_search_cache),
    init_metrics(),
    {ok, Host} = application:get_env(oc_chef_wm, redis_host),
    {ok, Port} = application:get_env(oc_chef_wm, redis_port),
    {ok, RedisDb} = application:get_env(oc_chef_wm, redis_db),
    StartUp = {?MODULE, eredis_start_wrapper, [Host, Port, RedisDb]},
    Children = [ {make_id(I), StartUp, permanent, brutal_kill, worker, [eredis]}
                 || I <- lists:seq(1, ClientCount) ],
    {ok, {{one_for_one, 60, 10}, Children}}.

make_id(I) ->
    "eredis_" ++ integer_to_list(I).

%% @doc Wrapper around `eredis:start_link/3` that registers the
%% created client with pg2.
eredis_start_wrapper(Host, Port, RedisDb) ->
    case eredis:start_link(Host, Port, RedisDb) of
        {ok, Pid} ->
            ok = pg2:join(redis_search_cache, Pid),
            {ok, Pid};
        Error ->
            Error
    end.

init_metrics() ->
    folsom_metrics:new_meter(search_cache_hit),
    folsom_metrics:new_meter(search_cache_miss),
    folsom_metrics:new_meter(search_cache_get),
    folsom_metrics:new_meter(search_cache_put),
    ok.
