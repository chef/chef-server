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
    ClientCount = get_env(eredis_client_pool_size, 0),
    ok = pg2:create(redis_search_cache),
    init_metrics(),
    case ClientCount of
        0 ->
            error_logger:info_msg("search cache DISABLED~n"),
            {ok, {{one_for_one, 60, 10}, []}};
        _ ->
            error_logger:info_msg("search cache ENABLED with ~B senders~n", [ClientCount]),
            Host = get_env(redis_host),
            Port = get_env(redis_port),
            RedisDb = get_env(redis_db, 0),
            StartUp = {?MODULE, eredis_start_wrapper, [Host, Port, RedisDb]},
            Children = [ {make_id(I), StartUp, permanent, brutal_kill, worker, [eredis]}
                         || I <- lists:seq(1, ClientCount) ],
            {ok, {{one_for_one, 60, 10}, Children}}
    end.

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
    folsom_metrics:new_meter(search_cache_no_redis),
    folsom_metrics:new_meter(search_cache_no_cache_allowed),
    folsom_metrics:new_histogram(search_cache_redis_get),
    folsom_metrics:new_histogram(search_cache_redis_set),
    folsom_metrics:new_histogram(search_cache_redis_member),
    ok.

get_env(Key, Default) ->
    case application:get_env(oc_chef_wm, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.

get_env(Key) ->
    case application:get_env(oc_chef_wm, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            error({required_config_missing, {oc_chef_wm, Key}})
    end.
