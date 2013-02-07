%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @doc Supervisor for eredis clients.
-module(oc_chef_wm_eredis_sup).

-behaviour(supervisor).

-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ok = pg2:create(redis_search_cache),
    init_metrics(),
    %% always start immediately with an empty child list. Children are added async by
    %% oc_chef_wm_eredis_starter which should be placed after this supervisor in the
    %% top-level supervision tree.
    {ok, {{one_for_one, 60, 300}, []}}.

init_metrics() ->
    folsom_metrics:new_meter(search_cache_hit),
    folsom_metrics:new_meter(search_cache_miss),
    folsom_metrics:new_meter(search_cache_get),
    folsom_metrics:new_meter(search_cache_put),
    folsom_metrics:new_meter(search_cache_error),
    folsom_metrics:new_meter(search_cache_no_redis),
    folsom_metrics:new_meter(search_cache_no_cache_allowed),
    folsom_metrics:new_histogram(search_cache_redis_get),
    folsom_metrics:new_histogram(search_cache_redis_set),
    folsom_metrics:new_histogram(search_cache_redis_member),
    ok.
