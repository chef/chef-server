-module(chef_wm_pooler_collector).

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   label_pairs/1,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-behaviour(prometheus_collector).

-include_lib("pooler/src/pooler.hrl").

-define(POOLER_IN_USE, erchef_pooler_members_in_use).
-define(POOLER_FREE, erchef_pooler_members_free).
-define(POOLER_MAX, erchef_pooler_members_max).
-define(QUEUED_REQUESTORS, erchef_pooler_queued_requestors).
-define(QUEUED_REQUESTORS_MAX, erchef_pooler_queued_requestors_max).

%%====================================================================
%% Collector API
%%====================================================================

deregister_cleanup(_) -> ok.


collect_mf(_Registry, Callback) ->

    % TODO(jaym) 08-23-17: This sucks, we should expose this in a sane way from pooler. Leaving for now
    %                 as part of the complaint was that chef server should deal with the internals
    %                 of chef server, not some external scripts. We're poking at a lot of the internal
    %                 details of pooler here.
    Pools = [{PoolName, gen_server:call(PoolName, dump_pool)} || PoolName <- monitored_pools()],

    Callback(create_gauge(?POOLER_IN_USE,
                          "Number of pool members currently being used.",
                          Pools)),
    Callback(create_gauge(?POOLER_FREE,
                          "Number of pool members currently available.",
                          Pools)),
    Callback(create_gauge(?POOLER_MAX,
                          "Max number of pool members allowed in the pool.",
                          Pools)),
    Callback(create_gauge(?QUEUED_REQUESTORS,
                          "Number of requestors blocking to take a pool member.",
                          Pools)),
    Callback(create_gauge(?QUEUED_REQUESTORS_MAX,
                          "Max number of requestors allowed to block on taking pool member.",
                          Pools)),
  ok.


collect_metrics(?POOLER_IN_USE, Pools) ->
    [gauge_metric([{pool_name, PoolName}], PoolData#pool.in_use_count) || {PoolName, PoolData} <- Pools];
collect_metrics(?POOLER_FREE, Pools) ->
    [gauge_metric([{pool_name, PoolName}], PoolData#pool.free_count) || {PoolName, PoolData} <- Pools];
collect_metrics(?POOLER_MAX, Pools) ->
    [gauge_metric([{pool_name, PoolName}], PoolData#pool.max_count) || {PoolName, PoolData} <- Pools];
collect_metrics(?QUEUED_REQUESTORS, Pools) ->
    [gauge_metric([{pool_name, PoolName}], queue:len(PoolData#pool.queued_requestors)) || {PoolName, PoolData} <- Pools];
collect_metrics(?QUEUED_REQUESTORS_MAX, Pools) ->
    [gauge_metric([{pool_name, PoolName}], PoolData#pool.queue_max) || {PoolName, PoolData} <- Pools].



create_gauge(Name, Help, Data) ->
    create_mf(Name, Help, gauge, ?MODULE, Data).

monitored_pools() ->
    % TODO(jaym) 08-23-17: Move this out to configuration
    [sqerl, oc_chef_authz_http, chef_index_http, chef_depsolver].
