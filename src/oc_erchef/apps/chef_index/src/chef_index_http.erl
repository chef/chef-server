-module(chef_index_http).

-export([
         request/3,
         create_pool/0,
         delete_pool/0
        ]).

-define(HEADERS, [{"Content-Type", "application/xml"}]).

request(Path, Method, Body) ->
    SolrConfig = envy:get(chef_index, solr_service, list),
    Timeout = proplists:get_value(timeout, SolrConfig),
    oc_httpc:request(?MODULE, Path, ?HEADERS, Method, Body, Timeout).

create_pool() ->
    Pools = get_pool_configs(),
    [oc_httpc:add_pool(PoolNameAtom, Config) || {PoolNameAtom, Config} <- Pools],
    ok.

delete_pool() ->
    Pools = get_pool_configs(),
    [ok = oc_httpc:delete_pool(PoolNameAtom) || {PoolNameAtom, _Config} <- Pools],
    ok.

get_pool_configs() ->
    Config = envy:get(chef_index, solr_service, [], any),
    [{?MODULE, Config}].
