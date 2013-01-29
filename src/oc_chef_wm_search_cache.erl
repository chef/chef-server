%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et

-module(oc_chef_wm_search_cache).

-export([
         add_no_cache_org/1,
         get/2,
         make_key/6,
         no_cache_orgs/0,
         no_cache_org/1,
         put/3,
         remove_no_cache_org/1
        ]).

-define(NO_CACHE_ORGS_KEY, <<",,NO_CACHE_ORGS">>).

%% @doc Fetch an entry from the search cache.
-spec get(_, {binary(), binary()}) -> not_found | {integer(), binary()}.
get(ReqId, {OrgName, _} = Key) ->
    folsom_metrics:notify({search_cache_get, 1}),
    Redis = redis_client(),
    get(caching_allowed(Redis, OrgName), ReqId, Redis, Key).

get(no_cache, _ReqId, _Redis, _Key) ->
    not_found;
get(_, _ReqId, no_redis, _Key) ->
    not_found;
get(cache, _ReqId, Redis, Key) ->
    Fun = fun() -> eredis:q(Redis, ["GET", key_to_bin(Key)]) end,
    case folsom_time(search_cache_redis_get, Fun) of
        {ok, undefined} ->
            folsom_metrics:notify({search_cache_miss, 1}),
            not_found;
        {ok, Value} ->
            folsom_metrics:notify({search_cache_hit, 1}),
            {Count, Gzip} = binary_to_term(Value),
            {Count, zlib:gunzip(Gzip)}
    end.

%% @doc Add entry to the search cache
-spec put(_, {binary(), binary()}, {integer(), binary()}) -> ok | error.
put(ReqId, {OrgName, _} = Key, {Count, Raw}) ->
    folsom_metrics:notify({search_cache_put, 1}),
    Redis = redis_client(),
    put(caching_allowed(Redis, OrgName), ReqId, Redis, Key, {Count, Raw}).

put(no_cache, _ReqId, _Redis, _Key, _Value) ->
    ok;
put(_, _ReqId, no_redis, _Key, _Value) ->
    ok;
put(cache, _ReqId, Redis, Key, {Count, Raw}) ->
    Gzip = zlib:gzip(Raw),
    Value = term_to_binary({Count, Gzip}),
    Cmd = ["SETEX", key_to_bin(Key), integer_to_list(cache_entry_ttl()), Value],
    Fun = fun() -> eredis:q(Redis, Cmd) end,
    case folsom_time(search_cache_redis_set, Fun) of
        {ok, <<"OK">>} ->
            ok;
        _ ->
            error
    end.

folsom_time(Label, Fun) ->
    folsom_metrics:histogram_timed_update(Label, Fun).

%% @doc Return a list of org names that are not allowed to be cached
no_cache_orgs() ->
    case redis_client() of
        no_redis ->
            no_redis;
        Redis ->
            eredis:q(Redis, ["SMEMBERS", ?NO_CACHE_ORGS_KEY])
    end.

%% @doc Determines if `OrgName' is allowed to be cached. Returns `cache' or `no_cache' for a
%% given org name.
no_cache_org(OrgName) ->
    caching_allowed(redis_client(), OrgName).

%% @doc Disables caching for the org `OrgName' by adding `OrgName' to the list of orgs not
%% allowed to be cached.
add_no_cache_org(OrgName) ->
    case redis_client() of
        no_redis ->
            no_redis;
        Redis ->
            Cmd = ["SADD", ?NO_CACHE_ORGS_KEY, OrgName],
            eredis:q(Redis, Cmd)
    end.

%% @doc Enables caching for the org `OrgName' by removing `OrgName' from the list of orgs
%% not allowed to be cached.
remove_no_cache_org(OrgName) ->
    case redis_client() of
        no_redis ->
            no_redis;
        Redis ->
            Cmd = ["SREM", ?NO_CACHE_ORGS_KEY, OrgName],
            eredis:q(Redis, Cmd)
    end.

%% @doc Return the TTL in seconds for entries in the search
%% cache. Will be used with Redis' EXPIRE command to set the
%% expiration for keys stored with SET.
-spec cache_entry_ttl() -> non_neg_integer().
cache_entry_ttl() ->
    case application:get_env(oc_chef_wm, search_cache_entry_ttl) of
        undefined ->
            %% default to 60 seconds
            60;
        {ok, Seconds} when is_integer(Seconds) ->
            Seconds
    end.

%% @doc Return the cache key for specified inputs that describe a search
%% request. The key combines request attributes and solr results to identify
%% unique search queries. The `Ids' are those returned from Solr. `ReqPath'
%% should be the raw HTTP request path including query parameters. `Path' is the
%% list of partial search paths or empty list. This function returns a tuple
%% `{OrgName, Digest}' so that {@link get/2} and {@link put/2} have easy access
%% to the org name to implement black lists for caching.
-spec make_key(binary(), integer(), integer(),
               [binary()], string(), [{binary(), [binary()]}]) -> {binary(), binary()}.
make_key(OrgName, BatchSize, Start, Ids, ReqPath, Paths) ->
    ToDigest = [lists:sort(Ids), BatchSize, Start, ReqPath, Paths],
    {OrgName, digest(ToDigest)}.

%% Produce the binary form of a cache key for use with Redis
key_to_bin({OrgName, Digest}) ->
    iolist_to_binary([OrgName, <<"|">>, Digest]).

digest(List) ->
    sha_to_hex(crypto:sha(term_to_binary(List))).

sha_to_hex(<<X:160/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~40.16.0b", [X])).

%% Obtain a non-exclusive client from the redis client "pool". If
%% there are no clients or some other error occurs, returns
%% `no_redis'.
redis_client() ->
    case pg2:get_closest_pid(redis_search_cache) of
        {error, _} ->
            folsom_metrics:notify({search_cache_no_redis, 1}),
            no_redis;
        Pid when is_pid(Pid) ->
            Pid
    end.

caching_allowed(no_redis, _OrgName) ->
    no_cache;
caching_allowed(Redis, OrgName) ->
    Cmd = ["SISMEMBER", ?NO_CACHE_ORGS_KEY, OrgName],
    Fun = fun() -> eredis:q(Redis, Cmd) end,
    case folsom_time(search_cache_redis_member, Fun) of
        {ok, <<"1">>} ->
            folsom_metrics:notify({search_cache_no_cache_allowed, 1}),
            no_cache;
        {ok, <<"0">>} ->
            cache
    end.
