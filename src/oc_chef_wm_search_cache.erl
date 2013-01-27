%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et

-module(oc_chef_wm_search_cache).

-export([
         get/2,
         make_key/6,
         put/3
        ]).

%% @doc Fetch an entry from the search cache.
-spec get(_, binary()) -> not_found | {integer(), binary()}.
get(ReqId, Key) ->
    folsom_metrics:notify({search_cache_get, 1}),
    get(ReqId, redis_client(), Key).

get(_ReqId, no_redis, _Key) ->
    not_found;
get(_ReqId, Redis, Key) ->
    case eredis:q(Redis, ["GET", Key]) of
        {ok, undefined} ->
            folsom_metrics:notify({search_cache_miss, 1}),
            not_found;
        {ok, Value} ->
            folsom_metrics:notify({search_cache_hit, 1}),
            {Count, Gzip} = binary_to_term(Value),
            {Count, zlib:gunzip(Gzip)}
    end.

%% @doc Add entry to the search cache
-spec put(_, binary(), {integer(), binary()}) -> ok | error.
put(ReqId, Key, {Count, Raw}) ->
    folsom_metrics:notify({search_cache_put, 1}),
    put(ReqId, redis_client(), Key, {Count, Raw}).

put(_ReqId, no_redis, _Key, _Value) ->
    ok;
put(_ReqId, Redis, Key, {Count, Raw}) ->
    Gzip = zlib:gzip(Raw),
    Value = term_to_binary({Count, Gzip}),
    Cmd = ["SETEX", Key, integer_to_list(cache_entry_ttl()), Value],
    case eredis:q(Redis, Cmd) of
        {ok, <<"OK">>} ->
            ok;
        _ ->
            error
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

%% @doc Return the cache key for specified inputs that describe a
%% search request. The key combines request attributes and solr
%% results to identify unique search queries. The `Ids' are those
%% returned from Solr. `ReqPath' should be the raw HTTP request path
%% including query parameters. `Path' is the list of partial search
%% paths or empty list.
-spec make_key(binary(), integer(), integer(),
               [binary()], string(), [{binary(), [binary()]}]) -> binary().
make_key(OrgName, BatchSize, Start, Ids, ReqPath, Paths) ->
    ToDigest = [lists:sort(Ids), BatchSize, Start, ReqPath, Paths],
    iolist_to_binary([OrgName, "|", digest(ToDigest)]).

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
            no_redis;
        Pid when is_pid(Pid) ->
            Pid
    end.
