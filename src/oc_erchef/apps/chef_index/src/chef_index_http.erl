-module(chef_index_http).

-export([
         request/3,
         request/4,
         get/1,
         get/2,
         get/3,
         post/2,
         post/3,
         delete/2,
         delete/3,
         create_pool/0,
         delete_pool/0
        ]).

-define(DEFAULT_HEADERS, [{"Content-Type", "application/xml"}]).

request(Path, Method, Body) ->
    request(Path, Method, Body, ?DEFAULT_HEADERS).

request(Path, Method, Body, Headers) ->
    SolrConfig = envy:get(chef_index, solr_service, list),
    Timeout = proplists:get_value(timeout, SolrConfig),
    oc_httpc:request(?MODULE, Path, Headers, Method, Body, Timeout).



%%
%% Simple helpers for requets when you only
%% care about success or failure.
%%
-spec get(list()) -> ok | {error, term()}.
get(Url) ->
    get(Url, [], ?DEFAULT_HEADERS).

-spec get(list(), iolist() | binary()) -> ok | {error, term()}.
get(Url, Body) ->
    get(Url, Body, ?DEFAULT_HEADERS).

-spec get(list(), iolist() | binary(), list()) -> ok | {error, term()}.
get(Url, Body, Headers) when is_list(Body) ->
    get(Url, iolist_to_binary(Body), Headers);
get(Url, Body, Headers) ->
    request_with_caught_errors(Url, get, Body, Headers).


-spec post(list(), iolist() | binary()) -> ok | {error, term()}.
post(Url, Body) ->
    post(Url, Body, ?DEFAULT_HEADERS).

-spec post(list(), iolist() | binary(), list()) -> ok | {error, term()}.
post(Url, Body, Headers) when is_list(Body) ->
    post(Url, iolist_to_binary(Body), Headers);
post(Url, Body, Headers) ->
    request_with_caught_errors(Url, post, Body, Headers).

-spec delete(list(), iolist() | binary()) -> ok | {error, term()}.
delete(Url, Body) ->
    delete(Url, Body, ?DEFAULT_HEADERS).

-spec delete(list(), iolist() | binary(), list()) -> ok | {error, term()}.
delete(Url, Body, Headers) when is_list(Body) ->
    delete(Url, iolist_to_binary(Body), Headers);
delete(Url, Body, Headers) ->
    request_with_caught_errors(Url, delete, Body, Headers).

request_with_caught_errors(Url, Method, Body, Headers) ->
    try
        case request(Url, Method, Body, Headers) of
            {ok, "200", _Head, _RespBody} -> ok;
            Error -> {error, Error}
        end
    catch
        How:Why ->
            error_logger:error_report({chef_index_http, Method, How, Why}),
            {error, Why}
    end.

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
