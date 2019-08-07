-module(data_collector_http).

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

-define(DEFAULT_HEADERS, [{"Content-Type", "application/json"}]).

default_headers() ->
    maybe_add_data_collector_token_header(?DEFAULT_HEADERS).

maybe_add_data_collector_token_header(Headers) ->
    case data_collector:token() of
        undefined ->
            Headers;
        Token when is_list(Token) ->
            [{"x-data-collector-token", Token} | Headers]
    end.

request(Path, Method, Body) ->
    request(Path, Method, Body, default_headers()).

request(Path, Method, Body, Headers) ->
    {ok, Timeout} = application:get_env(data_collector, timeout),
    oc_httpc:request(?MODULE, Path, Headers, Method, Body, Timeout).

%%
%% Simple helpers for requets when you only
%% care about success or failure.
%%
-spec get(list()) -> ok | {error, term()}.
get(Path) ->
    get(Path, [], default_headers()).

-spec get(list(), iolist() | binary()) -> ok | {error, term()}.
get(Path, Body) ->
    get(Path, Body, default_headers()).

-spec get(list(), iolist() | binary(), list()) -> ok | {error, term()}.
get(Path, Body, Headers) ->
    request_with_caught_errors(Path, get, Body, Headers).

-spec post(list(), iolist() | binary()) -> ok | {error, term()}.
post(Path, Body) ->
    post(Path, Body, []).

-spec post(list(), iolist() | binary(), list()) -> ok | {error, term()}.
post(Path, Body, Headers) ->
    request_with_caught_errors(Path, post, Body,
                               lists:append(default_headers(), Headers)).

-spec delete(list(), iolist() | binary()) -> ok | {error, term()}.
delete(Path, Body) ->
    delete(Path, Body, default_headers()).

-spec delete(list(), iolist() | binary(), list()) -> ok | {error, term()}.
delete(Path, Body, Headers) ->
    request_with_caught_errors(Path, delete, Body, Headers).

request_with_caught_errors(Path, Method, Body, Headers) when is_list(Body)->
    request_with_caught_errors(Path, Method, iolist_to_binary(Body), Headers);
request_with_caught_errors(Path, Method, Body, Headers) ->
    try
        case request(Path, Method, Body, Headers) of
            {ok, [$2|_], _Head, _RespBody} -> ok;
            Error -> {error, Error}
        end
    catch
        How:Why ->
            error_logger:error_report({?MODULE, Method, How, Why}),
            {error, Why}
    end.

%%
%% Pool management functions
%%
-spec create_pool() -> ok.
create_pool() ->
    lager:info("Creating Data Collector HTTP pool"),
    oc_httpc:add_pool(?MODULE, application:get_all_env()),
    ok.

-spec delete_pool() -> ok.
delete_pool() ->
    lager:info("Removing Data Collector HTTP pool"),
    oc_httpc:delete_pool(?MODULE),
    ok.
