%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@chef.io>
%% @doc oc_chef_authz's interface to the authz server
%%
%% Copyright 2011-2018 Chef Software, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%


-module(oc_chef_authz_http).

-define(X_OPS_REQUESTER_ID, "X-Ops-Requesting-Actor-Id").
-define(X_OPS_USER, {"X-Ops-User-Id", "front-end-service"}).
-define(IBROWSE_OPTIONS, [{response_format, binary}]).
-define(REQ_ID_HEADER, "X-Request-Id").

-type http_body() :: binary() | [].
-type requestor_id() :: binary().
-type http_method() :: atom().
-type http_path() :: string() | binary().
-type http_headers() :: [{string(), string()}].
-type req_id() :: string() | binary() | undefined.
-type request_fun() :: fun((http_path(), http_method(), http_headers(), http_body()) -> {error, _} | ok | {ok, _}).
-type new_client_fun() :: fun((request_fun()) -> any()).

-export([ping/0,
         request/5,
         request/6,
         request/7,
         with_new_http_client/3,
         with_new_http_client/5,
         create_pool/0,
         delete_pool/0
     ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

%
% Handle raw interaction with authz; set up the headers as expected.
%
-spec request(http_path(),
              http_method(),
              [{string(), string()}],
              http_body(),
              requestor_id()) -> ok | {ok, _} | {error, _}.
request(Path, Method, Headers, Body, RequestorId) ->
    request(undefined, no_pid, Path, Method, Headers, Body, RequestorId).

-spec request(req_id(),
              http_path(),
              http_method(),
              [{string(), string()}],
              http_body(),
              requestor_id()) -> ok | {ok, _} | {error, _}.
request(ReqId, Path, Method, Headers, Body, RequestorId) ->
    request(ReqId, no_pid, Path, Method, Headers, Body, RequestorId).

-spec request(req_id(), pid() | 'no_pid', http_path(), http_method(), [{string(),
              string()}], http_body(), requestor_id()) -> ok | {ok, jiffy:json_value()} | {error, _}.
request(ReqId, _Pid, Path, Method, Headers, Body, RequestorId) ->
    FullHeaders = full_headers(ReqId, RequestorId, Headers),
    AuthzConfig = envy:get(oc_chef_authz, authz_service, list),
    Timeout = proplists:get_value(timeout, AuthzConfig),
    Response = oc_httpc:request(?MODULE, to_str(Path), FullHeaders, Method, Body, Timeout),
    handle_ibrowse_response(Response).

handle_ibrowse_response({ok, Status, _, ResponseBody}) when Status =:= "200";
                                                            Status =:= "201" ->
    handle_response_body(ResponseBody);
handle_ibrowse_response({ok, "204", _H, _B}) ->
    ok;
handle_ibrowse_response({ok, "403", _H, _B}) ->
    {error, forbidden};
handle_ibrowse_response({ok, "404", _H, _B}) ->
    {error, not_found};
handle_ibrowse_response({ok, "500", _H, _B}) ->
    {error, server_error};
handle_ibrowse_response({ok, Status, _H, _B}) ->
    {error, Status};
handle_ibrowse_response({error, _} = Error) ->
    Error.

handle_response_body("{}") ->
    ok;
handle_response_body(<<"{}">>) ->
    ok;
handle_response_body(Body) ->
    {ok, jiffy:decode(Body)}.

full_headers(ReqId, RequestorId, Headers) ->
    Headers1 = add_requestor_id_header(RequestorId, add_req_id_header(ReqId, Headers)),
    [?X_OPS_USER,
     {"Accept", "application/json"},
     {"Content-Type", "application/json"} | Headers1].

add_requestor_id_header(undefined, Headers) ->
    Headers;
add_requestor_id_header(RequestorId, Headers) ->
    [{?X_OPS_REQUESTER_ID, to_str(RequestorId)} | Headers].

add_req_id_header(undefined, Headers) ->
    Headers;
add_req_id_header(ReqId, Headers) ->
    [{?REQ_ID_HEADER, to_str(ReqId)} | Headers].

to_str(S) when is_list(S) ->
    S;
to_str(B) when is_binary(B) ->
    binary_to_list(B).

%% @doc Create a new HTTP connection to the authz service specified in config and execute
%% `Fun'. See {@link with_new_http_client/3} for details.
-spec with_new_http_client(req_id(), requestor_id(), new_client_fun()) -> any().
with_new_http_client(ReqId, RequestorId, Fun) ->
    {RootUrl, Timeout} = authz_url_and_timeout(),
    with_new_http_client(ReqId, RequestorId, RootUrl, Timeout, Fun).

%% Create a new HTTP connection (backed by `ibrowse_http_client') to the authz service at
%% `RootUrl' and execute `Fun'. The function `Fun' will be passed three arguments: the `Pid'
%% of the HTTP connection, `RootUrl', and `Timeout'. The HTTP connection will be closed and
%% the result of `Fun' returned.
-spec with_new_http_client(req_id(),
                           requestor_id(),
                           string(),
                           non_neg_integer(),
                           new_client_fun()) -> any().
with_new_http_client(ReqId, RequestorId, _RootUrl, Timeout, Fun) ->
    DecoratedFun = fun(RealRequestFun) ->
                           DecoratedRequestFun = make_request_fun(ReqId, ignored, "", Timeout, RequestorId, RealRequestFun),
                           Fun(DecoratedRequestFun)
                           end,
    oc_httpc:multi_request(?MODULE, DecoratedFun, Timeout).

make_request_fun(ReqId, _Pid, _RawRootUrl, _Timeout, RequestorId, RealRequestFun) ->
    fun(Path, Method, Headers, Body) ->
            FullHeaders = full_headers(ReqId, RequestorId, Headers),
            Resp = RealRequestFun(Path, FullHeaders, Method, Body),
            handle_ibrowse_response(Resp)
    end.


-spec ping() -> pong | pang.
ping() ->
    {AuthzUrl, Timeout} = authz_url_and_timeout(),
    ReqId = "ping-request",
    RequestorId = <<"ping-requestor">>,
    with_new_http_client(ReqId, RequestorId, AuthzUrl, Timeout, fun ping/1).

ping(RequestFun) ->
    try
        Url = "/_ping",
        Headers = [{"Accept", "application/json"}],
        case RequestFun(Url, get, Headers, []) of
            {ok, _} ->
                pong;
            ok ->
                %% in case upstream status endpoint returns "{}"
                pong;
            {error, Reason} ->
                error_logger:error_report({oc_chef_authz_http_ping, pang, Reason}),
                pang
        end
    catch
        _How:Error ->
            error_logger:error_report({oc_chef_authz_http_ping, pang, Error}),
            pang
    end.

authz_url_and_timeout() ->
    Config = envy:get(oc_chef_authz, authz_service, list),
    Url = proplists:get_value(root_url, Config),
    Timeout = proplists:get_value(timeout, Config),
    {Url, Timeout}.

create_pool() ->
    Pools = get_pool_configs(),
    [oc_httpc:add_pool(PoolNameAtom, Config) || {PoolNameAtom, Config} <- Pools, Config /= []],
    ok.

delete_pool() ->
    Pools = get_pool_configs(),
    [ok = oc_httpc:delete_pool(PoolNameAtom) || {PoolNameAtom, _Config} <- Pools],
    ok.

get_pool_configs() ->
    Config = envy:get(oc_chef_authz, authz_service, [], any),
    [{?MODULE, Config}].
