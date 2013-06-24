%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @doc oc_chef_authz's interface to the authz server
%%
%% Copyright 2011-2013 Opscode, Inc. All Rights Reserved.
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

-define(x_ops_requester_id, "X-Ops-Requesting-Actor-Id").
-define(x_ops_user, {"X-Ops-User-Id", "front-end-service"}).
-define(IBROWSE_OPTIONS, [{response_format, binary}]).
-define(REQ_ID_HEADER, "X-Request-Id").

-type http_body() :: binary() | pid() | [].
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
         request/8,
         with_new_http_client/3,
         with_new_http_client/5]).

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

-spec request(req_id(),
              pid() | 'no_pid',
              http_path(),
              http_method(),
              [{string(), string()}],
              http_body(),
              requestor_id()) -> ok | {ok, _} | {error, _}.
request(ReqId, Pid, Path, Method, Headers, Body, RequestorId) ->
    FullHeaders = full_headers(ReqId, RequestorId, Headers),
    {ok, AuthzConfig} = application:get_env(oc_chef_authz, authz_service),
    AuthzHost = proplists:get_value(root_url, AuthzConfig),
    Timeout = proplists:get_value(timeout, AuthzConfig),
    Url = AuthzHost ++ "/" ++ to_str(Path),
    Response = ibrowse_request(Pid, Url, FullHeaders, Method, Body,
                               ?IBROWSE_OPTIONS, Timeout),
    handle_ibrowse_response(Response).

%% @doc Execute request on the HTTP connection managed by `Pid'. Since `Pid' encapsulates
%% the host and port, the `Path' is used as-is. When making a series of requests against the
%% same connection, this avoids querying for the root URL and timeout repeatedly.
-spec request(req_id(),
              pid(),
              http_path(),
              http_method(),
              [{string(), string()}],
              http_body(),
              requestor_id(),
              non_neg_integer()) -> ok | {ok, _} | {error, _}.
request(ReqId, Pid, Path, Method, Headers, Body, RequestorId, Timeout) when is_pid(Pid) ->
    FullHeaders = full_headers(ReqId, RequestorId, Headers),
    Response = ibrowse_request(Pid, to_str(Path), FullHeaders, Method, Body,
                               ?IBROWSE_OPTIONS, Timeout),
    handle_ibrowse_response(Response).

ibrowse_request(no_pid, Url, Headers, Method, Body, Options, Timeout) ->
    ibrowse:send_req(Url, Headers, Method, Body, Options, Timeout);
ibrowse_request(Pid, Url, Headers, Method, Body, Options, Timeout) ->
    ibrowse:send_req_direct(Pid, Url, Headers, Method, Body, Options, Timeout).


handle_ibrowse_response({ok, Status, _, ResponseBody}) when Status =:= "200";
                                                            Status =:= "201" ->
    handle_response_body(ResponseBody);
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
    [?x_ops_user,
     {"Accept", "application/json"},
     {"Content-Type", "application/json"} | Headers1].

add_requestor_id_header(undefined, Headers) ->
    Headers;
add_requestor_id_header(RequestorId, Headers) ->
    [{?x_ops_requester_id, to_str(RequestorId)} | Headers].

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
with_new_http_client(ReqId, RequestorId, RootUrl, Timeout, Fun) ->
    case spawn_http_client(RootUrl) of
        {ok, Pid} ->
            try
                RequestFun = make_request_fun(ReqId, Pid, RootUrl, Timeout, RequestorId),
                Fun(RequestFun)
            after
                stop_http_client(Pid)
            end;
        {error, Why} ->
            {error, Why}
    end.

%% @doc Create a new HTTP connection to the upstream authz service located at
%% `RootUrl'. When finished making requests, call {@link stop_http_client/1} to cleanup.
spawn_http_client(RootUrl) ->
    ibrowse_http_client:start_link(RootUrl).

%% @doc Close and stop an HTTP connection started via {@link spawn_http_client/0}. Allows
%% 200 ms for a clean shutdown before sending a kill message.
stop_http_client(Pid) ->
    case catch gen_server:call(Pid, stop, 200) of
        {'EXIT', {timeout, _}} ->
            exit(Pid, kill),
            ok;
        _ ->
            ok
    end.

make_request_fun(ReqId, Pid, RawRootUrl, Timeout, RequestorId) ->
    RootUrl = enforce_trailing_slash(RawRootUrl),
    fun(Path, Method, Headers, Body) ->
            FullHeaders = full_headers(ReqId, RequestorId, Headers),
            FullPath = join_url(RootUrl, to_str(Path)),
            Resp = ibrowse_request(Pid, FullPath, FullHeaders, Method, Body,
                                   ?IBROWSE_OPTIONS, Timeout),
            handle_ibrowse_response(Resp)
    end.

enforce_trailing_slash(S) ->
    Rev = lists:reverse(S),
    case Rev of
        [$/ | _Rest] ->
            S;
        RevNoSlash ->
            lists:reverse([$/ | RevNoSlash])
    end.

%% Join `Root' to `Path'. NOTE: assumes `Root' has a trailing slash.
join_url(Root, [$/ | JoinPath]) ->
        Root ++ JoinPath;
join_url(Root, JoinPath) ->
    Root ++ JoinPath.

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
            {ok, "200", _H, _B} ->
                pong;
            {ok, Status, _H, _B} ->
                error_logger:error_report({oc_chef_authz_http_ping, pang, Status}),
                pang;
            {error, Why} ->
                error_logger:error_report({oc_chef_authz_http_ping, pang, Why}),
                pang
        end
    catch
        _How:Error ->
            error_logger:error_report({oc_chef_authz_http_ping, pang, Error}),
            pang
    end.

authz_url_and_timeout() ->
    {ok, Config} = application:get_env(oc_chef_authz, authz_service),
    Url = proplists:get_value(root_url, Config),
    Timeout = proplists:get_value(timeout, Config),
    {Url, Timeout}.
