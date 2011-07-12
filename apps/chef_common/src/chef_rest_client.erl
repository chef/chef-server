%
% License:: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% @author Mark Anderson <mark@opscode.com>
% @author Kevin Smith <kevin@opscode.com>
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.0.1
% @doc Helper module for calling various Chef REST endpoints
% @end
-module(chef_rest_client).

-include_lib("chef_common/include/chef_rest_client.hrl").

-export([request/2,
         make_webui_account_chef_rest_client/1]).

-spec make_webui_account_chef_rest_client(string()) -> #chef_rest_client{}.

make_webui_account_chef_rest_client(UserName) when is_list(UserName) ->
    {ok, BaseUrl} = application:get_env(chef_common, account_api_url),
    {ok, PrivateKey} = chef_keyring:get_key(webui),
    #chef_rest_client{base_url = BaseUrl,
                      user_name = UserName,
                      private_key = PrivateKey,
                      request_source = web}.

-spec request(#chef_rest_client{}, string()) ->
    {ok, term()} | {error, {string(), string() | binary()}} | {error, term()}.

request(ChefClient = #chef_rest_client{}, Path) ->
    case do_chef_get(ChefClient, Path) of
        {ok, "200", _Headers, Json} ->
            {ok, ejson:decode(Json)};
        %% Treat all other response codes as errors
        {ok, Code, _Headers, Body} ->
            {error, {Code, Body}};
        Error ->
            {error, Error}
    end.

%% Internal functions
do_chef_get(#chef_rest_client{base_url = BaseUrl,
                              user_name = UserName,
                              private_key = PrivateKey,
                              request_source = RequestSource},
            Path) ->
    Url = BaseUrl ++ Path,
    ExtraHeaders = case RequestSource of
                       web ->
                           [{"x_ops_request_source", "web"}];
                       user ->
                           []
                   end,
    do_chef_get(Url, Path, UserName, PrivateKey, ExtraHeaders).

do_chef_get(Url, Path, User, PrivateKey, ExtraHeaders) ->
    Headers0 = generate_signed_headers(PrivateKey, User, <<"GET">>, Path),
    Headers = [{"Accept", "application/json"}|Headers0] ++ ExtraHeaders,
    ibrowse:send_req(Url, Headers, get).

generate_signed_headers(PrivateKey, User, Method, Path) ->
    Time = httpd_util:rfc1123_date(),
    SignedHeaders = chef_authn:sign_request(PrivateKey, User, Method, Time, Path),
    % TODO: control the type of K and V *before* getting in here It
    % looks like ibrowse only requires that header names be atom or
    % string, but values can be iolist.  It might be worth
    % investigating whether ibrowse can be taught how to handle header
    % names that are binaries to avoid conversion.
    [{ensure_list(K), ensure_list(V)} || {K, V} <- SignedHeaders].

-spec ensure_list(binary()) -> list().

ensure_list(B) when is_binary(B) ->
    binary_to_list(B);
ensure_list(T) ->
    T.
