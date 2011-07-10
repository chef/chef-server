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

-export([get_raw/4,
         get_cooked/4]).

get_cooked(Url, Path, User, PrivateKey) ->
    case do_chef_get(Url, Path, User, PrivateKey) of
        {ok, "200", _Headers, Json} ->
            {ok, mochijson2:decode(Json)};
        %% Treat all other response codes as errors
        {ok, Code, _Headers, Body} ->
            {error, {Code, Body}};
        Error ->
            Error
    end.

get_raw(Url, Path, User, PrivateKey) ->
    do_chef_get(Url, Path, User, PrivateKey).

%% Internal functions
do_chef_get(Url, Path, User, PrivateKey) ->
    Headers0 = generate_signed_headers(PrivateKey, User, <<"GET">>, Path),
    Headers = [{"Accept", "application/json"}|Headers0],
    ibrowse:send_req(Url, Headers, get).

generate_signed_headers(PrivateKey, User, Method, Path) ->
    Time = httpd_util:rfc1123_date(),
    SignedHeaders = chef_authn:sign_request(PrivateKey, User, Method, Time, Path),
    [{binary_to_list(K), binary_to_list(V)} || {K, V} <- SignedHeaders].
