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
% @end

% @doc Various functions for checking permissions
-module(chef_permissions).

-export([is_user_with_org/2]).

is_user_with_org(User, OrgName) when is_binary(OrgName) ->
    {Path, URL} = build_org_url_path(User),
    {ok, WebuiUser} = application:get_env(chef_common, webui_user),
    {ok, WebuiPrivateKey} = chef_keyring:get_key(webui),
    io:format("~p~n-----~p~n", [User, WebuiPrivateKey]),
    case chef_rest_client:get_cooked(URL, Path, User, WebuiPrivateKey) of
        {ok, [{Organizations}]} ->
            OrgNames = [Org || {<<"organization">>, Org} <- Organizations,
		               ej:get({<<"name">>}, Org) =:= OrgName],
	    io:format("OK! ~p in ~p from ~p~n", [OrgName, OrgNames, Organizations]),
            length(OrgNames) > 0;
        Error ->
	    io:format("Not OK :(~n"),
            error_logger:error_msg("Error checking membership for ~p in org ~p: ~p~n",
                                   [User, OrgName, Error]),
            false
    end.

%% Internal functions
build_org_url_path(User) ->
    {ok, ApiURL} = application:get_env(chef_common, account_api_url),
    Path = lists:flatten(io_lib:format("/users/~s/organizations", [User])),
    {Path, ApiURL ++ Path}.
