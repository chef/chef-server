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

-include_lib("chef_common/include/chef_rest_client.hrl").

-export([is_user_with_org/3]).

-spec is_user_with_org(#chef_rest_client{}, string(), string()) -> boolean().

is_user_with_org(ChefClient = #chef_rest_client{}, User, OrgName)
  when is_list(User), is_list(OrgName) ->
    OrgNameBinary = list_to_binary(OrgName),
    Path = "/users/" ++ ibrowse_lib:url_encode(User) ++ "/organizations",
    case chef_rest_client:request(ChefClient, Path) of
        {ok, Organizations} ->
	    IsUserInOrg = fun(Org) ->
                              ej:get({<<"organization">>, <<"name">>}, Org) =:= OrgNameBinary
                          end,
	    lists:any(IsUserInOrg, Organizations);
        Error ->
            error_logger:error_msg("Error checking membership for ~p in org ~p: ~p~n",
                                   [User, OrgName, Error]),
            false
    end.
