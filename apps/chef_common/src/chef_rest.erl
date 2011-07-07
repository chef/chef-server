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
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.0.1
% @end

% @doc chef_rest - Shortcuts and helper functions for making calls against the chef api
%
%
%
%
%
-module(chef_rest).

-export([
	 make_opscode_config/0,
	 is_user_associated_with_org/3,
	 chef_rest_get/3
%	 chef_rest_json/4
         ]).


%% -record (user_config, { user_name = null,
%% 			public_key = null } ).

-record (opscode_config, { webui_user_name,
 			   webui_private_key,
 			   account_api_url,
 			   chef_api_url} 
 	).

% TODO Refactor out  to hrl
-type organization_name() :: binary().
-type user_id() :: binary().

make_opscode_config() ->
    {ok, WebuiKey} = file:read_file("apps/chef_common/test/webui_priv.pem"),
    #opscode_config { webui_user_name   = <<"foo">>,
		      webui_private_key = WebuiKey,
		      account_api_url   = <<"http://localhost:4042/organizations/skynet">>,
		      chef_api_url      = <<"http://localhost:4000">>
		    }.
		      
chef_rest_get(Url, User, PrivateKey) ->
    HttpTime = httpd_util:rfc1123_date(),
    Path =  <<"/users/platform-superuser/organizations">>,
    SignedHeaders = chef_authn:sign_request(PrivateKey, <<"">>, User, <<"GET">>, HttpTime, Path),
    SignedHeaderStrings = [
			   {"Accept", "application/json"}
%			   , {"Content-type:", "application/json"}
			    |
			   [ {erlang:binary_to_list(K), erlang:binary_to_list(V)} || {K,V} <- SignedHeaders ] ] ,
    io:format("Headers: ~p~n", [SignedHeaderStrings]),
    Result = ibrowse:send_req(erlang:binary_to_list(Url), SignedHeaderStrings, get).

%
% Assumes get:
%
%chef_rest_json(Url, User, PrivateKey, Headers) ->
%   ok.  
-spec is_user_associated_with_org(any(), user_id(), organization_name()) -> binary(). % true|false when finished					 
is_user_associated_with_org(OpscodeConfig, UserName, OrgName) ->
    Path = iolist_to_binary([ OpscodeConfig#opscode_config.account_api_url, "/users/", UserName, "/organizations" ]),
    chef_rest_get(Path, UserName, OpscodeConfig#opscode_config.webui_private_key).
    % HttpTime = httpd_util:rfc1123_date(),
    %% TODO add x-ops-request-source header once I know what it means
    % SignedHeaders = chef_authn:sign_request(OpscodeConfig#opscode_config.webui_private_key,
    %					    <<"">>, UserName, <<"GET">>, HttpTime, Path),
    % Result = ibrowse:send_req(erlang:binary_to_list(Path), SignedHeaders, get).
    
    



	

