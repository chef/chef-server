%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @doc Resource module for Environment Roles endpoint
%% Copyright 2012-2018 Chef Software, Inc.
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


-module(chef_wm_environment_cookbooks).

%% chef_wm behaviour callbacks
-include("oc_chef_wm.hrl").
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-mixin([{oc_chef_wm_base, [forbidden/2,
                           is_authorized/2,
                           service_available/2,
                           content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2]}]).

%% Webmachine callbacks implented in this module (i.e., not mixed-in)
-export([allowed_methods/2,
         to_json/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #environment_state{}}.

request_type() ->
    "environment_cookbooks".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('GET', Req, #base_state{resource_state=EnvironmentState}=State) ->
    %% We need to figure out which cookbook, if any, we're restricting results to to figure
    %% out what value of 'num_versions' to use, so we might as well stick this into the
    %% state while we're here.
    CookbookName = case chef_wm_util:object_name(cookbook, Req) of
                       undefined -> all;
                       CBName -> CBName
                   end,

    %% Process NumVersions here and save it for later use if it is valid; otherwise bail out
    %%
    %% If you are looking at *all* cookbooks, not specifying a `num_versions' parameter
    %% means you pull back only the most recent version of each cookbook.  Looking at a
    %% specific one, however, defaults to pulling back information for *all* versions of
    %% that cookbook (all subject to environment version constraints, of course).
    %%
    %% Note that this just determines what the *default* value should be;
    %% chef_wm_util:num_versions/2 actually determines what the final value is, based on
    %% the actual request.
    Default = case CookbookName of
                  all -> 1;
                  _ -> all
              end,

    NumVersions = chef_wm_util:num_versions(Default, Req),
    {Req, State#base_state{
            resource_state=EnvironmentState#environment_state{
                             num_versions=NumVersions,
                             cookbook=CookbookName}}}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_guid = OrgId,
                           resource_state = #environment_state{cookbook=CookbookName}} = State) ->

    EnvironmentName = chef_wm_util:object_name(environment, Req),

    %% First we need to see if the environment even exists
    case chef_db:fetch(#chef_environment{org_id = OrgId, name = EnvironmentName}, DbContext) of
        #chef_environment{} = Env ->
            %% Now that we know the environment exists, we should check to see if the
            %% cookbook exists (maybe)
            case CookbookName of
                all ->
                    %% Not targeting a specific cookbook here; go on to do the real checking
                    do_forbidden(Req, State, Env);
                _ ->
                    %% We're actually looking for a specific cookbook; better see if it
                    %% exists first
                    case chef_db:cookbook_exists(DbContext, OrgId, CookbookName) of
                        true ->
                            %% Huzzah!  Do the real check now
                            do_forbidden(Req, State, Env);
                        false ->
                            %% 404 for you!
                            not_found(cookbook, CookbookName, Req, State)
                    end
            end;
        not_found ->
            %% Couldn't find the environment; 404 for you
            not_found(environment, EnvironmentName, Req, State)
    end.

%% @doc Helper function for auth_info/2, in order to make that
%% function a bit more readable.  Just generates the appropriate 404
%% response depending on whether or not the request Environment or
%% Cookbook were not found.
not_found(Type, Name, Req, State) ->
    Message = chef_wm_util:not_found_message(Type, Name),
    LogMsg = case Type of
                 environment -> env_not_found;
                 cookbook -> cookbook_not_found
             end,

    {{halt, 404},
     chef_wm_util:set_json_body(Req, Message),
     State#base_state{log_msg = LogMsg}}.

%% @doc Helper function for auth_info/2, in order to make that
%% function a bit more readable.  Just sticks the Environment into the
%% State.
do_forbidden(Req,
             #base_state{resource_state=EnvState}=State,
             #chef_environment{authz_id=AuthzId}=Environment) ->
    {{object, AuthzId}, Req, State#base_state{
                               resource_state = EnvState#environment_state{
                                                  chef_environment =Environment
                                                 }}}.

to_json(Req, #base_state{chef_db_context = DbContext,
                         organization_guid = OrgId,
                         resource_state = #environment_state{
                           num_versions = NumVersions,
                           cookbook = Cookbook,
                           chef_environment = #chef_environment{
                             name = EnvName
                            }}} = State) ->
    Results = chef_db:fetch_environment_filtered_cookbook_versions(
                DbContext, OrgId, EnvName, Cookbook, NumVersions),
    {chef_json:encode(process_filtered_results(Results, Req)),
     Req, State}.

%% @doc Process the results of filtering cookbook versions through an environment.
%% Generates an EJSON structure that will yield JSON like this:
%%
%% {
%%   "cookbook_1_name": {
%%     "url": "http://CHEF_SERVER/path/to/cookbook_1_name",
%%     "versions": [
%%                  {"version": "1.0.0",
%%                   "url": "http://CHEF_SERVER/path/to/cookbook_1_name/1.0.0"},
%%                  {"version": "0.5.0",
%%                   "url": "http://CHEF_SERVER/path/to/cookbook_1_name/0.5.0"}
%%                 ]
%%    },
%%   "cookbook_2_name": {
%%     "url": "http://CHEF_SERVER/path/to/cookbook_2_name",
%%     "versions": [
%%                  {"version", "1.5.0",
%%                   "url": "http://CHEF_SERVER/path/to/cookbook_2_name/1.5.0"},
%%                  {"version", "1.0.0",
%%                   "url": "http://CHEF_SERVER/path/to/cookbook_2_name/1.0.0"}
%%                 ]
%%    }
%% }
%% @end
-spec process_filtered_results([{CookbookName :: binary(),
                                 Versions :: [binary()]}],
                               Request :: wm_req()) -> ej:json_object().
process_filtered_results(Results, Req) ->
    CookbookUrlFun = oc_chef_wm_routes:bulk_route_fun(cookbook, Req),
    CookbookVersionUrlFun = oc_chef_wm_routes:bulk_route_fun(cookbook_version, Req),

    lists:foldl(fun({CookbookName, Versions}, Acc) ->
                        ej:set({CookbookName}, Acc,
                               {[{<<"url">>, CookbookUrlFun(CookbookName)},
                                 {<<"versions">>, [ {[{<<"url">>, CookbookVersionUrlFun(CookbookName, Version)},
                                                      {<<"version">>, Version}]}
                                                    || Version <- Versions ]}]})
                end,
                {[]},
                Results).
