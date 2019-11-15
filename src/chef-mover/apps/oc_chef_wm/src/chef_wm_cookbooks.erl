%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@chef.io>
%% Copyright 2012-2014 Chef Software, Inc. All Rights Reserved.
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


-module(chef_wm_cookbooks).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([allowed_methods/2,
         to_json/2 ]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #cookbook_state{}}.

request_type() ->
    "cookbooks".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

validate_request('GET', Req, #base_state{resource_state = CBState0} = State) ->
    NumVersions = chef_wm_util:num_versions(Req),
    CBState = CBState0#cookbook_state{num_versions=NumVersions},
    State1 = State#base_state{resource_state = CBState},
    {Req, State1}.

auth_info(Req, State) ->
    {{container, cookbook}, Req, State}.

%% @doc We generate three different kinds of JSON responses from this resource, based on the
%% `qualifier' URL path element.  If this is not present, then the 'default' JSON response
%% (listing all cookbooks) is returned.  Otherwise, '_latest' returns just the most recent
%% cookbooks, and '_recipes' returns the names of all the recipes in the latest cookbooks.
to_json(Req, State) ->
    to_json(wrq:path_info(qualifier, Req), Req, State).

to_json(undefined, Req, State) ->
    {all_cookbooks_json(Req, State), Req, State};
to_json("_latest", Req, State) ->
    {latest_cookbooks_json(Req, State), Req, State};
to_json("_recipes", Req, State) ->
    {cookbook_recipes_json(State), Req, State};
to_json(CookbookName0, Req, #base_state{chef_db_context = DbContext,
                                        organization_name = OrgName} = State) ->
    CookbookName = list_to_binary(CookbookName0),
    case chef_db:fetch_cookbook_versions(DbContext, OrgName, CookbookName) of
        [] ->
            Message = chef_wm_util:not_found_message(cookbook, CookbookName),
            {{halt, 404},
             chef_wm_util:set_json_body(Req, Message),
             State#base_state{log_msg = cookbook_not_found}};
        CookbookVersions ->
            AggregateCookbooks = aggregate_versions(CookbookVersions),
            CBList = make_cookbook_list(Req, AggregateCookbooks, all),
            {chef_json:encode({CBList}), Req, State}
    end.

%%
%% Helper functions
%%

%% @doc Generate a JSON string for a the version listing
%% @end
%%
-spec all_cookbooks_json(Request :: wm_req(),
                         State :: #base_state{}) -> JSON :: binary().
all_cookbooks_json(Req, #base_state{chef_db_context = DbContext,
                                    organization_name = OrgName,
                                    resource_state = CookbookState}) ->
    CookbookVersions = chef_db:fetch_cookbook_versions(DbContext, OrgName),
    #cookbook_state{num_versions = NumVersions} = CookbookState,
    AggregateCookbooks = aggregate_versions(CookbookVersions),
    CBList = make_cookbook_list(Req, AggregateCookbooks, NumVersions),
    chef_json:encode({CBList}).

%% @doc Generate a JSON hash mapping a cookbook's name to the URL for the most recent
%% version of that cookbook
-spec latest_cookbooks_json(Request :: wm_req(),
                            State :: #base_state{}) -> JSON :: binary().
latest_cookbooks_json(Req, #base_state{chef_db_context = DbContext,
                                       organization_name = OrgName}) ->
    Latest = chef_db:fetch_latest_cookbook_versions(DbContext, OrgName),
    Processed = process_latest_cookbooks(Latest, Req),
    chef_json:encode({Processed}).

%% @doc Generate a JSON array of cookbook-qualified names (i.e. "cookbook::recipe") for all
%% the recipes in the latest version of each cookbook in an organization.  Items are sorted
%% by cookbook name and recipe name.
-spec cookbook_recipes_json(State :: #base_state{}) -> JSON :: binary().
cookbook_recipes_json(#base_state{chef_db_context = DbContext,
                                  organization_name = OrgName}) ->
    Recipes = chef_db:fetch_latest_cookbook_recipes(DbContext, OrgName),
    %% Recipes is just a list of cookbook-qualified recipe names, so we don't need to do any
    %% further processing; just encode to JSON and we're done.
    chef_json:encode(Recipes).

%% @doc Generate a proplist mapping cookbook name to the URL for the latest
%% version of that cookbook.
-spec process_latest_cookbooks([{CookbookName :: binary(),
                                 VersionString :: binary()}],
                               Req :: wm_req()) -> [{CookbookName :: binary(),
                                                            LatestURL :: binary()}].
process_latest_cookbooks(Latest, Req) ->
    UrlGenerator = oc_chef_wm_routes:bulk_route_fun(cookbook_version, Req),
    [{CookbookName, UrlGenerator(CookbookName, VersionString)}
     || {CookbookName, VersionString} <- Latest ].

%% @doc Convert list of [Name, Major, Minor, Version] to a list of 2-tuples
%% of name, list of versions i.e
%% {Name, [ {Major, Minor, Version}, {Major, Minor,Version}, {...}, ... ]}
%%
%% @end
-spec aggregate_versions([ versioned_cookbook() ]) ->
    [ { binary(), [ {version()} ] } ].
aggregate_versions(CookbookVersions) ->
    Dict = lists:foldl(fun([Name, Version], Dict) ->
                           dict:append(Name, Version, Dict)
                       end,
        dict:new(),
        CookbookVersions),
    dict:to_list(Dict).

%% @doc Construct the per-cookbook map which contains the cookbook URL
%% and a per-version map containing the version and the URL for the version
%%
%% @end
-spec make_version_list(CookbookVersionFun::fun(),
        Versions::[ {binary(), version()}],
        NumVersions:: non_neg_integer() | all) ->
    list().
make_version_list(CookbookVersionFun, Versions, NumVersions) ->
    TrimmedVersions = case NumVersions of
        all -> Versions;
        _ -> lists:sublist(Versions, NumVersions)
    end,
    [ begin
        VersionBinary = chef_cookbook_version:version_to_binary(Version),
        {[{<<"version">>, VersionBinary},
          {<<"url">>, CookbookVersionFun(VersionBinary) }]}
      end  || Version <- TrimmedVersions].

-spec make_cookbook_list(Req::#wm_reqdata{},
                         Cookbooks:: [ { binary(), [ {version()} ] } ],
                         NumVersions:: all | non_neg_integer()) ->
    list().
make_cookbook_list(Req, Cookbooks, NumVersions) ->
    [ begin
          CookbookVersionFun = oc_chef_wm_routes:bulk_route_fun(cookbook_version, Name, Req),
          VersionList = make_version_list(CookbookVersionFun, Versions, NumVersions),
          CookbookFun = oc_chef_wm_routes:bulk_route_fun(cookbook, Req),
          { Name, {[
                     { <<"url">>, CookbookFun(Name)},
                     { <<"versions">>, VersionList }
                   ]}
          }
      end || {Name, Versions} <- Cookbooks].

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
