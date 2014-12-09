%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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

-module(chef_wm_routes).

-export([
         bulk_route_fun/2,
         bulk_route_fun/3,
         route/3,
         url_for_search_item_fun/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").


%% @doc Generate a function that produces URLs.  Use this when multiple URLs of the same
%% type must be produced at once to prevent the needless recomputation of static URL
%% information.
%% @end
%% Create a function that needs just the Role name to generate a URL.
bulk_route_fun(Type, Req) when Type =:= role;
                               Type =:= node;
                               Type =:= cookbook;
                               Type =:= environment;
                               Type =:= principal;
                               Type =:= client;
                               Type =:= data_bag;
                               Type =:= data_bag_item;
                               Type =:= user;
                               Type =:= organization;
                               Type =:= group;
                               Type =:= container ->
    BaseURI = chef_wm_util:base_uri(Req),
    Template = template_for_type(Type),
    fun(Name) ->
            render_template(Template, BaseURI, [Name])
    end;
%% Need to use this fun head instead of bulk_route_fun/3 for cookbook_versions in the case
%% that you need to generate URLs for versions of lots of different cookbooks at once,
%% instead of just for one cookbook.
bulk_route_fun(cookbook_version, Req) ->
    BaseURI = chef_wm_util:base_uri(Req),
    Template = template_for_type(cookbook_version),
    fun(CookbookName, VersionString) ->
            render_template(Template, BaseURI, [CookbookName, VersionString])
    end.

bulk_route_fun(Type, Name, Req) when Type =:= data_bag_item;
                                     Type =:= cookbook_version ->
    BaseURI = chef_wm_util:base_uri(Req),
    Template = template_for_type(Type),
    fun(SubName) ->
            render_template(Template, BaseURI, [Name, SubName])
    end.

%% @doc Generate a search URL.  Expects `Args' to be a proplist with a `search_index' key
%% (the value of which can be either a binary or string).  The organization in the URL will
%% be determined from the Webmachine request.
route(organization_search, Req, Args) ->
    %% Using pattern matching with lists:keyfind instead of just proplists:get_value just
    %% for extra sanity check
    {search_index, Index} = lists:keyfind(search_index, 1, Args),
    Template = "/search/~s",
    TemplateArgs = [Index],
    render_template(Template, Req, TemplateArgs);

%% Create a url for an individual role.  Requires a 'role_name' argument
route(node, Req, Args) -> route_rest_object("nodes", Req, Args);
route(role, Req, Args) -> route_rest_object("roles", Req, Args);
route(user, Req, Args) -> route_rest_object("users", Req, Args);
route(data_bag, Req, Args) -> route_rest_object("data", Req, Args);
route(data_bag_item, Req, [{name, {DataBagName, _}}]) -> route(data_bag, Req, [{name, DataBagName}]);
route(environment, Req, Args) -> route_rest_object("environments", Req, Args);
route(principal, Req, Args) -> route_rest_object("principals", Req, Args);
route(client, Req, Args) -> route_rest_object("clients", Req, Args);
route(sandbox, Req, Args) ->
    {id, Id} = lists:keyfind(id, 1, Args),
    Template = "/sandboxes/~s",
    TemplateArgs = [ Id],
    render_template(Template, Req, TemplateArgs);
route(cookbook_version, Req, Args) ->
    {name, Name} = lists:keyfind(name, 1, Args),
    %% FIXME: maybe just pull out name and version from req
    %% FIXME: this is wrong, but need something here
    Template = "/cookbooks/~s",
    TemplateArgs = [Name],
    {name, Name} = lists:keyfind(name, 1, Args),
    render_template(Template, Req, TemplateArgs);
route(organization, Req, Args) -> route_rest_object("organizations", Req, Args);
route(group, Req, Args) -> route_rest_object("groups", Req, Args);
route(container, Req, Args) -> route_rest_object("containers", Req, Args).

%% @doc utility method for generating a binary from a template and arguments.  The protocol
%% and host are derived from the Webmachine request via our own magic in `chef_wm_util',
%% since Webmachine doesn't do this for us.  As a result, the `template' should be for just
%% the path of the desired URL (including the leading "/" character!).  Thus, a "good"
%% template might be
%%
%%  "/search/~s"
%%
render_template(Template, BaseURI, Args) when is_list(BaseURI) ->
    iolist_to_binary(BaseURI ++ io_lib:format(Template, Args));
render_template(Template, Req, Args) ->
    render_template(Template, chef_wm_util:base_uri(Req), Args).

route_rest_object(ParentName, Req, Args) ->
    {name, Name} = lists:keyfind(name, 1, Args),
    Template = "/~s/~s",
    TemplateArgs = [ParentName, Name],
    render_template(Template, Req, TemplateArgs).

template_for_type(node) ->
    "/nodes/~s";
template_for_type(role) ->
    "/roles/~s";
template_for_type(cookbook) ->
    "/cookbooks/~s";
template_for_type(cookbook_version) ->
    "/cookbooks/~s/~s";
template_for_type(principal) ->
    "/principals/~s";
template_for_type(environment) ->
    "/environments/~s";
template_for_type(client) ->
    "/clients/~s";
template_for_type(data_bag) ->
    "/data/~s";
template_for_type(data_bag_item) ->
    "/data/~s/~s";
template_for_type({data_bag, _}) ->
    %% another way of asking for data_bag_item
    "/data/~s/~s";
template_for_type(user) ->
    "/users/~s";
template_for_type(organization) ->
    "/organizations/~s";
template_for_type(container) ->
    "/containers/~s";
template_for_type(group) ->
    "/groups/~s".

%% This is extracted from search, needs more cleanup
url_for_search_item_fun(Req, Type, _OrgName) ->
    BaseURI = chef_wm_util:base_uri(Req),
    Template = template_for_type(Type),
    fun(ItemEjson) ->
            Args = make_args(ItemEjson, Type),
            render_template(Template, BaseURI, Args)
    end.

make_args(Item, {data_bag, Bag}) ->
    [Bag, data_bag_item_id(Item)];
make_args(Item, _) ->
    [ej:get({<<"name">>}, Item)].

%% extract name

%% When data_bag_items are stored in couchdb, they are wrapped in cruft such that the actual
%% item is under the 'raw_data' key of the wrapper. When data_bag_items are stored in SQL,
%% just the item is stored. This helper function extracts the id of the data_bag_item for
%% either type. We could have used darklaunch here, but since there are two cases and it is
%% easy to test for, we just inspect the data at hand. This can go away once data_bag_items
%% are only in SQL.
%% TODO: when data bags are no longer in couchdb, clean this
data_bag_item_id(Item) ->
    case ej:get({<<"id">>}, Item) of
        undefined ->
            %% we must have a structure coming out of couchdb wrapped in data_bag_item cruft
            ej:get({<<"raw_data">>, <<"id">>}, Item);
        Id when is_binary(Id) ->
            Id
    end.
        
