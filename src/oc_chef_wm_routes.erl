%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011-2012 Opscode, Inc.
-module(oc_chef_wm_routes).

-export([
         bulk_route_fun/2,
         bulk_route_fun/3,
         route/3,
         url_for_search_item_fun/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").

%% TODO: Refactor nodes resource to use this for URI generation

%% @doc utility method for generating a binary from a template and arguments.  The protocol
%% and host are derived from the Webmachine request via our own magic in `chef_wm_util',
%% since Webmachine doesn't do this for us.  As a result, the `template' should be for just
%% the path of the desired URL (including the leading "/" character!).  Thus, a "good"
%% template might be
%%
%%  "/organizations/~s/search/~s"
%%
render_template(Template, BaseURI, Args) when is_list(BaseURI) ->
    iolist_to_binary(BaseURI ++ io_lib:format(Template, Args));
render_template(Template, Req, Args) ->
    render_template(Template, chef_wm_util:base_uri(Req), Args).

%% @doc Extract the organization name from the Request's path.  Depends on us always using
%% the atom `organization_id' in our dispatch rules for the organization name.
org_name(Req) ->
    list_to_binary(wrq:path_info(organization_id, Req)).

%% @doc Generate a search URL.  Expects `Args' to be a proplist with a `search_index' key
%% (the value of which can be either a binary or string).  The organization in the URL will
%% be determined from the Webmachine request.
route(organization_search, Req, Args) ->
    %% Using pattern matching with lists:keyfind instead of just proplists:get_value just
    %% for extra sanity check
    Org = org_name(Req),
    {search_index, Index} = lists:keyfind(search_index, 1, Args),

    Template = "/organizations/~s/search/~s",
    TemplateArgs = [Org, Index],
    render_template(Template, Req, TemplateArgs);

%% Create a url for an individual role.  Requires a 'role_name' argument
route(node, Req, Args) -> route_organization_rest_object("nodes", Req, Args);
route(role, Req, Args) -> route_organization_rest_object("roles", Req, Args);
route(data_bag, Req, Args) -> route_organization_rest_object("data", Req, Args);
route(environment, Req, Args) -> route_organization_rest_object("environments", Req, Args);
route(principal, Req, Args) -> route_organization_rest_object("principals", Req, Args);
route(client, Req, Args) -> route_organization_rest_object("clients", Req, Args);
route(container, Req, Args) -> route_organization_rest_object("containers", Req, Args);
route(sandbox, Req, Args) ->
    Org = org_name(Req),
    {id, Id} = lists:keyfind(id, 1, Args),
    Template = "/organizations/~s/sandboxes/~s",
    TemplateArgs = [Org, Id],
    render_template(Template, Req, TemplateArgs);
route(cookbook_version, Req, Args) ->
    Org = org_name(Req),
    {name, Name} = lists:keyfind(name, 1, Args),
    %% FIXME: maybe just pull out name and version from req
    %% FIXME: this is wrong, but need something here
    Template = "/organizations/~s/cookbooks/~s",
    TemplateArgs = [Org, Name],
    {name, Name} = lists:keyfind(name, 1, Args),
    render_template(Template, Req, TemplateArgs).

route_organization_rest_object(ParentName, Req, Args) ->
    Org = org_name(Req),
    {name, Name} = lists:keyfind(name, 1, Args),
    Template = "/organizations/~s/~s/~s",
    TemplateArgs = [Org, ParentName, Name],
    render_template(Template, Req, TemplateArgs).


%% @doc Extract various bits of information from a Webmachine request.
%%
%% Returns a tuple with the base URI and Chef Organization name.
-spec extract_from_req(Req :: #wm_reqdata{}) -> {nonempty_string(), binary()}.
extract_from_req(Req) ->
    Org = org_name(Req),
    BaseURI = chef_wm_util:base_uri(Req),
    {BaseURI, Org}.

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
                               Type =:= container;
                               Type =:= group ->
    {BaseURI, Org} = extract_from_req(Req),
    Template = template_for_type(Type),
    fun(Name) ->
            render_template(Template, BaseURI, [Org, Name])
    end;
%% Need to use this fun head instead of bulk_route_fun/3 for cookbook_versions in the case
%% that you need to generate URLs for versions of lots of different cookbooks at once,
%% instead of just for one cookbook.
bulk_route_fun(cookbook_version, Req) ->
    {BaseURI, Org} = extract_from_req(Req),
    Template = template_for_type(cookbook_version),
    fun(CookbookName, VersionString) ->
            render_template(Template, BaseURI, [Org, CookbookName, VersionString])
    end.

bulk_route_fun(Type, Name, Req) when Type =:= data_bag_item;
                                     Type =:= cookbook_version ->
    {BaseURI, Org} = extract_from_req(Req),
    Template = template_for_type(Type),
    fun(SubName) ->
            render_template(Template, BaseURI, [Org, Name, SubName])
    end.

template_for_type(node) ->
    "/organizations/~s/nodes/~s";
template_for_type(role) ->
    "/organizations/~s/roles/~s";
template_for_type(cookbook) ->
    "/organizations/~s/cookbooks/~s";
template_for_type(cookbook_version) ->
    "/organizations/~s/cookbooks/~s/~s";
template_for_type(environment) ->
    "/organizations/~s/environments/~s";
template_for_type(principal) ->
    "/organizations/~s/principals/~s";
template_for_type(client) ->
    "/organizations/~s/clients/~s";
template_for_type(data_bag) ->
    "/organizations/~s/data/~s";
template_for_type(data_bag_item) ->
    "/organizations/~s/data/~s/~s";
template_for_type({data_bag, _}) ->
    %% another way of asking for data_bag_item
    "/organizations/~s/data/~s/~s";
template_for_type(container) ->
    "/organizations/~s/containers/~s";
template_for_type(group) ->
    "/organizations/~s/groups/~s".


%% This is extracted from search, needs more cleanup
url_for_search_item_fun(Req, Type, OrgName) ->
    BaseURI = chef_wm_util:base_uri(Req),
    Template = template_for_type(Type),
    fun(ItemEjson) ->
            Args = make_args(ItemEjson, Type, OrgName),
            render_template(Template, BaseURI, Args)
    end.

make_args(Item, {data_bag, Bag}, OrgName) ->
    [OrgName, Bag, data_bag_item_id(Item)];
make_args(Item, client, OrgName) ->
    [OrgName, ej:get({<<"clientname">>}, Item)];
make_args(Item, _, OrgName) ->
    [OrgName, ej:get({<<"name">>}, Item)].

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
