%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2011-2012 Opscode, Inc.
-module(chef_wm_routes).

-export([
         bulk_route_fun/2,
         bulk_route_fun/3,
         route/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").

%% TODO: Refactor nodes resource to use this for URI generation

%% @doc utility method for generating a binary from a template and arguments.  The protocol
%% and host are derived from the Webmachine request via our own magic in `chef_rest_wm',
%% since Webmachine doesn't do this for us.  As a result, the `template' should be for just
%% the path of the desired URL (including the leading "/" character!).  Thus, a "good"
%% template might be
%%
%%  "/organizations/~s/search/~s"
%%
render_template(Template, BaseURI, Args) when is_list(BaseURI) ->
    iolist_to_binary(BaseURI ++ io_lib:format(Template, Args));
render_template(Template, Req, Args) ->
    render_template(Template, chef_rest_wm:base_uri(Req), Args).

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
route(client, Req, Args) -> route_organization_rest_object("clients", Req, Args);
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
    BaseURI = chef_rest_wm:base_uri(Req),
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
                               Type =:= client;
                               Type =:= data_bag;
                               Type =:= data_bag_item ->
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
template_for_type(client) ->
    "/organizations/~s/clients/~s";
template_for_type(data_bag) ->
    "/organizations/~s/data/~s";
template_for_type(data_bag_item) ->
    "/organizations/~s/data/~s/~s".
