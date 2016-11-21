%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @author Seth Falcon <seth@chef.io>
%% @author Marc Paradise <marc@chef.io>
%% @copyright 2011-2014 Chef Software, Inc
-module(oc_chef_wm_routes).

-export([
         bulk_route_fun/2,
         bulk_route_fun/3,
         default_orgname/0,
         route/3,
         maybe_org_name/1,
         org_name/1,
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

%% @doc Extract the organization name from the Request's path.  If organization name is
%% present it must be captured as the atom `organization_id' in our dispatch rules
%% for the organization name.
maybe_org_name(#wm_reqdata{} = Req) ->
    maybe_org_name(wrq:path_info(organization_id, Req));
maybe_org_name(undefined) ->
    undefined;
maybe_org_name(Name) ->
    list_to_binary(Name).

%% @doc Extract the organization name from the Request's path.  If organization name is
%% present it must be captured as the atom `organization_id' in our dispatch rules
%% for the organization name. Get the orgname from envy if org is undefined
org_name(#wm_reqdata{} = Req) ->
    case maybe_org_name(Req) of
        undefined -> default_orgname();
        R         -> R
    end.

%% @doc Gets the configured default orgname.
default_orgname() ->
    envy:get(oc_chef_wm, default_orgname, fun is_valid_default_orgname/1).

%% @doc Validates the default_orgname setting. It can be either undefined or binary
is_valid_default_orgname(S) when is_binary(S) ->
    true;
is_valid_default_orgname(undefined) ->
    true.

%% @doc Generate a search URL.  Expects `Args' to be a proplist with a `search_index' key
%% (the value of which can be either a binary or string).  The organization in the URL will
%% be determined from the Webmachine request. If organization does not exist in the request
%% information, it will call the OSC route/3 function to generate URLs without an organization
route(user, Req, Args) ->
    route_rest_object("users", Req, Args);
route(Type, Req, Args) ->
    org_route(Type, Req, Args).

%% Internal route function that generates routes with orgname
org_route(organization_search, Req, Args) ->
    %% Using pattern matching with lists:keyfind instead of just proplists:get_value just
    %% for extra sanity check
    Org = org_name(Req),
    {search_index, Index} = lists:keyfind(search_index, 1, Args),

    Template = "/organizations/~s/search/~s",
    TemplateArgs = [Org, Index],
    render_template(Template, Req, TemplateArgs);

%% Create a url for an individual role.  Requires a 'role_name' argument
org_route(node, Req, Args) -> route_organization_rest_object("nodes", Req, Args);
org_route(role, Req, Args) -> route_organization_rest_object("roles", Req, Args);
org_route(user, Req, Args) -> route_rest_object("users", Req, Args);
org_route(data_bag, Req, Args) -> route_organization_rest_object("data", Req, Args);
org_route(data_bag_item, Req, [{name, {DataBagName, _}}]) -> route(data_bag, Req, [{name, DataBagName}]);
org_route(environment, Req, Args) -> route_organization_rest_object("environments", Req, Args);
org_route(principal, Req, Args) -> route_organization_rest_object("principals", Req, Args);
org_route(client, Req, Args) -> route_organization_rest_object("clients", Req, Args);
org_route(container, Req, Args) -> route_organization_rest_object("containers", Req, Args);
org_route(policy, Req, Args) -> route_organization_rest_object("policies", Req, Args);
org_route(policy_group, Req, Args) -> route_organization_rest_object("policy_groups", Req, Args);
org_route(group, Req, Args) -> route_organization_rest_object("groups", Req, Args);
org_route(association, Req, Args) -> route_organization_rest_object("users", Req, Args);
org_route(invite, Req, Args) -> route_organization_rest_object("association_requests", Req, Args);
org_route(cookbook_version, Req, Args) -> route_organization_rest_object("cookbooks", Req, Args);
org_route(cookbook_artifact_version, Req, Args) -> route_organization_rest_object("cookbook_artifacts", Req, Args);
org_route(sandbox, Req, Args) ->
    Org = org_name(Req),
    {id, Id} = lists:keyfind(id, 1, Args),
    Template = "/organizations/~s/sandboxes/~s",
    TemplateArgs = [Org, Id],
    render_template(Template, Req, TemplateArgs);
org_route(organization, Req, Args) ->
    {name, Name} = lists:keyfind(name, 1, Args),
    Template = "/organizations/~s",
    TemplateArgs = [Name],
    render_template(Template, Req, TemplateArgs);
org_route(client_key, Req, Args) ->
    {object_name, ParentName} = lists:keyfind(object_name, 1, Args),
    {name, Name} = lists:keyfind(name, 1, Args),
    {BaseURI, Org} = extract_from_req(Req),
    Template = template_for_type(client_key),
    render_template(Template, BaseURI, [Org, ParentName, Name]);
org_route(org_user_key, Req, Args) ->
    {object_name, ParentName} = lists:keyfind(object_name, 1, Args),
    {name, Name} = lists:keyfind(name, 1, Args),
    {BaseURI, Org} = extract_from_req(Req),
    Template = template_for_type(org_user_key),
    render_template(Template, BaseURI, [Org, ParentName, Name]);
org_route(user_key, Req, Args) ->
    {object_name, ParentName} = lists:keyfind(object_name, 1, Args),
    {name, Name} = lists:keyfind(name, 1, Args),
    BaseURI = chef_wm_util:base_uri(Req),
    Template = template_for_type(user_key),
    render_template(Template, BaseURI, [ParentName, Name]);
org_route(universe, Req, _) ->
    Org = org_name(Req),
    Template = "/organizations/~s/universe",
    TemplateArgs = [Org],
    render_template(Template, Req, TemplateArgs).

route_organization_rest_object(ParentName, Req, Args) ->
    Org = org_name(Req),
    {name, Name} = lists:keyfind(name, 1, Args),
    Template = "/organizations/~s/~s/~s",
    TemplateArgs = [Org, ParentName, Name],
    render_template(Template, Req, TemplateArgs).

route_rest_object(ParentName, Req, Args) ->
    {name, Name} = lists:keyfind(name, 1, Args),
    Template = "/~s/~s",
    TemplateArgs = [ParentName, Name],
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
bulk_route_fun(Type, Req) ->
    org_bulk_route_fun(Type, Req).

bulk_route_fun(Type, Name, Req) when Type =:= data_bag_item;
                                     Type =:= cookbook_version;
                                     Type =:= cookbook_artifact_version;
                                     Type =:= client_key;
                                     Type =:= org_user_key ->
    {BaseURI, Org} = extract_from_req(Req),
    Template = template_for_type(Type),
    fun(SubName) ->
            render_template(Template, BaseURI, [Org, Name, SubName])
    end;
bulk_route_fun(Type, Name, Req) when Type =:= user_key ->
    BaseURI = chef_wm_util:base_uri(Req),
    Template = template_for_type(Type),
    fun(SubName) ->
            render_template(Template, BaseURI, [Name, SubName])
    end.

%% Internal bulk_route_fun that renders URLs with orgname
org_bulk_route_fun(association, Req) ->
    BaseURI = chef_wm_util:base_uri(Req),
    Template = template_for_type(user),
    fun(Name) ->
            render_template(Template, BaseURI, [Name])
    end;
org_bulk_route_fun(organization, Req) ->
    BaseURI = chef_wm_util:base_uri(Req),
    Template = template_for_type(organization),
    fun(Name) ->
            render_template(Template, BaseURI, [Name])
    end;
org_bulk_route_fun(user, Req) ->
    BaseURI = chef_wm_util:base_uri(Req),
    Template = template_for_type(user),
    fun(Name) ->
            render_template(Template, BaseURI, [Name])
    end;
%% Need to use this fun head instead of bulk_route_fun/3 for cookbook_versions in the case
%% that you need to generate URLs for versions of lots of different cookbooks at once,
%% instead of just for one cookbook.
org_bulk_route_fun(cookbook_version, Req) ->
    {BaseURI, Org} = extract_from_req(Req),
    Template = template_for_type(cookbook_version),
    fun(CookbookName, VersionString) ->
            render_template(Template, BaseURI, [Org, CookbookName, VersionString])
    end;
org_bulk_route_fun(cookbook_artifact_version, Req) ->
    {BaseURI, Org} = extract_from_req(Req),
    Template = template_for_type(cookbook_artifact_version),
    fun(CookbookName, Identifier) ->
            render_template(Template, BaseURI, [Org, CookbookName, Identifier])
    end;
org_bulk_route_fun(Type, Req) ->
    {BaseURI, Org} = extract_from_req(Req),
    Template = template_for_type(Type),
    fun(Name) ->
            render_template(Template, BaseURI, [Org, Name])
    end.


template_for_type(node) ->
    "/organizations/~s/nodes/~s";
template_for_type(role) ->
    "/organizations/~s/roles/~s";
template_for_type(cookbook) ->
    "/organizations/~s/cookbooks/~s";
template_for_type(cookbook_version) ->
    "/organizations/~s/cookbooks/~s/~s";
template_for_type(cookbook_artifact) ->
    "/organizations/~s/cookbook_artifacts/~s";
template_for_type(cookbook_artifact_version) ->
    "/organizations/~s/cookbook_artifacts/~s/~s";
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
    "/organizations/~s/groups/~s";
template_for_type(association) ->
    "/organizations/~s/users/~s";
template_for_type(organization) ->
    "/organizations/~s";
template_for_type(user) ->
    "/users/~s";
template_for_type(user_key) ->
    "/users/~s/keys/~s";
template_for_type(org_user_key) ->
    "/organizations/~s/users/~s/keys/~s";
template_for_type(client_key) ->
    "/organizations/~s/clients/~s/keys/~s";
template_for_type(policy) ->
    "/organizations/~s/policies/~s";
template_for_type(policy_group) ->
    "/organizations/~s/policy_groups/~s".



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
make_args(Item, policies, OrgName) ->
    [OrgName, ej:get({<<"policy_group">>}, ej:get({<<"name">>}, Item))];
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
