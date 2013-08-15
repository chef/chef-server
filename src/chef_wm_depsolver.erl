%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @doc Resource module for Chef Depsolver endpoint
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_wm_depsolver).

%% chef_wm behaviour callbacks
-include("chef_wm.hrl").
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

%% Mix in platform-specific Webmachine callback implementations
-mixin([{?BASE_RESOURCE, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

%% Mix in universal Webmachine callback implementations, common to all
%% Chef platforms
-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2]}]).

%% Webmachine callbacks implented in this module (i.e., not mixed-in)
-export([allowed_methods/2,
         post_is_create/2,
         process_post/2]).

%% Internal types
-type cookbook_with_version() :: binary() | {binary(), binary()}.

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #depsolver_state{}}.

request_type() ->
    "depsolver".

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

validate_request('POST', Req, #base_state{resource_state=DepsolverState}=State) ->
    Body = wrq:req_body(Req),
    {ok, JsonBody} = chef_depsolver:parse_binary_json(Body),
    Runlist = ej:get({<<"run_list">>}, JsonBody),
    CookbookList = cookbooks_for_runlist(Runlist),
    EnvName = chef_wm_util:object_name(environment, Req),
    State1 = State#base_state{resource_state = DepsolverState#depsolver_state{run_list_cookbooks = CookbookList,
                                                                              environment_name = EnvName}},
    {Req, State1}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_name = OrgName,
                           resource_state = #depsolver_state{environment_name = EnvName}} = State) ->
    forbidden_for_environment(chef_db:fetch_environment(DbContext, OrgName, EnvName),
                              Req,
                              State).

%% @doc helper function for auth_info/2 which when given the output of chef_db:fetch_environment,
%% checks the permissions of the requestor against the environment and cookbook container
forbidden_for_environment(not_found, Req,
                          #base_state{resource_state = #depsolver_state{environment_name = EnvName}} = State) ->
    {{halt, 404},
     chef_wm_util:with_error_body(Req, not_found_message(environment, EnvName)),
     State#base_state{log_msg = environment_not_found}};
forbidden_for_environment(#chef_environment{authz_id = EnvAuthzId} = Env, Req,
                         #base_state{resource_state = ResourceState} = State) ->
    %% Set this here before passing it out; downstream functions will need it
    State1 = State#base_state{resource_state = ResourceState#depsolver_state{chef_environment = Env}},
    {[{container, cookbook, read}, {object, EnvAuthzId, read}], Req, State1}.

post_is_create(Req, State) ->
    {false, Req, State}.

process_post(Req, #base_state{reqid = ReqId,
                              chef_db_context = DbContext,
                              organization_name = OrgName,
                              resource_state = #depsolver_state{run_list_cookbooks = Cookbooks,
                                                                environment_name = EnvName,
                                                                chef_environment = Env}} = State) ->
    EnvConstraints = chef_object:depsolver_constraints(Env),
    case chef_db:fetch_all_cookbook_version_dependencies(DbContext, OrgName) of
        {error, Error} ->
            error_logger:error_msg("Dependency retrieval failure for org ~p with environment ~p: ~p~n",
                                   [OrgName, EnvName, Error]),
            server_error(Req, State, <<"Dependency retrieval failed">>, dep_retrieval_failure);
        AllVersions ->
            case not_found_cookbooks(AllVersions, Cookbooks) of
                ok ->
                    Deps = ?SH_TIME(ReqId, chef_depsolver, solve_dependencies,
                                    (AllVersions, EnvConstraints, Cookbooks)),
                    handle_depsolver_results(ok, Deps, Req, State);
                NotFound ->
                    %% We ignore Deps result if expanded run list contains missing
                    %% cookbooks, so no need to call depsolver at all.
                    handle_depsolver_results(NotFound, ignore, Req, State)
            end
    end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

%% @doc We are supplied with a list of recipes.  chef-client 0.10 has
%% already expanded roles before passing them to us.  We can have bare
%% recipes (== foo), default recipes ( == foo::default) or named
%% recipes (== foo::bar).  We also can have these three variants with
%% a version appended (== foo::bar@1.2.2)
%%
%% We expand the runlist to a set of cookbooks with dups removed.  If
%% a versioned recipe is provided in the runlist we return it as tuple
%% of {cookbook_name, version}
-spec cookbooks_for_runlist(Runlist::[binary()]) ->  [cookbook_with_version()].
cookbooks_for_runlist(Runlist) ->
    Cookbooks = [ cookbook_for_recipe(split_version(Item)) || Item <- Runlist ],
    remove_dups(Cookbooks).

-spec split_version(Recipe::binary()) -> cookbook_with_version().
split_version(Recipe) when is_binary(Recipe) ->
    case re:split(Recipe, <<"@">>) of
        [Name] ->
            Name;
        [Name, Version] ->
            {Name, Version}
    end.

%% @doc helper function which translates a full recipe names to the
%% name of the cookbook which contains the recipe.
%%
%% If a version is specified in the recipe it is retained in the
%% cookbook tuple
-spec cookbook_for_recipe(cookbook_with_version()) -> cookbook_with_version().
cookbook_for_recipe({Recipe, Version}) ->
    {cookbook_for_recipe(Recipe), Version};
cookbook_for_recipe(Recipe) ->
    case re:split(Recipe, <<"::">>) of
        [Cookbook, _Recipe] ->
            Cookbook;
        [Cookbook] ->
            Cookbook
    end.

-spec remove_dups([cookbook_with_version()]) -> [cookbook_with_version()].
remove_dups(L) ->
    WithIdx = lists:zip(L, lists:seq(1, length(L))),
    [ Elt || {Elt, _} <- lists:ukeysort(2, lists:ukeysort(1, WithIdx)) ].

%% @doc given a map of cookbook names to versions and a list of
%% cookbook versions in the run_list, return the list of cookbook
%% which are not in the database.
%% @end
%%
%% TODO - look at the nested loops and complexity of this operation -
%% cookbook_missing calls proplists:is_defined/2 which will traverse
%% the AllVersions structure for each cookbook lookup.  It might be
%% better to loop over the list of Cookbooks instead
-spec not_found_cookbooks(AllVersions :: [depsolver:dependency_set()],
                          Cookbooks :: [cookbook_with_version()]) ->
                                 ok | {not_found, [cookbook_with_version()]}.
not_found_cookbooks(AllVersions, Cookbooks) ->
    NotFound = [ cookbook_name(Cookbook) || Cookbook <- Cookbooks, cookbook_missing(Cookbook, AllVersions)],
    case NotFound of
        [] -> ok;
        _ -> {not_found, NotFound}
    end.

%% @doc helper function to return the name of a cookbook that is in a
%% processed run_list where it could be either a name or a {Name,
%% Version} tuple
-spec cookbook_name(cookbook_with_version()) -> binary().
cookbook_name(Cookbook) when is_binary(Cookbook) ->
    Cookbook;
cookbook_name({Name, _Version}) ->
    Name.

%% @doc helper function to check if a (possibly versioned) cookbook is in the
%% set of all cookbook versions.
%%
%% In order to work in the same manner as the ruby code it will only check for a
%% cookbook name in the list of all cookbook version. This means if any version of a cookbook
%% exists it returns false
-spec cookbook_missing(CB::cookbook_with_version(),
                       AllVersions::[depsolver:dependency_set()]) -> boolean().
cookbook_missing(CB, AllVersions) when is_binary(CB) ->
    not proplists:is_defined(CB, AllVersions);
cookbook_missing({Name, _Version}, AllVersions) ->
    cookbook_missing(Name, AllVersions).

%% @doc Given the output from not_found_cookbooks/2 and
%% chef_depsolver:solve_dependencies/3, format an appropriate response
%% document
handle_depsolver_results({not_found, CookbookNames}, _Deps, Req, State) when is_list(CookbookNames)->
    precondition_failed(Req, State,
                        not_found_message(cookbook_version, CookbookNames),
                        cookbook_version_not_found);
handle_depsolver_results(ok, {error, resolution_timeout}, Req, State) ->
        precondition_failed(Req, State,
                            timeout_message(),
                            solve_timeout);
handle_depsolver_results(ok, {error, {unreachable_package, Unreachable}}, Req, State) ->
    precondition_failed(Req, State,
                        not_reachable_message(Unreachable),
                        not_reachable_dep);
handle_depsolver_results(ok, {error, _} = Ret, Req, State) ->
    precondition_failed(Req, State,
                        unable_to_solve_message(Ret),
                        unable_to_solve_deps);
handle_depsolver_results(ok, {ok, Cookbooks}, Req, #base_state{chef_db_context = DbContext,
                                                               organization_name = OrgName} = State) ->
    %% TODO - helper function to deal with the call and match on a chef_cookbook version
    CookbookRecords = [ chef_db:fetch_cookbook_version(DbContext, OrgName, Cookbook) || Cookbook <- Cookbooks],
    assemble_response(Req, State, CookbookRecords).

%% @doc Utility function to remove some of the verbosity
precondition_failed(Req, State, ErrorData, LogMsg) when is_atom(LogMsg) ->
    wm_halt(412, Req, State, ErrorData, LogMsg).

%% @doc Utility function to remove some of the verbosity.  Note that
%% this is specific to Chef, and has absolutely nothing to do with the
%% Webmachine callback.
forbid(Req, State, ErrorData, LogMsg) ->
    wm_halt(403, Req, State, ErrorData, LogMsg).

server_error(Req, State, ErrorData, LogMsg) ->
    wm_halt(500, Req, State, ErrorData, LogMsg).

wm_halt(Code, Req, State, ErrorData, LogMsg) ->
    {{halt, Code},
     chef_wm_util:with_error_body(Req, ErrorData),
     State#base_state{log_msg = LogMsg}}.

%% @doc Assemble a JSON response object which is a map of cookbook
%% name to cookbook object for all cookbook versions which have been
%% found by depsolving.
%%
%% Note the cookbook object we return back is a stripped-down version,
%% removing large fields such as long_description and attributes in
%% the metadata that are not required by chef-client
assemble_response(Req, State, CookbookVersions) ->
    case ?BASE_RESOURCE:check_cookbook_authz(CookbookVersions, Req, State) of
        ok ->
            %% We iterate over the list again since we only want to construct the s3urls
            %% if the authz check has succeeded.  We use a minimal version of the
            %% cookbook which has just enough information for chef-client to run
            JsonList = {
                    [ { CBV#chef_cookbook_version.name,
                       chef_cookbook:minimal_cookbook_ejson(CBV) }
                      || CBV <- CookbookVersions ]
                    },
            CBMapJson = chef_json:encode(JsonList),
            {true, wrq:append_to_response_body(CBMapJson, Req), State};
        {error, Msg} ->
            forbid(Req, State, Msg, {read, forbidden});
        {timeout, Msg} ->
            server_error(Req, State, Msg, {timeout, cookbook_authz})
    end.

%%------------------------------------------------------------------------------
%% Message Functions
%%------------------------------------------------------------------------------

-spec not_found_message(environment | cookbook_version,
                        EnvironmentName :: binary() | [CookbookName :: binary()]) ->
                               Message :: binary() | {[{Key :: binary(), Message :: binary() |
                                                                         [CookbookName :: binary()]}]}.
not_found_message(environment, Name) ->
    iolist_to_binary(["environment '", Name, "' not found"]);
not_found_message(cookbook_version, [CookbookName]) when is_binary(CookbookName) ->
    {[{<<"message">>, list_to_binary(["Run list contains invalid items: no such cookbook ",
                                  CookbookName, "."])},
      {<<"non_existent_cookbooks">>, [CookbookName]},
      {<<"cookbooks_with_no_versions">>, []}]};
not_found_message(cookbook_version, CookbookNames) ->
    Reason = iolist_to_binary(["Run list contains invalid items: no such cookbooks ",
                               bin_str_join(CookbookNames, <<", ">>), "."]),
    {[{<<"message">>, Reason},
      {<<"non_existent_cookbooks">>, CookbookNames},
      {<<"cookbooks_with_no_versions">>, []}]}.

not_reachable_message(CookbookName) ->
    Reason = iolist_to_binary(["Unable to satisfy constraints on cookbook ",
                               CookbookName,
                               ", which does not exist."]),
    {[{<<"message">>, Reason},
      {<<"non_existent_cookbooks">>, [ CookbookName ]},
      {<<"most_constrained_cookbooks">>,[]}]}.

timeout_message() ->
    {[{<<"message">>, <<"unable to solve dependencies in alotted time">>},
      {<<"non_existent_cookbooks">>, []},
      {<<"most_constrained_cookbooks">>,[]}]}.

%% Main entry point for parsing the depsolver error structure
%% It consists of a list of solutions that were tried where each solution has the form:
%%
%%   {RawPaths::[ { Roots::[constraint()], Path::[pkg()] }],
%%    FailingDeps::[constraint()]}
%%
%% The nth item in RawPaths failed due to the constraint in the n'th item of FailingDeps
%%
%% The textual error message will show all solutions but we fill in the structures only
%% with the first solution
%%
unable_to_solve_message({error, [Why| _Rest]} = Details) ->
    Reason = iolist_to_binary(depsolver:format_error(Details)),
    { RawPaths, FailingDeps } = Why,
    unable_to_solve_message(Reason, RawPaths, FailingDeps).

%% versions specified that do not exist, so output a "no versions match" error
unable_to_solve_message(_Reason, RawPaths, []) ->
    RunlistItems = formatted_roots(RawPaths),
    InvalidItemsReason = iolist_to_binary(["Run list contains invalid items: no versions match the constraints on cookbook ",
                                           bin_str_join(RunlistItems, <<",">>),
                                           "."
                                          ]),
    {[{<<"message">>, InvalidItemsReason},
      {<<"non_existent_cookbooks">>, []},
      {<<"cookbooks_with_no_versions">>, RunlistItems}]};
%% If constraint paths are non-empty, find the reason that the constraint is not satisfied
%% and return the run list item ()root of the failure tree) along with the most constrained
%% cookbook in the tree
unable_to_solve_message(Reason, RawPaths, FailingDeps) ->
    RunlistItems = formatted_roots(RawPaths),
    Constraints = formatted_constraints(FailingDeps),
    {[{<<"message">>, Reason},
      {<<"unsatisfiable_run_list_item">>, RunlistItems},
      {<<"non_existent_cookbooks">>, []},
      {<<"most_constrained_cookbooks">>, Constraints}]}.

%% @doc Give a set of run list items that are roots in the dependency
%% tree, format them as binaries.  They could be either simple names
%% or name, version tuple pairs
-spec formatted_roots([{Root::[depsolver:constraint()],
                        Path::[depsolver:pkg()]}]) -> [binary()].
formatted_roots(RawRoots) ->
    %% A RootSet is a list of packages
    Roots = [ RootSet || {RootSet, _} <- RawRoots],
    %% We flatten to turn [ [ Roots] ] into something we can iterate over
    [ iolist_to_binary(depsolver_culprit:format_constraint(Ver)) || Ver <- lists:flatten(Roots) ].

%% @doc Give a set of constraint paths representing failing
%% dependencies, extract out the constraints which could not be met
-spec formatted_constraints(FailingDeps::[{Root::[depsolver:constraint()],
                                           Constraints::[depsolver:constraint()]}]) -> [binary()].
formatted_constraints(FailingDeps) ->
    ConstraintSets = [ ConstraintSet || {_Root, ConstraintSet} <- FailingDeps ],
    %% Each ConstraintSet is a list so we flatten to turn [ [ Constraint] ] into something we can iterate over
    [iolist_to_binary(depsolver_culprit:format_constraint(Con)) || Con <- lists:flatten(ConstraintSets)].

%%------------------------------------------------------------------------------
%% Miscellaneous Utilities
%%------------------------------------------------------------------------------

%% Helpers to construct pieces of error messages from lists of
%% cookbook names
-spec bin_str_join(Names::[binary()],
                   Sep::<<_:8,_:_*8>>,
                   Acc::[binary()]) -> [binary()].
bin_str_join([], _Sep, Acc) ->
    Acc;
bin_str_join([H], _Sep, Acc) ->
    [H | Acc];
bin_str_join([Name| Rest], Sep, Acc) ->
    bin_str_join(Rest, Sep, [Sep , Name | Acc]).

-spec bin_str_join(Names::[binary()], Sep::<<_:8,_:_*8>>) -> binary().
bin_str_join(Names, Sep) ->
    Reverse = lists:reverse(Names),
    list_to_binary(bin_str_join(Reverse, Sep, [])).
