%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2015 Chef, Inc. All Rights Reserved.
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

-module(itest_cookbook_util).

-include_lib("eunit/include/eunit.hrl").
-include("chef_db.hrl").
-include("chef_types.hrl").

%%------------------------------------------------------------------------------
%% Cookbook-related Helper Functions
%%------------------------------------------------------------------------------
make_cookbook(Prefix) ->
    AzId = chef_test_suite_helper:make_az_id(Prefix),
    OrgId = chef_test_suite_helper:the_org_id(),
    Name = cookbook_name_from_prefix(Prefix),
    {AzId, OrgId, Name}.

%% @doc Just providing a label for the cookbook structure for this test module
-type cookbook() :: {CookbookAuthzId :: binary(),
                     OrgId :: binary(),
                     CookbookName :: binary()}.

%% Proplist containing at least a 'version' property, but optionally a
%% 'recipe_names' property as well.
-type version_spec() :: [proplists:property()].

%% @doc This pattern is used throughout the cookbook testing.
%% Specifies the prefix for a cookbook, as well as the versions of
%% that cookbook, that should be inserted into the database for tests
-type cookbook_spec() :: {CookbookPrefix :: binary(),
                          Versions :: [version_spec()]}.

%% @doc A foreachx setup function for testing cookbooks.  Pairs with
%% cookbook_cleanup/2.
-spec cookbook_setup([cookbook_spec()]) -> [{cookbook(), [#chef_cookbook_version{}]}].
cookbook_setup(CookbookSpecs) ->
    %% Need to remove all cookbooks before the tests,
    %% because currently, the rest of the cookbook tests
    %% in this module leave things behind.  These tests
    %% depend on ALL the cookbooks in an org, so we must
    %% have a clean slate to begin tests
    remove_all_cookbooks(),
    process_cookbook_specs(CookbookSpecs).

%% A foreachx cleanup function for testing cookbooks.  Pairs with
%% cookbook_setup/1.
cookbook_cleanup(_, _) ->
    %% This is more to provide a clean slate for other tests that come
    %% afterwards, since cookbook_setup/1 clears out the cookbooks
    %% before each of the tests
    remove_all_cookbooks().

%% @doc Execute statements that delete all data in cookbook-related
%% tables.  Useful for setup or cleanup functions.
%%
%% Relies on "extra" prepared statements that are added to Sqerl by
%% this module.  These statements DO NOT EXIST for "normal" Erchef
%% code.
remove_all_cookbooks() ->
    Statements = [delete_cookbook_version_checksums, delete_cookbook_versions, delete_cookbooks],
    [ sqerl:statement(S, []) || S <- Statements ].

%% Utility function to extract the `NumVersions' latest cookbook
%% versions of a given cookbook from a set of "unfolded" cookbook
%% specs.
%%
%% Used to generate expected results for retrieving latest cookbooks
%% as well as retrieving the current set of recipes.
latest_versions_from_unfolded_specs(CookbookName, UnfoldedSpecs, NumVersions) ->
    %% Grab all the versions for this cookbook from our specs
    AllVersions = proplists:get_all_values(CookbookName, UnfoldedSpecs),
    SortedVersions = sort_by_version_desc(AllVersions),

    %% Only take the number of versions we want
    case NumVersions of
        all ->
            SortedVersions;
        _ ->
            lists:sublist(SortedVersions, NumVersions)
    end.

sort_by_version_desc(VersionSpecs) ->
    SortFn = fun(A, B) ->
                     AVersion = proplists:get_value(version, A),
                     BVersion = proplists:get_value(version, B),
                     AVersion >= BVersion
             end,
    lists:sort(SortFn, VersionSpecs).

%% @doc Given a list of cookbook specs and a number of versions to
%% retrieve, generates the same result that the call to
%% chef_sql:fetch_latest_cookbook_versions/2 would, given that the
%% cookbooks and versions specified in the cookbook specs are the only
%% ones in the database for our testing org
-spec latest_from_cookbook_specs([cookbook_spec()], NumVersions::non_neg_integer()) ->
                                        [{CookbookName::binary(),
                                          VersionString::binary()}].
latest_from_cookbook_specs(Specs, NumVersions) ->
    Unfolded = unfold_specs(Specs),
    CookbookNames = lists:sort(proplists:get_keys(Unfolded)),
    lists:flatmap(fun(CookbookName) ->
                          Versions = latest_versions_from_unfolded_specs(CookbookName, Unfolded, NumVersions),

                          %% Convert the tuples to version binaries
                          %% and assemble results into the form that
                          %% we should get back from the chef_sql call
                          [ {CookbookName, version_tuple_to_binary(V)}
                            || {V, _RecipeNames} <- Versions]
                  end,
                  CookbookNames).

%% @doc Takes a nested cookbook spec and flattens it out, transforming
%% cookbook prefixes to full names.
-spec unfold_specs([cookbook_spec()]) ->
                          [{CookbookName::binary(),
                            {Version::version(), RecipeNames::[binary()]}}].
unfold_specs(CookbookSpecs) when is_list(CookbookSpecs) ->
    lists:flatmap(fun unfold_spec/1, CookbookSpecs).

unfold_spec({CookbookPrefix, Versions}) ->
    [ { cookbook_name_from_prefix(CookbookPrefix), V} || V <- Versions ].

%% @doc Takes a list of cookbook specs, expands them into full
%% cookbooks / cookbook versions, and inserts them all into the
%% database.  Returns the "inflated" specs.
%%
%% See generate_cookbook_and_versions/1 for more.
-spec process_cookbook_specs([cookbook_spec()]) -> [{cookbook(), [#chef_cookbook_version{}]}].
process_cookbook_specs(CookbookSpecs) when is_list(CookbookSpecs) ->
    [ generate_cookbook_and_versions(Spec) || Spec <- CookbookSpecs ].

make_cookbook_version_from_spec(CookbookPrefix, Properties) when is_binary(CookbookPrefix),
                                                                 is_list(Properties) ->

    %% Serialized Object Creation
    BaseObject = {[]},
    RecipeNames = proplists:get_value(recipe_names, Properties, []),
    SerializedObject = insert_recipe_manifest_for_names(BaseObject, RecipeNames),

    %% Dependencies
    Dependencies = proplists:get_value(dependencies, Properties, []),

    Cookbook = make_cookbook(CookbookPrefix),

    Version = proplists:get_value(version, Properties),

    make_cookbook_version(cookbook_version_prefix(CookbookPrefix, Version),
                          Version,
                          Cookbook,
                          [{serialized_object, SerializedObject},
                           {dependencies, Dependencies}]).

%% @doc Given a cookbook spec, generate records for the indicated
%% cookbook versions and insert them into the database.  Returns the
%% cookbook information along with the cookbook version records (you
%% can think of this as "inflating" the original cookbook spec, if you
%% like).
-spec generate_cookbook_and_versions(cookbook_spec()) -> {cookbook(), [#chef_cookbook_version{}]}.
generate_cookbook_and_versions({CookbookPrefix, Versions}) when is_binary(CookbookPrefix),
                                                                is_list(Versions) ->
    Cookbook = make_cookbook(CookbookPrefix),
    CookbookVersions = [ make_cookbook_version_from_spec(CookbookPrefix, VersionSpec)
                         || VersionSpec <- Versions ],
    add_cookbook_versions_to_db(CookbookVersions),
    {Cookbook, CookbookVersions}.

-spec add_cookbook_versions_to_db([#chef_cookbook_version{}]) -> ok.
add_cookbook_versions_to_db(CookbookVersions) when is_list(CookbookVersions)->
    [ chef_sql:create_cookbook_version(V) || V <- CookbookVersions ],
    ok.

%% @doc Utility function to generate a unique Cookbook Version prefix
%% from a Cookbook prefix and a version
cookbook_version_prefix(CookbookPrefix, {_Major, _Minor, _Patch}=Version) ->
    iolist_to_binary([CookbookPrefix,
                      <<"_ver_">>,
                      version_tuple_to_binary(Version)]).

%% @doc Converts a version tuple to a binary string.
%%
%% Example: {1,0,0} -> <<"1.0.0">>
version_tuple_to_binary({Major, Minor, Patch}) ->
    iolist_to_binary([integer_to_binary(Major), <<".">>,
                      integer_to_binary(Minor), <<".">>,
                      integer_to_binary(Patch)]).

-spec recipes_from_cookbook_specs([cookbook_spec()]) -> [QualifiedRecipeName::binary()].
recipes_from_cookbook_specs(Specs) ->
    Unfolded = unfold_specs(Specs),
    CookbookNames = lists:sort(proplists:get_keys(Unfolded)),
    lists:flatmap(fun(CookbookName) ->
                          %% Grab just the recipe names from the latest version
                          [Latest] = latest_versions_from_unfolded_specs(CookbookName,
                                                                         Unfolded,1),

                          RecipeNames = proplists:get_value(recipe_names, Latest, []),
                          qualified_recipe_names(CookbookName, RecipeNames)
                  end,
                  CookbookNames).

%% @doc Generate a sorted list of cookbook-qualified recipe names
qualified_recipe_names(CookbookName, RecipeNames) ->
    [ iolist_to_binary([CookbookName, <<"::">>, Name])
      || Name <- lists:sort(RecipeNames) ].

%% @doc Create a list of dummy recipe manifest items, based on the
%% recipe names
make_recipes(Names) ->
    [ make_recipe(Name) || Name <- Names ].

%% @doc Generates the EJson for a "dummy" recipe manifest item, based
%% solely on the desired name of the recipe
make_recipe(Name) ->
    {[{<<"name">>, Name},
      {<<"path">>, iolist_to_binary([<<"recipes/default/">>, Name, <<".rb">>])},
      {<<"checksum">>, <<"deadbeefdeadbeefdeadbeefdeadbeef">>},
      {<<"specificity">>, <<"default">>}]}.

%% @doc Given the EJson for the `serialized_object' field of a
%% cookbook version record, replace any existing recipe manifest with
%% one generated from `RecipeNames'.
insert_recipe_manifest_for_names(EJsonBody, RecipeNames) ->
    Recipes = make_recipes(RecipeNames),
    ej:set({<<"recipes">>}, EJsonBody, Recipes).

%% @doc Encode `EJson' to a JSON string, and then GZip it
encode_and_compress(EJson) ->
    JSON = chef_json:encode(EJson),
    zlib:gzip(JSON).

cookbook_name_from_prefix(Prefix) ->
    <<"cookbook_", Prefix/binary>>.

%% @doc For the purposes of these tests, you can either specify a
%% version as a single integer, or a full {Major, Minor, Patch} tuple.
%% In the case of the former, the "real" version will be {0,0,Patch},
%% in accordance with the pattern of our cookbook-related testing
%% functions up to this point.
-spec version_tuple( non_neg_integer() | version() | #chef_cookbook_version{}) -> version().
version_tuple(Patch) when is_integer(Patch) ->
    {0,0,Patch};
version_tuple(#chef_cookbook_version{major = Maj, minor = Min, patch = Patch}) ->
    {Maj, Min, Patch};
version_tuple(Version) when is_tuple(Version) ->
    Version.

make_cookbook_version(Prefix, Version, Cookbook) when is_integer(Version);
                                                      is_tuple(Version) ->
    make_cookbook_version(Prefix, Version, Cookbook, []).

%% @doc Create a chef_cookbook_version record for the given cookbook,
%% with optional properties.  Currently supported properties are:
%%
%% serialized_object
%% dependencies
%%
%% More can be added in the future as needed.
make_cookbook_version(Prefix, Version, {AuthzId, OrgId, Name}, Properties) when is_integer(Version);
                                                                                is_tuple(Version) ->
    Id = chef_test_suite_helper:make_id(Prefix),
    {Major, Minor, Patch} = version_tuple(Version),

    #chef_cookbook_version{id=Id,
                           authz_id=AuthzId,
                           org_id=OrgId,
                           name=Name,
                           major=Major, minor=Minor, patch=Patch, frozen=false,
                           meta_attributes=Prefix,
                           meta_deps= process_property(dependencies, Properties),
                           meta_long_desc= <<"">>,
                           metadata=Prefix,
                           last_updated_by= chef_test_suite_helper:actor_id(),
                           created_at= {datetime, {{2011,10,1},{16,47,46}}},
                           updated_at= {datetime, {{2011,10,1},{16,47,46}}},
                           serialized_object= process_property(serialized_object, Properties),
                           checksums = [] }.

%% @doc Handle various cookbook version properties intelligently,
%% ensuring that data is correctly formatted and that sane defaults
%% are used.
process_property(serialized_object=Property, Properties) ->
    case proplists:lookup(Property, Properties) of
        {Property, <<31, 139, _Rest/binary>>=Value} ->
            %% Already GZipped
            Value;
        {Property, JSON} when is_binary(JSON) ->
            zlib:gzip(JSON);
        {Property, EJson} ->
            itest_cookbook_util:encode_and_compress(EJson);
        none ->
            itest_cookbook_util:encode_and_compress({[]})
    end;
process_property(dependencies=Property, Properties) ->
    case proplists:lookup(Property, Properties) of
        {Property, Binary} when is_binary(Binary) ->
            %% Already JSON
            Binary ;
        {Property, PropertyList} when is_list(PropertyList)->
            chef_json:encode({PropertyList});
        none ->
            chef_json:encode({[]})
    end.

cookbook_version_list(Cookbook) ->
    [ make_cookbook_version(<<"01">>, 1, Cookbook),
      make_cookbook_version(<<"02">>, 2, Cookbook) ].

%% @doc Helper function for testing checksum existence.
-spec checksum_exists(OrgId :: binary(), ChecksumId :: binary()) ->
                             boolean() | {error, term()}.
checksum_exists(OrgId, ChecksumId) ->
    case sqerl:select(<<"SELECT checksum FROM checksums WHERE org_id = $1 AND checksum = $2">>, [OrgId, ChecksumId], first_as_scalar, [checksum]) of
        {ok, Checksum} -> Checksum =/= none;
        {error, Reason} -> {error, Reason}
    end.


