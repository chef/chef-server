%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author James Casey <james@opscode.com>
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


-module(chef_cookbook).

-export([
         assemble_cookbook_ejson/1,
         assemble_cookbook_ejson_with_s3urls/1,
         minimal_cookbook_ejson/1,
         minimal_cookbook_ejson_with_s3urls/1,
         parse_binary_json/2,
         extract_checksums/1,
         version_to_binary/1,
         parse_version/1,
         qualified_recipe_names/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("chef_types.hrl").

%% FIXME:  This should be pulled out into app.config
-define(GET_URL_TTL, 900).

-define(DEFAULT_FIELD_VALUES,
        [
         {<<"json_class">>, <<"Chef::CookbookVersion">>},
         {<<"chef_type">>, <<"cookbook_version">>},
         {<<"frozen?">>, false}
        ]).


%% ["definitions", "name", "attributes", "files", "json_class", "providers", "metadata",
%%  "libraries", "templates", "resources", "cookbook_name", "version", "recipes", "root_files",
%%  "frozen?", "chef_type"]

-define(VALIDATION_CONSTRAINTS,
        [
         {<<"name">>,               {match, "^[[:alnum:]_:.-]+$"}},
         {<<"cookbook_name">>,      {match, "^[[:alnum:]_:.-]+$"}},
         {<<"json_class">>,         {match, "Chef::CookbookVersion"}},
         {<<"chef_type">>,          {match, "cookbook_version"}}
         %% FIXME: more to come soon
        ]).

%% @doc Convert a binary JSON string representing a Chef Cookbook Version into an
%% EJson-encoded Erlang data structure.
%% @end
-spec parse_binary_json(binary(), {binary(), binary()}) -> { ok, ejson_term() }. % or throw
parse_binary_json(Bin, {UrlName, UrlVersion}) ->
    %% avoid parsing the cookbook JSON if name or version from URL is invalid
    valid_name(UrlName),
    valid_version(UrlVersion),
    Cookbook0 = ejson:decode(Bin),
    Cookbook = set_default_values(Cookbook0),
    validate_cookbook(Cookbook, {UrlName, UrlVersion}).


%% TODO: merge set_default_values and validate_role?

%% @doc If certain fields are missing from a Role, fill them in with
%% sane default values.
-spec set_default_values(ejson_term()) -> ejson_term().
set_default_values(Cookbook) ->
    set_default_values(Cookbook, ?DEFAULT_FIELD_VALUES).
set_default_values(Cookbook, Defaults) ->
    lists:foldl(fun({Key, Default}, Current) ->
                        case ej:get({Key}, Current) of
                            undefined ->
                                ej:set({Key}, Current, Default);
                            _ -> Current
                        end
                end,
                Cookbook,
                Defaults).

-spec validate_cookbook(Cookbook :: ej:json_object(),
                        {UrlName :: binary(),
                         UrlVersion :: binary()}) -> {ok, ej:json_object()}.
validate_cookbook(Cookbook, {UrlName, UrlVersion}) ->
    %% WARNING: UrlName and UrlVersion are assumed to be valid
    case ej:valid(cookbook_spec(UrlName, UrlVersion), Cookbook) of
        ok -> {ok, Cookbook};
        Bad -> throw(Bad)
    end.

valid_name(Name) ->
    {Regex, Msg} = chef_regex:regex_for(cookbook_name),
    case re:run(Name, Regex) of
        nomatch ->
            throw({bad_cookbook_name, Name, Msg});
        _ ->
            ok
    end.

valid_version(Version) ->
    {Regex, Msg} = chef_regex:regex_for(cookbook_version),
    case re:run(Version, Regex) of
        nomatch ->
            throw({bad_cookbook_version, Version, Msg});
        _ ->
            ok
    end.

cookbook_spec(CBName, CBVersion) ->
    {[
      {<<"name">>, <<CBName/binary, "-", CBVersion/binary>>},
      {<<"cookbook_name">>, CBName},
      {<<"json_class">>, <<"Chef::CookbookVersion">>},
      {<<"chef_type">>, <<"cookbook_version">>},
      {<<"metadata">>, {[
                        {<<"version">>, CBVersion},
                        {{opt, <<"name">>}, {string_match, chef_regex:regex_for(cookbook_name)}},
                        {{opt, <<"description">>}, string},
                        {{opt, <<"long_description">>}, string},
                        {{opt, <<"maintainer">>}, string},
                        %% do we want to regex on email address or at least verify that we
                        %% have a '@'?
                        {{opt, <<"maintainer_email">>}, string},
                        {{opt, <<"license">>}, string},

                        {{opt, <<"platforms">>}, constraint_map_spec(cookbook_name)},

                        {{opt, <<"dependencies">>}, constraint_map_spec(cookbook_name)},
                        {{opt, <<"recommendations">>}, constraint_map_spec(cookbook_name)},
                        {{opt, <<"suggestions">>}, constraint_map_spec(cookbook_name)},
                        {{opt, <<"conflicting">>}, constraint_map_spec(cookbook_name)},
                        {{opt, <<"replacing">>}, constraint_map_spec(cookbook_name)},

                        {{opt, <<"providing">>}, constraint_map_spec(recipe_name)},
                         %% FIXME: what's this?
                         {{opt, <<"groupings">>}, object}

                       ]}},
      {{opt, <<"attributes">>}, file_list_spec()},
      {{opt, <<"definitions">>}, file_list_spec()},
      {{opt, <<"files">>}, file_list_spec()},
      {{opt, <<"libraries">>}, file_list_spec()},
      {{opt, <<"providers">>}, file_list_spec()},
      {{opt, <<"recipes">>}, file_list_spec()},
      {{opt, <<"resources">>}, file_list_spec()},
      {{opt, <<"root_files">>}, file_list_spec()},
      {{opt, <<"templates">>}, file_list_spec()},
      {{opt, <<"version">>}, CBVersion}
     ]}.

file_list_spec() ->
    {array_map, {[{<<"name">>, string},
                  {<<"path">>, string},
                  {<<"checksum">>, string},
                  {<<"specificity">>, string}
                 ]}}.

-define(COOKBOOK_SEGMENTS, [<<"attributes">>,
                            <<"definitions">>,
                            <<"files">>,
                            <<"libraries">>,
                            <<"providers">>,
                            <<"recipes">>,
                            <<"resources">>,
                            <<"root_files">>,
                            <<"templates">>]).

extract_checksums(CBJson) ->
    Sums = [ begin
                 Segment = ej:get({SegName}, CBJson, []),
                 extract_checksums_from_segment(Segment)
             end || SegName <- ?COOKBOOK_SEGMENTS ],
    lists:usort(lists:append(Sums)).

extract_checksums_from_segment(Segment) ->
    [ ej:get({<<"checksum">>}, Item) || Item <- Segment ].

%% FIXME: It isn't clear to me if we want to validate the platforms data
%% separately. Technically the names and versions might not conform to our cookbook-specific
%% policy since they describe external entities like Ubuntu, OS X, etc. However, the
%% existing Ruby code uses the same validation across the board.
%%
%% generic_contraint_map_spec() ->
%%     {object_map,
%%      {{keys, string},
%%       {values, {fun_match, {fun valid_constraint/1, string,
%%                             <<"Invalid version constraint">>}}}}}.

constraint_map_spec(RegexName) ->
    {object_map,
     {{keys, {string_match, chef_regex:regex_for(RegexName)}},
      {values, {fun_match, {fun valid_cookbook_constraint/1, string,
                            <<"Invalid version constraint">>}}}}}.

valid_cookbook_constraint(Str) when is_binary(Str) ->
    case chef_object:parse_constraint(Str) of
        {_Constr, Version} ->
            {Regex, _Msg} = chef_regex:regex_for(cookbook_version),
            case re:run(Version, Regex) of
                nomatch ->
                    error;
                _ ->
                    ok
            end;
        error ->
            error
    end;
valid_cookbook_constraint([Str]) ->
    %% allow an array with a single constraint for back-compat (see
    %% chef/version_constraint.rb).
    valid_cookbook_constraint(Str);
valid_cookbook_constraint([_Str|_Rest]) ->
    %% disallow multiple contraints
    error.

parse_version(Version) when is_binary(Version) ->
    Parts = [list_to_integer(binary_to_list(V))
             || V <- binary:split(Version, <<".">>, [global])],
    case length(Parts) of
        3 -> list_to_tuple(Parts);
        _ -> error(badarg)
    end.

%% @doc given a version tuple {Major, Minor, Patch} return it as a
%% binary()
%% @end
-spec version_to_binary({Major::non_neg_integer(),
                         Minor::non_neg_integer(),
                         Patch::non_neg_integer()}) ->
                     binary().
version_to_binary({Major, Minor, Patch}) ->
    iolist_to_binary([integer_to_list(Major), ".",
                      integer_to_list(Minor), ".",
                      integer_to_list(Patch)]).

%% @doc "Rehydrates" a versioned cookbook as a complete, fully decompressed EJson data
%% structure.
%%
%% The Ejson structure is a 1-1 mapping from the #chef_cookbook_version{} and does
%% not contain any extra information, e.g. S3 URLs.
-spec assemble_cookbook_ejson(#chef_cookbook_version{}) -> ejson_term().
assemble_cookbook_ejson(#chef_cookbook_version{
                           frozen=Frozen,
                           serialized_object=XCookbookJSON,
                           metadata=XMetadataJSON,
                           meta_attributes=XMetaAttributesJSON,
                           meta_long_desc=XLongDescription,
                           meta_deps=DependenciesJSON}) ->
    %% The serialized_object is everything but the metadata, and metadata in turn is all the
    %% metadata except the attributes, long description, and dependencies.  All need to be
    %% merged back together.
    %%
    %% The dependencies are stored as an uncompressed JSON string, the long description is
    %% just a compressed string (not JSON), and the attributes and metadata are both
    %% compressed JSON strings.

    Metadata = lists:foldl(fun({Key, Data}, MD) ->
                                   ej:set({Key}, MD, inflate(Key, Data))
                           end,
                           inflate(<<"metadata">>, XMetadataJSON),
                           [{<<"attributes">>, XMetaAttributesJSON},
                            {<<"dependencies">>, DependenciesJSON},
                            {<<"long_description">>, XLongDescription}]),

    %% Now that the metadata is assembled, piece the final cookbook together
    lists:foldl(fun({Key, Data}, CB) ->
                        ej:set({Key}, CB, Data)
                end,
                inflate(<<"cookbook">>, XCookbookJSON),
                [{<<"frozen?">>, Frozen},
                 {<<"metadata">>, Metadata}]).

-spec minimal_cookbook_ejson(#chef_cookbook_version{}) -> ejson_term().
minimal_cookbook_ejson(#chef_cookbook_version{frozen=Frozen,
                                              serialized_object=XCookbookJSON,
                                              metadata=XMetadataJSON}) ->
    %% The serialized_object is everything but the metadata, and metadata in turn is all the
    %% metadata except the attributes, long description, and dependencies.  We do not
    %% add in the sub pieces of the metadata when merging

    Metadata = inflate(<<"metadata">>, XMetadataJSON),

    lists:foldl(fun({Key, Data}, CB) ->
                        ej:set({Key}, CB, Data)
                end,
                inflate(<<"cookbook">>, XCookbookJSON),
                [{<<"frozen?">>, Frozen},
                 {<<"metadata">>, Metadata}]).

%% @doc "Rehydrates" a versioned cookbook as a decompressed EJson data
%% structure.
%%
%% It also includes s3 urls for each file in segements.  This can be used by clients which then
%% need to subsequently download the files of the cookbook
-spec assemble_cookbook_ejson_with_s3urls(CookbookVersion::#chef_cookbook_version{}) -> ejson_term().
assemble_cookbook_ejson_with_s3urls(#chef_cookbook_version{org_id = OrgId} = CookbookVersion) ->
    annotate_with_s3_urls(assemble_cookbook_ejson(CookbookVersion), OrgId).

%% @doc "Rehydrates" a minimal versioned cookbook as a decompressed EJson data structure which
%% can be used by chef-client when loading cookbooks to apply
%%
%% It also includes s3 urls for each file in segements.  This can be used by clients which then
%% need to subsequently download the files of the cookbook
-spec minimal_cookbook_ejson_with_s3urls(CookbookVersion::#chef_cookbook_version{}) -> ejson_term().
minimal_cookbook_ejson_with_s3urls(#chef_cookbook_version{org_id = OrgId} = CookbookVersion) ->
    annotate_with_s3_urls(minimal_cookbook_ejson(CookbookVersion), OrgId).

%% @doc Add S3 download URLs for all files in the cookbook
annotate_with_s3_urls(Ejson, OrgId) ->
    lists:foldl(fun(Segment, CB) ->
                    case ej:get({Segment}, CB) of
                        undefined -> CB;
                        Data ->
                            WithUrls = add_urls_for_segment(OrgId, Data),
                            ej:set({Segment}, CB, WithUrls)
                    end
                end,
                Ejson,
                ?COOKBOOK_SEGMENTS).

%% @doc Return a sorted list of cookbook-qualified names, given a cookbook name and its
%% compressed 'serialized object' representation.
%%
%% Note that results are NOT sorted (callers of this code generally combine results from
%% many cookbooks, so sorting at this stage would be superfluous).
-spec qualified_recipe_names(CookbookName :: binary(),
                             SerializedObject :: binary()) -> RecipeNames :: [binary()].
qualified_recipe_names(CookbookName, SerializedObject) ->
    Recipes = extract_recipe_names(SerializedObject),
    [ maybe_qualify_name(CookbookName, RecipeName)
      || RecipeName <- Recipes ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Takes a compressed binary string, representing components of a chef_cookbook_version
%% record and decompresses (if necessary) and converts JSON to EJson (also, if necessary).
%%
%% Note that `Type' is binary because of how this function gets used in
%% `assemble_cookbook_ejson', where the data that gets passed in also gets used as keys in
%% an EJson structure
-spec inflate(Type :: binary(), Data :: binary()) -> ejson_term() | binary().
inflate(<<"long_description">>, Data) ->
    chef_db_compression:decompress(Data);
inflate(<<"dependencies">>, Data) ->
    ejson:decode(Data);
inflate(_Type, Data) ->
    chef_db_compression:decompress_and_decode(Data).

%% @doc Given the compressed JSON string representing a Cookbook Version object, extracts
%% the names of all the recipes for that Cookbook Version.  NOTE: results are NOT sorted.
%%
%% Originally intended to operate on the `serialized_object' data of a Cookbook Version (the
%% piece that actually has the recipes in it), but could also work on the gzipped JSON
%% string for the entire cookbook, if that's ever a thing you'd want to do.
-spec extract_recipe_names(XCookbookJSON::binary()) -> [RecipeName::binary()].
extract_recipe_names(<<31, 139, _Rest/binary>>=XCookbookJSON) ->
    EJson = chef_db_compression:decompress_and_decode(XCookbookJSON),

    %% Pull out just the recipe segment of the serialized object
    Manifest = ej:get({<<"recipes">>}, EJson),

    %% Collect just the name of each recipe in the manifest.  Results are NOT sorted in the
    %% end.
    [ ej:get({<<"name">>}, Recipe) || Recipe <- Manifest].

%% @doc given a list of files for a particular segment add in a S3 URL per file
%% based on the checksum
add_urls_for_segment(OrgId, FileList) ->
    [ ej:set({<<"url">>},
             File,
             chef_s3:generate_presigned_url(OrgId,
                                            ?GET_URL_TTL,
                                            get,
                                            ej:get({<<"checksum">>}, File)))
     || File <- FileList].

%% @doc Removes any ".rb" suffix from a recipe name
-spec recipe_name(Name :: binary()) -> NameWithoutSuffix :: binary().
recipe_name(Name) when is_binary(Name) ->
    filename:basename(Name, <<".rb">>).

%% @doc Generate a proper, possibly cookbook-qualified name from a
%% cookbook name and a recipe name.  Any Ruby filename suffix is
%% removed from the recipe name, and default recipes take just the
%% name of the cookbook.
-spec maybe_qualify_name(CookbookName :: binary(),
                         RecipeName :: binary()) -> MaybeQualifiedName :: binary().
maybe_qualify_name(CookbookName, RecipeName) ->
    %% Remove ".rb" suffix, if present
    StrippedName = recipe_name(RecipeName),

    %% "default" recipes don't need to be cookbook-qualified
    case StrippedName of
        <<"default">> ->
            CookbookName;
        _ ->
            <<CookbookName/binary, "::", StrippedName/binary>>
    end.
