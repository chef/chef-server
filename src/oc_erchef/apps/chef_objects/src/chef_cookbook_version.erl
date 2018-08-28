%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author James Casey <james@chef.io>
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

-module(chef_cookbook_version).

-include("chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").


-export([
         assemble_cookbook_ejson/2,
         assemble_cookbook_ejson/3,
         annotate_with_urls/3,
         authz_id/1,
         base_cookbook_name/1,
         constraint_map_spec/1,
         single_cookbook_version_spec/0,
         valid_cookbook_constraint/1,
         ejson_for_indexing/2,
         ensure_v0_metadata/1,
         ensure_v2_metadata/1,
         extract_checksums/1,
         fields_for_update/1,
         fields_for_insert/1,
         fields_for_fetch/1,
         id/1,
         is_indexed/1,
         minimal_cookbook_ejson/3,
         name/1,
         org_id/1,
         new_record/4,
         parse_binary_json/2,
         parse_binary_json/3,
         parse_version/1,
         qualified_recipe_names/2,
         record_fields/1,
         set_created/2,
         set_updated/2,
         set_api_version/2,
         type_name/1,
         update_from_ejson/2,
         version_to_binary/1,
         wants_all_files/1,
         list/2
        ]).

%% database named queries
-export([
         bulk_get_query/1,
         create_query/1,
         delete_query/1,
         find_query/1,
         list_query/1,
         update_query/1
        ]).

-mixin([{chef_object_default_callbacks, [ fetch/2, update/2 ]}]).

-ifdef(TEST).
-compile(export_all).
-endif.


%% This is the maximum size of an int value in postgres used to store major, minor, and
%% patch versions.
-define(MAX_VERSION, 9223372036854775807).

-define(DEFAULT_S3_URL_TTL, 900).

-define(DEFAULT_FIELD_VALUES,
        [
         {<<"json_class">>, <<"Chef::CookbookVersion">>},
         {<<"chef_type">>, <<"cookbook_version">>},
         {<<"frozen?">>, false}
        ]).

-define(VALID_KEYS_V2,
        [<<"all_files">>, <<"chef_type">>, <<"cookbook_name">>, <<"frozen?">>,
         <<"json_class">>, <<"metadata">>, <<"name">>, <<"version">>]).

-define(VALID_KEYS,
        [<<"attributes">>, <<"chef_type">>, <<"cookbook_name">>,
         <<"definitions">>, <<"files">>, <<"frozen?">>, <<"json_class">>, <<"libraries">>,
         <<"metadata">>, <<"name">>, <<"providers">>, <<"recipes">>, <<"resources">>,
         <<"root_files">>, <<"templates">>, <<"version">>]).

-define(VALIDATION_CONSTRAINTS,
        [
         {<<"name">>,               {match, "^[[:alnum:]_:.-]+$"}},
         {<<"cookbook_name">>,      {match, "^[[:alnum:]_:.-]+$"}},
         {<<"json_class">>,         {match, "Chef::CookbookVersion"}},
         {<<"chef_type">>,          {match, "cookbook_version"}}
         %% FIXME: more to come soon
        ]).

-behaviour(chef_object).

authz_id(#chef_cookbook_version{authz_id = AuthzId})->
    AuthzId.

-spec name(#chef_cookbook_version{}) -> binary().
name(#chef_cookbook_version{name = Name}) ->
    Name.

-spec id(#chef_cookbook_version{}) -> object_id().
id(#chef_cookbook_version{id = Id}) ->
    Id.

-spec org_id(#chef_cookbook_version{}) -> object_id().
org_id(#chef_cookbook_version{org_id = OrgId}) ->
    OrgId.

-spec set_created(#chef_cookbook_version{}, object_id()) -> #chef_cookbook_version{}.
set_created(#chef_cookbook_version{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_cookbook_version{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

-spec set_updated(#chef_cookbook_version{}, object_id()) -> #chef_cookbook_version{}.
set_updated(#chef_cookbook_version{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_cookbook_version{updated_at = Now, last_updated_by = ActorId}.

%% TODO: this doesn't need an argument
type_name(#chef_cookbook_version{}) ->
    cookbook_version.

-spec new_record(api_version(), object_id(), object_id(), ejson_term()) -> #chef_cookbook_version{}.
new_record(ApiVersion, OrgId, AuthzId, CBVData) ->
    %% name for a cookbook_version is actually cb_name-cb_version which is good for ID
    %% creation
    Name = ej:get({<<"name">>}, CBVData),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    {Major, Minor, Patch} = parse_version(ej:get({<<"metadata">>, <<"version">>},
                                                 CBVData)),

    Metadata0 = ej:get({<<"metadata">>}, CBVData),

    MAttributes = compress_maybe(ej:get({<<"attributes">>}, Metadata0, {[]}),
                                 cookbook_meta_attributes),

    %% Do not compress the deps!
    Deps = chef_json:encode(ej:get({<<"dependencies">>}, Metadata0, {[]})),

    LongDesc = compress_maybe(ej:get({<<"long_description">>}, Metadata0, <<"">>),
                              cookbook_long_desc),

    Metadata = compress_maybe(lists:foldl(fun(Key, MD) ->
                                                  ej:delete({Key}, MD)
                                          end, Metadata0, [<<"attributes">>,
                                                           <<"dependencies">>,
                                                           <<"long_description">>]),
                              cookbook_metadata),

    Data = compress_maybe(ej:delete({<<"metadata">>}, CBVData),
                          chef_cookbook_version),
    #chef_cookbook_version{server_api_version = ApiVersion,
                           id = Id,
                           authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
                           org_id = OrgId,
                           name = ej:get({<<"cookbook_name">>}, CBVData),
                           major = Major,
                           minor = Minor,
                           patch = Patch,
                           frozen = ej:get({<<"frozen?">>}, CBVData, false),
                           meta_attributes = MAttributes,
                           meta_deps = Deps,
                           meta_long_desc = LongDesc,
                           metadata = Metadata,
                           checksums = extract_checksums(CBVData),
                           serialized_object = Data}.

compress_maybe(Data, cookbook_long_desc) ->
    chef_db_compression:compress(cookbook_long_desc, Data);
compress_maybe(Data, Type) ->
    chef_db_compression:compress(Type, chef_json:encode(Data)).

%% @doc Convert a binary JSON string representing a Chef Cookbook Version into an
%% EJson-encoded Erlang data structure.
%% @end
-spec parse_binary_json(binary(), {binary(), binary()}, boolean()) -> { ok, ejson_term() }. % or throw
parse_binary_json(Bin, {UrlName, UrlVersion}, AllFiles) ->
    %% avoid parsing the cookbook JSON if name or version from URL is invalid
    valid_name(UrlName),
    valid_version(UrlVersion),
    Cookbook0 = chef_json:decode(Bin),
    Cookbook = set_default_values(Cookbook0),
    validate_cookbook(Cookbook, {UrlName, UrlVersion}, AllFiles).

parse_binary_json(Bin, {UrlName, UrlVersion}) ->
    parse_binary_json(Bin, {UrlName, UrlVersion}, false).

%% TODO: merge set_default_values and validate_role?

%% @doc If certain fields are missing from a Role, fill them in with
%% sane default values.
-spec set_default_values(ejson_term()) -> ejson_term().
set_default_values(Cookbook) ->
    chef_object_base:set_default_values(Cookbook, ?DEFAULT_FIELD_VALUES).

-spec validate_cookbook(Cookbook :: ej:json_object(),
                        {UrlName :: binary(), UrlVersion :: binary()},
                        AllFiles :: boolean()) -> {ok, ej:json_object()}.
validate_cookbook(Cookbook, {UrlName, UrlVersion}, AllFiles) when AllFiles =:= false ->
    Spec = cookbook_spec(UrlName, UrlVersion),
    case chef_object_base:strictly_valid(Spec, ?VALID_KEYS, Cookbook) of
        ok -> {ok, Cookbook};
        Bad -> throw(Bad)
    end;
validate_cookbook(Cookbook, {UrlName, UrlVersion}, _) ->
    %% WARNING: UrlName and UrlVersion are assumed to be valid
    Spec = cookbook_spec_v2(UrlName, UrlVersion),

    case chef_object_base:strictly_valid(Spec, ?VALID_KEYS_V2, Cookbook) of
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
    case is_valid_version(Version) of
        false ->
            throw({bad_cookbook_version, Version, <<"Invalid cookbook version">>});
        true ->
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
                        %% Accept anything for "providing"
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

cookbook_spec_v2(CBName, CBVersion) ->
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

                        {{opt, <<"dependencies">>}, constraint_map_spec(cookbook_name)}
                       ]}},
      {{opt, <<"all_files">>}, file_list_spec()},
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

-spec extract_checksums(ejson_term()) -> [binary()].
extract_checksums(CBJson) ->
    Segments = [ <<"all_files">> | ?COOKBOOK_SEGMENTS ],
    Sums = [ begin
                 Segment = ej:get({SegName}, CBJson, []),
                 extract_checksums_from_segment(Segment)
             end || SegName <- Segments ],
    lists:usort(lists:append(Sums)).

extract_checksums_from_segment(Segment) ->
    [ ej:get({<<"checksum">>}, Item) || Item <- Segment ].

constraint_map_spec(RegexName) ->
    {object_map,
     {{keys, {string_match, chef_regex:regex_for(RegexName)}},
      {values, single_cookbook_version_spec()}}}.

single_cookbook_version_spec() ->
    {fun_match, {fun valid_cookbook_constraint/1, string,
                 <<"Invalid version constraint">>}}.

valid_cookbook_constraint(Str) when is_binary(Str) ->
    case chef_object_base:parse_constraint(Str) of
        {_Constr, Version} ->
            case is_valid_version(Version) of
                false ->
                    error;
                true ->
                    ok
            end;
        error ->
            error
    end.

%% Returns true if `Version' is a binary that can be parsed into a valid cookbook version
%% and false otherwise.
-spec is_valid_version(binary()) -> boolean().
is_valid_version(Version) ->
    %% since parse_version uses list_to_integer blindly, try/catch on badarg is the best way
    %% to capture all invalid cases.
    try
        {_, _, _} = parse_version(Version),
        true
    catch
        error:badarg ->
            false
    end.

%% @doc Given a binary parse it to a valid cookbook version tuple {Major, Minor, Patch} or
%% raise a `badarg' error. Each of `Major', `Minor', and `Patch' must be non-negative
%% integer values less than `?MAX_VERSION' (max size of value in pg int column). It is
%% acceptable to provide a value with one or two dots (1.0 is the same as 1.0.0). Less
%% than one dot or more than two dots is an error.
%%
%% @end
-spec parse_version(Version::binary()) -> {Major::non_neg_integer(),
                                           Minor::non_neg_integer(),
                                           Patch::non_neg_integer()}.
parse_version(Version) when is_binary(Version) ->
    Parts = [list_to_integer(binary_to_list(V))
             || V <- binary:split(Version, <<".">>, [global])],
    %% normalize will error if too many dotted elements
    VList = normalize_version(Parts),
    NotNeg = not_neg(VList),
    NotLarger = not_larger_than(?MAX_VERSION, VList),
    case {NotNeg, NotLarger} of
        {true, true} ->
            list_to_tuple(VList);
        _ ->
            error(badarg)
    end.


normalize_version([X, Y]) ->
    [X, Y, 0];
normalize_version([_, _, _] = V) ->
    V;
normalize_version(_) ->
    error(badarg).

not_neg(L) ->
    not lists:any(fun(X) when X < 0 ->
                          true;
                     (_) ->
                          false
                  end, L).

not_larger_than(Max, L) ->
    not lists:any(fun(X) when X > Max ->
                          true;
                     (_) ->
                          false
                  end, L).

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
-spec assemble_cookbook_ejson(#chef_cookbook_version{}, string(), boolean()) -> ejson_term().
assemble_cookbook_ejson(#chef_cookbook_version{
                           org_id = OrgId,
                           frozen=Frozen,
                           serialized_object=XCookbookJSON,
                           metadata=XMetadataJSON,
                           meta_attributes=XMetaAttributesJSON,
                           meta_long_desc=XLongDescription,
                           meta_deps=DependenciesJSON}, ExternalUrl, AllFiles) ->
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
    CBJson = inflate(<<"cookbook">>, XCookbookJSON),
    CBJson1 = case AllFiles of
                 true -> ensure_v2_metadata(CBJson);
                 false -> ensure_v0_metadata(CBJson)
             end,
    CookbookJSON = annotate_with_urls(CBJson1, OrgId, ExternalUrl),

    %% Now that the metadata is assembled, piece the final cookbook together
    lists:foldl(fun({Key, Data}, CB) ->
                        ej:set({Key}, CB, Data)
                end,
                CookbookJSON,
                [{<<"frozen?">>, Frozen},
                 {<<"metadata">>, Metadata}]).

assemble_cookbook_ejson(CB, ExternalUrl) ->
    assemble_cookbook_ejson(CB, ExternalUrl, false).

-spec minimal_cookbook_ejson(#chef_cookbook_version{}, string(), integer()) -> ejson_term().
minimal_cookbook_ejson(#chef_cookbook_version{org_id = OrgId,
                                              frozen=Frozen,
                                              serialized_object=XCookbookJSON,
                                              metadata=XMetadataJSON,
                                              meta_deps=DependenciesJSON},
                       ExternalUrl,
                       ApiVersion) ->
    %% The serialized_object is everything but the metadata, and metadata in turn is all the
    %% metadata except the attributes and long description.  We do not add in the sub pieces
    %% of the metadata when merging

    Metadata0 = inflate(<<"metadata">>, XMetadataJSON),
    Metadata = ej:set({<<"dependencies">>}, Metadata0,
                      inflate(<<"dependencies">>, DependenciesJSON)),

    AllFiles = wants_all_files(ApiVersion),
    CBJson = inflate(<<"cookbook">>, XCookbookJSON),
    CBJson1 = case AllFiles of
                 true -> ensure_v2_metadata(CBJson);
                 false -> ensure_v0_metadata(CBJson)
             end,
    CookbookJSON = annotate_with_urls(CBJson1, OrgId, ExternalUrl),

    lists:foldl(fun({Key, Data}, CB) ->
                        ej:set({Key}, CB, Data)
                end,
                CookbookJSON,
                [{<<"frozen?">>, Frozen},
                 {<<"metadata">>, Metadata}]).

%% Annotate a set of segments with URLs, for v0 cookbook requests
annotate_segments_with_urls(Ejson, OrgId, ExternalUrl) ->
    lists:foldl(fun(Segment, CB) ->
                        case ej:get({Segment}, CB) of
                            undefined -> CB;
                            Data ->
                                WithUrls = add_urls_for_segment(OrgId, Data, ExternalUrl),
                                ej:set({Segment}, CB, WithUrls)
                        end
                end,
                Ejson,
                ?COOKBOOK_SEGMENTS).

%% @doc Add download URLs for all files in the cookbook
%% Expects the whole cookbook JSON
-spec annotate_with_urls(ejson_term(), object_id(), string()) -> ejson_term().
annotate_with_urls(Ejson, OrgId, ExternalUrl) ->
    case ej:get({<<"all_files">>}, Ejson) of
        undefined ->
            annotate_segments_with_urls(Ejson, OrgId, ExternalUrl);
        Data ->
            WithUrls = add_urls_for_segment(OrgId, Data, ExternalUrl),
            ej:set({<<"all_files">>}, Ejson, WithUrls)
    end.

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


%% @doc for a given qualified recipe name, extract the base cookbook
%% name. If the input is not a qualified recipe name, return it
%% unmodified.
-spec base_cookbook_name(binary() | string()) -> binary().
base_cookbook_name(Recipe) ->
    case re:split(Recipe, <<"::">>, [{return, binary}]) of
        [Cookbook, _Recipe] ->
            Cookbook;
        [Cookbook] ->
            Cookbook
    end.

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
    chef_json:decode(Data);
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
    Manifest = case ej:get({<<"all_files">>}, EJson) of
                   undefined ->
                       ej:get({<<"recipes">>}, EJson);
                   Data ->
                       get_specific_segment(<<"recipes">>, Data)
               end,

    %% Collect just the name of each recipe in the manifest.  Results are NOT sorted in the
    %% end.
    [ ej:get({<<"name">>}, Recipe) || Recipe <- Manifest].

get_specific_segment(Segment, Data) ->
    IsSegment = fun(Record) -> [Seg | _ ] = get_segment_from_record(Record), Seg == Segment end,
    lists:filter(IsSegment, Data).

get_segment_from_record(Record) ->
    Name = ej:get({<<"name">>}, Record),
    binary:split(Name, [<<"/">>]).

%% @doc Return the s3_url_ttl from the application environment, if it is
%% undefined return the default value set in ?DEFAULT_S3_URL_TTL
url_ttl() ->
    envy:get(chef_objects, s3_url_ttl, ?DEFAULT_S3_URL_TTL, pos_integer).

%% @doc Given a list of files for a particular segment add in a S3 URL per file
%% based on the checksum
add_urls_for_segment(OrgId, FileList, ExternalUrl) ->
    [ ej:set({<<"url">>},
             File,
             chef_s3:generate_presigned_url(OrgId,
                                            url_ttl(),
                                            get,
                                            ej:get({<<"checksum">>}, File),
                                            ExternalUrl))
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

create_query(_ObjectRec) ->
    insert_cookbook_version.

update_query(_ObjectRec) ->
    update_cookbook_version.

delete_query(_ObjectRec) ->
    delete_cookbook_version_by_id.

find_query(_ObjectRec) ->
    find_cookbook_version_by_orgid_name_version.

list_query(_ObjectRec) ->
    list_cookbook_versions_by_orgid.

bulk_get_query(_ObjectRec) ->
    error(not_implemented).

%% for v2 metadata, the "segment" is encoded in to the name of file
%% so for data that is v0 on disk we have to prepend the segment to the name
add_segment_to_filename(Segment, File) ->
    Fn = ej:get({<<"name">>}, File),
    Fn1 = iolist_to_binary([Segment, "/", Fn]),
    ej:set({<<"name">>}, File, Fn1).

remove_segment_from_filename(File) ->
    [Segment | Name ] = get_segment_from_record(File),
    case Name of
        % if Name is nil, then we have a root file (like metadata.rb) - so we'll set the name to the segment, and the segment to <<"root_files">>
        [] ->
            { <<"root_files">>, ej:set({<<"name">>}, File, Segment)};
        _ ->
            Record = ej:set({<<"name">>}, File, lists:last(Name)),
            { Segment, Record }
    end.

%% A v2 cookbook version contains only the "all_files" key, which is a list of all the file parts
%% { "all_files": [ { "name": "recipes/default.rb", "path": "recipes/default.rb", … } ] }
%% We have to transform that into segments (?COOKBOOK_SEGMENTS) containing a list of file parts for that segment
%% { "recipes": [ { "name": "default.rb", path: "recipes/default.rb", … } ] }
%%
%% Note: We silently drop files that land in segments not listed in ?COOKBOOK_SEGMENTS
populate_segments(Data, Metadata) ->
    BySegment = lists:foldl(fun file_by_segment/2, #{}, Data),

    lists:foldl(fun(Segment, CB) ->
                        ej:set({Segment}, CB, maps:get(Segment, BySegment, [] ))
                end,
                Metadata,
                ?COOKBOOK_SEGMENTS).

file_by_segment(File, Map) ->
    { Segment, Record } = remove_segment_from_filename(File),
    case Map of
        #{ Segment := List } ->
            Map#{Segment => [Record | List]};
        _ ->
            Map#{Segment => [Record]}
    end.

%% Minor note:
%% The old populate_all_files code replaced the all_files entry; if there were multiple segments we would end up with only the last
%% Check test coverage to make sure that makes sense.
ensure_v2_metadata(Ejson) ->
    case ej:get({<<"all_files">>}, Ejson) of
        undefined ->
            AllFiles = lists:flatten([ expand_segment(Segment, ej:get({Segment}, Ejson, []) ) || Segment <- ?COOKBOOK_SEGMENTS ] ),
            Ejson1 = ej:set({<<"all_files">>}, Ejson, AllFiles),

            lists:foldl(fun(Segment, CB) -> ej:delete({Segment}, CB) end,
                        Ejson1,
                        ?COOKBOOK_SEGMENTS);
        _ ->
            Ejson
    end.

expand_segment(Segment, Data) ->
    [ add_segment_to_filename(Segment, File) || File <- Data ].

ensure_v0_metadata(Ejson) ->
    case ej:get({<<"all_files">>}, Ejson) of
        undefined -> Ejson;
        Data ->
            Ejson1 = populate_segments(Data, Ejson),
            ej:delete({<<"all_files">>}, Ejson1)
    end.

update_from_ejson(#chef_cookbook_version{server_api_version = ApiVersion,
                                         org_id = OrgId,
                                         authz_id = AuthzId,
                                         frozen = FrozenOrig} = CookbookVersion,
                  CookbookVersionData) ->
    UpdatedVersion = new_record(ApiVersion, OrgId, AuthzId, CookbookVersionData),
    %% frozen is immutable once it is set to true
    Frozen = FrozenOrig =:= true orelse UpdatedVersion#chef_cookbook_version.frozen,
    CookbookVersion#chef_cookbook_version{frozen            = Frozen,
                                          meta_attributes   = UpdatedVersion#chef_cookbook_version.meta_attributes,
                                          meta_deps         = UpdatedVersion#chef_cookbook_version.meta_deps,
                                          meta_long_desc    = UpdatedVersion#chef_cookbook_version.meta_long_desc,
                                          metadata          = UpdatedVersion#chef_cookbook_version.metadata,
                                          checksums         = UpdatedVersion#chef_cookbook_version.checksums,
                                          serialized_object = UpdatedVersion#chef_cookbook_version.serialized_object}.

is_indexed(_ObjectRec) ->
    false.

ejson_for_indexing(#chef_cookbook_version{}, _CBV) ->
    error(not_indexed).

fields_for_insert(#chef_cookbook_version{
                     'id' = Id,
                     'major' = Major,
                     'minor' = Minor,
                     'patch' = Patch,
                     'frozen' = Frozen,
                     'meta_attributes' = MetaAttributes,
                     'meta_deps' = MetaDeps,
                     'meta_long_desc' = MetaLongDesc,
                     'metadata' = Metadata,
                     'serialized_object' = SerializedObject,
                     'last_updated_by' = LastUpdatedBy,
                     'created_at' = CreatedAt,
                     'updated_at' = UpdatedAt,
                     'org_id' = OrgId,
                     'name' = Name}) ->
    [Id,
     Major,
     Minor,
     Patch,
     Frozen,
     MetaAttributes,
     MetaDeps,
     MetaLongDesc,
     Metadata,
     SerializedObject,
     LastUpdatedBy,
     CreatedAt,
     UpdatedAt,
     OrgId,
     Name].

fields_for_update(#chef_cookbook_version{ id                = Id,
                                          frozen            = Frozen,
                                          meta_attributes   = MetaAttributes,
                                          meta_deps         = MetaDeps,
                                          meta_long_desc    = MetaLongDesc,
                                          metadata          = Metadata,
                                          serialized_object = SerializeObject,
                                          last_updated_by   = LastUpdatedBy,
                                          updated_at        = UpdatedAt }) ->
    [Frozen, MetaAttributes, MetaDeps, MetaLongDesc, Metadata, SerializeObject, LastUpdatedBy, UpdatedAt, Id].

fields_for_fetch(#chef_cookbook_version{org_id = OrgId,
                                        name = Name,
                                        major = Major,
                                        minor = Minor,
                                        patch = Patch}) ->
    [OrgId, Name, Major, Minor, Patch].

record_fields(_ApiVersion) ->
    record_info(fields, chef_cookbook_version).

-spec(list(#chef_cookbook_version{}, chef_object:select_callback()) -> chef_object:select_return()).
list(#chef_cookbook_version{org_id = OrgId} = CBV, CallbackFun) ->
    CallbackFun({list_query(CBV), [OrgId], [name]}).

set_api_version(ObjectRec, Version) ->
    ObjectRec#chef_cookbook_version{server_api_version = Version}.

-spec wants_all_files('bad_value_requested' | integer()) -> boolean().
wants_all_files(Version) when Version =:= 0 orelse Version =:= 1 ->
    false;
wants_all_files(_) ->
    true.
