%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Jean Rouge <jean@chef.io>
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.

-module(oc_chef_wm_cookbook_artifact_version_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("../../../include/chef_types.hrl").
-include("../../../include/oc_chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, {parse_transform, lager_transform}]).

-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000004">>).
-define(AUTHZ_ID, <<"00000000000000000000000000000005">>).
-define(CLIENT_NAME, <<"test-client">>).
-define(ORG_NAME, <<"cookbook-artifact-test">>).

init_per_suite(Config) ->
    Config2 = setup_helper:base_init_per_suite([{org_name, ?ORG_NAME},
                                             {org_authz_id, ?ORG_AUTHZ_ID},
                                             {authz_id, ?AUTHZ_ID},
                                             {client_name, ?CLIENT_NAME}
                                             | Config]),

    %% we need to create a few checksums in the DB
    %% to be used in our cookbook artifacts
    OrgId = ?config(org_id, Config2),
    ok = chef_sql:mark_checksums_as_uploaded(OrgId,
                                             canonical_example_checksums()),

    Config2.

end_per_suite(Config) ->
    setup_helper:base_end_per_suite(Config).

init_per_testcase(_, Config) ->
    ok = meck:new(chef_s3),
    ok = meck:expect(chef_s3, generate_presigned_url, 5,
                     fun(_, _, _, Checksum, _) ->
                         <<"http://fake.url/for/", Checksum/binary>>
                     end),
    Config.

end_per_testcase(_, Config) ->
    ok = meck:unload(chef_s3),
    Config.

all() ->
    [
      db_round_trip,
      db_missing_checksum,
      http_round_trip,
      http_get_nonexistant,
      http_post_conflict,
      http_missing_checksum
    ].

%% this tests that low level DB ops (create and fetch)
%% work as expected
%% granted, it's not testing the WM module, so maybe it should
%% live somewhere else?
db_round_trip(Config) ->
    Context = ?config(context, Config),
    OrgId = ?config(org_id, Config),

    Name = "db_round_trip_name",
    Identifier = "db_round_trip_identifier",
    Ejson = canonical_example(Name, Identifier),

    %% let's create a cookbook artifact version in the DB
    CBAVRecord = artifact_version_rec_from_json(Config, Ejson),
    ?assertEqual(ok, chef_db:create(CBAVRecord, Context, ?AUTHZ_ID)),

    %% and then fetch it
    RecFromDB = chef_db:fetch(CBAVRecord, Context),

    %% the one from the DB should have an ID
    RecIDFromDB = oc_chef_cookbook_artifact_version:id(RecFromDB),
    ?assert(erlang:is_integer(RecIDFromDB)),

    %% and modulo a few things, it should be equal to what we inserted
    ChecksumsFromDB = RecFromDB#oc_chef_cookbook_artifact_version.checksums,
    ModifiedRecFromDB = RecFromDB#oc_chef_cookbook_artifact_version{id = undefined,
                                                                    %% they get reversed when saving
                                                                    checksums = lists:reverse(ChecksumsFromDB)},
    OriginalCreatedAt = CBAVRecord#oc_chef_cookbook_artifact_version.created_at,
    DateTimeCreatedAt = {datetime, ec_date:parse(erlang:binary_to_list(OriginalCreatedAt))},
    ModifiedCBAVRecord = CBAVRecord#oc_chef_cookbook_artifact_version{created_at = DateTimeCreatedAt,
                                                                      created_by = ?AUTHZ_ID},
    ?assertEqual(ModifiedCBAVRecord,
                 ModifiedRecFromDB).

%% this is also a low-level test; it tests that creating
%% an artifact with a missing checksum yields an explicit
%% DB error tuple
db_missing_checksum(Config) ->
    Context = ?config(context, Config),

    CorruptJson = missing_checksum_example("db_missing_checksum_name",
                                           "db_missing_checksum_identifier"),
    CBAVRecord = artifact_version_rec_from_json(Config, CorruptJson),

    ?assertEqual({error, invalid_checksum},
                 chef_db:create(CBAVRecord, Context, ?AUTHZ_ID)).

%% creates and then retrieves a cookbook artifact version
%% using the HTTP endpoint
http_round_trip(Config) ->
    OrgId = ?config(org_id, Config),

    Name = "http_round_trip_name",
    Identifier = "http_round_trip_identifier",

    %% let's create the artifact
    CreateEjson = canonical_example(Name, Identifier),
    ?assertMatch({"201", _},
                 http_create_cookbook_artifact(Name, Identifier, CreateEjson)),

    %% and then we retrieve it
    {GetCode, GetBody} = http_fetch_cookbook_artifact(Name, Identifier),
    ?assertEqual("200", GetCode),

    %% and the only difference with the original JSON that we sent should
    %% be the download URLs on each file
    ExpectedGetBody = chef_cookbook_version:annotate_with_s3_urls(CreateEjson, OrgId, ""),
    ?assertEqual(ok, ej:valid(ExpectedGetBody, GetBody)).

http_get_nonexistant(_Config) ->
    Name = Identifier = "i_dont_exist",
    ?assertMatch({"404", _},
                 http_fetch_cookbook_artifact(Name, Identifier)).

http_post_conflict(_Config) ->
    Name = "http_post_conflict_name",
    Identifier = "http_post_conflict_identifier",

    CreateEjson = canonical_example(Name, Identifier),
    ?assertMatch({"201", _},
                 http_create_cookbook_artifact(Name, Identifier, CreateEjson)),
    ?assertMatch({"409", _},
                 http_create_cookbook_artifact(Name, Identifier, CreateEjson)).

http_missing_checksum(_Config) ->
    Name = "http_missing_checksum_name",
    Identifier = "http_missing_checksum_identifier",

    CorruptJson = missing_checksum_example(Name, Identifier),

    ExpectedErrorJson = {[{<<"error">>,
                           [<<"Manifest has a checksum that hasn't been uploaded.">>]}]},
    ?assertEqual({"400", ExpectedErrorJson},
                 http_create_cookbook_artifact(Name, Identifier, CorruptJson)).

%% Helper funs

artifact_version_rec_from_json(Config, Json) ->
    OrgId = ?config(org_id, Config),
    AuthzId = chef_object_base:make_guid(),
    oc_chef_cookbook_artifact_version:new_record(OrgId, AuthzId, Json).

http_create_cookbook_artifact(Name, Identifier, Ejson) ->
    http_request(post, route_suffix_for(Name, Identifier), Ejson).

http_fetch_cookbook_artifact(Name, Identifier) ->
    http_request(get, route_suffix_for(Name, Identifier), null).

route_suffix_for(Name, Identifier) ->
    "/" ++ Name ++ "/" ++ Identifier.

http_request(Method, RouteSuffix, JsonOrNull) ->
    {ok, RespCode, _, RespBody} = ibrowse:send_req(url_for(RouteSuffix),
                     [{"x-ops-userid", "test-client"},
                      {"accept", "application/json"},
                      {"content-type", "application/json"}],
                     Method, body(JsonOrNull)),
    {RespCode, ejson:decode(RespBody)}.

url_for(RouteSuffix) ->
    OrgNameStr = erlang:binary_to_list(?ORG_NAME),
    "http://localhost:8000/organizations/" ++ OrgNameStr
      ++ "/cookbook_artifacts" ++ RouteSuffix.

body(null) -> <<>>;
body(Json) -> ejson:encode(Json).

-define(COOKBOOK_ARTIFACT_VERSION_CANONICAL_EXAMPLE, <<"
    {
      \"definitions\": [
        {
          \"name\": \"unicorn_config.rb\",
          \"checksum\": \"c92b659171552e896074caa58dada0c2\",
          \"path\": \"definitions/unicorn_config.rb\",
          \"specificity\": \"default\"
        }
      ],
      \"attributes\": [],
      \"files\": [],
      \"json_class\": \"Chef::CookbookArtifactVersion\",
      \"providers\": [],
      \"metadata\": {
        \"dependencies\": {\"ruby\": [], \"rubygems\": []},
        \"name\": \"unicorn\",
        \"maintainer_email\": \"ops@opscode.com\",
        \"attributes\": {},
        \"license\": \"Apache 2.0\",
        \"suggestions\": {},
        \"platforms\": {},
        \"maintainer\": \"Opscode, Inc\",
        \"long_description\": \"= LICENSE AND AUTHOR:\\n\\nAuthor:: Adam Jacob...\",
        \"recommendations\": {},
        \"version\": \"0.1.2\",
        \"conflicting\": {},
        \"recipes\": {\"unicorn\": \"Installs unicorn rubygem\"},
        \"groupings\": {},
        \"replacing\": {},
        \"description\": \"Installs/Configures unicorn\",
        \"providing\": {}
      },
      \"libraries\": [],
      \"templates\": [
        {
          \"name\": \"unicorn.rb.erb\",
          \"checksum\": \"36a1cc1b225708db96d48026c3f624b2\",
          \"path\": \"templates/default/unicorn.rb.erb\",
          \"specificity\": \"default\"
        }
      ],
      \"resources\": [],
      \"name\": \"unicorn\",
      \"identifier\": \"a_really_awesome_identifier\",
      \"recipes\": [
        {
          \"name\": \"default.rb\",
          \"checksum\": \"ba0dadcbca26710a521e0e3160cc5e20\",
          \"path\": \"recipes/default.rb\",
          \"specificity\": \"default\"
        }
      ],
      \"root_files\": [
        {
          \"name\": \"README.rdoc\",
          \"checksum\": \"d18c630c8a68ffa4852d13214d0525a6\",
          \"path\": \"README.rdoc\",
          \"specificity\": \"default\"
        },
        {
          \"name\": \"metadata.rb\",
          \"checksum\": \"967087a09f48f234028d3aa27a094882\",
          \"path\": \"metadata.rb\",
          \"specificity\": \"default\"
        },
        {
          \"name\": \"metadata.json\",
          \"checksum\": \"45b27c78955f6a738d2d42d88056c57c\",
          \"path\": \"metadata.json\",
          \"specificity\": \"default\"
        }
      ],
      \"chef_type\": \"cookbook_artifact_version\"
    }">>).

canonical_example() ->
    ejson:decode(?COOKBOOK_ARTIFACT_VERSION_CANONICAL_EXAMPLE).
canonical_example(Name, Identifier) ->
    Renamed = ej:set({<<"name">>}, canonical_example(),
                     erlang:list_to_binary(Name)),
    ej:set({<<"identifier">>}, Renamed, erlang:list_to_binary(Identifier)).

canonical_example_checksums() ->
    chef_cookbook_version:extract_checksums(canonical_example()).

missing_checksum_example(Name, Identifier) ->
    Ejson = canonical_example(Name, Identifier),
    [FirstDefinition | OtherDefinitions] = ej:get({<<"definitions">>}, Ejson),
    CorruptDefinition = ej:set({<<"checksum">>}, FirstDefinition,
                               <<"730ef25567f94a63ecd6e0393b9eaa87">>),
    CorruptDefinitions = [CorruptDefinition | OtherDefinitions],
    ej:set({<<"definitions">>}, Ejson, CorruptDefinitions).
