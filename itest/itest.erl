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

-module(itest).

-compile([export_all]).

-exports([setup_env/0, basic_test_/0,
          statements/1]).

-define(GET_ARG(Name, Args), proplists:get_value(Name, Args)).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_objects/include/chef_types.hrl").

make_id(Prefix) when is_binary(Prefix) ->
    case size(Prefix) of
        Size when Size > 32 ->
            error(prefix_too_long_for_id);
        Size when Size =:= 32 ->
            Prefix;
        Size ->
            iolist_to_binary([Prefix, lists:duplicate(32 - Size, $0)])
    end.

make_az_id(Prefix) ->
    make_id(<<"a11", Prefix/binary>>).

actor_id() ->
    make_az_id(<<"ffff">>).

the_org_id() ->
    make_id(<<"aa1">>).

other_org_id() ->
    make_id(<<"bb2">>).

make_data_bag(Prefix) ->
    Id = make_id(Prefix),
    AzId = make_az_id(Prefix),
    OrgId = the_org_id(),
    Name = <<"data_bag_", Prefix/binary>>,
    #chef_data_bag{id = Id, authz_id = AzId, org_id = OrgId,
                   name = Name,
                   last_updated_by = actor_id(),
                   created_at = {datetime, {{2011,10,1},{16,47,46}}},
                   updated_at = {datetime, {{2011,10,1},{16,47,46}}} }.

make_node(Prefix) ->
    Id = make_id(Prefix),
    AzId = make_az_id(Prefix),
    Name = <<"node_", Prefix/binary>>,
    #chef_node{
                id=Id, authz_id=AzId, org_id=the_org_id(), name=Name,
                environment="_default", last_updated_by="noone", serialized_object= <<"">>,
                created_at= {datetime,{{2011,10,1},{16,47,46}}}, updated_at= {datetime,{{2011,10,1},{16,47,46}}} }.

make_sandbox(Prefix) ->
    #chef_sandbox{id=make_id(Prefix),
                  org_id=the_org_id(),
                  created_at = {datetime,{{2011,10,1},{16,47,46}}},
                  checksums=[{<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, false},
                             {<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>, false},
                             {<<"cccccccccccccccccccccccccccccccc">>, false},
                             {<<"dddddddddddddddddddddddddddddddd">>, false}]}.

node_list() ->
    [ make_node(<<"01">>) ].

data_bags() ->
    [make_data_bag(<<"01">>),
     make_data_bag(<<"02">>),
     make_data_bag(<<"03">>),
     (make_data_bag(<<"04">>))#chef_data_bag{org_id = other_org_id()}].

make_data_bag_item(Prefix, BagName) ->
    Id = make_id(Prefix),
    Name = <<"item_", Prefix/binary>>,
    #chef_data_bag_item{id= Id, org_id= the_org_id(), item_name= Name, data_bag_name= BagName,
                        last_updated_by= actor_id(),
                        created_at= {datetime, {{2011,10,1},{16,47,46}}},
                        updated_at= {datetime, {{2011,10,1},{16,47,46}}},
                        serialized_object= Prefix }.

data_bag_items() ->
    [
     %% NOTE: we delete data_bag_01 as part of the delete test, so put the items into other bags
     make_data_bag_item(<<"101">>, <<"data_bag_02">>),
     make_data_bag_item(<<"102">>, <<"data_bag_02">>),
     make_data_bag_item(<<"103">>, <<"data_bag_02">>)
    ].

cookbook_name_from_prefix(Prefix) ->
    <<"cookbook_", Prefix/binary>>.

make_client(Prefix) ->
    AzId = make_az_id(Prefix),
    #chef_client{
	    id = AzId,
	    org_id = the_org_id(),
	    name = AzId,
	    authz_id = AzId,
	    validator = false,
	    public_key =
	    <<"MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAwxOFcrbsV7bEbqzOvW5u"
	      "W5lyB23qsenlUdIGyRttqzGEaki01s7X+PpYy4BLfmVVmA6A6FCbL38CzzTUFX1a"
	      "p6LYQR2Pb1tYjBjZZMUiVnjEgl12Zd1JF8dsPMj2BgPggx5GaGLvCOsajZ0YCDgW"
	      "WkoO/HAEbztFIx2jdSCyD0ZH0ep4fSGDjmkN+5XurS0dBH8J5qPeJjriA/s/RzUb"
	      "ULjr3gvfg49onHxr/kTKbhc78GBOfKSH1ftECCoWnidadW7/lfKbAZ3xiSjLsIxS"
	      "KxavHMeCuSgyReDZpsFOn2Saie26jvLxWrGyn870yIh36wMvCvWKwUQPnluSnstJ"
	      "xwIDAQAB">>,
	    pubkey_version = 1,
	    last_updated_by = actor_id(),
	    created_at = {datetime, {{2011,10,1},{16,47,46}}},
	    updated_at = {datetime, {{2011,10,1},{16,47,46}}}
    }.

make_cookbook(Prefix) ->
    AzId = make_az_id(Prefix),
    OrgId = the_org_id(),
    Name = cookbook_name_from_prefix(Prefix),
    {AzId, OrgId, Name}.

%% @doc For the purposes of these tests, you can either specify a
%% version as a single integer, or a full {Major, Minor, Patch} tuple.
%% In the case of the former, the "real" version will be {0,0,Patch},
%% in accordance with the pattern of our cookbook-related testing
%% functions up to this point.
-spec version_tuple( non_neg_integer() | version() ) -> version().
version_tuple(Patch) when is_integer(Patch) ->
    {0,0,Patch};
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
    Id = make_id(Prefix),
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
                           last_updated_by= actor_id(),
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
            encode_and_compress(EJson);
        none ->
            encode_and_compress({[]})
    end;
process_property(dependencies=Property, Properties) ->
    case proplists:lookup(Property, Properties) of
        {Property, Binary} when is_binary(Binary) ->
            %% Already JSON
            Binary ;
        {Property, PropertyList} when is_list(PropertyList)->
            ejson:encode({PropertyList});
        none ->
            ejson:encode({[]})
    end.

cookbook_version_list(Cookbook) ->
    [ make_cookbook_version(<<"01">>, 1, Cookbook),
      make_cookbook_version(<<"02">>, 2, Cookbook) ].

get_db_type() ->
    {ok, [[Type]]} = init:get_argument(db_type),
    list_to_atom(Type).

read_db_config() ->
    Type = get_db_type(),
    Path = filename:join([filename:dirname(code:which(?MODULE)), atom_to_list(Type) ++ ".config"]),
    {ok, Config} = file:consult(Path),
    Config.

setup_env() ->
    Type = get_db_type(),
    Info = read_db_config(),
    ok = application:set_env(sqerl, db_type, Type),
    ok = application:set_env(sqerl, db_host, ?GET_ARG(host, Info)),
    ok = application:set_env(sqerl, db_port, ?GET_ARG(port, Info)),
    ok = application:set_env(sqerl, db_user, "itest"),
    ok = application:set_env(sqerl, db_pass, "itest"),
    ok = application:set_env(sqerl, db_name, ?GET_ARG(db, Info)),
    ok = application:set_env(sqerl, idle_check, 10000),
    %% we could also call it like this:
    %% {prepared_statements, statements(Type)},
    %% {prepared_statements, "itest/statements_pgsql.conf"},
    ok = application:set_env(sqerl, prepared_statements, {?MODULE, statements, [Type]}),

    %% In production we use 5, but I'm using 2 here for the time being
    %% to exercise the joining together of multiple database calls.  See the TODO
    %% in the "Environment-filtered Recipes Tests" section for more.
    ok = application:set_env(chef_db, bulk_fetch_batch_size, 2),

    ColumnTransforms = case Type of
                           pgsql ->
                               [{<<"created_at">>,
                                 fun sqerl_transformers:convert_YMDHMS_tuple_to_datetime/1},
                                {<<"updated_at">>,
                                 fun sqerl_transformers:convert_YMDHMS_tuple_to_datetime/1}];
                           mysql ->
                               [{<<"frozen">>,
                                 fun sqerl_transformers:convert_integer_to_boolean/1},
                                {<<"validator">>,
                                 fun sqerl_transformers:convert_integer_to_boolean/1}]
                       end,
    ok = application:set_env(sqerl, column_transforms, ColumnTransforms),

    PoolConfig = [{name, "sqerl"},
                  {max_count, 3},
                  {init_count, 1},
                  {start_mfa, {sqerl_client, start_link, []}}],
    ok = application:set_env(pooler, pools, [PoolConfig]),
    application:start(crypto),
    application:start(emysql),
    application:start(public_key),
    application:start(ssl),
    application:start(epgsql),

    %% Temporarily disable logging for sqerl startup
    %% TODO: refactor so we only setup and destroy once
    error_logger:tty(false),
    application:start(sqerl),
    error_logger:tty(true).

%% @doc Shutdown all the infrastructure that was started up by
%% setup_env/0.  Use as a final cleanup function for test suites.
destroy_env() ->
    %% TODO: pull this list of apps out, and perhaps use it in setup_env/0 as well.
    Apps = [sqerl, epgsql, ssl, public_key, emysql, crypto],
    %% Suppress output of logging for shutdowns... it muddies up the output
    error_logger:tty(false),
    [ application:stop(App) || App <- Apps ],
    error_logger:tty(true).

%% @doc Until Sqerl gets the ability to fire off generated SQL, use
%% transactions, or we get a more sane integration test setup that
%% allows for cleaning up the database between tests, I'm adding some
%% prepared statements to Sqerl that will do the kind of cleanup I
%% need.  Feel free to add more as the need arises.
cleanup_statements() ->
    [{delete_cookbook_versions,
      <<"DELETE FROM cookbook_versions">>},
     {delete_cookbook_version_checksums,
      <<"DELETE FROM cookbook_version_checksums">>},
     {delete_cookbooks,
      <<"DELETE FROM cookbooks">>}].

statements(mysql) ->
    {ok, Statements} = file:consult("priv/mysql_statements.config"),
    Statements ++ cleanup_statements();
statements(pgsql) ->
    {ok, Statements} = file:consult("priv/pgsql_statements.config"),
    Statements ++ cleanup_statements().

basic_test_() ->
    {foreach,
     %% Setup
     fun() ->
             setup_env()
     end,
     %% Cleanup
     fun(_) ->
             destroy_env()
     end,
     %% Tests
     [
      {<<"Node Operations">>,
       [
        {<<"Insert operations">>, fun insert_node_data/0}
       ]
      },
      {<<"Client Operations">>,
       [
        {<<"Insert operations">>, fun insert_client_data/0},
        {<<"Fetch operations">>,  fun fetch_client_data/0},
        {<<"Bulk Fetch operations">>,  fun bulk_fetch_client_data/0},
        {<<"Delete operations">>, fun delete_client_data/0}
       ]
      },
      {<<"Data Bag Operations">>,
       [ {<<"Insert operations">>, fun insert_data_bag_data/0},
         {<<"Fetch multiple">>, fun fetch_data_bags/0},
         {<<"Fetch single">>, fun fetch_data_bag/0},
         {<<"Delete">>, fun delete_data_bag/0} ] }
      ,
      {<<"Data Bag Item Operations">>,
       [ {<<"Insert operations">>, fun insert_data_bag_item_data/0},
         {<<"Fetch multiple">>, fun fetch_data_bag_items/0},
         {<<"Fetch single">>, fun fetch_data_bag_item/0},
         {<<"Id get">>, fun fetch_data_bag_item_ids/0},
         {<<"Bulk Get">>, fun bulk_get_data_bag_items/0},
         {<<"Update">>, fun update_data_bag_item/0},
         {<<"Delete">>, fun delete_data_bag_item/0}
       ]},
      {<<"Sandbox Operations">>,
       [
        {<<"Insert operations">>, fun insert_sandbox/0},
        {<<"Mark most checksums as uploaded">>, fun mark_some_checksums_as_uploaded/0},
        {<<"Check which checksums are not uploaded">>, fun check_non_uploaded_checksums/0},
        {<<"Upload and verify last checksum">>, fun upload_last_checksum/0},
        {<<"Fetch sandbox">>, fun fetch_sandbox/0},
        {<<"Delete sandbox">>, fun delete_sandbox/0}

       ]},
      {<<"Cookbook Operations">>,
       [ {<<"Insert operations">>, fun insert_cookbook_data/0},
         {<<"Fetch AuthzId">>, fun fetch_cookbook_authz/0}
       ]},
      {<<"Cookbook Version Operations">>,
       [ {<<"Insert operations">>, fun insert_cookbook_version_data/0},
         {<<"Insert cookbook with null id">>, fun insert_cbv_null_id/0},
         {<<"Insert cookbook with no id">>, fun insert_cbv_no_id/0},
         {<<"Insert cookbook with unknown checksums">>, fun insert_cbv_with_unknown_checksums/0},
         {<<"Insert cookbook with frozen">>, fun insert_cbv_with_frozen/0},
         {<<"Fetch single, does not exist">>, fun fetch_cookbook_version_not_exist/0},
         {<<"Fetch single, no checksums">>, fun fetch_cookbook_version_no_checksums/0},
         {<<"Fetch single with checksums">>, fun fetch_cookbook_version_checksums/0},
         {<<"Fetch different version of cookbook returns authzid">>,
          fun fetch_cookbook_version_different_version/0},
         {<<"Fetch versions">>, fun fetch_cookbook_versions/0},
         {<<"Fetch latest version of cookbook">>, fun fetch_latest_cookbook_version/0},
         {<<"Fetch latest version of cookbook, different version on server">>, fun fetch_latest_cookbook_version_different_versions/0},
	 {<<"Update existing cookbook">>, fun update_cookbook_version_checksums/0},
	 {<<"Update existing cookbook with deletions">>, fun update_cookbook_version_checksums_with_deletions/0},
	 {<<"Update existing cookbook with additions">>, fun update_cookbook_version_checksums_with_additions/0},
	 {<<"Update existing cookbook with deletions and additions">>, fun update_cookbook_version_checksums_with_deletions_and_additions/0},
	 {<<"Update existing cookbook with checkums that have not been uploaded">>, fun update_cookbook_version_checksums_with_missing_checksums/0},
	 {<<"Update existing cookbook when deleting a shared checksum">>, fun update_cookbook_version_checksums_with_shared_checksums/0},
         {<<"Delete with checksums">>, fun delete_cookbook_version_checksums/0},
         {<<"Delete multiple versions">>, fun delete_cookbook_multiple_versions/0}
       ]},
      {<<"Full Cookbook flow integration Tests">>,
       [ {<<"Create for first time">>, fun cookbook_create_from_scratch/0},
         {<<"Create new version of existing cookbook">>, fun cookbook_create_new_version/0}
       ]},
      {"Latest Cookbook Tests",
       {foreachx,
        fun cookbook_setup/1,
        fun cookbook_cleanup/2,
        [{Specs, fun(CookbookSpecs, _) ->
                         {Description,
                          fun() ->
                                  Expected = latest_from_cookbook_specs(CookbookSpecs, NumVersions),
                                  {ok, Actual} = chef_sql:fetch_latest_cookbook_versions(the_org_id(), all, NumVersions),
                                  ?_assertEqual(Expected, Actual)
                          end}
                 end}
         || {Description, Specs, NumVersions} <- [
                                                  %% Ask for just the latest versions of cookbooks
                                                  %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                                  {"Nothing in the database",
                                                   [],
                                                   1},

                                                  {"Single cookbook, single version",
                                                   [{<<"one">>, [[{version, {0,0,1}}]]}],
                                                   1},


                                                  {"Multiple cookbooks, single version each",
                                                   [
                                                    {<<"one">>, [[{version, {0,0,1}}]]},
                                                    {<<"two">>, [[{version, {0,0,1}}]]},
                                                    {<<"three">>, [[{version, {0,0,1}}]]}
                                                   ],
                                                   1},

                                                  {"Multiple cookbooks, 3 versions each",
                                                   [
                                                    {<<"one1">>, [
                                                                  [{version, {1,0,0}}],
                                                                  [{version, {1,0,5}}],
                                                                  [{version, {2,0,0}}]
                                                                 ]},
                                                    {<<"two1">>, [
                                                                  [{version, {0,0,1}}],
                                                                  [{version, {0,0,2}}],
                                                                  [{version, {0,5,0}}]
                                                                 ]},
                                                    {<<"three1">>, [
                                                                    [{version, {6,0,0}}],
                                                                    [{version, {7,0,5}}],
                                                                    [{version, {7,5,0}}]
                                                                   ]}
                                                   ],
                                                   1},

                                                  %% Now, ask for multiple versions
                                                  %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                                  {"Ask for more than one version, but less than total versions present",
                                                   [
                                                    {<<"one2">>, [
                                                                  [{version, {1,0,0}}],
                                                                  [{version, {1,0,5}}],
                                                                  [{version, {2,0,0}}]
                                                                 ]},
                                                    {<<"two2">>, [
                                                                  [{version, {0,0,1}}],
                                                                  [{version, {0,0,2}}],
                                                                  [{version, {0,5,0}}]
                                                                 ]},
                                                    {<<"three2">>, [
                                                                    [{version, {6,0,0}}],
                                                                    [{version, {7,0,5}}],
                                                                    [{version, {7,5,0}}]
                                                                   ]}
                                                   ],
                                                   2},

                                                  {"Ask for more versions than exist in the database; should return all versions",
                                                   [
                                                    {<<"one5">>, [
                                                                  [{version, {1,0,0}}],
                                                                  [{version, {1,0,5}}],
                                                                  [{version, {2,0,0}}]
                                                                 ]},
                                                    {<<"two5">>, [
                                                                  [{version, {0,0,1}}],
                                                                  [{version, {0,0,2}}],
                                                                  [{version, {0,5,0}}]
                                                                 ]},
                                                    {<<"three5">>, [
                                                                    [{version, {6,0,0}}],
                                                                    [{version, {7,0,5}}],
                                                                    [{version, {7,5,0}}]
                                                                   ]}
                                                   ],
                                                   5},

                                                  {"Explicitly asking for 'all' versions should return all versions",
                                                   [
                                                    {<<"one5">>, [
                                                                  [{version, {1,0,0}}],
                                                                  [{version, {1,0,5}}],
                                                                  [{version, {2,0,0}}]
                                                                 ]},
                                                    {<<"two5">>, [
                                                                  [{version, {0,0,1}}],
                                                                  [{version, {0,0,2}}],
                                                                  [{version, {0,5,0}}]
                                                                 ]},
                                                    {<<"three5">>, [
                                                                    [{version, {6,0,0}}],
                                                                    [{version, {7,0,5}}],
                                                                    [{version, {7,5,0}}]
                                                                   ]}
                                                   ],
                                                   all}
                                                 ]]
       } %% foreachx
      }, %% Latest Cookbook Tests
      {"Cookbook Recipes Tests",
       {foreachx,
        fun cookbook_setup/1,
        fun cookbook_cleanup/2,
        [{Specs, fun(CookbookSpecs, _) ->
                         {Description,
                          fun() -> Expected = recipes_from_cookbook_specs(CookbookSpecs),
                                   {ok, Actual} = chef_sql:fetch_latest_cookbook_recipes(the_org_id()),
                                   ?assertEqual(Expected, Actual)
                          end}
                 end}
         || {Description, Specs} <- [
                                     {"Nothing in the database",
                                      []},

                                     {"One cookbook, 3 recipes; all recipes returned",
                                      [{<<"one">>, [[{version, {1,0,0}},
                                                     {recipe_names, [<<"recipeOne">>,
                                                                     <<"recipeTwo">>,
                                                                     <<"recipeThree">>]}]]}]},

                                     {"Two cookbooks, one version each; all recipes returned",
                                      [{<<"one">>, [
                                                    [{version, {1,0,0}},
                                                     {recipe_names, [<<"recipeOne">>,
                                                                     <<"recipeTwo">>,
                                                                     <<"recipeThree">>]}]
                                                   ]},
                                       {<<"two">>, [
                                                    [{version, {1,5,0}},
                                                     {recipe_names, [<<"foo">>,
                                                                     <<"bar">>]}]
                                                   ]}
                                      ]},

                                     {"Two cookbooks, one with no recipes (pathological case)",
                                      [{<<"one">>, [
                                                    [{version, {1,0,0}}]
                                                   ]},
                                       {<<"two">>, [
                                                    [{version, {1,5,0}},
                                                     {recipe_names, [<<"foo">>,
                                                                     <<"bar">>]}
                                                    ]
                                                   ]}
                                      ]},

                                     {"Multiple versions of multiple cookbooks; only the latest are returned",
                                      [{<<"one">>, [
                                                    [{version, {1,0,0}},
                                                     {recipe_names, [<<"recipeOne">>,
                                                                     <<"recipeTwo">>,
                                                                     <<"recipeThree">>]}],
                                                    [{version, {1,6,1}},
                                                     {recipe_names, [<<"webserver">>,
                                                                     <<"database">>]}],
                                                    [{version, {1,5,0}},
                                                     {recipe_names, [<<"spaghetti_carbonara">>,
                                                                     <<"chicken_saltimbocca">>,
                                                                     <<"boeuf_bourgignone">>]}]
                                                   ]},
                                       {<<"two">>, [
                                                    [{version, {1,5,0}},
                                                     {recipe_names, [<<"foo">>,
                                                                     <<"bar">>]}],
                                                    [{version, {0,0,1}},
                                                     {recipe_names, [<<"mongodb">>,
                                                                     <<"devnulldb">>,
                                                                     <<"webscale_utils">>]}]
                                                   ]}
                                      ]}
                                    ] %% End specs
        ]} %% foreachx
      }, %% Cookbook Recipes Tests

      {"Cookbook Version Dependency Retrieval Tests",
       {foreachx,
        fun cookbook_setup/1,
        fun cookbook_cleanup/2,
        [{Spec, fun(_,_) ->
                        {Description,
                         fun() ->
                                 {ok, Actual} = chef_sql:fetch_all_cookbook_version_dependencies(the_org_id()),
                                 ?assertEqual(Expected, Actual)
                         end}
                end}
         || {Description, Spec, Expected} <- [
                                              {"No Cookbooks!",
                                               [],
                                               []
                                              },

                                              {"One cookbook, no dependencies",
                                               [{<<"one">>, [
                                                             [{version, {1,0,0}}]
                                                            ]}],
                                               [{<<"cookbook_one">>, [{<<"1.0.0">>, []}]}]
                                              },

                                              {"Multiple cookbooks, one version each, no dependencies",
                                               [{<<"one">>, [
                                                             [{version, {1,0,0}}]
                                                            ]},
                                                {<<"two">>, [
                                                             [{version, {1,0,0}}]
                                                            ]},
                                                {<<"three">>, [
                                                               [{version, {1,0,0}}]
                                                              ]}
                                               ],
                                               [
                                                {<<"cookbook_two">>, [{<<"1.0.0">>, []}]},
                                                {<<"cookbook_three">>, [{<<"1.0.0">>, []}]},
                                                {<<"cookbook_one">>, [{<<"1.0.0">>, []}]}
                                               ]},

                                              {"Multiple cookbooks, multiple versions, no dependencies",
                                               [{<<"one">>, [
                                                             [{version, {1,0,0}}],
                                                             [{version, {2,0,0}}]
                                                            ]},
                                                {<<"two">>, [
                                                             [{version, {1,0,0}}],
                                                             [{version, {2,0,0}}]
                                                            ]},
                                                {<<"three">>, [
                                                               [{version, {1,0,0}}],
                                                               [{version, {2,0,0}}]
                                                              ]}
                                               ],
                                               [

                                                {<<"cookbook_two">>, [
                                                                      {<<"1.0.0">>, []},
                                                                      {<<"2.0.0">>, []}
                                                                     ]},
                                                {<<"cookbook_three">>, [
                                                                        {<<"1.0.0">>, []},
                                                                        {<<"2.0.0">>, []}
                                                                       ]},
                                                {<<"cookbook_one">>, [
                                                                      {<<"1.0.0">>, []},
                                                                      {<<"2.0.0">>, []}
                                                                     ]}
                                               ]
                                              },

                                              {"Multiple cookbooks, multiple versions, multiple dependencies",
                                               [
                                                {<<"one">>, [
                                                             [{version, {1,0,0}},
                                                              {dependencies, [
                                                                              {<<"foo">>, <<"= 1.0.0">>},
                                                                              {<<"bar">>, <<"> 2.0">>},
                                                                              {<<"baz">>, <<"> 3">>}
                                                                             ]}],
                                                             [{version, {2,0,0}},
                                                              {dependencies, [
                                                                              {<<"foo">>, <<"= 1.0.0">>},
                                                                              {<<"bar">>, <<"> 2.0">>},
                                                                              {<<"baz">>, <<"> 3">>}
                                                                             ]}
                                                             ]
                                                            ]},
                                                {<<"two">>, [
                                                             [{version, {1,0,0}},
                                                              {dependencies, [
                                                                              {<<"foo">>, <<"= 1.0.0">>},
                                                                              {<<"bar">>, <<"> 2.0">>},
                                                                              {<<"baz">>, <<"> 3">>}
                                                                             ]}],
                                                             [{version, {2,0,0}},
                                                              {dependencies, [
                                                                              {<<"foo">>, <<"= 1.0.0">>},
                                                                              {<<"bar">>, <<"> 2.0">>},
                                                                              {<<"baz">>, <<"> 3">>}
                                                                             ]}
                                                             ]
                                                            ]},
                                                {<<"three">>, [
                                                               [{version, {1,0,0}},
                                                                {dependencies, [
                                                                                {<<"foo">>, <<"= 1.0.0">>},
                                                                                {<<"bar">>, <<"> 2.0">>},
                                                                                {<<"baz">>, <<"> 3">>}
                                                                               ]}],
                                                               [{version, {2,0,0}},
                                                                {dependencies, [
                                                                                {<<"foo">>, <<"= 1.0.0">>},
                                                                                {<<"bar">>, <<"> 2.0">>},
                                                                                {<<"baz">>, <<"> 3">>}
                                                                               ]}
                                                               ]
                                                              ]}

                                               ],
                                               [
                                                {<<"cookbook_two">>, [
                                                                      {<<"1.0.0">>, [{<<"foo">>, <<"1.0.0">>, '='},
                                                                                     {<<"bar">>, <<"2.0">>, '>'},
                                                                                     {<<"baz">>, <<"3">>, '>'}]},
                                                                      {<<"2.0.0">>, [{<<"foo">>, <<"1.0.0">>, '='},
                                                                                     {<<"bar">>, <<"2.0">>, '>'},
                                                                                     {<<"baz">>, <<"3">>, '>'}]}
                                                                     ]},
                                                {<<"cookbook_three">>, [
                                                                        {<<"1.0.0">>, [{<<"foo">>, <<"1.0.0">>, '='},
                                                                                       {<<"bar">>, <<"2.0">>, '>'},
                                                                                       {<<"baz">>, <<"3">>, '>'}]},
                                                                        {<<"2.0.0">>, [{<<"foo">>, <<"1.0.0">>, '='},
                                                                                       {<<"bar">>, <<"2.0">>, '>'},
                                                                                       {<<"baz">>, <<"3">>, '>'}]}
                                                                       ]},
                                                {<<"cookbook_one">>, [{<<"1.0.0">>, [{<<"foo">>, <<"1.0.0">>, '='},
                                                                                     {<<"bar">>, <<"2.0">>, '>'},
                                                                                     {<<"baz">>, <<"3">>, '>'}]},
                                                                      {<<"2.0.0">>, [{<<"foo">>, <<"1.0.0">>, '='},
                                                                                     {<<"bar">>, <<"2.0">>, '>'},
                                                                                     {<<"baz">>, <<"3">>, '>'}]}
                                                                     ]}

                                               ]
                                              }
                                             ]
        ]
       } %% foreachx
      }, %% Cookbook Version Dependency Retrieval Tests
      {"Environment-filtered Cookbook Versions Tests",
       {foreachx,
        fun(EnvironmentSpec) ->
                AllCookbooks = [{<<"one">>, [
                                             [{version, {3,0,0}}],
                                             [{version, {2,0,0}}]
                                            ]},
                                {<<"two">>, [
                                             [{version, {1,2,0}}],
                                             [{version, {1,0,0}}]
                                            ]},
                                {<<"three">>, [
                                               [{version, {1,0,0}}],
                                               [{version, {0,5,0}}],
                                               [{version, {0,0,1}}]
                                              ]}
                               ],

                cookbook_setup(AllCookbooks),
                FullEnvironment = environment_from_spec(EnvironmentSpec),
                add_environment_to_db(FullEnvironment),
                FullEnvironment
        end,
        fun(_, Environment) ->
                remove_all_cookbooks(),
                destroy_environment(Environment),
                ok
        end,
        [{EnvironmentSpec,
          fun(_, RealEnvironment) ->
                  {Description,
                   fun() ->
                           EnvName = RealEnvironment#chef_environment.name,
                           {ok, Actual} = chef_sql:fetch_environment_filtered_cookbook_versions(the_org_id(), EnvName, all, NumVersions),
                           ?assertEqual(Expected, Actual)
                   end}
          end}
         || {Description, EnvironmentSpec, NumVersions, Expected} <- [
                                                                      {"multiple cookbooks, no environment constraints, one version",
                                                                       <<"testing">>,
                                                                       1,
                                                                       [{<<"cookbook_one">>, [<<"3.0.0">>]},
                                                                        {<<"cookbook_three">>, [<<"1.0.0">>]},
                                                                        {<<"cookbook_two">>, [<<"1.2.0">>]}
                                                                       ]
                                                                      },

                                                                      {"multiple cookbooks, no environment constraints, all versions",
                                                                       <<"testing">>,
                                                                       all,
                                                                       [{<<"cookbook_one">>, [<<"3.0.0">>, <<"2.0.0">>]},
                                                                        {<<"cookbook_three">>, [<<"1.0.0">>, <<"0.5.0">>, <<"0.0.1">>]},
                                                                        {<<"cookbook_two">>, [<<"1.2.0">>, <<"1.0.0">>]}
                                                                       ]
                                                                      },

                                                                      {"multiple cookbooks, no environment constraints, no versions",
                                                                       <<"testing">>,
                                                                       0,
                                                                       [{<<"cookbook_one">>, []},
                                                                        {<<"cookbook_three">>, []},
                                                                        {<<"cookbook_two">>, []}
                                                                       ]
                                                                      },

                                                                      {"multiple cookbooks, environment constraints, no versions",
                                                                       {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"> 1.0.0">>},
                                                                                                             {<<"cookbook_two">>, <<"< 1.2.3">>}]}]},
                                                                       0,
                                                                       [{<<"cookbook_one">>, []},
                                                                        {<<"cookbook_three">>, []},
                                                                        {<<"cookbook_two">>, []}
                                                                       ]
                                                                      },

                                                                      {"multiple cookbooks, environment constraints, one version",
                                                                       {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"> 1.0.0">>},
                                                                                                             {<<"cookbook_two">>, <<"< 1.2.3">>}]}]},
                                                                       1,
                                                                       [{<<"cookbook_one">>, [<<"3.0.0">>]},
                                                                        {<<"cookbook_three">>, [<<"1.0.0">>]},
                                                                        {<<"cookbook_two">>, [<<"1.2.0">>]}
                                                                       ]
                                                                      },

                                                                      {"multiple cookbooks, different constraints, one version",
                                                                       {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"< 2.5.0">>},
                                                                                                             {<<"cookbook_three">>, <<"= 0.5.0">>}]}]},
                                                                       1,
                                                                       [{<<"cookbook_one">>, [<<"2.0.0">>]},
                                                                        {<<"cookbook_three">>, [<<"0.5.0">>]},
                                                                        {<<"cookbook_two">>, [<<"1.2.0">>]}
                                                                       ]
                                                                      },

                                                                      {"multiple cookbooks, different constraints (completely removing one cookbook), one version",
                                                                       {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"= 6.6.6">>},
                                                                                                             {<<"cookbook_three">>, <<"= 0.5.0">>}]}]},
                                                                       1,
                                                                       [{<<"cookbook_three">>, [<<"0.5.0">>]},
                                                                        {<<"cookbook_two">>, [<<"1.2.0">>]}
                                                                       ]
                                                                      }

                                                                     ]
        ]
       } %% foreachx
      }, %% Environment-filtered Cookbook Versions Tests
      {"Environment-filtered Recipes Tests",
       %% TODO: Consider more robust tests, with many more cookbooks
       %% and versions in order to cover various combinations of the
       %% "bulk get" queries.  As it currently stands, we have a
       %% bulk_fetch_batch_size of 2, so this will at least cover some
       %% of the result-set joining functionality.
       {foreachx,
        fun(EnvironmentSpec) ->
                AllCookbooks = [{<<"one">>, [
                                             [{version, {3,0,0}},
                                              {recipe_names, [<<"foo">>]}],
                                             [{version, {2,0,0}},
                                              {recipe_names, [<<"foo">>, <<"bar">>]}]
                                            ]},
                                {<<"two">>, [
                                             [{version, {1,2,0}},
                                              {recipe_names, [<<"default">>, <<"baz">>]}],
                                             [{version, {1,0,0}},
                                              {recipe_names, [<<"baz">>]}]
                                            ]},
                                {<<"three">>, [
                                               [{version, {1,0,0}},
                                                {recipe_names, [<<"server">>, <<"client">>, <<"default">>]}],
                                               [{version, {0,5,0}},
                                                {recipe_names, [<<"server">>, <<"client">>]}],
                                               [{version, {0,0,1}},
                                                {recipe_names, [<<"server">>]}]
                                              ]}
                               ],

                cookbook_setup(AllCookbooks),
                FullEnvironment = environment_from_spec(EnvironmentSpec),
                add_environment_to_db(FullEnvironment),
                FullEnvironment
        end,
        fun(_, Environment) ->
                remove_all_cookbooks(),
                destroy_environment(Environment),
                ok
        end,
        [{EnvironmentSpec,
          fun(_, RealEnvironment) ->
                  {Description,
                   fun() ->
                           EnvName = RealEnvironment#chef_environment.name,
                           {ok, Actual} = chef_sql:fetch_environment_filtered_recipes(the_org_id(), EnvName),
                           ?assertEqual(Expected, Actual)
                   end}
          end}
         || {Description, EnvironmentSpec, Expected} <- [
                                                         {"multiple cookbooks, no environment constraints",
                                                          <<"testing">>,
                                                          [
                                                           <<"cookbook_one::foo">>,
                                                           <<"cookbook_three">>,
                                                           <<"cookbook_three::client">>,
                                                           <<"cookbook_three::server">>,
                                                           <<"cookbook_two">>,
                                                           <<"cookbook_two::baz">>
                                                          ]
                                                         },
                                                         {"multiple cookbooks, environment constraints",
                                                          {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"> 1.0.0">>},
                                                                                                {<<"cookbook_two">>, <<"< 1.2.3">>}]}]},
                                                          [
                                                           <<"cookbook_one::foo">>,
                                                           <<"cookbook_three">>,
                                                           <<"cookbook_three::client">>,
                                                           <<"cookbook_three::server">>,
                                                           <<"cookbook_two">>,
                                                           <<"cookbook_two::baz">>
                                                          ]
                                                         },

                                                         {"multiple cookbooks, different constraints",
                                                          {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"< 2.5.0">>},
                                                                                                {<<"cookbook_three">>, <<"= 0.5.0">>}]}]},
                                                          [
                                                           <<"cookbook_one::bar">>,
                                                           <<"cookbook_one::foo">>,
                                                           <<"cookbook_three::client">>,
                                                           <<"cookbook_three::server">>,
                                                           <<"cookbook_two">>,
                                                           <<"cookbook_two::baz">>
                                                          ]
                                                         },

                                                         {"multiple cookbooks, different constraints (completely removing one cookbook)",
                                                          {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"= 6.6.6">>},
                                                                                                {<<"cookbook_three">>, <<"= 0.5.0">>}]}]},
                                                          [
                                                           <<"cookbook_three::client">>,
                                                           <<"cookbook_three::server">>,
                                                           <<"cookbook_two">>,
                                                           <<"cookbook_two::baz">>
                                                          ]
                                                         },
                                                         {"multiple cookbooks, different constraints (completely removing all cookbooks)",
                                                          {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"= 6.6.6">>},
                                                                                                {<<"cookbook_two">>, <<"= 6.6.6">>},
                                                                                                {<<"cookbook_three">>, <<"= 6.6.6">>}]}]},
                                                          []
                                                         }
                                                        ]
        ]
       } %% foreachx
      }, %% Environment-filtered Recipes Tests
      {"Environment-filtered Cookbook Versions Tests for a Single Cookbook",
       {foreachx,
        fun(EnvironmentSpec) ->
                AllCookbooks = [{<<"one">>, [
                                             [{version, {3,0,0}}],
                                             [{version, {2,0,0}}]
                                            ]},
                                {<<"two">>, [
                                             [{version, {1,2,0}}],
                                             [{version, {1,0,0}}]
                                            ]},
                                {<<"three">>, [
                                               [{version, {1,0,0}}],
                                               [{version, {0,5,0}}],
                                               [{version, {0,0,1}}]
                                              ]}
                               ],

                cookbook_setup(AllCookbooks),
                FullEnvironment = environment_from_spec(EnvironmentSpec),
                add_environment_to_db(FullEnvironment),
                FullEnvironment
        end,
        fun(_, Environment) ->
                remove_all_cookbooks(),
                destroy_environment(Environment),
                ok
        end,
        [{EnvironmentSpec,
          fun(_, RealEnvironment) ->
                  {Description,
                   fun() ->
                           EnvName = RealEnvironment#chef_environment.name,
                           lists:map(fun({CookbookName, NumToVersions}) ->
                                             lists:map(fun({NumVersions, Versions}) ->
                                                               {ok, Actual} = chef_sql:fetch_environment_filtered_cookbook_versions(the_org_id(),
                                                                                                                                    EnvName, CookbookName,
                                                                                                                                    NumVersions),
                                                               ?assertEqual([{CookbookName, Versions}], Actual)
                                                       end,
                                                       NumToVersions)
                                     end,
                                     ResultsSpec)
                   end}
          end}
         || {Description, EnvironmentSpec, ResultsSpec} <- [
                                                            {"no environment constraints",
                                                             <<"testing">>,
                                                             [{<<"cookbook_one">>, [{0, []},
                                                                                    {1, [<<"3.0.0">>]},
                                                                                    {2, [<<"3.0.0">>, <<"2.0.0">>]},
                                                                                    {3, [<<"3.0.0">>, <<"2.0.0">>]},
                                                                                    {all, [<<"3.0.0">>, <<"2.0.0">>]}]},
                                                              {<<"cookbook_two">>, [{0, []},
                                                                                    {1, [<<"1.2.0">>]},
                                                                                    {2, [<<"1.2.0">>, <<"1.0.0">>]},
                                                                                    {3, [<<"1.2.0">>, <<"1.0.0">>]},
                                                                                              {all, [<<"1.2.0">>, <<"1.0.0">>]}]},
                                                              {<<"cookbook_three">>, [{0, []},
                                                                                      {1, [<<"1.0.0">>]},
                                                                                      {2, [<<"1.0.0">>, <<"0.5.0">>]},
                                                                                      {3, [<<"1.0.0">>, <<"0.5.0">>, <<"0.0.1">>]},
                                                                                      {4, [<<"1.0.0">>, <<"0.5.0">>, <<"0.0.1">>]},
                                                                                      {all, [<<"1.0.0">>, <<"0.5.0">>, <<"0.0.1">>]}]}]
                                                             },

                                                            {"Add environment constraints that are functionally equivalenat to no constraints",
                                                             {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"> 1.0.0">>},
                                                                                                   {<<"cookbook_two">>, <<"< 1.2.3">>}]}]},
                                                             [{<<"cookbook_one">>, [{0, []},
                                                                                    {1, [<<"3.0.0">>]},
                                                                                    {2, [<<"3.0.0">>, <<"2.0.0">>]},
                                                                                    {3, [<<"3.0.0">>, <<"2.0.0">>]},
                                                                                    {all, [<<"3.0.0">>, <<"2.0.0">>]}]},
                                                              {<<"cookbook_two">>, [{0, []},
                                                                                    {1, [<<"1.2.0">>]},
                                                                                    {2, [<<"1.2.0">>, <<"1.0.0">>]},
                                                                                    {3, [<<"1.2.0">>, <<"1.0.0">>]},
                                                                                    {all, [<<"1.2.0">>, <<"1.0.0">>]}]},
                                                              {<<"cookbook_three">>, [{0, []},
                                                                                      {1, [<<"1.0.0">>]},
                                                                                      {2, [<<"1.0.0">>, <<"0.5.0">>]},
                                                                                      {3, [<<"1.0.0">>, <<"0.5.0">>, <<"0.0.1">>]},
                                                                                      {4, [<<"1.0.0">>, <<"0.5.0">>, <<"0.0.1">>]},
                                                                                      {all, [<<"1.0.0">>, <<"0.5.0">>, <<"0.0.1">>]}]}]
                                                            },
                                                            {"Try different constraints",
                                                             {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"< 2.5.0">>},
                                                                                                   {<<"cookbook_three">>, <<"= 0.5.0">>}]}]},
                                                             [{<<"cookbook_one">>, [{0, []},
                                                                                    {1, [<<"2.0.0">>]},
                                                                                    {2, [<<"2.0.0">>]},
                                                                                    {3, [<<"2.0.0">>]},
                                                                                    {all, [<<"2.0.0">>]}]},
                                                              {<<"cookbook_two">>, [{0, []},
                                                                                    {1, [<<"1.2.0">>]},
                                                                                    {2, [<<"1.2.0">>, <<"1.0.0">>]},
                                                                                    {3, [<<"1.2.0">>, <<"1.0.0">>]},
                                                                                    {all, [<<"1.2.0">>, <<"1.0.0">>]}]},
                                                              {<<"cookbook_three">>, [{0, []},
                                                                                      {1, [<<"0.5.0">>]},
                                                                                      {2, [<<"0.5.0">>]},
                                                                                      {3, [<<"0.5.0">>]},
                                                                                      {4, [<<"0.5.0">>]},
                                                                                      {all, [<<"0.5.0">>]}]}]
                                                            },

                                                            {"Try different constraints (completely removing one cookbook)",
                                                             {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"= 6.6.6">>},
                                                                                                   {<<"cookbook_three">>, <<"= 0.5.0">>}]}]},
                                                             [{<<"cookbook_one">>, [{0, []},
                                                                                    {1, []},
                                                                                    {2, []},
                                                                                    {3, []},
                                                                                    {all, []}]},
                                                              {<<"cookbook_two">>, [{0, []},
                                                                                    {1, [<<"1.2.0">>]},
                                                                                    {2, [<<"1.2.0">>, <<"1.0.0">>]},
                                                                                    {3, [<<"1.2.0">>, <<"1.0.0">>]},
                                                                                    {all, [<<"1.2.0">>, <<"1.0.0">>]}]},
                                                              {<<"cookbook_three">>, [{0, []},
                                                                                      {1, [<<"0.5.0">>]},
                                                                                      {2, [<<"0.5.0">>]},
                                                                                      {3, [<<"0.5.0">>]},
                                                                                      {4, [<<"0.5.0">>]},
                                                                                      {all, [<<"0.5.0">>]}]}]
                                                            }
                                                           ]
        ]
       } %% foreachx
      } %% Environment-filtered Cookbook Versions Tests for a Single Cookbook

     ]}.

%%%======================================================================
%%% NODES
%%%======================================================================

insert_node_data() ->
    Nodes = node_list(),
    Expected = lists:duplicate(length(Nodes), {ok, 1}),
    Results = [chef_sql:create_node(Node) || Node <- Nodes ],
    ?assertEqual(Expected, Results).

%%%======================================================================
%%% CLIENTS
%%%======================================================================

insert_client_data() ->
    Clients = [ make_client(<<"client01">>), make_client(<<"client02">>) ],
    Expected = lists:duplicate(length(Clients), {ok, 1}),
    Results = [chef_sql:create_client(Client) || Client <- Clients ],
    ?assertEqual(Expected, Results).

fetch_client_data() ->
    Expected = make_client(<<"client03">>),

    % Assume an existing client
    ?assertEqual({ok, 1}, chef_sql:create_client(Expected)),

    {ok, Got} = chef_sql:fetch_client(Expected#chef_client.org_id, Expected#chef_client.name),
    ?assertEqual(Expected, Got).

bulk_fetch_client_data() ->
  Clients =  [ make_client(<<"client_bulk", Num/binary>>) || Num <- [ <<"0">>, <<"1">>, <<"2">> ] ],
  [ ?assertEqual({ok, 1}, chef_sql:create_client(C)) || C <- Clients ],
  Ids = [ C#chef_client.id || C <- Clients ],
  Expected = [ [{<<"name">>, C#chef_client.name},
                {<<"clientname">>, C#chef_client.name},
                {<<"public_key">>, C#chef_client.public_key}] || C <- Clients],

  {ok, Got} = chef_sql:bulk_get_clients(Ids),
  ?assertEqual(length(Got), 3),
  ?assertEqual(Expected, Got).

delete_client_data() ->
    Existing = make_client(<<"client04">>),

    % Assume an existing client
    ?assertEqual({ok, 1}, chef_sql:create_client(Existing)),
    {ok, BeforeDelete} = chef_sql:fetch_client(Existing#chef_client.org_id, Existing#chef_client.name),
    ?assertEqual(Existing, BeforeDelete),

    ?assertEqual({ok, 1}, chef_sql:delete_client(Existing#chef_client.id)),
    % Is {ok, not_found} correct?
    % This is what chef_sql:fetch_object returns
    ?assertEqual({ok, not_found}, chef_sql:fetch_client(Existing#chef_client.org_id, Existing#chef_client.name)).

%%%======================================================================
%%% DATA BAGS
%%%======================================================================

insert_data_bag_data() ->
    Expected = lists:duplicate(length(data_bags()), {ok, 1}),
    Results = [chef_sql:create_data_bag(Bag) || Bag <- data_bags() ],
    ?assertEqual(Expected, Results).

fetch_data_bags() ->
    DBS = data_bags(),
    Expected = [ Db#chef_data_bag.name || Db <- DBS, Db#chef_data_bag.org_id =:= the_org_id() ],
    {ok, Results} = chef_sql:fetch_data_bags(the_org_id()),
    ?assertEqual(Expected, Results).

fetch_data_bag() ->
    Db = hd(data_bags()),
    {ok, Got} = chef_sql:fetch_data_bag(Db#chef_data_bag.org_id, Db#chef_data_bag.name),
    ?assertEqual(Db, Got).

%% bulk_get_data_bags() ->
%%     Ids = lists:sort([ list_to_binary(Db#chef_data_bag.id) ||
%%                          Db <- data_bags(),
%%                          Db#chef_data_bag.org_id =:= the_org_id() ]),
%%     Expected = lists:sort([ list_to_binary(Db#chef_data_bag.name) ||
%%                               Db <- data_bags(),
%%                               Db#chef_data_bag.org_id =:= the_org_id() ]),
%%     {ok, Results} = chef_sql:bulk_get_data_bags(Ids),
%%    ?assertEqual(Expected, lists:sort(Results)).

delete_data_bag() ->
    First = hd(data_bags()),
    ?assertEqual({ok, 1}, chef_sql:delete_data_bag(First#chef_data_bag.id)),
    %% verify data is gone
    ?assertEqual({ok, not_found}, chef_sql:fetch_data_bag(First#chef_data_bag.org_id,
                                                          First#chef_data_bag.name)),
    %% deleting a non existing data bag is OK
    ?assertEqual({ok, not_found}, chef_sql:delete_data_bag(First#chef_data_bag.id)).

%%%======================================================================
%%% DATA BAGS
%%%======================================================================

insert_data_bag_item_data() ->
    Expected = lists:duplicate(length(data_bag_items()), {ok, 1}),
    Results = [chef_sql:create_data_bag_item(Bag) || Bag <- data_bag_items() ],
    ?assertEqual(Expected, Results).

fetch_data_bag_items() ->
    DBS = data_bag_items(),
    Expected = [ Db#chef_data_bag_item.item_name || Db <- DBS, Db#chef_data_bag_item.org_id =:= the_org_id(),
                                                    Db#chef_data_bag_item.data_bag_name =:= <<"data_bag_02">> ],
    {ok, Results} = chef_sql:fetch_data_bag_items(the_org_id(), <<"data_bag_02">>),
    ?assertEqual(Expected, Results).

fetch_data_bag_item()->
    Item = hd(data_bag_items()),

    {ok, Got} = chef_sql:fetch_data_bag_item(Item#chef_data_bag_item.org_id,
                                             Item#chef_data_bag_item.data_bag_name,
                                             Item#chef_data_bag_item.item_name),
    ?assertEqual(Item, Got).

fetch_data_bag_item_ids() ->
    Expected = [ Db#chef_data_bag_item.id ||
                   Db <- data_bag_items(),
                   Db#chef_data_bag_item.org_id =:= the_org_id(),
                   Db#chef_data_bag_item.data_bag_name =:= <<"data_bag_02">>],
    {ok, Results} = chef_sql:fetch_data_bag_item_ids(the_org_id(), <<"data_bag_02">>),
    ?assertEqual(Expected,Results).

bulk_get_data_bag_items()-> ok.

update_data_bag_item()->
    [Old | _T] = [ Db ||
                     Db <- data_bag_items(),
                     Db#chef_data_bag_item.org_id =:= the_org_id(),
                     Db#chef_data_bag_item.data_bag_name =:= <<"data_bag_02">>],
    NewData = <<"new object">>,
    New = Old#chef_data_bag_item{serialized_object= NewData},
    {ok, UResults} = chef_sql:update_data_bag_item(New),
    ?assertEqual(1, UResults),
    {ok, FResults} = chef_sql:fetch_data_bag_item(the_org_id(), New#chef_data_bag_item.data_bag_name, New#chef_data_bag_item.item_name),
    ?assertEqual(NewData,
                 (FResults#chef_data_bag_item.serialized_object)).


delete_data_bag_item()->
    Item = hd(data_bag_items()),
    {ok, DResults} = chef_sql:delete_data_bag_item(Item#chef_data_bag_item.id),
    ?assertEqual(1, DResults),
    {ok, FResults} = chef_sql:fetch_data_bag_item(Item#chef_data_bag_item.org_id, Item#chef_data_bag_item.data_bag_name, Item#chef_data_bag_item.item_name),
    ?assertEqual(not_found, FResults).

%%%======================================================================
%%% SANDBOXES AND CHECKSUMS
%%%======================================================================

insert_sandbox() ->
    ?assertEqual({ok, 1},
                 chef_sql:create_sandbox(make_sandbox(<<"abcd">>))).

mark_some_checksums_as_uploaded() ->
    #chef_sandbox{checksums=ChecksumSpec} = make_sandbox(<<"abcd">>),
    %% Hold one aside for checking non-uploaded checksums
    Checksums = [C || {C, false} <- ChecksumSpec, C /= <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>],
    ?assertEqual(ok,
                 chef_sql:mark_checksums_as_uploaded(the_org_id(), Checksums)).

check_non_uploaded_checksums() ->
    ?assertEqual([<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>],
                 chef_sql:non_uploaded_checksums(make_id(<<"abcd">>), the_org_id())).

upload_last_checksum() ->
    ?assertEqual(ok,
                 chef_sql:mark_checksums_as_uploaded(the_org_id(), [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>])),
    ?assertEqual([],
                 chef_sql:non_uploaded_checksums(make_id(<<"abcd">>), the_org_id())).

fetch_sandbox() ->
    {ok, ActualValue} = chef_sql:fetch_sandbox(the_org_id(), make_id(<<"abcd">>)),
    ?assertEqual(#chef_sandbox{id=make_id(<<"abcd">>),
                               org_id=the_org_id(),
                               created_at={datetime,{{2011,10,1},{16,47,46}}},
                               checksums=[{<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, true},
                                          {<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>, true},
                                          {<<"cccccccccccccccccccccccccccccccc">>, true},
                                          {<<"dddddddddddddddddddddddddddddddd">>, true}]},
                 ActualValue).
%% delete the sandbox
delete_sandbox() ->
    ?assertEqual({ok, 1},
                 chef_sql:delete_sandbox(make_id(<<"abcd">>))).

%%%======================================================================
%%% COOKBOOKS
%%%======================================================================

insert_cookbook_data() ->
    {AuthzId, OrgId, Name} = make_cookbook(<<"insert_cookbook">>),
    CbStub = #chef_cookbook_version{authz_id=AuthzId,
                                    org_id=OrgId,
                                    name=Name},
    ?assertEqual(false, chef_sql:cookbook_exists(OrgId, Name)),
    Results = chef_sql:create_cookbook_if_needed(CbStub),
    ?assertEqual(true, chef_sql:cookbook_exists(OrgId, Name)),
    ?assertEqual(ok, Results).

fetch_cookbook_authz() ->
    {AuthzId, OrgId, Name} = make_cookbook(<<"fetch_cookbook_authz">>),
    CbStub = #chef_cookbook_version{authz_id=AuthzId,
                                    org_id=OrgId,
                                    name=Name},
    ?assertEqual(not_found, chef_sql:fetch_cookbook_authz(OrgId, Name)),
    Results = chef_sql:create_cookbook_if_needed(CbStub),
    ?assertEqual(ok, Results),

    Got = chef_sql:fetch_cookbook_authz(OrgId, Name),
    ?assertEqual(AuthzId, Got).

%%%======================================================================
%%% COOKBOOK VERSIONS
%%%======================================================================

%% covers both insert new cookbook_version and insert new version of cookbook_version
insert_cookbook_version_data() ->
    Cookbook = make_cookbook(<<"insert">>),
    CookbookVersions = cookbook_version_list(Cookbook),
    Expected = lists:duplicate(length(CookbookVersions), {ok, 1}),
    Results = [chef_sql:create_cookbook_version(CookbookVersion) || CookbookVersion <- CookbookVersions ],
    ?assertEqual(Expected, Results).

insert_cbv_null_id() ->
    {AuthzId, OrgId, Name} = make_cookbook(<<"insert_invalid">>),
    %% interesting line is first one - 'id = null'
    CookbookVersion = #chef_cookbook_version{id = null,
                                             authz_id=AuthzId,
                                             org_id=OrgId,
                                             name=Name,
                                             major=0, minor=0, patch=0, frozen=false,
                                             meta_attributes= <<"">>,
                                             meta_deps= <<"">>,
                                             meta_long_desc= <<"">>,
                                             metadata= <<"">>,
                                             last_updated_by= actor_id(),
                                             created_at= {datetime, {{2011,10,1},{16,47,46}}},
                                             updated_at= {datetime, {{2011,10,1},{16,47,46}}},
                                             serialized_object= <<"">>,
                                             checksums = [] },

    {error, Reason} = chef_sql:create_cookbook_version(CookbookVersion),
    Reason.

insert_cbv_no_id() ->
    {AuthzId, OrgId, Name} = make_cookbook(<<"insert_invalid">>),
    %% This record is missing an 'id' field
    CookbookVersion = #chef_cookbook_version{
      authz_id=AuthzId,
      org_id=OrgId,
      name=Name,
      major=0, minor=0, patch=0, frozen=false,
      meta_attributes= <<"">>,
      meta_deps= <<"">>,
      meta_long_desc= <<"">>,
      metadata= <<"">>,
      last_updated_by= actor_id(),
      created_at= {datetime, {{2011,10,1},{16,47,46}}},
      updated_at= {datetime, {{2011,10,1},{16,47,46}}},
      serialized_object= <<"">>,
      checksums = [] },
    ?assertError({undefined_in_record, CookbookVersion},
                 chef_sql:create_cookbook_version(CookbookVersion)).

insert_cbv_with_unknown_checksums() ->
    Cookbook = make_cookbook(<<"insert_unknown">>),
    CookbookVersion = make_cookbook_version(<<"001insert_unknown">>, 0, Cookbook),
    CookbookVersion1 = CookbookVersion#chef_cookbook_version{checksums= [ <<"cksum1">> ]},
    ?assertEqual({error, invalid_checksum}, chef_sql:create_cookbook_version(CookbookVersion1)).

insert_cbv_with_frozen() ->
    Cookbook = make_cookbook(<<"insert_frozen">>),
    CookbookVersion = make_cookbook_version(<<"001insert_frozen">>, 0, Cookbook),
    CookbookVersion1 = CookbookVersion#chef_cookbook_version{frozen = true },
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion1)).

fetch_cookbook_version_not_exist() ->
    Cookbook = make_cookbook(<<"not_exist">>),
    CookbookVersion = make_cookbook_version(<<"001not_exist">>, 0, Cookbook),

    Got = chef_sql:fetch_cookbook_version(CookbookVersion#chef_cookbook_version.org_id,
                                          {CookbookVersion#chef_cookbook_version.name,
                                           {CookbookVersion#chef_cookbook_version.major,
                                            CookbookVersion#chef_cookbook_version.minor,
                                            CookbookVersion#chef_cookbook_version.patch}}),
    ?assertEqual(not_found, Got).

fetch_cookbook_version_no_checksums() ->
    Cookbook = make_cookbook(<<"fetch">>),
    CookbookVersion = make_cookbook_version(<<"001fetch">>, 0, Cookbook),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion)),
    Got = chef_sql:fetch_cookbook_version(CookbookVersion#chef_cookbook_version.org_id,
                                          {CookbookVersion#chef_cookbook_version.name,
                                           {CookbookVersion#chef_cookbook_version.major,
                                            CookbookVersion#chef_cookbook_version.minor,
                                            CookbookVersion#chef_cookbook_version.patch}}),
    ?assertEqual(CookbookVersion, Got).

fetch_cookbook_version_checksums() ->
    Cookbook = make_cookbook(<<"fetch_checksums">>),
    CookbookVersion0 = make_cookbook_version(<<"001fetch_checksums">>, 0, Cookbook),
    Checksums = [ make_id(<<"checksum1">>),
                  make_id(<<"checksum2">>)],
    CookbookVersion = CookbookVersion0#chef_cookbook_version{checksums=Checksums},
    ok = chef_sql:mark_checksums_as_uploaded(CookbookVersion#chef_cookbook_version.org_id,
                                             Checksums),

    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion)),
    Got = chef_sql:fetch_cookbook_version(CookbookVersion#chef_cookbook_version.org_id,
                                          {CookbookVersion#chef_cookbook_version.name,
                                           {CookbookVersion#chef_cookbook_version.major,
                                            CookbookVersion#chef_cookbook_version.minor,
                                            CookbookVersion#chef_cookbook_version.patch}}),
    ?assertEqual(CookbookVersion, Got).

fetch_cookbook_version_different_version() ->
    Cookbook = make_cookbook(<<"fetch_different">>),
    CookbookVersion0 = make_cookbook_version(<<"001fetch_different">>, 0, Cookbook),

    CookbookVersion = CookbookVersion0#chef_cookbook_version{patch=1},

    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion0)),
    Expected = {cookbook_exists, CookbookVersion0#chef_cookbook_version.authz_id},
    Got = chef_sql:fetch_cookbook_version(CookbookVersion#chef_cookbook_version.org_id,
                                          {CookbookVersion#chef_cookbook_version.name,
                                           {CookbookVersion#chef_cookbook_version.major,
                                            CookbookVersion#chef_cookbook_version.minor,
                                            CookbookVersion#chef_cookbook_version.patch}}),
    ?assertEqual(Expected, Got).

fetch_cookbook_versions() ->
    OrgId = the_org_id(),
    {ok, Versions} = chef_sql:fetch_cookbook_versions(OrgId),
    %% FIXME Hard to test the exact return value given we don't have
    %% a clean state.  For now check the shape of the returned
    %% structure
    [ begin
          [Name, {Major, Minor, Patch}] = Row,
          ?assertMatch( X when is_binary(X), Name),
          ?assertMatch( X when is_integer(X) andalso X >= 0 , Major),
          ?assertMatch( X when is_integer(X) andalso X >= 0, Minor),
          ?assertMatch( X when is_integer(X) andalso X >= 0, Patch)
      end || Row <- Versions].

%% @doc This tests pulling the latest version of a cookbook using the _latest endpoint
fetch_latest_cookbook_version() ->
    Cookbook = make_cookbook(<<"the_latest">>),
    CookbookVersion = make_cookbook_version(<<"001the_latest">>, 0, Cookbook),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion)),
    Expected = CookbookVersion,
    Got = chef_sql:fetch_latest_cookbook_version(CookbookVersion#chef_cookbook_version.org_id, CookbookVersion#chef_cookbook_version.name),
    ?assertEqual(Expected, Got).

%% @doc This tests pulling the latest version of a cookbook using the _latest endpoint with two of the same cookbook
%% with different versions in the system
fetch_latest_cookbook_version_different_versions() ->
    Cookbook = make_cookbook(<<"the_old">>),
    CookbookVersion = make_cookbook_version(<<"001the_old">>, 0, Cookbook),
                                                % First param is the primary id of the cookbook - different for every cookbook and cookbook version
    CookbookVersionToo = make_cookbook_version(<<"002the_old">>, 1, Cookbook),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion)),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersionToo)),
    Expected = CookbookVersionToo,
    Got = chef_sql:fetch_latest_cookbook_version(CookbookVersion#chef_cookbook_version.org_id, CookbookVersion#chef_cookbook_version.name),
    ?assertEqual(Expected, Got).

%% @doc This tests we can update with checksums for a cookbook version.
% Checksums must be unique when we mark it as uploaded
update_cookbook_version_checksums() ->
    Cookbook = make_cookbook(<<"update_checksums">>),
    ExistingVersion = make_cookbook_version(<<"update_version_01">>, 1, Cookbook),
    Checksums = [ make_id(<<"1checksum2">>),
                  make_id(<<"1checksum1">>) ],
    update_cookbook_version_checksums(ExistingVersion, Checksums, Checksums).

update_cookbook_version_checksums_with_deletions() ->
    Cookbook = make_cookbook(<<"update_checksums">>),
    ExistingVersion = make_cookbook_version(<<"update_version_02">>, 2, Cookbook),
    ExistingChecksums = [ make_id(<<"2checksum2_2">>), make_id(<<"2checksum1">>), make_id(<<"2checksum3">>) ],
    UpdatedChecksums = [ make_id(<<"2checksum1">>) ],
    update_cookbook_version_checksums(ExistingVersion, ExistingChecksums, UpdatedChecksums).

update_cookbook_version_checksums_with_additions() ->
    Cookbook = make_cookbook(<<"update_checksums">>),
    ExistingVersion = make_cookbook_version(<<"update_version_03">>, 3, Cookbook),
    ExistingChecksums = [ make_id(<<"3checksum2">>) ],
    UpdatedChecksums = [ make_id(<<"3checksum1">>), make_id(<<"3checksum2">>), make_id(<<"3checksum0">>) ],
    update_cookbook_version_checksums(ExistingVersion, ExistingChecksums, UpdatedChecksums).

update_cookbook_version_checksums_with_deletions_and_additions() ->
    Cookbook = make_cookbook(<<"update_checksums">>),
    ExistingVersion = make_cookbook_version(<<"update_version_04">>, 4, Cookbook),
    ExistingChecksums = [ make_id(<<"4checksum2">>), make_id(<<"4checksum1">>), make_id(<<"4checksums6">>) ],
    UpdatedChecksums = [ make_id(<<"4checksums6">>), make_id(<<"4checksum5">>), make_id(<<"4checksum0">>) ],
    update_cookbook_version_checksums(ExistingVersion, ExistingChecksums, UpdatedChecksums).


update_cookbook_version_checksums(#chef_cookbook_version{} = ExistingVersion, ExistingChecksums, UpdatedChecksums)
    when is_list(ExistingChecksums) andalso is_list(UpdatedChecksums) ->
    %% This CB exists since we created it in an earlier test - we retrieve it to
    %% get the full CB including checksums
    NormalizedExistingChecksums = lists:sort(ExistingChecksums),
    NormalizedUpdatedChecksums  = lists:sort(UpdatedChecksums),
    UpdatedVersion = ExistingVersion#chef_cookbook_version{ meta_long_desc = <<"Updated Description">>, checksums = NormalizedUpdatedChecksums },

    % Assume new checksums have been marked as uploaded
    UploadedChecksums = lists:usort(lists:append(ExistingChecksums, UpdatedChecksums)),
    ?assertEqual(ok, chef_sql:mark_checksums_as_uploaded(UpdatedVersion#chef_cookbook_version.org_id, UploadedChecksums)),

    % Create a new cookbook version
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(ExistingVersion#chef_cookbook_version{ checksums = ExistingChecksums })),

    % A cookbook version should already exist in the datastore
    Existing = chef_sql:fetch_cookbook_version(ExistingVersion#chef_cookbook_version.org_id,
                                               {ExistingVersion#chef_cookbook_version.name,
                                                 {ExistingVersion#chef_cookbook_version.major,
                                                   ExistingVersion#chef_cookbook_version.minor,
                                                   ExistingVersion#chef_cookbook_version.patch}}),
    ?assertEqual(ExistingVersion#chef_cookbook_version{checksums = []}, Existing#chef_cookbook_version{checksums = []}),
    ?assertEqual(NormalizedExistingChecksums, lists:sort(Existing#chef_cookbook_version.checksums)),


    ?assertEqual({ok, 1}, chef_sql:update_cookbook_version(UpdatedVersion)),
    Updated = chef_sql:fetch_cookbook_version(UpdatedVersion#chef_cookbook_version.org_id,
                                               {UpdatedVersion#chef_cookbook_version.name,
                                                 {UpdatedVersion#chef_cookbook_version.major,
                                                   UpdatedVersion#chef_cookbook_version.minor,
                                                   UpdatedVersion#chef_cookbook_version.patch}}),
    ?assertEqual(UpdatedVersion#chef_cookbook_version{checksums = []}, Updated#chef_cookbook_version{checksums = []}),
    ?assertEqual(UpdatedVersion#chef_cookbook_version.meta_long_desc, Updated#chef_cookbook_version.meta_long_desc),

    ?assertEqual(NormalizedUpdatedChecksums, lists:sort(Updated#chef_cookbook_version.checksums)).

update_cookbook_version_checksums_with_missing_checksums() ->
    %% This CB exists since we created it in an earlier test - we retrieve it to
    %% get the full CB including checksums
    Cookbook = make_cookbook(<<"update_checksums">>),
    ExistingVersion = make_cookbook_version(<<"update_version_05">>, 5, Cookbook),
    MissingChecksum = make_id(<<"5missing1">>),
    ExistingChecksums = [ make_id(<<"5checksum2">>), make_id(<<"5checksum1">>), make_id(<<"5checksums6">>) ],
    UpdatedChecksums = [ make_id(<<"5checksums2">>), MissingChecksum ],

    UpdatedVersion = ExistingVersion#chef_cookbook_version{ meta_long_desc = <<"Updated Description">>, checksums = UpdatedChecksums },

    % Assume existing checksum have been uploaded
    ?assertEqual(ok, chef_sql:mark_checksums_as_uploaded(UpdatedVersion#chef_cookbook_version.org_id, ExistingChecksums)),

    % Create a new cookbook version
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(ExistingVersion#chef_cookbook_version{ checksums = ExistingChecksums })),

    % A cookbook version should already exist in the datastore
    Existing = chef_sql:fetch_cookbook_version(ExistingVersion#chef_cookbook_version.org_id,
                                               {ExistingVersion#chef_cookbook_version.name,
                                                 {ExistingVersion#chef_cookbook_version.major,
                                                   ExistingVersion#chef_cookbook_version.minor,
                                                   ExistingVersion#chef_cookbook_version.patch}}),
    ?assertEqual(ExistingVersion#chef_cookbook_version{checksums = []}, Existing#chef_cookbook_version{checksums = []}),
    ?assertEqual(lists:sort(ExistingChecksums), lists:sort(Existing#chef_cookbook_version.checksums)),

    ?assertEqual({error, {checksum_missing, MissingChecksum}}, chef_sql:update_cookbook_version(UpdatedVersion)).

update_cookbook_version_checksums_with_shared_checksums() ->
    Cookbook = make_cookbook(<<"update_checksums">>),
    OlderVersion    = make_cookbook_version(<<"update_version_06">>, 6, Cookbook),
    ExistingVersion = make_cookbook_version(<<"update_version_07">>, 7, Cookbook),

    SharedChecksum = make_id(<<"6shared1">>),
    OlderChecksums = [ make_id(<<"6checksum2">>), make_id(<<"5checksums6">>), SharedChecksum ],
    ExistingChecksums = [ make_id(<<"7checksum2">>), make_id(<<"7checksums6">>), SharedChecksum ],
    UpdatedChecksums = [ make_id(<<"7checksums2">>) ],

    UpdatedVersion = ExistingVersion#chef_cookbook_version{ meta_long_desc = <<"Updated Description">>, checksums = UpdatedChecksums },

    % Assume existing checksum have been uploaded
    ?assertEqual(ok, chef_sql:mark_checksums_as_uploaded(UpdatedVersion#chef_cookbook_version.org_id, OlderChecksums)),
    ?assertEqual(ok, chef_sql:mark_checksums_as_uploaded(UpdatedVersion#chef_cookbook_version.org_id, ExistingChecksums)),
    ?assertEqual(ok, chef_sql:mark_checksums_as_uploaded(UpdatedVersion#chef_cookbook_version.org_id, UpdatedChecksums)),

    % Create two cookbook version with one shared checksum
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(OlderVersion#chef_cookbook_version{ checksums = OlderChecksums })),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(ExistingVersion#chef_cookbook_version{ checksums = ExistingChecksums })),

    % A cookbook version should already exist in the datastore
    Existing = chef_sql:fetch_cookbook_version(ExistingVersion#chef_cookbook_version.org_id,
                                               {ExistingVersion#chef_cookbook_version.name,
                                                 {ExistingVersion#chef_cookbook_version.major,
                                                   ExistingVersion#chef_cookbook_version.minor,
                                                   ExistingVersion#chef_cookbook_version.patch}}),
    ?assertEqual(lists:sort(ExistingChecksums), lists:sort(Existing#chef_cookbook_version.checksums)),

    % Update a version which causes the shared checksum to be deleted
    ?assertEqual({ok, 1}, chef_sql:update_cookbook_version(UpdatedVersion)),
    Updated = chef_sql:fetch_cookbook_version(UpdatedVersion#chef_cookbook_version.org_id,
                                               {UpdatedVersion#chef_cookbook_version.name,
                                                 {UpdatedVersion#chef_cookbook_version.major,
                                                   UpdatedVersion#chef_cookbook_version.minor,
                                                   UpdatedVersion#chef_cookbook_version.patch}}),
    % Prove that checksums have been updated
    ?assertEqual(UpdatedVersion#chef_cookbook_version{checksums = []}, Updated#chef_cookbook_version{checksums = []}),
    ?assertEqual(UpdatedVersion#chef_cookbook_version.meta_long_desc, Updated#chef_cookbook_version.meta_long_desc),
    ?assertEqual(lists:sort(UpdatedChecksums), lists:sort(Updated#chef_cookbook_version.checksums)),

    % Prove that the shared checksum have not been overwritten
    Older = chef_sql:fetch_cookbook_version(OlderVersion#chef_cookbook_version.org_id,
                                               {OlderVersion#chef_cookbook_version.name,
                                                 {OlderVersion#chef_cookbook_version.major,
                                                   OlderVersion#chef_cookbook_version.minor,
                                                   OlderVersion#chef_cookbook_version.patch}}),
    ?assertEqual(lists:sort(OlderChecksums), lists:sort(Older#chef_cookbook_version.checksums)).


%% @doc This tests we can delete with checksums for a cookbook version
delete_cookbook_version_checksums() ->
    %% This CB exists since we created it in an earlier test - we retrieve it to
    %% get the full CB including checksums
    Cookbook = make_cookbook(<<"fetch_checksums">>),
    CookbookVersion = make_cookbook_version(<<"001fetch_checksums">>, 0, Cookbook),
    Got = chef_sql:fetch_cookbook_version(CookbookVersion#chef_cookbook_version.org_id,
                                          {CookbookVersion#chef_cookbook_version.name,
                                           {CookbookVersion#chef_cookbook_version.major,
                                            CookbookVersion#chef_cookbook_version.minor,
                                            CookbookVersion#chef_cookbook_version.patch}}),

    % Verify all checksums exist
    [Checksum1, Checksum2] = Got#chef_cookbook_version.checksums,
    ?assertEqual(true, chef_sql:checksum_exists(Checksum1)),
    ?assertEqual(true, chef_sql:checksum_exists(Checksum2)),

    %% We should have gotten back a list of deleted checksums
    {ok, N, DeletedChecksums} = chef_sql:delete_cookbook_version(Got),

    ?assertEqual(2, N), %% Last version of this cookbook, so deleting the cookbook as well
    ?assertEqual(lists:sort(Got#chef_cookbook_version.checksums),
                 lists:sort(DeletedChecksums)),

    ?assertEqual(not_found, chef_sql:fetch_cookbook_version(Got#chef_cookbook_version.org_id,
                                                            {Got#chef_cookbook_version.name,
                                                             {Got#chef_cookbook_version.major,
                                                              Got#chef_cookbook_version.minor,
                                                              Got#chef_cookbook_version.patch}})),

    %% Ensure the checksums don't exist in the checksum table
    ?assertEqual(false, chef_sql:checksum_exists(Checksum1)),
    ?assertEqual(false, chef_sql:checksum_exists(Checksum2)).

%% @doc check that the cookbook row is still in the database until all
%% versions of the cookbook have been deleted
delete_cookbook_multiple_versions() ->
    {AuthzId, OrgId, Name} = make_cookbook(<<"delete_multiple">>),
    ?assertEqual(not_found, chef_sql:fetch_cookbook_authz(OrgId, Name)),

    CookbookVersion0 = make_cookbook_version(<<"000delete_multiple">>, 0, {AuthzId, OrgId, Name}),
    CookbookVersion1 = make_cookbook_version(<<"001delete_multiple">>, 1, {AuthzId, OrgId, Name}),
    Checksums = [ make_id(<<"checksum1">>),
                  make_id(<<"checksum2">>)],
    CookbookVersion20 = CookbookVersion0#chef_cookbook_version{checksums=Checksums},
    CookbookVersion21 = CookbookVersion1#chef_cookbook_version{checksums=Checksums},
    ok = chef_sql:mark_checksums_as_uploaded(CookbookVersion20#chef_cookbook_version.org_id,
                                             Checksums),

    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion20)),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion21)),
    Got = chef_sql:fetch_cookbook_authz(OrgId, Name),

    %% No checksums should be deleted from the checksum table so we should get
    %% back an empty list.
    ?assertEqual({ok, 1, []}, chef_sql:delete_cookbook_version(CookbookVersion20)),
    Got = chef_sql:fetch_cookbook_authz(OrgId, Name),

    %% All checksums should be deleted when the second (and final) cookbook
    %% version is deleted.
    {ok, N, DeletedChecksums} = chef_sql:delete_cookbook_version(CookbookVersion21),
    ?assertEqual(2, N), %% Last version of this cookbook, so deleting the cookbook as well
    ?assertEqual(lists:sort(Checksums),
                 lists:sort(DeletedChecksums)),

    %not_found = chef_sql:fetch_cookbook_authz(OrgId, Name).
    ?assertEqual(not_found, chef_sql:fetch_cookbook_authz(OrgId, Name)).

%% Combined integration test for entire cookbook create workflow.
%% Starts with (org_id, cookbook_name, cookbook_version)
%%
%% 1. Get #chef_cookbook{} record in Org with given cookbook_name
%% 2. CHECK AUTHZ
%%  a. if {ok, Cookbook}  check for READ on #chef_cookbook.authz_id
%%  b. if {ok, not_found}, check for CREATE on
%%     cookbook_container_authz_id, create new AuthzId

%% 3. Create #chef_cookbook_version{}
%%  a. If {ok, not_found) then create_cookbook_version/1
%%  b. If exists update_cookbook_version/?
%%
%% TODO - integrate cookbook_version_checksums
%% TODO - 3b. write cookbook update code


%% Cover the case where nothing exists in the DB
cookbook_create_from_scratch() ->
    OrgId = the_org_id(),
    Name = <<"cookbook_itest_create">>,

    %% Step 1.
    %% TODO: fetch_cookbook_version with helpful return values
    ?assertEqual(not_found, chef_sql:fetch_cookbook_authz(OrgId, Name)),

    %% Step 2b. - AUTHZ
    AzId = make_az_id(<<"itest_create">>),
    Cookbook = {AzId, OrgId, Name},

    %% Step 3.
    %% TODO: add chef_object:new_record for this
    CbVersion = make_cookbook_version(<<"001itest_create">>, 1, Cookbook),

    %% Step 3a.
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CbVersion)).

%% Cover the case where the cookbook exists, but not the actual cookbook version
cookbook_create_new_version() ->
    OrgId = the_org_id(),
    Name = <<"cookbook_itest_create_new">>,

    %% Setup - upload a different version of the Cookbook
    Cookbook0 = {make_az_id(<<"itest_create_new">>), OrgId, Name},
    CbVersion0 = make_cookbook_version(<<"000itest_create_new">>, 0, Cookbook0),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CbVersion0)),

    %% Step 1.
    AuthzId = chef_sql:fetch_cookbook_authz(OrgId, Name),

    %% Step 2a. - AUTHZ
    %%   We check Cookbook.authz_id here

    %% Step 3.
    CbVersion = make_cookbook_version(<<"001itest_create_new">>, 1, {OrgId, Name, AuthzId}),

    %% Step 3a.
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CbVersion)).








%% Utility Functions for Cookbook Setup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%% @doc Does what it says.
%%
%% Example: 1 -> <<"1">>
integer_to_binary(Int) when is_integer(Int) ->
    list_to_binary(integer_to_list(Int)).

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
    JSON = ejson:encode(EJson),
    zlib:gzip(JSON).

%%------------------------------------------------------------------------------
%% Environment-related Helper Functions
%%------------------------------------------------------------------------------

environment_name_from_prefix(Prefix) ->
    <<"env_", Prefix/binary>>.

%% TODO: Other places use this same date... they should use this
%% function, instead
default_date() ->
    {datetime, {{2011,10,1},{16,47,46}}}.

add_environment_to_db(#chef_environment{}=Environment) ->
    chef_sql:create_environment(Environment).

destroy_environment(#chef_environment{id=Id}) ->
    chef_sql:delete_environment(Id).

environment_from_spec({Prefix, Properties}) ->
    make_environment(Prefix, Properties);
environment_from_spec(Prefix) when is_binary(Prefix)->
    make_environment(Prefix, []).

%% TODO: This doesn't handle the "_default" environment yet
make_environment(Prefix, Properties) ->
    AuthzId = make_az_id(Prefix),
    OrgId = the_org_id(),
    Id = make_id(Prefix),

    %% These are the various properties of the Environment's
    %% `serialized_object' field we currently give hooks for
    Keys = [cookbook_versions],
    Object = lists:foldl(fun(Property, Acc) ->
                                 ej:set({atom_to_binary(Property, utf8)},
                                        Acc,
                                        process_environment_property(Property, Properties))
                         end,
                         {[]},
                         Keys),

    #chef_environment{
                       id = Id,
                       authz_id = AuthzId,
                       org_id = OrgId,
                       name = environment_name_from_prefix(Prefix),
                       last_updated_by = actor_id(),
                       created_at = default_date(),
                       updated_at = default_date(),
                       serialized_object = encode_and_compress(Object)
                     }.

process_environment_property(cookbook_versions=Property, Properties) ->
    case proplists:lookup(Property, Properties) of
        {Property, Value} when is_list(Value)->
            %% Wrap for EJson
            {Value};
        none ->
            {[]}
    end.
