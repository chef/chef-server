-module(chef_sql_environment_cookbooks).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_db.hrl").
-include_lib("chef_objects/include/chef_types.hrl").

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
    AuthzId = itest_util:make_az_id(Prefix),
    OrgId = itest_util:the_org_id(),
    Id = itest_util:make_id(Prefix),

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
                       last_updated_by = itest_util:actor_id(),
                       created_at = default_date(),
                       updated_at = default_date(),
                       serialized_object = itest_cookbook_util:encode_and_compress(Object)
                     }.

process_environment_property(cookbook_versions=Property, Properties) ->
    case proplists:lookup(Property, Properties) of
        {Property, Value} when is_list(Value)->
            %% Wrap for EJson
            {Value};
        none ->
            {[]}
    end.

environment_name_from_prefix(Prefix) ->
    <<"env_", Prefix/binary>>.

%% TODO: Other places use this same date... they should use this
%% function, instead
default_date() ->
    {datetime, {{2011,10,1},{16,47,46}}}.

environment_cookbooks() ->
    [
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

               itest_cookbook_util:cookbook_setup(AllCookbooks),
               FullEnvironment = environment_from_spec(EnvironmentSpec),
               add_environment_to_db(FullEnvironment),
               FullEnvironment
       end,
       fun(_, Environment) ->
               itest_cookbook_util:remove_all_cookbooks(),
               destroy_environment(Environment),
               ok
       end,
       [{EnvironmentSpec,
         fun(_, RealEnvironment) ->
                 {Description,
                  fun() ->
                          EnvName = RealEnvironment#chef_environment.name,
                          {ok, Actual} = chef_sql:fetch_environment_filtered_cookbook_versions(itest_util:the_org_id(), EnvName, all, NumVersions),
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

               itest_cookbook_util:cookbook_setup(AllCookbooks),
               FullEnvironment = environment_from_spec(EnvironmentSpec),
               add_environment_to_db(FullEnvironment),
               FullEnvironment
       end,
       fun(_, Environment) ->
               itest_cookbook_util:remove_all_cookbooks(),
               destroy_environment(Environment),
               ok
       end,
       [{EnvironmentSpec,
         fun(_, RealEnvironment) ->
                 {Description,
                  fun() ->
                          EnvName = RealEnvironment#chef_environment.name,
                          {ok, Actual} = chef_sql:fetch_environment_filtered_recipes(itest_util:the_org_id(), EnvName),
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

               itest_cookbook_util:cookbook_setup(AllCookbooks),
               FullEnvironment = environment_from_spec(EnvironmentSpec),
               add_environment_to_db(FullEnvironment),
               FullEnvironment
       end,
       fun(_, Environment) ->
               itest_cookbook_util:remove_all_cookbooks(),
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
                                                              {ok, Actual} = chef_sql:fetch_environment_filtered_cookbook_versions(itest_util:the_org_id(),
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
    ].

