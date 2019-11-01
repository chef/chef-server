-module(chef_sql_environment_cookbooks).

-include_lib("eunit/include/eunit.hrl").
-include("chef_db.hrl").
-include("chef_types.hrl").

test_all() ->
    [ environment_cookbooks_test(Description,
                                 EnvironmentSpec,
                                 NumVersions,
                                 Expected)
      || {Description, EnvironmentSpec, NumVersions, Expected} <- environment_cookbooks_specs() ],

    [ environment_recipes_test(Description,
                               EnvironmentSpec,
                               Expected)
      || {Description, EnvironmentSpec, Expected} <- environment_recipes_specs() ],

    [ environment_filtered_cookbooks_test(Description,
                                          EnvironmentSpec,
                                          ResultsSpec)
      || {Description, EnvironmentSpec, ResultsSpec} <- environment_filtered_cookbooks_spec() ].

environment_cookbooks_test(Description, EnvironmentSpec, NumVersions, Expected) ->
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
    ct:pal("~p", [Description]),
    itest_cookbook_util:cookbook_setup(AllCookbooks),
    FullEnvironment = environment_from_spec(EnvironmentSpec),
    itest_util:create_record(FullEnvironment),
    {ok, PersistedEnvironment} = itest_util:fetch_record(FullEnvironment),
    {ok, Actual} = chef_sql:fetch_environment_filtered_cookbook_versions(itest_util:the_org_id(),
                                                                         PersistedEnvironment, all, NumVersions),
    ?assertEqual(Expected, Actual),
    itest_cookbook_util:remove_all_cookbooks(),
    itest_util:delete_record(FullEnvironment).


environment_recipes_test(Description, EnvironmentSpec, Expected) ->
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
    ct:pal("~p", [Description]),
    itest_cookbook_util:cookbook_setup(AllCookbooks),
    FullEnvironment = environment_from_spec(EnvironmentSpec),
    itest_util:create_record(FullEnvironment),
    {ok, PersistedEnvironment} = itest_util:fetch_record(FullEnvironment),
    {ok, Actual} = chef_sql:fetch_environment_filtered_recipes(itest_util:the_org_id(), PersistedEnvironment),
    ?assertEqual(Expected, Actual),
    itest_cookbook_util:remove_all_cookbooks(),
    itest_util:delete_record(FullEnvironment).

environment_filtered_cookbooks_test(Description, EnvironmentSpec, ResultsSpec) ->
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
    ct:pal("~p", [Description]),
    itest_cookbook_util:cookbook_setup(AllCookbooks),
    FullEnvironment = environment_from_spec(EnvironmentSpec),
    itest_util:create_record(FullEnvironment),
    {ok, PersistedEnvironment} = itest_util:fetch_record(FullEnvironment),

    lists:map(fun({CookbookName, NumToVersions}) ->
                      lists:map(fun({NumVersions, Versions}) ->
                                        {ok, Actual} = chef_sql:fetch_environment_filtered_cookbook_versions(itest_util:the_org_id(),
                                                                                                             PersistedEnvironment, CookbookName,
                                                                                                             NumVersions),
                                        ?assertEqual([{CookbookName, Versions}], Actual)
                                end,
                                NumToVersions)
              end,
              ResultsSpec),
    itest_cookbook_util:remove_all_cookbooks(),
    itest_util:delete_record(FullEnvironment).

environment_cookbooks_specs() ->
    [
     {"[environment cookbooks] multiple cookbooks, no environment constraints, one version",
      <<"testing">>,
      1,
      [{<<"cookbook_one">>, [<<"3.0.0">>]},
       {<<"cookbook_three">>, [<<"1.0.0">>]},
       {<<"cookbook_two">>, [<<"1.2.0">>]}
      ]
     },

     {"[environment cookbooks] multiple cookbooks, no environment constraints, all versions",
      <<"testing">>,
      all,
      [{<<"cookbook_one">>, [<<"3.0.0">>, <<"2.0.0">>]},
       {<<"cookbook_three">>, [<<"1.0.0">>, <<"0.5.0">>, <<"0.0.1">>]},
       {<<"cookbook_two">>, [<<"1.2.0">>, <<"1.0.0">>]}
      ]
     },

     {"[environment cookbooks] multiple cookbooks, no environment constraints, no versions",
      <<"testing">>,
      0,
      [{<<"cookbook_one">>, []},
       {<<"cookbook_three">>, []},
       {<<"cookbook_two">>, []}
      ]
     },

     {"[environment cookbooks] multiple cookbooks, environment constraints, no versions",
      {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"> 1.0.0">>},
                                            {<<"cookbook_two">>, <<"< 1.2.3">>}]}]},
      0,
      [{<<"cookbook_one">>, []},
       {<<"cookbook_three">>, []},
       {<<"cookbook_two">>, []}
      ]
     },

     {"[environment cookbooks] multiple cookbooks, environment constraints, one version",
      {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"> 1.0.0">>},
                                            {<<"cookbook_two">>, <<"< 1.2.3">>}]}]},
      1,
      [{<<"cookbook_one">>, [<<"3.0.0">>]},
       {<<"cookbook_three">>, [<<"1.0.0">>]},
       {<<"cookbook_two">>, [<<"1.2.0">>]}
      ]
     },

     {"[environment cookbooks] multiple cookbooks, different constraints, one version",
      {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"< 2.5.0">>},
                                            {<<"cookbook_three">>, <<"= 0.5.0">>}]}]},
      1,
      [{<<"cookbook_one">>, [<<"2.0.0">>]},
       {<<"cookbook_three">>, [<<"0.5.0">>]},
       {<<"cookbook_two">>, [<<"1.2.0">>]}
      ]
     },

     {"[environment cookbooks] multiple cookbooks, different constraints (completely removing one cookbook), one version",
      {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"= 6.6.6">>},
                                            {<<"cookbook_three">>, <<"= 0.5.0">>}]}]},
      1,
      [{<<"cookbook_three">>, [<<"0.5.0">>]},
       {<<"cookbook_two">>, [<<"1.2.0">>]}
      ]
     }
    ].

environment_recipes_specs() ->
    [
     {"[environment recipes] multiple cookbooks, no environment constraints",
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
     {"[environment recipes] multiple cookbooks, environment constraints",
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

     {"[environment recipes] multiple cookbooks, different constraints",
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

     {"[environment recipes] multiple cookbooks, different constraints (completely removing one cookbook)",
      {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"= 6.6.6">>},
                                            {<<"cookbook_three">>, <<"= 0.5.0">>}]}]},
      [
       <<"cookbook_three::client">>,
       <<"cookbook_three::server">>,
       <<"cookbook_two">>,
       <<"cookbook_two::baz">>
      ]
     },
     {"[environment recipes] multiple cookbooks, different constraints (completely removing all cookbooks)",
      {<<"testing">>, [{cookbook_versions, [{<<"cookbook_one">>, <<"= 6.6.6">>},
                                            {<<"cookbook_two">>, <<"= 6.6.6">>},
                                            {<<"cookbook_three">>, <<"= 6.6.6">>}]}]},
      []
     }
    ].

environment_filtered_cookbooks_spec() ->
    [
     {"[environment filtered] no environment constraints",
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

     {"[environment filtered] Add environment constraints that are functionally equivalenat to no constraints",
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
     {"[environment filtered] Try different constraints",
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

     {"[environment filtered] Try different constraints (completely removing one cookbook)",
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
    ].

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
