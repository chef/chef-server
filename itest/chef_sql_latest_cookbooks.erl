-module(chef_sql_latest_cookbooks).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_db.hrl").
-include_lib("chef_objects/include/chef_types.hrl").

latest_cookbooks() ->
    {foreachx,
     fun itest_cookbook_util:cookbook_setup/1,
     fun itest_cookbook_util:cookbook_cleanup/2,
     [{Specs, fun(CookbookSpecs, _) ->
                      {Description,
                       fun() ->
                               Expected = itest_cookbook_util:latest_from_cookbook_specs(CookbookSpecs, NumVersions),
                               {ok, Actual} = chef_sql:fetch_latest_cookbook_versions(itest_util:the_org_id(), all, NumVersions),
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
    }.
