-module(chef_sql_cookbook_deps).

-include_lib("eunit/include/eunit.hrl").
-include("chef_db.hrl").
-include("chef_types.hrl").


test_all() ->
    [ test_deps_retrieval(Desc, Specs, Expected)
      || {Desc, Specs, Expected} <- cbv_specs() ].

test_deps_retrieval(Description, Specs, Expected) ->
    ct:pal("~s", [Description]),
    itest_cookbook_util:cookbook_setup(Specs),
    {ok, Actual} = chef_sql:fetch_all_cookbook_version_dependencies(itest_util:the_org_id()),
    ?assertEqual(Expected, Actual),
    itest_cookbook_util:cookbook_cleanup(a, a).

cbv_specs() ->
    [
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
    ].

