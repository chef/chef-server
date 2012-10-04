%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2012

-module(chef_data_bag_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").

validate_data_bag_test_() ->
    [
     {Message,
      fun() ->
              Actual = fun() ->
                               chef_data_bag:validate_data_bag(EJson)
                       end,
              case ShouldSucceed of
                  true ->
                      ?assertEqual({ok, EJson}, Actual());
                  false ->
                      ?assertThrow(#ej_invalid{}, Actual())
              end
      end
     } || {Message, EJson, ShouldSucceed} <-
              [
               {"Normal data bag succeeds",
                {[{<<"name">>, <<"foo">>},
                  {<<"chef_type">>, <<"data_bag">>},
                  {<<"json_class">>, <<"Chef::DataBag">>}]},
                true},
               {"Data bag without 'json_class' succeeds",
                {[{<<"name">>, <<"foo">>},
                  {<<"chef_type">>, <<"data_bag">>}]},
                true},
               {"Data bag with 'wrong' 'json_class' succeeds",
                {[{<<"name">>, <<"foo">>},
                  {<<"chef_type">>, <<"data_bag">>},
                  {<<"json_class">>, <<"Chef::Node">>}]},
                true},
               {"Data bag without 'chef_type' succeds",
                {[{<<"name">>, <<"foo">>},
                  {<<"json_class">>, <<"Chef::DataBag">>}]},
                true},
               {"Data bag with 'wrong' 'chef_type' succeeds",
                {[{<<"name">>, <<"foo">>},
                  {<<"chef_type">>, <<"node">>},
                  {<<"json_class">>, <<"Chef::DataBag">>}]},
                true},
               {"Data bag with just a name succeeds",
                {[{<<"name">>, <<"foo">>}]},
                true},
               {"Data bag with extra fields succeeds",
                {[{<<"name">>, <<"foo">>},
                  {<<"blahblah">>,<<"blahblahblah">>}]},
                true}
              ]

              ++ [{<<"Data bag with bad name '", Name/binary, "' fails">>,
                   {[{<<"name">>, Name}]},
                   false}
                  || Name <- [<<"$$$">>,
                              <<"foo@#)">>]]

              ++ [{<<"Data bag with good name '", Name/binary, "' succeeds">>,
                   {[{<<"name">>, Name}]},
                   true}
                  || Name <- [<<"foo">>,
                              <<"foo.bar">>,
                              <<"foo-bar">>,
                              <<"foo:bar">>,
                              <<"foo_bar">>,
                              <<"foo-123-bar">>,
                              <<"FOO">>]]
    ].
