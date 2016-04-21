%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @copyright 2012

-module(chef_data_bag_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").
-include("chef_types.hrl").

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

new_record_test() ->
    OrgId = <<"12345678123456781234567812345678">>,
    AuthzId = <<"00000000000000000000000011111111">>,
    DataBagData = <<"my-data_bag">>,
    DataBag = chef_data_bag:new_record(?API_MIN_VER, OrgId, AuthzId, DataBagData),
    ?assertMatch(#chef_data_bag{}, DataBag),
    ?assertEqual(DataBagData, chef_data_bag:name(DataBag)),
    ?assert(is_binary(chef_data_bag:id(DataBag))),
    ?assertEqual(data_bag, chef_data_bag:type_name(DataBag)).

ejson_for_indexing_test() ->
    %% data bags are not indexed. For compatility reasons, we still need to
    %% define the ejson_for_indexing callback, but we want the result to be
    %% empty
    Bag = #chef_data_bag{name = <<"the_bag_name">>},
    ?assertError(not_indexed, chef_object:ejson_for_indexing(Bag, <<"the_bag_name">>)).

id_test() ->
    ?assertEqual(<<"1">>, chef_object:id(#chef_data_bag{id = <<"1">>})).

name_test() ->
    ?assertEqual(<<"a_name">>, chef_object:name(#chef_data_bag{name =  <<"a_name">>})).

type_name_test() ->
    ?assertEqual(data_bag, chef_object:type_name(#chef_data_bag{})).
