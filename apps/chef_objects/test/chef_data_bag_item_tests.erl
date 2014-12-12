%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@lambda.local>
%% @copyright 2012

-module(chef_data_bag_item_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").
-include("../../include/chef_types.hrl").

validate_data_bag_item_test_() ->
    [
     {Message,
      fun() ->
              Actual = fun() ->
                               chef_data_bag_item:validate(EJson)
                       end,
              case ShouldSucceed of
                  true ->
                      ?assertEqual({ok, EJson}, Actual());
                  false ->
                      ?assertThrow(#ej_invalid{}, Actual())
              end
      end
     } || {Message, EJson, ShouldSucceed} <-
              [{<<"Data bag item with bad id '", Id/binary, "' fails">>,
                {[{<<"id">>, Id},
                  {<<"foo">>, <<"bar">>},
                  {<<"baz">>, 123}]},
                false}
               || Id <- [<<"$$$">>,
                         <<"foo@#)">>]]

              ++ [{<<"Data bag item with good id '", Id/binary, "' succeeds">>,
                   {[{<<"id">>, Id},
                     {<<"foo">>, <<"bar">>},
                     {<<"baz">>, 123}]},
                   true}
                  || Id <- [<<"foo">>,
                            <<"foo.bar">>,
                              <<"foo-bar">>,
                            <<"foo:bar">>,
                            <<"foo_bar">>,
                            <<"foo-123-bar">>,
                            <<"FOO">>]]
    ].

new_record_test() ->
    OrgId = <<"12345678123456781234567812345678">>,
    Data = {[{<<"id">>, <<"my-item">>}, {<<"alpha">>, <<"bravo">>}]},
    Item = chef_data_bag_item:new_record(OrgId, no_authz_id, {<<"my-bag">>, Data}),
    ?assertMatch(#chef_data_bag_item{}, Item),
    %% TODO: validate more fields?
    ?assertEqual({<<"my-bag">>,<<"my-item">>}, chef_data_bag_item:name(Item)).

ejson_for_indexing_test() ->
    RawItem = {[{<<"id">>, <<"the_item_name">>},
                {<<"a_key">>, <<"a_value">>}]},
    Item = #chef_data_bag_item{data_bag_name = <<"the_bag_name">>,
                               item_name = <<"the_item_name">>},
    Expected = {[{<<"id">>, <<"the_item_name">>},
                 {<<"a_key">>, <<"a_value">>},
                 {<<"chef_type">>, <<"data_bag_item">>},
                 {<<"data_bag">>, <<"the_bag_name">>}]},
    Got = chef_object:ejson_for_indexing(Item, RawItem),
    ?assertEqual(Expected, Got).

ejson_for_indexing_api_conformance_test() ->
    RawItem = {[{<<"id">>, <<"the_item_name">>},
                {<<"a_key">>, <<"a_value">>}]},
    Item = #chef_data_bag_item{data_bag_name = <<"the_bag_name">>,
                               item_name = <<"the_item_name">>},
    Expected = {[{<<"id">>, <<"the_item_name">>},
                 {<<"a_key">>, <<"a_value">>},
                 {<<"chef_type">>, <<"data_bag_item">>},
                 {<<"data_bag">>, <<"the_bag_name">>}]},
    Got = chef_object:ejson_for_indexing(Item, {<<"the_bag_name">>, RawItem}),
    ?assertEqual(Expected, Got).

update_from_ejson_test_() ->
    RawItem = {[{<<"id">>, <<"the_item_name">>},
                {<<"a_key">>, <<"a_value">>}]},
    Item = #chef_data_bag_item{data_bag_name = <<"the_bag_name">>,
                               item_name = <<"old_item_name">>},
    [{"chef_data_bag_item fields are set from json for all dbs",
      [
       {atom_to_list(DbType),
        fun() ->
                Item1 = chef_object:update_from_ejson(Item, RawItem),
                GotData = Item1#chef_data_bag_item.serialized_object,
                GotEjson = jiffy:decode(chef_db_compression:decompress(GotData)),
                ?assertEqual(<<"the_item_name">>, Item1#chef_data_bag_item.item_name),
                ?assertEqual(RawItem, GotEjson)
        end} || DbType <- [mysql, pgsql] ]}
    ].

id_test() ->
    ?assertEqual(<<"1">>, chef_object:id(#chef_data_bag_item{id = <<"1">>})).

name_test() ->
    ?assertEqual({<<"bag name">>, <<"item name">>},
                 chef_object:name(#chef_data_bag_item{data_bag_name = <<"bag name">>,
                                                      item_name =  <<"item name">>})).

type_name_test() ->
    ?assertEqual(data_bag_item, chef_object:type_name(#chef_data_bag_item{})).
