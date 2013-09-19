%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@lambda.local>
%% @copyright 2012

-module(chef_data_bag_item_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").
-include("chef_types.hrl").

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
