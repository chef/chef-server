-module(chef_index_expand_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ROLE,
        {[{<<"name">>,<<"web_role">>},
          {<<"description">>,<<"something something">>},
          {<<"json_class">>,<<"Chef::Role">>},
          {<<"chef_type">>,<<"role">>},
          {<<"default_attributes">>,
           {[{<<"test1">>,1},{<<"test2">>,<<"2">>}]}},
          {<<"override_attributes">>,
           {[{<<"test1">>,8},{<<"rideover">>,<<"10-4">>}]}},
          {<<"run_list">>,[<<"apache2">>,<<"php">>]},
          {<<"env_run_lists">>,
           {[{<<"prod">>,[<<"nginx">>]}]}}]}).

-define(DB_ITEM,
        {[
          {<<"data_bag">>, <<"sport-balls">>},
          {<<"chef_type">>, <<"data_bag_item">>},
          {<<"soccerballs">>, 2},
          {<<"baseballs">>, 4},
          {<<"id">>, <<"balls">>},
          {<<"footballs">>, {[
                              {<<"round">>, 2},
                              {<<"egg">>, 18}
                             ]}}
         ]}).

flatten_non_recursive_type_test() ->
    Input = {[{<<"a_null">>, null},
              {<<"a_true">>, true},
              {<<"a_false">>, false},
              {<<"a_int">>, 2},
              {<<"a_float">>, 1.23},
              {<<"a_string">>, <<"hello">>},
              {<<"q1">>, <<"with \"quotes\"">>},
              {<<"q2 \"2\"">>, <<"with quotes in key">>}
             ]},
    Expanded = chef_index_expand:flatten(solr, Input),
    %% Expected final result when flattened should be space separated
    %% as below. Formatting of floats is tricky. We should investigate
    %% what the Ruby code does.
    Expect = <<"a_false__=__false "
               "a_float__=__1.23 "
               "a_int__=__2 "
               "a_null__=__ "
               "a_string__=__hello "
               "a_true__=__true "
               "q1__=__with &quot;quotes&quot; "
               "q2 &quot;2&quot;__=__with quotes in key ">>,
    ?assertEqual(Expect, iolist_to_binary(Expanded)).

flatten_lists_test() ->
    Input = {[{<<"k1">>, [null, true, false,
                          <<"a">>, 0, 1.123,
                          [<<"b">>, 2], <<"c">>]}]},
    Expanded = chef_index_expand:flatten(solr, Input),
    Expect = <<"k1__=__ "
               "k1__=__0 k1__=__1.123 "
               "k1__=__2 k1__=__a k1__=__b k1__=__c "
               "k1__=__false k1__=__true ">>,
    ?assertEqual(Expect, iolist_to_binary(Expanded)).

example_test() ->
    {ok, Bin} = chef_index_test_utils:read_file("sample_node.json"),
    Node = jiffy:decode(Bin),
    Expanded = chef_index_expand:flatten(solr, Node),
    file:write_file(chef_index_test_utils:filename("sample.out"), Expanded),
    ok.

example_nested_test() ->
    Input = {[
              {<<"k1">>, [<<"a1">>, <<"a2">>, [<<"aa1">>, <<"aa2">>]]},
              {<<"k2">>, 5},
              {<<"k3">>, {[{<<"kk1">>, <<"h">>},
                           {<<"kk2">>, [1, 2]},
                           {<<"kk3">>, {[
                                         {<<"kkk1">>, true},
                                         {<<"kkk2">>, <<"i<&>">>},
                                         {<<"kk&k3">>, [<<"j">>,
                                                        {[
                                                          {<<"lkk<>k1">>, 1},
                                                          {<<"lkkk2">>, 2}
                                                         ]}]}
                                        ]}}
                          ]}}]},
    Expanded = chef_index_expand:flatten(solr, Input),
    file:write_file("../test/example_nested.out", Expanded),
    ok.

example_flat_test() ->
    Input = {[{<<"a_null">>, null},
              {<<"a_true">>, true},
              {<<"a_false">>, false},
              {<<"a_int">>, 2},
              {<<"a_float">>, 1.23},
              {<<"a_string">>, <<"hello, \"you\"">>}
             ]},
    Expanded = chef_index_expand:flatten(solr, Input),
    file:write_file("../test/example_flat.out", Expanded),
    ok.

flatten_nested_test() ->
    Input = {[
              {<<"k1">>, [<<"a1">>, <<"a2">>, [<<"aa1">>, <<"aa2">>]]},
              {<<"k2">>, 5},
              {<<"k3">>, {[{<<"kk1">>, <<"h">>},
                           {<<"kk2">>, [1, 2]},
                           {<<"kk3">>, {[
                                         {<<"kkk1">>, true},
                                         {<<"kkk2">>, <<"i<&>">>},
                                         {<<"kk&k3">>, [<<"j">>,
                                                        {[
                                                          {<<"lkk<>k1">>, 1},
                                                          {<<"lkkk2">>, 2}
                                                         ]}]}
                                        ]}}
                          ]}}]},
    Expect =  <<"k1__=__a1 "
                "k1__=__a2 "
                "k1__=__aa1 "
                "k1__=__aa2 "
                "k2__=__5 "
                "k3__=__kk1 "
                "k3__=__kk2 "
                "k3__=__kk3 "
                "k3_kk1__=__h "
                "k3_kk2__=__1 "
                "k3_kk2__=__2 "
                "k3_kk3__=__kk&amp;k3 "
                "k3_kk3__=__kkk1 "
                "k3_kk3__=__kkk2 "
                "k3_kk3_kk&amp;k3__=__j "
                "k3_kk3_kk&amp;k3__=__lkk&lt;&gt;k1 "
                "k3_kk3_kk&amp;k3__=__lkkk2 "
                "k3_kk3_kk&amp;k3_lkk&lt;&gt;k1__=__1 "
                "k3_kk3_kk&amp;k3_lkkk2__=__2 "
                "k3_kk3_kkk1__=__true "
                "k3_kk3_kkk2__=__i&lt;&amp;&gt; "
                "kk&amp;k3__=__j "
                "kk&amp;k3__=__lkk&lt;&gt;k1 "
                "kk&amp;k3__=__lkkk2 "
                "kk1__=__h "
                "kk2__=__1 "
                "kk2__=__2 "
                "kk3__=__kk&amp;k3 "
                "kk3__=__kkk1 "
                "kk3__=__kkk2 "
                "kkk1__=__true "
                "kkk2__=__i&lt;&amp;&gt; "
                "lkk&lt;&gt;k1__=__1 "
                "lkkk2__=__2 ">>,
    ?assertEqual(Expect, iolist_to_binary(chef_index_expand:flatten(solr, Input))).

flatten_and_xml_escape_test() ->
    Input = {[
              {<<"A & W">>, <<"The \"question\" is < > !&">>}
             ]},
    Expect = <<"A &amp; W__=__The &quot;question&quot; is &lt; &gt; !&amp; ">>,
    ?assertEqual(Expect, iolist_to_binary(chef_index_expand:flatten(solr, Input))).

doc_construction_test_() ->
    MinItem = {[{<<"key1">>, <<"value1">>},
                {<<"key2">>, <<"value2">>}]},
    {setup,
     fun() ->
             meck:new(chef_index_http, []),
             application:set_env(chef_index, search_provider, solr)
     end,
     fun(_) ->
             meck:unload()
     end,
     [{"happy path add",
       fun() ->
               D = chef_index_expand:doc_for_index(role, <<"abc123">>, "dbdb1212", MinItem),
               Expect = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                          "<update>"
                          "<add><doc>"
                          "<field name=\"X_CHEF_id_CHEF_X\">abc123</field>"
                          "<field name=\"X_CHEF_database_CHEF_X\">chef_dbdb1212</field>"
                          "<field name=\"X_CHEF_type_CHEF_X\">role</field>"
                          "<field name=\"content\">"
                          "X_CHEF_database_CHEF_X__=__chef_dbdb1212 "
                          "X_CHEF_id_CHEF_X__=__abc123 "
                          "X_CHEF_type_CHEF_X__=__role "
                          "key1__=__value1 key2__=__value2 </field>"
                          "</doc></add></update>">>,
               meck:expect(chef_index_http, post,
                           fun("/update", Doc) ->
                                   ?assertEqual(Expect, Doc),
                                   ok
                           end),
               ?assertEqual(ok,chef_index_expand:send_item(D))
       end},

      {"happy path delete",
       fun() ->
               D = chef_index_expand:doc_for_delete(role, <<"abc123">>, "dbdb1212"),
               Expect = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                          "<update><delete><id>abc123</id></delete></update>">>,
               meck:expect(chef_index_http, post,
                           fun("/update", Doc) ->
                                   ?assertEqual(Expect, Doc),
                                   ok
                           end),
               ?assertEqual(ok, chef_index_expand:send_delete(D))
       end},

      {"special handling for data bag items",
       fun() ->
               D = chef_index_expand:doc_for_index(<<"sport-balls">>, <<"abc123">>, "dbdb1212", ?DB_ITEM),
               Expect = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                          "<update>"
                          "<add>"
                          "<doc>"
                          "<field name=\"X_CHEF_id_CHEF_X\">abc123</field>"
                          "<field name=\"X_CHEF_database_CHEF_X\">chef_dbdb1212</field>"
                          "<field name=\"X_CHEF_type_CHEF_X\">data_bag_item</field>"
                          "<field name=\"data_bag\">sport-balls</field>"
                          "<field name=\"content\">"
                          "X_CHEF_database_CHEF_X__=__chef_dbdb1212 "
                          "X_CHEF_id_CHEF_X__=__abc123 "
                          "X_CHEF_type_CHEF_X__=__data_bag_item "
                          "baseballs__=__4 "
                          "chef_type__=__data_bag_item "
                          "data_bag__=__sport-balls "
                          "egg__=__18 "
                          "footballs__=__egg "
                          "footballs__=__round "
                          "footballs_egg__=__18 "
                          "footballs_round__=__2 "
                          "id__=__balls "
                          "round__=__2 "
                          "soccerballs__=__2 "
                          "</field>"
                          "</doc>"
                          "</add>"
                          "</update>">>,
               meck:expect(chef_index_http, post,
                           fun("/update", Doc) ->
                                   ?assertEqual(Expect, Doc),
                                   ok
                           end),
               ?assertEqual(ok, chef_index_expand:send_item(D))
       end},

      {"error from chef_index_http",
       fun() ->
               Doc = chef_index_expand:doc_for_index(role, <<"abc123">>, "dbdb1212", MinItem),
               meck:expect(chef_index_http, post,
                           fun("/update", _Doc) ->
                                   {error, {ok, "500", [], <<"oh no">>}}
                           end),
               ?assertEqual({error, {ok, "500", [], <<"oh no">>}}, chef_index:update(Doc))
       end}
     ]}.

solr_api_test_() ->
    MinItem = {[{<<"key1">>, <<"value1">>},
                {<<"key2">>, <<"value-2">>}]},
    {foreach,
     fun() ->
             application:set_env(chef_index, search_provider, solr),
             meck:new(chef_index_http, [])

     end,
     fun(_) ->
             meck:unload()
     end,
     [fun(_) ->
              [{"send_item",
                fun() ->
                        chef_index_test_utils:set_provider(solr),
                        Expect = send_item_xml_expect(),
                        meck:expect(chef_index_http, post,
                                    fun("/update", Doc) ->
                                            ?assertEqual(Expect, Doc),
                                            ok
                                    end),
                        AddDoc = chef_index_expand:doc_for_index(role, <<"a1">>, <<"db1">>, MinItem),
                        ?assertEqual(ok, chef_index_expand:send_item(AddDoc))
                end},
               {"send_delete",
                fun() ->
                        chef_index_test_utils:set_provider(solr),
                        Expect = send_delete_xml_expect(),
                        meck:expect(chef_index_http, post,
                                    fun("/update", Doc) ->
                                            ?assertEqual(Expect, Doc),
                                            ok
                                    end),
                        DelDoc = chef_index_expand:doc_for_delete(role, <<"a5">>, <<"db3">>),
                        ?assertEqual(ok, chef_index_expand:send_delete(DelDoc))
                end
               }
              ]
     end]
    }.

es_api_test_() ->
    MinItem = {[{<<"key1">>, <<"value1">>},
                {<<"key2">>, <<"value-2">>}]},
    JsonContentType = [{"Content-Type", "application/json"}],
    {foreach,
     fun() ->
             application:set_env(chef_index, search_provider, elasticsearch)
     end,
     fun(_) ->
             meck:unload()
     end,
     [fun(_) ->
              [{"send_item",
                fun() ->
                        chef_index_test_utils:set_provider(elasticsearch),
                        Expect = es_send_item_json_expect(),
                        meck:expect(chef_index_http, post,
                                    fun("/_bulk", Doc, Headers) ->
                                            ?assertEqual(JsonContentType, Headers),
                                            ?assertEqual(Expect, Doc),
                                            ok
                                    end),
                        AddDoc = chef_index_expand:doc_for_index(role, <<"a1">>, <<"db1">>, MinItem),
                        ?assertEqual(ok, chef_index_expand:send_item(AddDoc))
                end},
               {"send_delete",
                fun() ->
                        chef_index_test_utils:set_provider(elasticsearch),
                        Expect = es_send_delete_json_expect(),
                        meck:expect(chef_index_http, post,
                                    fun("/_bulk", Doc, Headers) ->
                                            ?assertEqual(JsonContentType, Headers),
                                            ?assertEqual(Expect, Doc),
                                            ok
                                    end),
                        DelDoc = chef_index_expand:doc_for_delete(role, <<"a5">>, <<"db3">>),
                        ?assertEqual(ok, chef_index_expand:send_delete(DelDoc))
                end
               }
              ]
     end]
    }.

send_delete_xml_expect() ->
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<update>"
      "<delete>"
      "<id>a5</id>"
      "</delete>"
      "</update>">>.

send_item_xml_expect() ->
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<update>"
      "<add>"
      "<doc>"
      "<field name=\"X_CHEF_id_CHEF_X\">a1</field>"
      "<field name=\"X_CHEF_database_CHEF_X\">chef_db1</field>"
      "<field name=\"X_CHEF_type_CHEF_X\">role</field>"
      "<field name=\"content\">"
      "X_CHEF_database_CHEF_X__=__chef_db1 "
      "X_CHEF_id_CHEF_X__=__a1 "
      "X_CHEF_type_CHEF_X__=__role "
      "key1__=__value1 key2__=__value-2 </field>"
      "</doc>"
      "</add>"
      "</update>">>.

es_send_delete_json_expect() ->
    <<"{\"delete\":{\"_index\":\"chef\",\"_type\":\"object\",\"_id\":\"a5\" }}\n">>.

es_send_item_json_expect() ->
    <<"{\"index\":{\"_index\":\"chef\",\"_type\":\"object\",\"_id\":\"a1\"}}\n"
      "{\"content\":\"X_CHEF_database_CHEF_X__=__chef_db1 X_CHEF_id_CHEF_X__=__a1 X_CHEF_type_CHEF_X__=__role key1__=__value1 key2__=__value-2 \","
      "\"X_CHEF_id_CHEF_X\":\"a1\",\"X_CHEF_database_CHEF_X\":\"chef_db1\",\"X_CHEF_type_CHEF_X\":\"role\"}\n">>.
