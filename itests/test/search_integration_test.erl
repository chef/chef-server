-module(search_integration_test).

-include("../src/chef_req.hrl").
-include_lib("eunit/include/eunit.hrl").

search_endpoint_test_() ->
    {setup,
     fun() ->
             ok = chef_req:start_apps(),
             KeyPath = "/tmp/opscode-platform-test/clownco-org-admin.pem",
             ReqConfig = chef_req:make_config("http://localhost:9021",
                                              "clownco-org-admin", KeyPath),
             chef_req:delete_client("clownco", "searchclient01", ReqConfig),
             ClientConfig = chef_req:make_client("clownco", "searchclient01", ReqConfig),
             {ClientConfig, ReqConfig}
     end,
     fun(_) ->
             test_utils:test_cleanup(ignore)
     end,
     fun({ClientConfig, UserConfig}) ->
             [search_tests(ClientConfig),
              search_tests(UserConfig),
              bad_input_search_tests(ClientConfig)]
     end}.

bad_input_search_tests(#req_config{name = Name}=ReqConfig) ->
    Label = " (" ++ Name ++ ")",
    [
     {"bad query" ++ Label,
      fun() ->
              Path = search_path("clownco", "node", "a[b"),
              {ok, Code, _H, Body} = chef_req:request(get, Path, ReqConfig),
              ?assertEqual("400", Code),
              Expect = <<"{\"error\":[\"invalid search query: 'a[b'\"]}">>,
              ?assertEqual(Expect, Body)
      end},

     {"bad start" ++ Label,
      fun() ->
              Path = search_path("clownco", "node", "a:b&start=nooo"),
              {ok, Code, _H, Body} = chef_req:request(get, Path, ReqConfig),
              ?assertEqual("400", Code),
              Expect = <<"{\"error\":[\"invalid 'start' value: 'nooo'\"]}">>,
              ?assertEqual(Expect, Body)
      end},

     {"bad rows" ++ Label,
      fun() ->
              Path = search_path("clownco", "node", "a:b&rows=-20"),
              {ok, Code, _H, Body} = chef_req:request(get, Path, ReqConfig),
              ?assertEqual("400", Code),
              Expect = <<"{\"error\":[\"invalid 'rows' value: '-20'\"]}">>,
              ?assertEqual(Expect, Body)
      end},

     {"missing header" ++ Label,
      %% FIXME: add tests to validate error code and message for all
      %% required authn headers
      fun() ->
              Path = search_path("clownco", "node", "a:b"),
              {ok, Code, _H, Body} = chef_req:missing_header_request("X-Ops-Timestamp",
                                                                     get, Path, [],
                                                                     ReqConfig),
              ?assertEqual("400", Code),
              ErrorMsg = list_to_binary("{\"error\":[\"missing required authentication header(s) "
                                        "'X-Ops-Timestamp'\"]}"),
              ?assertEqual(ErrorMsg, Body)
      end},

     {"stale timestamp request" ++ Label,
      %% FIXME: add tests to validate error code and message for all
      %% required authn headers
      fun() ->
              Path = search_path("clownco", "node", "a:b"),
              {ok, Code, _H, Body} = chef_req:stale_request(get, Path, [],
                                                            ReqConfig),
              ?assertEqual("401", Code),
              ErrorMsg = iolist_to_binary(["{\"error\":[\"Failed to authenticate as ",
                                           Name, ".",
                                           " Synchronize the clock on your host.\"]}"]),
              ?assertEqual(ErrorMsg, Body)
      end}
    ].


search_tests(#req_config{name = Name}=ReqConfig) ->
    Label = " (" ++ Name ++ ")",
    [
     {"empty node search" ++ Label,
      fun() ->
              Path = search_path("clownco", "node", "no_field:not_exist"),
              {ok, "200", _H, Body} = chef_req:request(get, Path, ReqConfig),
              Json = ejson:decode(Body),
              ?assertEqual(0, ej:get({"total"}, Json)),
              ?assertEqual([], ej:get({"rows"}, Json)),
              ?assertEqual(0, ej:get({"start"}, Json))
      end},

     {"data bag does not exist search" ++ Label,
      fun() ->
              Path = search_path("clownco", "no_such_bag", "*:*"),
              {ok, "404", _H, Body} = chef_req:request(get, Path, ReqConfig),
              Expect = <<"{\"error\":[\"I don't know "
                         "how to search for no_such_bag "
                         "data objects.\"]}">>,
              ?assertEqual(Expect, Body)
      end},

     {"bad key" ++ Label,
      fun() ->
              KeyPath = "../test/akey.pem",
              Config = chef_req:make_config("http://localhost:9021", Name, KeyPath),
              Path = search_path("clownco", "node", "no_field:not_exist"),
              {ok, "401", _H, Body} = chef_req:request(get, Path, Config),

              Expect = iolist_to_binary(["{\"error\":[\"Invalid signature ",
                                         "for user or client '",
                                         Name, "'\"]}"]),
              ?assertEqual(Expect, Body)
      end}
    ].

search_path(Org, Type, Query) ->
    "/organizations/" ++ Org ++ "/search/" ++ Type
        ++ "?q=" ++ Query.
