-module(search_integration_test).

-include("../src/chef_req.hrl").
-include_lib("eunit/include/eunit.hrl").

search_as_client_test_() ->
    {setup,
     fun() ->
             ok = chef_req:start_apps(),
             KeyPath = "/tmp/opscode-platform-test/clownco-org-admin.pem",
             ReqConfig = chef_req:make_config("http://localhost",
                                              "clownco-org-admin", KeyPath),
             {_, ClientConfig} = make_client("clownco", "client01", ReqConfig),
             ClientConfig
     end,
     fun(#req_config{name = Name}=ReqConfig) ->
             delete_client("clownco", Name, ReqConfig)
     end,
     fun(ReqConfig) ->
             [
              {"empty node search as a client",
               fun() ->
                       Path = search_path("clownco", "node",
                                          "no_field:not_exist"),
                       {ok, "200", _H, Body} = chef_req:request(get, Path,
                                                                ReqConfig),
                       Json = ejson:decode(Body),
                       ?assertEqual(0, ej:get({"total"}, Json)),
                       ?assertEqual([], ej:get({"rows"}, Json)),
                       ?assertEqual(0, ej:get({"start"}, Json))
               end},

              {"data bag does not exist search as a client",
               fun() ->
                       Path = search_path("clownco", "no_such_bag", "*:*"),
                       {ok, "404", _H, Body} = chef_req:request(get, Path,
                                                                ReqConfig),
                       Expect = "{\"error\":[\"I don't know "
                           "how to search for no_such_bag "
                           "data objects.\"]}",
                       ?assertEqual(Expect, Body)
               end},

              {"bad key client",
               fun() ->
                       Name = ReqConfig#req_config.name,
                       KeyPath = "../test/akey.pem",
                       Config = chef_req:make_config("http://localhost",
                                                     Name, KeyPath),
                       Path = search_path("clownco", "node",
                                          "no_field:not_exist"),
                       {ok, "401", _H, Body} = chef_req:request(get, Path, Config),
                       ?assertEqual("{\"error\":[\"'client01' not associated with "
                                    "organization 'clownco'\"]}", Body)
               end}
             ]
     end}.

search_as_user_test_() ->
    {setup,
     fun() ->
             ok = chef_req:start_apps(),
             KeyPath = "/tmp/opscode-platform-test/clownco-org-admin.pem",
             chef_req:make_config("http://localhost",
                                  "clownco-org-admin", KeyPath)
     end,
     fun(_X) ->
             stopping
     end,
     fun(ReqConfig) ->
             [
              {"empty node search as clownco-org-admin",
                fun() ->
                        Path = search_path("clownco", "node",
                                           "no_field:not_exist"),
                        {ok, "200", _H, Body} = chef_req:request(get, Path,
                                                                 ReqConfig),
                        Json = ejson:decode(Body),
                        ?assertEqual(0, ej:get({"total"}, Json)),
                        ?assertEqual([], ej:get({"rows"}, Json)),
                        ?assertEqual(0, ej:get({"start"}, Json))
                end},

               {"data bag does not exist search as clownco-org-admin",
                fun() ->
                        Path = search_path("clownco", "no_such_bag", "*:*"),
                        {ok, "404", _H, Body} = chef_req:request(get, Path,
                                                                 ReqConfig),
                        Expect = "{\"error\":[\"I don't know "
                            "how to search for no_such_bag "
                            "data objects.\"]}",
                        ?assertEqual(Expect, Body)
                end},

               {"unknown user or client",
                fun() ->
                        KeyPath = "../test/akey.pem",
                        Config = chef_req:make_config("http://localhost",
                                                      "no-such-user", KeyPath),
                        Path = search_path("clownco", "node",
                                           "no_field:not_exist"),
                        {ok, "401", _H, Body} = chef_req:request(get, Path, Config),
                        ?assertEqual("{\"error\":[\"Failed to authenticate as "
                                     "'no-such-user'. Ensure that your node_name "
                                     "and client key are correct.\"]}",
                                     Body)
                end},

               {"bad key",
                fun() ->
                        KeyPath = "../test/akey.pem",
                        Config = chef_req:make_config("http://localhost",
                                                      "clownco-org-admin", KeyPath),
                        Path = search_path("clownco", "node",
                                           "no_field:not_exist"),
                        {ok, "401", _H, Body} = chef_req:request(get, Path, Config),
                        ?assertEqual("{\"error\":[\"'clownco-org-admin' not "
                                     "associated with organization 'clownco'\"]}", Body)
                end}
             ]
     end
    }.


search_path(Org, Type, Query) ->
    "/organizations/" ++ Org ++ "/search/" ++ Type
        ++ "?q=" ++ Query.

make_client(Org, ClientName, Config) ->
    Path = "/organizations/" ++ Org ++ "/clients",
    ReqBody = iolist_to_binary([<<"{\"name\":\"">>, ClientName, <<"\"}">>]),
    {ok, "201", _H, Body} = chef_req:request(post, Path, ReqBody, Config),
    Client = ejson:decode(Body),
    ClientConfig = chef_req:clone_config(Config, ClientName,
                                         ej:get({<<"private_key">>}, Client)),
    {Client, ClientConfig}.

delete_client(Org, ClientName, Config) ->
    Path = "/organizations/" ++ Org ++ "/clients/" ++ ClientName,
    {ok, Code, _H, _Body} = chef_req:request(delete, Path, Config),
    Code.
