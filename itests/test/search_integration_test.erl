-module(search_integration_test).

-include("../src/chef_req.hrl").
-include_lib("eunit/include/eunit.hrl").

search_test_() ->
    {foreach,
     % setup
     fun() ->
             ok = chef_req:start_apps(),
             KeyPath = "/tmp/opscode-platform-test/clownco-org-admin.pem",
             chef_req:make_config("http://localhost",
                                  "clownco-org-admin", KeyPath)
     end,
     fun(_X) ->
             stopping
     end,
     [
       fun(ReqConfig) ->
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
               end}
       end,

       fun(ReqConfig) ->
               {"data bag does not exist search as clownco-org-admin",
               fun() ->
                       Path = search_path("clownco", "no_such_bag", "*:*"),
                       {ok, "404", _H, Body} = chef_req:request(get, Path,
                                                                ReqConfig),
                       Expect = "{\"error\":[\"I don't know "
                           "how to search for no_such_bag "
                           "data objects.\"]}",
                       ?assertEqual(Expect, Body)
               end}
       end,

      {"unknown user",
       fun() ->
               KeyPath = "../test/akey.pem",
               Config = chef_req:make_config("http://localhost",
                                             "no-such-user", KeyPath),
                       Path = search_path("clownco", "node",
                                          "no_field:not_exist"),
               {ok, "401", _H, Body} = chef_req:request(get, Path, Config)
               %% TODO: validate error message in Body
       end},

      {"bad key",
       fun() ->
               KeyPath = "../test/akey.pem",
               Config = chef_req:make_config("http://localhost",
                                             "clownco-org-admin", KeyPath),
                       Path = search_path("clownco", "node",
                                          "no_field:not_exist"),
               {ok, "401", _H, _Body} = chef_req:request(get, Path, Config)
               %% TODO: validate error message in Boyd
       end}

     ]}.


search_path(Org, Type, Query) ->
    "/organizations/" ++ Org ++ "/search/" ++ Type
        ++ "?q=" ++ Query.
