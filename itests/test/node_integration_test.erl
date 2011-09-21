-module(node_integration_test).

-include("../src/chef_req.hrl").
-include_lib("eunit/include/eunit.hrl").

basic_node_create_test_() ->
    {setup,
     fun() ->
             ok = chef_req:start_apps(),
             KeyPath = "/tmp/opscode-platform-test/clownco-org-admin.pem",
             ReqConfig = chef_req:make_config("http://localhost",
                                              "clownco-org-admin", KeyPath),
             {_, ClientConfig} = chef_req:make_client("clownco", "client01", ReqConfig),
             {ReqConfig, ClientConfig}
     end,
     fun({_, #req_config{name = Name}=ReqConfig}) ->
             chef_req:delete_client("clownco", Name, ReqConfig)
     end,
     fun({UserConfig, ClientConfig}) ->
             [basic_node_tests_for_config(UserConfig),
              basic_node_tests_for_config(ClientConfig)]
     end}.

basic_node_tests_for_config(#req_config{name = Name}=ReqConfig) ->
    Label = " (" ++ Name ++ ")",
    {NodeName, NodeJson} = sample_node(),
    [
     {"create a new node" ++ Label,
      fun() ->
              Path = "/organizations/clownco/nodes",
              {ok, Code, _H, Body} = chef_req:request(post, Path, NodeJson, ReqConfig),
              ?assertEqual("201", Code),
              NodeUrl = <<"http://localhost/organizations/clownco/nodes/", NodeName/binary>>,
              Expect = ejson:encode({[{<<"uri">>, NodeUrl}]}),
              ?assertEqual(Expect, Body)
      end},

     {"conflict when node name already exists" ++ Label,
      fun() ->
              Path = "/organizations/clownco/nodes",
              {ok, Code, _H, Body} = chef_req:request(post, Path, NodeJson, ReqConfig),
              ?assertEqual("409", Code),
              ?assertEqual(<<"{\"error\":[\"Node already exists\"]}">>, Body)
      end},

     {"org 'no-such-org' does not exist" ++ Label,
      fun() ->
              Path = "/organizations/no-such-org/nodes",
              {ok, Code, _H, Body} = chef_req:request(post, Path, NodeJson, ReqConfig),
              ?assertEqual("404", Code),
              ?assertEqual(<<"{\"error\":[\"organization no-such-org does not exist.\"]}">>,
                           Body)
      end},

     {"POST of invalid JSON is a 400" ++ Label,
      fun() ->
              Path = "/organizations/clownco/nodes",
              InvalidJson = <<"{not:json}">>,
              {ok, Code, _H, _Body} = chef_req:request(post, Path, InvalidJson, ReqConfig),
              ?assertEqual("400", Code)
      end}
    ].

sample_node() ->
    sample_node(make_node_name(<<"node-">>)).

sample_node(Name) ->
    {Name,
     <<"{\"normal\":{\"is_anyone\":\"no\"},\"name\":\"", Name/binary,
       "\",\"override\":{},"
       "\"default\":{},\"json_class\":\"Chef::Node\",\"automatic\":{},"
       "\"chef_environment\":\"_default\",\"run_list\":[],\"chef_type\":\"node\"}">>}.

make_node_name(Prefix) when is_binary(Prefix) ->
    Rand = bin_to_hex(crypto:rand_bytes(3)),
    <<Prefix/binary, Rand/binary>>;
make_node_name(Prefix) when is_list(Prefix) ->
    make_node_name(list_to_binary(Prefix)).

bin_to_hex(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [X])
                      || X <- binary_to_list(Bin)]).
