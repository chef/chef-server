-module(node_integration_test).

-include("../src/chef_req.hrl").
-include_lib("eunit/include/eunit.hrl").

node_endpoint_test_() ->
    {timeout, 60, {inorder, {setup,
     fun() ->
             ok = chef_req:start_apps(),
             db_tool:connect(),
             ok = db_tool:truncate_nodes_table(),
             KeyPath = "/tmp/opscode-platform-test/clownco-org-admin.pem",
             ReqConfig = chef_req:make_config("http://localhost",
                                              "clownco-org-admin", KeyPath),

             ok = chef_req:delete_client("clownco", "client01", ReqConfig),
             ok = chef_req:delete_client("clownco", "client02", ReqConfig),
             ClientConfig = chef_req:make_client("clownco", "client01", ReqConfig),
             WeakClientConfig = chef_req:make_client("clownco", "client02", ReqConfig),
             chef_req:remove_client_from_group("clownco", "client02", "clients", ReqConfig),
             {ReqConfig, ClientConfig, WeakClientConfig}
     end,
     fun({_, _, _}) ->
             test_utils:nuke_nodes_from_solr(),
             test_utils:test_cleanup(ignore)
     end,
     fun({UserConfig, ClientConfig, WeakClientConfig}) ->
             [
              basic_env_node_list_tests(UserConfig),
              named_node_permissions(UserConfig, ClientConfig, WeakClientConfig),
              basic_named_node_ops(UserConfig),
              basic_named_node_ops(ClientConfig),
              basic_node_create_tests_for_config(UserConfig),
              basic_node_create_tests_for_config(ClientConfig),
              node_permissions_tests(UserConfig, WeakClientConfig),
              basic_node_list_tests_for_config(UserConfig),
              basic_node_list_tests_for_config(ClientConfig)
             ]
     end}}}.

named_node_permissions(UserConfig, ClientConfig, WeakClientConfig) ->
    Path = "/organizations/clownco/nodes/",
    {setup,
     fun() ->
             {UserNode, _UserNodeUrl} = create_node("clownco", UserConfig),
             {ClientNode, _ClientNodeUrl} = create_node("clownco", ClientConfig),
             {UserNode, ClientNode}
     end,
     fun(_) -> cleanup end,
     fun({UserNode, ClientNode}) ->
             [{"admin user can read client created node",
               fun() ->
                       NodePath = Path ++ ClientNode,
                       ?assertEqual("200", code_for_request(get, NodePath, UserConfig))
               end},

              {"client can read user created node",
               fun() ->
                       NodePath = Path ++ UserNode,
                       ?assertEqual("200", code_for_request(get, NodePath, ClientConfig))
               end},

              {"weak client cannot read user created node",
               fun() ->
                       NodePath = Path ++ UserNode,
                       ?assertEqual("403", code_for_request(get, NodePath, WeakClientConfig))
               end},

              {"weak client cannot read client created node",
               fun() ->
                       NodePath = Path ++ ClientNode,
                       ?assertEqual("403", code_for_request(get, NodePath, WeakClientConfig))
               end},

              {"weak client cannot update client created node",
               fun() ->
                       NodePath = Path ++ ClientNode,
                       {_Name, SampleNode} = sample_node(ClientNode),
                       ?assertEqual("403", code_for_request(put, NodePath, SampleNode, WeakClientConfig))
               end},

              {"weak client cannot delete client created node",
               fun() ->
                       NodePath = Path ++ ClientNode,
                       ?assertEqual("403", code_for_request(delete, NodePath, WeakClientConfig))
               end},

              %% FIXME: probably should put these in their own block with
              %% foreach or setup or something since we depend on creation and
              %% after the deletion nobody else can use it.
              {"admin user can update client created node",
               fun() ->
                       NodePath = Path ++ ClientNode,
                       {_Name, SampleNode} = sample_node(ClientNode),
                       ?assertEqual("200", code_for_request(put, NodePath, SampleNode, UserConfig))
               end},

              {"admin user can delete client created node",
               fun() ->
                       NodePath = Path ++ ClientNode,
                       ?assertEqual("200", code_for_request(delete, NodePath, UserConfig))
               end}
              %% TODO:
              %% Moar testing
              %% verify admin user has all perms on a client created node
              %% clarify what perms a client should have on an admin created node and test them.
             ]
     end}.
    
basic_named_node_ops(#req_config{name = Name}=ReqConfig) ->
    Label = " (" ++ Name ++ ")",
    Path = "/organizations/clownco/nodes/",
    {setup,
     fun() ->
             {AName, _AUrl} = create_node("clownco", ReqConfig),
             AName
     end,
     fun(_) -> cleanup end,
     fun(AName) ->
             [
              {"GET a non-existing node" ++ Label,
               fun() ->
                       NoName = "a-node-that-does-not-exist-xxx",
                       NoNodePath = Path ++ NoName,
                       {ok, Code, _H, Body} = chef_req:request(get, NoNodePath, ReqConfig),
                       ?assertEqual("404", Code),
                       Expect = iolist_to_binary(["{\"error\":[\"node '", NoName,
                                                  "' not found\"]}"]),
                       ?assertEqual(Expect, Body)
               end},

              {"Fetch, modify, verify, and delete a node" ++ Label,
               fun() ->
                       %% GET the node
                       NodePath = Path ++ AName,
                       {ok, GetCode, _H1, Body1} = chef_req:request(get, NodePath, ReqConfig),
                       ?assertEqual("200", GetCode),
                       TheNode = ejson:decode(Body1),
                       ?assertEqual(AName, ej:get({<<"name">>}, TheNode)),

                       %% modify and PUT it back
                       NewNode = ej:set({<<"normal">>}, TheNode, {[{<<"volume">>, 11}]}),
                       NewNodeJson = ejson:encode(NewNode),
                       {ok, PutCode, _H2, Body2} = chef_req:request(put, NodePath,
                                                                    NewNodeJson, ReqConfig),
                       ?assertEqual("200", PutCode),
                       ?assertEqual(NewNodeJson, Body2),

                       %% GET it and verify it has the new attribute
                       {ok, GetCode2, _H3, Body3} = chef_req:request(get, NodePath, ReqConfig),
                       ?assertEqual("200", GetCode2),
                       GotNode = ejson:decode(Body3),
                       ?assertEqual(11, ej:get({<<"normal">>, <<"volume">>}, GotNode)),

                       %% DELETE it
                       {ok, DelCode, _H4, Body4} = chef_req:request(delete, NodePath, ReqConfig),
                       ?assertEqual("200", DelCode),
                       ?assertEqual(NewNodeJson, Body4),

                       %% verify we get a 404
                       {ok, GetCode3, _H5, _Body5} = chef_req:request(get, NodePath, ReqConfig),
                       ?assertEqual("404", GetCode3)
               end}
             ]
     end}.

basic_node_list_tests_for_config(#req_config{name = Name}=ReqConfig) ->
    Label = " (" ++ Name ++ ")",
    Path = "/organizations/clownco/nodes",
    {foreach,
     fun() -> ok = db_tool:truncate_nodes_table() end,
     fun(_) -> cleanup end,
     [
      {"list nodes, empty nodes table" ++ Label,
       fun() ->
               {ok, Code, _H, Body} = chef_req:request(get, Path, ReqConfig),
               ?assertEqual("200", Code),
               NodeList = ejson:decode(Body),
               ?assertEqual({[]}, NodeList)
       end},

      {"list nodes, single node" ++ Label,
       fun() ->
               {AName, AUrl} = create_node("clownco", ReqConfig),
               {ok, Code, _H, Body} = chef_req:request(get, Path, ReqConfig),
               ?assertEqual("200", Code),
               NodeList = ejson:decode(Body),
               ?assertEqual({[{AName, AUrl}]}, NodeList)
       end}

      %% There is a latent problem with this test.  On my laptop with
      %% the created node count set to 11 instead of 5 I consistently
      %% get a timeout error.  Reducing the count *seems* to resolve
      %% it, although I have now seen at least one case where on
      %% another test I got a req_timeout (I think that's from
      %% emysql).  Probably didn't see this before, because the
      %% container ACL cloning was broken.  Now that it is fixed, we
      %% make 7 authz calls per node create so it's probably quite a
      %% bit slower.
      ,{timeout, 60, {"list more than one nodes" ++ Label,
       fun() ->
               NamePairs = [ create_node("clownco", ReqConfig) || _I <- lists:seq(1, 5) ],
               Path = "/organizations/clownco/nodes",
               {ok, Code, _H, Body} = chef_req:request(get, Path, ReqConfig),
               ?assertEqual("200", Code),
               {NodeList} = ejson:decode(Body),
               ?assertEqual(lists:sort(NamePairs), lists:sort(NodeList))
       end}}
     ]}.

basic_env_node_list_tests(#req_config{name = Name}=ReqConfig) ->
    Label = " (" ++ Name ++ ")",
    {foreach,
     fun() -> ok = db_tool:truncate_nodes_table() end,
     fun(_) -> cleanup end,
     [
      {"no such environment is a 404" ++ Label,
       fun() ->
               NoSuch = "/organizations/clownco/environments/no-such-env/nodes",
               {ok, Code, _H, Body} = chef_req:request(get, NoSuch, ReqConfig),
               ?assertEqual("404", Code),
               Msg = <<"{\"error\":[\"Cannot load environment no-such-env\"]}">>,
               ?assertEqual(Msg, Body)
       end},

      {"environment list nodes, single node" ++ Label,
       fun() ->
               {AName, AUrl} = create_node("clownco", ReqConfig),
               Path = "/organizations/clownco/environments/_default/nodes",
               {ok, Code, _H, Body} = chef_req:request(get, Path, ReqConfig),
               ?assertEqual("200", Code),
               NodeList = ejson:decode(Body),
               ?assertEqual({[{AName, AUrl}]}, NodeList)
       end}

      ,{timeout, 60, {"environment list more than one nodes" ++ Label,
       fun() ->
               NamePairs = [ create_node("clownco", ReqConfig) || _I <- lists:seq(1, 5) ],
               Path = "/organizations/clownco/nodes",
               {ok, Code, _H, Body} = chef_req:request(get, Path, ReqConfig),
               ?assertEqual("200", Code),
               {NodeList} = ejson:decode(Body),
               ?assertEqual(lists:sort(NamePairs), lists:sort(NodeList))
       end}}
      
     ]}.
               
node_permissions_tests(_UserConfig, WeakClientConfig) ->
    Path = "/organizations/clownco/nodes",
    [
     {"POST without create on nodes container",
       fun() ->
               {_, Node403} = sample_node(),
               {ok, Code, _H, Body} = chef_req:request(post, Path, Node403,
                                                       WeakClientConfig),
               ?assertEqual("403", Code),
               ?assertEqual(<<"{\"error\":[\"missing create permission\"]}">>, Body)
       end},
     {"GET without read on nodes container",
       fun() ->
               {ok, Code, _H, Body} = chef_req:request(get, Path,
                                                       WeakClientConfig),
               ?assertEqual("403", Code),
               ?assertEqual(<<"{\"error\":[\"missing read permission\"]}">>, Body)
       end}
    ].

basic_node_create_tests_for_config(#req_config{name = Name}=ReqConfig) ->
    Label = " (" ++ Name ++ ")",
    {NodeName, NodeJson} = sample_node(),
    Path = "/organizations/clownco/nodes",
    [
     {"create a new node" ++ Label,
      fun() ->
              {ok, Code, _H, Body} = chef_req:request(post, Path, NodeJson, ReqConfig),
              ?assertEqual("201", Code),
              NodeUrl = <<"http://localhost/organizations/clownco/nodes/", NodeName/binary>>,
              Expect = ejson:encode({[{<<"uri">>, NodeUrl}]}),
              ?assertEqual(Expect, Body)
      end},

     {"conflict when node name already exists" ++ Label,
      %% Note that this test assumes the previous "create a new node" test ran successfully
      fun() ->
              {ok, Code, _H, Body} = chef_req:request(post, Path, NodeJson, ReqConfig),
              ?assertEqual("409", Code),
              ?assertEqual(<<"{\"error\":[\"Node already exists\"]}">>, Body)
      end},

     {"org 'no-such-org' does not exist" ++ Label,
      fun() ->
              BadPath = "/organizations/no-such-org/nodes",
              {ok, Code, _H, Body} = chef_req:request(post, BadPath, NodeJson, ReqConfig),
              ?assertEqual("404", Code),
              ?assertEqual(<<"{\"error\":[\"organization no-such-org does not exist.\"]}">>,
                           Body)
      end},

     {"POST of invalid JSON is a 400" ++ Label,
      fun() ->
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

create_node(Org, #req_config{api_root = Root}=ReqConfig) ->
    {AName, ANode} = sample_node(),
    Path = "/organizations/" ++ Org ++ "/nodes",
    {ok, "201", _, _} = chef_req:request(post, Path, ANode, ReqConfig),
    Url = list_to_binary(Root ++ Path ++ "/" ++ AName),
    {AName, Url}.
    

code_for_request(Method, Path, Config) ->
    code_for_request(Method, Path, [], Config).

code_for_request(Method, Path, ReqBody, Config) ->
    {ok, Code, _Headers, _Body} = chef_req:request(Method, Path, ReqBody, Config),
    Code.
    
