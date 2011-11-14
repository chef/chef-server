-module(node_integration_test).

-include("../src/chef_req.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(node_fields, [ <<"automatic">>, <<"default">>, <<"normal">>, <<"override">>, 
		       <<"chef_environment">>, <<"json_class">>, <<"chef_type">>, 
		       <<"run_list">> ]
       ).

node_endpoint_test_() ->
    {timeout, 120, {inorder, {setup,
     fun() ->
             ok = chef_req:start_apps(),
             db_tool:connect(),
             ok = db_tool:truncate_nodes_table(),
             KeyPath = "/tmp/opscode-platform-test/clownco-org-admin.pem",
             ReqConfig = chef_req:make_config("http://localhost:9021", "clownco-org-admin", 
					      KeyPath),

	     WebuiKeyPath = "/etc/opscode/webui_priv.pem",
	     WebuiConfig = chef_req:make_config("http://localhost:9021", "clownco-org-admin",
						WebuiKeyPath),


             ok = chef_req:delete_client("clownco", "client01", ReqConfig),
             ok = chef_req:delete_client("clownco", "client02", ReqConfig),
             ClientConfig = chef_req:make_client("clownco", "client01", ReqConfig),
             WeakClientConfig = chef_req:make_client("clownco", "client02", ReqConfig),
             chef_req:remove_client_from_group("clownco", "client02", "clients", ReqConfig),
             {ReqConfig, ClientConfig, WeakClientConfig, WebuiConfig}
     end,
     fun({_, _, _, _}) ->
             test_utils:nuke_nodes_from_solr(),
             test_utils:test_cleanup(ignore)
     end,
     fun({UserConfig, ClientConfig, WeakClientConfig, WebuiConfig}) ->
             [
              node_search_tests(UserConfig),
              node_search_tests(ClientConfig),
              basic_env_node_list_tests(UserConfig),
              named_node_permissions(UserConfig, ClientConfig, WeakClientConfig),
              basic_named_node_ops(ClientConfig),
              basic_named_node_ops(UserConfig),
	      invalid_named_node_ops(ClientConfig),
	      invalid_named_node_creation(ClientConfig),
              updated_at_and_last_updated_by(ClientConfig, UserConfig),
              basic_node_create_tests_for_config(UserConfig),
              basic_node_create_tests_for_config(ClientConfig),
	      node_permissions_tests(UserConfig, WeakClientConfig),
              basic_node_list_tests_for_config(UserConfig),
	      basic_node_list_tests_for_config(ClientConfig),
	      webui_key_tests(UserConfig, WebuiConfig)
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

node_search_tests(ReqConfig) ->
    %% This test verifies that created and updated nodes are
    %% searchable and that the precendence rules for node attributes
    %% are respected for indexing/search.
    {timeout, 60, {"node search coherency",
     fun() ->
             %% create a node with a "test_tag" key in the default section.
             NodeName = make_node_name("search-node-"),
             Node1 = make_node(NodeName, <<"dev">>,
                               [<<"recipe[apache2]">>, <<"role[web]">>],
                               [{default, {[{<<"test_tag">>, <<"default">>}]}}]),
             create_node("clownco", {NodeName, Node1}, ReqConfig),
             test_utils:force_solr_commit(),
             %% now verify that we can search by top-level attributes
             %% as well as inflated role: and recipe:
             Queries = ["name:" ++ binary_to_list(NodeName),
                        "chef_environment:dev",
                        "role:web",
                        "recipe:apache2",
                        "run_list:recipe\\[apache2\\]",
                        "run_list:role\\[web\\]",
                        "test_tag:default"],
             [ assert_node_found_on_search(NodeName, Query, ReqConfig)
               || Query <- Queries ],

             %% now update the node with a value for test_tag set at
             %% normal to see it take precedence over default value.
             Json1 = ejson:decode(Node1),
             Json2 = ej:set({<<"normal">>}, Json1, {[{<<"test_tag">>, <<"normal">>}]}),
             Node2 = ejson:encode(Json2),
             NodePath = "/organizations/clownco/nodes/" ++ binary_to_list(NodeName),
             {ok, "200", _, _} = chef_req:request(put, NodePath, Node2, ReqConfig),

             test_utils:force_solr_commit(),
             assert_node_found_on_search(NodeName, "test_tag:normal", ReqConfig),
             assert_node_not_found_on_search(NodeName, "test_tag:default", ReqConfig),

             %% finally, update and set a value for test_tag in the
             %% override section.
             Json3 = ej:set({<<"override">>}, Json2, {[{<<"test_tag">>, <<"override">>}]}),
             Node3 = ejson:encode(Json3),
             {ok, "200", _, _} = chef_req:request(put, NodePath, Node3, ReqConfig),
             test_utils:force_solr_commit(),
             assert_node_found_on_search(NodeName, "test_tag:override", ReqConfig),
             assert_node_not_found_on_search(NodeName, "test_tag:default", ReqConfig),
             assert_node_not_found_on_search(NodeName, "test_tag:normal", ReqConfig)
     end}}.

assert_node_found_on_search(NodeName, Query, ReqConfig) ->
    Path = search_path("clownco", "node", Query),
    {ok, "200", _H, Body} = chef_req:request(get, Path, ReqConfig),
    Json = ejson:decode(Body),
    Rows = ej:get({"rows"}, Json),
    ?assertEqual({Query, true},
                 {Query, lists:any(fun(Row) -> NodeName =:= ej:get({"name"}, Row) end, Rows)}).

assert_node_not_found_on_search(NodeName, Query, ReqConfig) ->
    Path = search_path("clownco", "node", Query),
    {ok, "200", _H, Body} = chef_req:request(get, Path, ReqConfig),
    Json = ejson:decode(Body),
    Rows = ej:get({"rows"}, Json),
    ?assert(lists:all(fun(Row) -> NodeName =/= ej:get({"name"}, Row) end, Rows)).
    

updated_at_and_last_updated_by(ClientConfig, UserConfig) ->
    {setup,
     fun() ->
             {AName, _AUrl} = create_node("clownco", ClientConfig),
             %% Pause here to ensure that any updates we do happen in
             %% the next second which will allow us to validate that
             %% updated_at is getting written correctly.
             Now = calendar:now_to_universal_time(os:timestamp()),
             timer:sleep(1000),
             {AName, Now}
     end,
     fun(_) -> cleanup end,
     fun({AName, Now}) ->
             [
              {"updated_at and last_updated_by set on node update",
               fun() ->
                       %% GET the node
                       NodePath = "/organizations/clownco/nodes/" ++ AName,
                       {ok, "200", _H1, Body1} = chef_req:request(get, NodePath, ClientConfig),
                       TheNode = ejson:decode(Body1),
                       %% fetch the actor ID that last updated the node
                       Actor0 = proplists:get_value(last_updated_by,
                                                    db_tool:metadata_for_node(AName)),

                       %% modify and PUT it back, use UserConfig so that we can verify that
                       %% last_updated_by works as expected.
                       NewNode = ej:set({<<"normal">>}, TheNode, {[{<<"volume">>, 11}]}),
                       NewNodeJson = ejson:encode(NewNode),
                       {ok, "200", _H2, _Body2} = chef_req:request(put, NodePath,
                                                                   NewNodeJson, UserConfig),

                       %% check that updated_at was updated
                       NodeMeta = db_tool:metadata_for_node(AName),
                       ?assertMatch({{_,_,_},{_,_,_}}, proplists:get_value(updated_at, NodeMeta)),
                       ?assert(Now < proplists:get_value(updated_at, NodeMeta)),
                       ?assert(Actor0 =/= proplists:get_value(last_updated_by, NodeMeta))
               end}
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

   
delete_field_test_helper(NodePath, ReqConfig, Node, Field, ResponseCode, Expected) ->
    %% modify and PUT it back in various broken ways.
    
    %% Missing name should error and not change the name
    NewNode = ej:delete({Field}, Node),
    NewNodeJson = ejson:encode(NewNode),
    {ok, PutCode, _H2, _Body2} = chef_req:request(put, NodePath, NewNodeJson, ReqConfig),
%    ?debugVal({Field, PutCode, ResponseCode}),
    ?assertEqual(ResponseCode, PutCode),
    {ok, _GetCode, _H1, Body2} = chef_req:request(get, NodePath, ReqConfig),
    Node2 =  ejson:decode(Body2),
%    ?debugVal({Field, ej:get({Field}, Node), Expected}),
    ?assertEqual(Expected, ej:get({Field}, Node2)).
    
    
invalid_named_node_ops(#req_config{name = Name}=ReqConfig) ->
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
	      {"Fetch, modify a node with a different name" ++ Label,
               fun() ->

                       %% GET the node
                       NodePath = Path ++ AName,
                       {ok, GetCode, _H1, Body1} = chef_req:request(get, NodePath, ReqConfig),
                       ?assertEqual("200", GetCode),
                       TheNode = ejson:decode(Body1),
                       ?assertEqual(AName, ej:get({<<"name">>}, TheNode)),


                       %% modify and PUT it back
                       NewNode = ej:set({<<"name">>}, TheNode, <<"not_the_original_name">>),
                       NewNodeJson = ejson:encode(NewNode),
                       {ok, PutCode, _H2, _Body2} = chef_req:request(put, NodePath,
                                                                    NewNodeJson, ReqConfig),
                       ?assertEqual("200", PutCode)
               end},
              {"Fetch, modify a node with a bad name" ++ Label,
               fun() ->

                       %% GET the node
                       NodePath = Path ++ AName,
                       {ok, GetCode, _H1, Body1} = chef_req:request(get, NodePath, ReqConfig),
                       ?assertEqual("200", GetCode),
                       TheNode = ejson:decode(Body1),
                       ?assertEqual(AName, ej:get({<<"name">>}, TheNode)),

                       %% modify and PUT it back
                       NewNode = ej:set({<<"name">>}, TheNode, <<" bad%#@Q#*name ">>),
                       NewNodeJson = ejson:encode(NewNode),
                       {ok, PutCode, _H2, _Body2} = chef_req:request(put, NodePath,
                                                                    NewNodeJson, ReqConfig),
                       ?assertEqual("400", PutCode)
               end},
	      {generator, 
	       fun() ->
	       	       [ {"Fetch, modify a node with missing field '" ++ binary_to_list(Field) ++ "' : " ++ Label,
	       		  fun() ->
	       			  %% GET the node
	       			  NodePath = Path ++ AName,
	       			  {ok, GetCode, _H1, Body1} = chef_req:request(get, NodePath, ReqConfig),
	       			  ?assertEqual("200", GetCode),
	       			  TheNode = ejson:decode(Body1),
	       			  %% modify and PUT it back in various broken ways.
	       			  delete_field_test_helper(NodePath,ReqConfig, TheNode, Field, Code, Expected)
	       		  end }
	       		 || {Field, Code, Expected} <-
	       			[{<<"name">>, "200", AName},
				 {<<"automatic">>, "200", {[]}},
	       			 {<<"default">>, "200", {[]}},
	       			 {<<"normal">>, "200", {[]}},
	       			 {<<"override">>, "200", {[]}},
	       			 {<<"chef_environment">>, "200", <<"_default">>},
	       			 {<<"json_class">>, "200", <<"Chef::Node">>},
	       			 {<<"chef_type">>, "200", <<"node">> },
	       			 {<<"run_list">>, "400", []}
	       			]
	       	       ]
	       end
	      },
	      {"Fetch, modify a node with a bad env" ++ Label,
               fun() ->
		       
                       %% GET the node
                       NodePath = Path ++ AName,
                       {ok, GetCode, _H1, Body1} = chef_req:request(get, NodePath, ReqConfig),
                       ?assertEqual("200", GetCode),
                       TheNode = ejson:decode(Body1),

                       %% modify and PUT it back
                       NewNode = ej:set({<<"chef_environment">>}, TheNode, <<" bad%#@Q#*name ">>),
                       NewNodeJson = ejson:encode(NewNode),
                       {ok, PutCode, _H2, _Body2} = chef_req:request(put, NodePath,
                                                                    NewNodeJson, ReqConfig),
                       ?assertEqual("400", PutCode)
               end}
             ]
     end}.

invalid_named_node_creation(#req_config{name = Name}=ReqConfig) ->
    Label = " (" ++ Name ++ ")",
    Path  = "/organizations/clownco/nodes/",
    {setup,
     fun() -> 
	     {AName, ANodeJson} = sample_node(),
	     ANode = ejson:decode(ANodeJson),
	     {AName, ANode}
     end,
     fun(_) -> cleanup end,
     fun({AName, ANode}) ->
             [
	      {"Create a node with an empty name" ++ Label,
               fun() ->
		       %% CREATE a node with no name
		       BadNode = ej:delete({<<"name">>}, ANode),
		       BadNodeJson = ejson:encode(BadNode),
		       {ok, PostCode, _H1, _B1} =
			   chef_req:request(post, Path, BadNodeJson, ReqConfig),
		       ?assertEqual("400", PostCode)
               end},
	      {"Create a node with an bad name" ++ Label,
               fun() ->
		       %% CREATE a node with no name
		       BadNode = ej:set({<<"name">>}, ANode, <<" bad%#@Q#*name ">>),
		       BadNodeJson = ejson:encode(BadNode),
		       {ok, PostCode, _H1, _B1} =
			   chef_req:request(post, Path, BadNodeJson, ReqConfig),
		       ?assertEqual("400", PostCode)
               end},
	      {generator, 
	       fun() ->
	       	       [ {"Create a node with missing field '" ++ binary_to_list(Field) ++ "' : " ++ Label,
	       		  fun() ->
	       			  NodePath = Path ++ AName,
				  {ok, _, _, _} = chef_req:request(delete, NodePath, ReqConfig),

	      			  BadNode = ej:delete({Field}, ANode),
	      			  BadNodeJson = ejson:encode(BadNode),
	      			  {ok, PostCode, _H1, _B1} =
	      			      chef_req:request(post, Path, BadNodeJson, ReqConfig),

	      			  ?assertEqual(Code, PostCode),
	       			  %% GET the node
	       			  {ok, GetCode, _H2, Body2} = chef_req:request(get, NodePath, ReqConfig),

	       			  ?assertEqual(ExpectedGetCode, GetCode),

				  case GetCode of 
				      "200" -> 
					  TheNode = ejson:decode(Body2),
					  ExpectedNode = ej:set({Field}, BadNode, Expected),
					  [ ?assertEqual(ej:get({Key}, ExpectedNode),
							 ej:get({Key}, TheNode))
					    ||  Key <- ?node_fields ];
				      _ -> ok
				  end,
				  
				  {ok, _, _, _} = chef_req:request(delete, NodePath, ReqConfig)
	       		  end }
	       		 || {Field, Code, Expected, ExpectedGetCode} <-
	      			 [
				  {<<"automatic">>, "201", {[]}, "200"},
	      			  {<<"default">>, "201", {[]}, "200"},
	      			  {<<"normal">>, "201", {[]}, "200"},
	      			  {<<"override">>, "201", {[]}, "200"},
	      			  {<<"chef_environment">>, "201", <<"_default">>, "200"},
	      			  {<<"json_class">>, "201", <<"Chef::Node">>, "200"},
	      			  {<<"chef_type">>, "201", <<"node">>, "200"},
	      			  {<<"run_list">>, "400", [], "404"}
				 ]
	      	       ]
	       end
	      }
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
               Msg = <<"{\"error\":[\"Cannot load environment 'no-such-env'\"]}">>,
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
              Now = calendar:now_to_universal_time(os:timestamp()),
              %% Pause here to ensure that any updates we do happen in
              %% the next second which will allow us to validate that
              %% updated_at is getting written correctly.
              timer:sleep(1000),
              {ok, Code, _H, Body} = chef_req:request(post, Path, NodeJson, ReqConfig),
              ?assertEqual("201", Code),
              NodeUrl = <<"http://localhost:9021/organizations/clownco/nodes/", NodeName/binary>>,
              Expect = ejson:encode({[{<<"uri">>, NodeUrl}]}),
              ?assertEqual(Expect, Body),

              NodeMeta = db_tool:metadata_for_node(NodeName),
              ?assertMatch({{_,_,_},{_,_,_}}, proplists:get_value(created_at, NodeMeta)),
              ?assertMatch({{_,_,_},{_,_,_}}, proplists:get_value(updated_at, NodeMeta)),
              ?assert(Now < proplists:get_value(created_at, NodeMeta)),
              ?assert(Now < proplists:get_value(updated_at, NodeMeta))
              
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
              ?assertEqual(<<"{\"error\":[\"organization 'no-such-org' does not exist.\"]}">>,
                           Body)
      end},

     {"POST of invalid JSON is a 400" ++ Label,
      fun() ->
              InvalidJson = <<"{not:json}">>,
              {ok, Code, _H, _Body} = chef_req:request(post, Path, InvalidJson, ReqConfig),
              ?assertEqual("400", Code)
      end},
     {"POST of a bad name is a 500" ++ Label,
      fun() ->
	      Node = ejson:decode(NodeJson),
              NewNode = ej:set({<<"name">>}, Node, <<" bad%#@Q#*name ">>),
	      NewNodeJson = ejson:encode(NewNode),
              {ok, Code, _H, _Body} = chef_req:request(post, Path, NewNodeJson, ReqConfig),
              ?assertEqual("400", Code)
      end},
     {"POST of a bad environment is a 500" ++ Label,
      fun() ->
	      Node = ejson:decode(NodeJson),
	      NewNode = ej:set({<<"chef_environment">>}, Node, <<" bad%#@Q#*name ">>),
	      NewNodeJson = ejson:encode(NewNode),
	      {ok, Code, _H, _Body} = chef_req:request(post, Path, NewNodeJson, ReqConfig),
              ?assertEqual("400", Code)
      end}
    ].

webui_key_tests(#req_config{name = Name}=ReqConfig,
		#req_config{name = WebUIName}=WebUIConfig) ->
    Label = " (" ++ WebUIName ++ ")",
    {NodeName, NodeJson} = sample_node(),
    Path = "/organizations/clownco/nodes",
    [
     {"create a new node" ++ Label,
      fun() ->
   	      Now = calendar:now_to_universal_time(os:timestamp()),
              %% Pause here to ensure that any updates we do happen in
              %% the next second which will allow us to validate that
              %% updated_at is getting written correctly.
              timer:sleep(1000),
	      Headers = [{"X-OPS-REQUEST-SOURCE", "web"}, 
		         {"X-OPS-WEBKEY-TAG", "default"}],
	      {ok, Code, _H, Body} = chef_req:request(post, Path, Headers, NodeJson, WebUIConfig),
	      ?assertEqual("201", Code),
	      NodeUrl = <<"http://localhost:9021/organizations/clownco/nodes/", NodeName/binary>>,
              Expect = ejson:encode({[{<<"uri">>, NodeUrl}]}),
              ?assertEqual(Expect, Body),

              NodeMeta = db_tool:metadata_for_node(NodeName),
              ?assertMatch({{_,_,_},{_,_,_}}, proplists:get_value(created_at, NodeMeta)),
              ?assertMatch({{_,_,_},{_,_,_}}, proplists:get_value(updated_at, NodeMeta)),
              ?assert(Now < proplists:get_value(created_at, NodeMeta)),
              ?assert(Now < proplists:get_value(updated_at, NodeMeta))
              
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

create_node(Org, ReqConfig) ->
    {AName, ANode} = sample_node(),
    create_node(Org, {AName, ANode}, ReqConfig).

create_node(Org, {AName, ANode}, #req_config{api_root = Root}=ReqConfig) ->
    Path = "/organizations/" ++ Org ++ "/nodes",
    {ok, "201", _, _} = chef_req:request(post, Path, ANode, ReqConfig),
    Url = list_to_binary(Root ++ Path ++ "/" ++ AName),
    {AName, Url}.

code_for_request(Method, Path, Config) ->
    code_for_request(Method, Path, [], Config).

code_for_request(Method, Path, ReqBody, Config) ->
    {ok, Code, _Headers, _Body} = chef_req:request(Method, Path, ReqBody, Config),
    Code.

make_node(Name, EnvName, RunList, Attrs) ->
    Node = {[
             %% top-level attributes
             {<<"name">>, Name},
             {<<"chef_type">>, <<"node">>},
             {<<"chef_environment">>, EnvName},
             {<<"run_list">>, RunList},
             %% grouped attrs
             {<<"default">>, proplists:get_value(default, Attrs, {[]})},
             {<<"normal">>, proplists:get_value(normal, Attrs, {[]})},
             {<<"override">>, proplists:get_value(override, Attrs, {[]})},
             {<<"automatic">>, proplists:get_value(automatic, Attrs, {[]})}
            ]},
    ejson:encode(Node).

search_path(Org, Type, Query) ->
    "/organizations/" ++ Org ++ "/search/" ++ Type ++ "?q=" ++ Query.
