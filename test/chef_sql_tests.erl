-module(chef_sql_tests).

-include_lib("eunit/include/eunit.hrl").
-include("chef_types.hrl").

%% Used in testing flatten_record
-record(test_record, {'field1',
                      'field2'
                     }).

flatten_record_test_() ->
    [{"flatten record of simple record is ok",
        fun() ->
            R = #test_record{field1= <<"foo">>,
                         field2= <<"bar">> },
            Got = chef_sql:flatten_record(R),
            ?assertEqual([<<"foo">>, <<"bar">>], Got)
        end
      },
      {"throw on undefined as a default value",
        fun() ->
            R = #test_record{field1= <<"foo">>},
            ?assertError({undefined_in_record, R},
                         chef_sql:flatten_record(R))
        end
      },
      {"throw on explicit undefined used as a value",
        fun() ->
            R = #test_record{field1= <<"foo">>, field2= undefined},
            ?assertError({undefined_in_record, R},
                         chef_sql:flatten_record(R))
        end
      }
    ].

%% FIXME
%% I'm seeing an error like this when trying to run this test:
%%
%% undefined
%% *unexpected termination of test process*
%% ::{compile_forms,error}
%%
%% I think it is time we revisit how we test this app and move things to live db testing
%%
-ifdef(I_LIKE_MOCKS).
-export([simple_node/1, simple_node/0]).


-include_lib("emysql/include/emysql.hrl").

-define(test_node_id, <<"08454be1af1a4e2baa9d08955995ebd9">>).
-define(bad_node_id, <<"08454be1af1a4e2baa9d08955995beef">>).
-define(test_node_name, "i-cleanup").
-define(test_org_name, <<"clownco">>).
-define(test_org_admin, <<"clownco-org-admin">>).
%% fetch_user UserName test
%% - found
%% - not_found
%% - error

-define(AUTOMECK_FILE(TestName), filename:join(["..", "test", "automeck_config",
                                                atom_to_list(?MODULE) ++ "_" ++ atom_to_list(TestName) ++
                                                    ".config"])).

fetch_node_test_() ->
    {foreach,
     fun() -> test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({Server, _SuperUser}) ->
              automeck:mocks(?AUTOMECK_FILE(fetch_node1)),
              {"a node searched for and isn't found",
               fun() ->
                       TestOrgId = chef_db:fetch_org_id(Server, ?test_org_name),
                       R = chef_sql:fetch_node(TestOrgId, ?test_node_name),
                       ?assertEqual({ok, not_found}, R)
               end
              }
      end,
      fun({Server, _SuperUser}) ->
              automeck:mocks(?AUTOMECK_FILE(fetch_node2)),
              {"a node searched for is found, and matches record used to create it",
               fun() ->
                       TestOrgId = chef_db:fetch_org_id(Server, ?test_org_name),
                       Node = simple_node(TestOrgId),
                       R = chef_sql:create_node(Node),
                       ?assertEqual(R, {ok, 1}),
                       {ok, Node2} = chef_sql:fetch_node(TestOrgId, ?test_node_name),
                       ?assertEqual(Node#chef_node.id,
                                    Node2#chef_node.id),
                       ?assertEqual(Node#chef_node.authz_id,
                                    Node2#chef_node.authz_id),
                       ?assertEqual(Node#chef_node.org_id,
                                    Node2#chef_node.org_id),
                       ?assertEqual(Node#chef_node.environment,
                                    Node2#chef_node.environment),
                       ?assertEqual(Node#chef_node.serialized_object,
                                    Node2#chef_node.serialized_object),
                       ?assertEqual(Node, Node2)
               end}
      end]}.

fetch_user_test_() ->
    {foreach,
     fun() -> test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({_Server, _SuperUser}) ->
              automeck:mocks(?AUTOMECK_FILE(fetch_user1)),
              {"A user is searched for and found, returning a chef_user record",
               fun() ->
                       ?assertMatch({ok,{chef_user,
                                         <<"a1">>,
                                         <<"b2">>,
                                         <<"alice">>,
                                         1,
                                         <<"abcdef1234567890">>}},
                                    chef_sql:fetch_user("alice"))
               end}
      end,

      fun({_Server, _SuperUser}) ->
              automeck:mocks(?AUTOMECK_FILE(fetch_user2)),
              {"A user is searched for and not found",
               fun() ->
                       ?assertMatch({ok, not_found},
                                    chef_sql:fetch_user("bob"))
               end}
      end]}.


fetch_nodes_test_() ->
    {foreach,
     fun() -> test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({Server, _SuperUser}) ->
              automeck:mocks(?AUTOMECK_FILE(fetch_nodes1)),
              {"list nodes where there are none",
               fun() ->
                       TestOrgId = chef_db:fetch_org_id(Server, ?test_org_name),
                       ?assertEqual({ok, not_found}, chef_sql:fetch_nodes(TestOrgId))
               end}
      end,
      fun({Server, _SuperUser}) ->
              automeck:mocks(?AUTOMECK_FILE(fetch_nodes2)),
              {"list nodes where there is one",
               fun() ->
                       TestOrgId = chef_db:fetch_org_id(Server, ?test_org_name),
                       Node = simple_node(TestOrgId),
                       R = chef_sql:create_node(Node),
                       ?assertEqual(R, {ok, 1}),
                       {ok, Nodes} = chef_sql:fetch_nodes(TestOrgId),
                       ?assertEqual(Nodes, [iolist_to_binary(?test_node_name)])
               end}
      end,
      fun({_Server, _SuperUser}) ->
              automeck:mocks(?AUTOMECK_FILE(fetch_nodes3)),
              {"list nodes from a given environment",
               fun() ->
                       ?assertMatch({ok, [<<"clownco-test-node">>,<<"clownco-test-node2">>]},
                                    chef_sql:fetch_nodes("clownco","_default")),
                       ?assertMatch({ok, [<<"clownco-test-node3">>]},
                                    chef_sql:fetch_nodes("clownco","prod")),
                       ?assertMatch({ok, not_found},
                                    chef_sql:fetch_nodes("clownco","fake")),
                       ?assertMatch({ok, not_found},
                                    chef_sql:fetch_nodes("skynet","_default"))
               end}
      end]}.

update_node_test_() ->
    {foreach,
     fun() -> test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({Server, _SuperUser}) ->
              automeck:mocks(?AUTOMECK_FILE(update_node1)),
              {"a node updated that doesn't exist",
               fun() ->
                       TestOrgId = chef_db:fetch_org_id(Server, ?test_org_name),
                       Node = simple_node(TestOrgId),
                       ?assertEqual({ok, not_found}, chef_sql:update_node(Node))
               end}
      end,
      fun({Server, _SuperUser}) ->
              automeck:mocks(?AUTOMECK_FILE(update_node2)),
              {"a node searched for is found, and matches record used to create it",
               fun() ->
                       TestOrgId = chef_db:fetch_org_id(Server, ?test_org_name),
                       Node = simple_node(TestOrgId),
                       R = chef_sql:create_node(Node),
                       Node2 = Node#chef_node{ last_updated_by = <<"Test">> },
                       ?assertEqual(R, {ok, 1}),
                       R = chef_sql:update_node(Node2),
                       ?assertEqual(R, {ok, 1})
               end}
      end]}.

create_node_test_() ->
     {foreach,
      fun() ->
              application:set_env(sqerl, db_type, mysql),
              test_utils:test_setup()
      end,
      fun(_) ->
              application:unset_env(sqerl, db_type),
              meck:unload()
      end,
      [fun({Server, _SuperUser}) ->
               automeck:mocks(?AUTOMECK_FILE(create_node)),
               {"a node is created that already exists",
                fun() ->
                        TestOrgId = chef_db:fetch_org_id(Server, ?test_org_name),
                        Node1 = simple_node(TestOrgId),
                        R1 = chef_sql:create_node(Node1),
                        ?assertEqual({ok, 1}, R1),
                        Node2 = Node1#chef_node{ created_at={datetime, {{3000,1,1},{00,00,00}}} },
                        R2 = chef_sql:create_node(Node2),
                        ?assertMatch({conflict, _}, R2)
                end}
       end]}.



delete_node_test_() ->
     {foreach,
      fun() -> test_utils:test_setup() end,
      fun(_) -> meck:unload() end,
      [{"delete a non-existient node",
        fun() ->
                automeck:mocks(?AUTOMECK_FILE(delete_node1)),
                R = chef_sql:delete_node(?bad_node_id),
                ?assertEqual({ok, not_found}, R)
        end},
       {"delete a node that already exists, where node ID is passed as a binary",
        fun() ->
                automeck:mocks(?AUTOMECK_FILE(delete_node2)),
                ?assertEqual({ok, 1},
                             chef_sql:create_node(simple_node())),
                ?assertEqual({ok, 1},
                             chef_sql:delete_node(?test_node_id))
        end},
       {"delete a node that already exists, where node ID is passed as a list",
        fun() ->
                automeck:mocks(?AUTOMECK_FILE(delete_node2)),
                ?assertEqual({ok, 1},
                             chef_sql:create_node(simple_node())),
                ?assertEqual({ok, 1},
                             chef_sql:delete_node(binary_to_list(?test_node_id)))
        end},
       {"delete a node that already exists, where node ID is passed as a node record",
        fun() ->
                automeck:mocks(?AUTOMECK_FILE(delete_node2)),
                ?assertEqual({ok, 1},
                             chef_sql:create_node(simple_node())),
                ?assertEqual({ok, 1},
                             chef_sql:delete_node(simple_node()))
        end}
      ]}.

bulk_get_nodes_test_() ->
    {foreach,
     fun() -> test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [{"fetch serializations of up to 5 nodes at a time",
       fun() ->
               automeck:mocks(?AUTOMECK_FILE(bulk_get_nodes1)),
               ?assertMatch({ok, [<<"node1-serialization">>]},
                            chef_sql:bulk_get_nodes(["n1"])),
               ?assertMatch({ok, [<<"node1-serialization">>, <<"node2-serialization">>]},
                            chef_sql:bulk_get_nodes(["n1", "n2"])),
               ?assertMatch({ok, [<<"node1-serialization">>, <<"node2-serialization">>, <<"node3-serialization">>]},
                            chef_sql:bulk_get_nodes(["n1", "n2", "n3"])),
               ?assertMatch({ok, [<<"node1-serialization">>, <<"node2-serialization">>, <<"node3-serialization">>]},
                            chef_sql:bulk_get_nodes(["n1", "n2", "n3", "not_there"])),
               ?assertMatch({ok, [<<"node1-serialization">>, <<"node2-serialization">>, <<"node3-serialization">>]},
                            chef_sql:bulk_get_nodes(["n1", "n2", "n3", "not_there", "nope"]))
       end}]}.

simple_node() ->
    simple_node(<<"850b1974960a41c8b0fcc0c8a3b8310a">>).
simple_node(OrgId) ->
    {chef_node,
     ?test_node_id,
     <<"f09927a1e90c3c9db916a22ca5fa703b">>,
     OrgId,
     iolist_to_binary(?test_node_name),
     <<"_default">>,
     <<"f8214110fab5245be0d8becf4f61d681">>,
     {datetime, {{2011,9,8},{19,38,44}}},
     {datetime, {{2011,9,8},{19,38,44}}},
     <<"{\"normal\":{\"ec2opts\":{\"lvm\":{\"ephemeral_mountpoint\":\"/mnt\",\"use_ephemeral\":true,\"ephemeral_logical_volume\":\"store\",\"ephemeral_devices\":{\"m1.small\":[\"/dev/sda2\"],\"m1.xlarge\":[\"/dev/sdb\",\"/dev/sdc\",\"/dev/sdd\",\"/dev/sde\"],\"m1.large\":[\"/dev/sdb\",\"/dev/sdc\"]},\"ephemeral_volume_group\":\"ephemeral\"}},\"runit\":{\"sv_bin\":\"/usr/bin/sv\",\"service_dir\":\"/etc/service\",\"sv_dir\":\"/etc/sv\"},\"authorization\":{\"sudo\":{\"groups\":[],\"users\":[]}},\"tags\":[],\"network\":{\"interfaces\":{\"eth0\":{\"addresses\":{}}}},\"postfix\":{\"myhostname\":\"opscode.com\",\"myorigin\":\"$myhostname\",\"smtp_sasl_passwd\":\"Y3vDOkz8YDcxqL\",\"smtp_sasl_security_options\":\"noanonymous\",\"smtp_sasl_password_maps\":\"hash:/etc/postfix/sasl_passwd\",\"mail_type\":\"client\",\"mail_relay_networks\":\"127.0.0.0/8\",\"smtp_sasl_user_name\":\"community\",\"relayhost\":\"lists.opscode.com\",\"smtp_use_tls\":\"yes\",\"mydomain\":\"opscode.us\",\"smtp_tls_cafile\":\"/etc/postfix/cacert.pem\"}},\"name\":\"i-cleanup\",\"override\":{\"dynect\":{\"ec2\":{\"env\":\"rspreprod\",\"type\":\"nanoc-wp\"}},\"wordpress\":{\"version\":\"3.1.2\",\"db\":{\"table_prefix\":\"blog_\",\"database\":\"wordpress\",\"user\":\"wordpress\"}},\"network\":{\"interfaces\":{\"eth0\":{\"addresses\":{}}}},\"postfix\":{\"smtp_sasl_passwd\":\"Y3vDOkz8YDcxqL\",\"smtp_sasl_security_options\":\"noanonymous\",\"smtp_sasl_auth_enable\":\"yes\",\"relayhost\":\"lists.opscode.com\",\"smtp_sasl_user_name\":\"community\"}},\"default\":{\"dynect\":{\"ttl\":600,\"zone\":\"opscode.us\",\"domain\":\"opscode.us\",\"username\":\"opscodeus\",\"customer\":\"opscode\",\"ec2\":{\"env\":\"na\",\"type\":\"unknown\"},\"password\":\"Tu3rAErLrgD8gsan\"},\"wordpress\":{\"dir\":\"/var/www/wordpress\",\"checksum\":\"c68588ca831b76ac8342d783b7e3128c9f4f75aad39c43a7f2b33351634b74de\",\"version\":\"3.0.4\",\"db\":{\"table_prefix\":\"wp_\",\"database\":\"wordpressdb\",\"host\":\"localhost\",\"user\":\"wordpressuser\"}},\"munin\":{\"graph_strategy\":\"cron\"},\"mysql\":{\"ebs_vol_dev\":\"/dev/sdi\",\"ebs_vol_size\":50,\"datadir\":\"/var/lib/mysql\",\"ec2_path\":\"/mnt/mysql\",\"tunable\":{\"key_buffer\":\"256M\",\"innodb_buffer_pool_size\":\"256M\",\"back_log\":\"128\",\"thread_cache_size\":8,\"thread_concurrency\":10,\"query_cache_size\":\"16M\",\"net_write_timeout\":\"30\",\"max_connections\":\"800\",\"thread_cache\":\"128\",\"max_allowed_packet\":\"16M\",\"net_read_timeout\":\"30\",\"table_cache\":\"128\",\"wait_timeout\":\"180\",\"log_slow_queries\":\"/var/log/mysql/slow.log\",\"table_open_cache\":\"128\",\"myisam_recover\":\"BACKUP\",\"query_cache_limit\":\"1M\",\"long_query_time\":2,\"max_heap_table_size\":\"32M\",\"thread_stack\":\"256K\"},\"bind_address\":\"0.0.0.0\"},\"nanoc\":{\"www_root\":\"/var/www/nanoc\"},\"network\":{\"type\":\"single\",\"interfaces\":{\"eth0\":{\"flags\":[\"UP\",\"BROADCAST\",\"RUNNING\",\"MULTICAST\"],\"number\":\"0\",\"addresses\":{\"fe80::1031:39ff:fe15:5e2b\":{\"scope\":\"Link\",\"prefixlen\":\"64\",\"family\":\"inet6\"},\"12:31:39:15:5e:2b\":{\"family\":\"lladdr\"},\"10.207.93.213\":{\"netmask\":\"255.255.255.0\",\"broadcast\":\"10.207.93.255\",\"family\":\"inet\"}},\"mtu\":\"1500\",\"type\":\"eth\",\"arp\":{\"10.207.93.1\":\"fe:ff:ff:ff:ff:ff\"},\"encapsulation\":\"Ethernet\"}},\"source\":\"dhcp\"},\"splunk\":{\"auth\":\"admin:VYjsrtHdtieZD2QtX5za\"},\"monitor_group\":\"wordpress\",\"ntp\":{\"is_server\":false,\"service\":\"ntp\",\"servers\":[\"0.us.pool.ntp.org\",\"1.us.pool.ntp.org\"]},\"postfix\":{\"myhostname\":\"opscode.com\",\"myorigin\":\"$myhostname\",\"smtp_sasl_passwd\":\"Y3vDOkz8YDcxqL\",\"smtp_sasl_password_maps\":\"hash:/etc/postfix/sasl_passwd\",\"smtp_sasl_security_options\":\"noanonymous\",\"mail_type\":\"client\",\"smtp_sasl_auth_enable\":\"yes\",\"relayhost\":\"lists.opscode.com\",\"mail_relay_networks\":\"127.0.0.0/8\",\"smtp_sasl_user_name\":\"community\",\"smtp_use_tls\":\"yes\",\"mydomain\":\"opscode.us\"},\"app_environment\":\"rs-preprod\"},\"json_class\":\"Chef::Node\",\"automatic\":{\"fqdn\":\"nanoc-wp-rspreprod-i-cleanup.opscode.us\",\"platform_version\":\"10.04\",\"command\":{\"ps\":\"ps -ef\"},\"ipaddress\":\"10.207.93.213\",\"idletime_seconds\":16207739,\"os\":\"linux\",\"domain\":\"opscode.us\",\"idletime\":\"187 days 14 hours 08 minutes 59 seconds\",\"chef_packages\":{\"ohai\":{\"ohai_root\":\"/usr/lib/ruby/gems/1.8/gems/ohai-0.6.4/lib/ohai\",\"version\":\"0.6.4\"},\"chef\":{\"version\":\"0.10.4.rc.5\",\"chef_root\":\"/usr/lib/ruby/gems/1.8/gems/chef-0.10.4.rc.5/lib\"}},\"ohai_time\":1314126442.40705,\"current_user\":\"dan\",\"os_version\":\"2.6.32-309-ec2\",\"recipes\":[\"dynect\",\"xfs\",\"ubuntu\",\"screen\",\"sudo\",\"perl\",\"runit\",\"zsh\",\"git\",\"build-essential\",\"motd\",\"mlocate\",\"users::sysadmins\",\"users::system\",\"opscode-github\",\"ssh\",\"ntp\",\"ebs-snapshots\",\"munin::client\",\"nagios::client\",\"openssl\",\"postfix\",\"hardware\",\"troubleshooting\",\"vim\",\"emacs\",\"bundler\",\"opscode-benchmark\",\"annoyances\",\"logrotate\",\"estatsd::client\",\"splunk42::client\",\"splunk42::chef-app\",\"splunk42::unix-app\",\"splunk42::opscode-platform-app\",\"wordpress\",\"corpsite-www-nanoc\",\"network\",\"splunk42\"],\"hostname\":\"nanoc-wp-rspreprod-i-cleanup\",\"uptime\":\"206 days 01 hours 23 minutes 06 seconds\",\"macaddress\":\"12:31:39:15:5e:2b\",\"ec2\":{\"public_hostname\":\"ec2-184-73-134-178.compute-1.amazonaws.com\",\"placement_availability_zone\":\"us-east-1c\",\"block_device_mapping_root\":\"/dev/sda1\",\"profile\":\"default-paravirtual\",\"public_keys_0_openssh_key\":\"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCn+3l8chLT68XMXm3/ZL4BAWBVDsKK+wFj1gTxwkUsxrD2hp7lnXtPujo0kizWCP10ptaWhZZf/r7uSduSipcIUBm/RpNo3awnh6smAUmyB8RzckRVCkMr2hf9E8El9717Y1u3TXstyqhh3n//TCjm0fgxAjAanIIqlancUA2uqHI6EzQweIIYY6vWkRt+rLuc4yP1dUitLMPH4LRhUyXkZTOikSzboK3PxUcmrphy5FnAaJQ0FgAyTCBTPRW3aTAPQwGkRguI6ECMaITebq4CiemHsrIXKZqT6B31Eswr7gbwvI/XvEV7w/L4rrz6RzQCfsLcEZgxbCKYyfl8PZOX opscode-preprod-20101208\\n\",\"instance_id\":\"i-cleanup\",\"instance_type\":\"m1.small\",\"block_device_mapping_ephemeral0\":\"/dev/sda2\",\"local_ipv4\":\"10.207.93.213\",\"local_hostname\":\"domU-12-31-39-15-5E-2B.compute-1.internal\",\"reservation_id\":\"r-73d0c619\",\"public_ipv4\":\"184.73.134.178\",\"kernel_id\":\"aki-6603f70f\",\"hostname\":\"domU-12-31-39-15-5E-2B.compute-1.internal\",\"ami_id\":\"ami-9a9c6bf3\",\"block_device_mapping_swap\":\"sda3\",\"ami_launch_index\":\"0\",\"ami_manifest_path\":\"(unknown)\",\"security_groups\":[\"rs-preprod\",\"platform-lb\"],\"block_device_mapping_ami\":\"/dev/sda1\",\"userdata\":null},\"roles\":[\"nanoc-builder\",\"wordpress\",\"nanoc-wp\",\"rs-preprod\",\"splunk\",\"base\"],\"cloud\":{\"public_hostname\":\"ec2-184-73-134-178.compute-1.amazonaws.com\",\"public_ips\":[\"184.73.134.178\"],\"private_ips\":[\"10.207.93.213\"],\"local_ipv4\":\"10.207.93.213\",\"public_ipv4\":\"184.73.134.178\",\"local_hostname\":\"domU-12-31-39-15-5E-2B.compute-1.internal\",\"provider\":\"ec2\"},\"platform\":\"ubuntu\",\"uptime_seconds\":17803386},\"chef_environment\":\"_default\",\"run_list\":[\"role[nanoc-wp]\",\"role[nanoc-builder]\",\"role[rs-preprod]\"],\"chef_type\":\"node\"}">>}.


clear_nodes() ->
    ok.
-endif.


%% Sandbox Tests

sandbox_rows() ->
  [
   [{<<"sandbox_id">>, <<"deadbeefdeadbeefdeadbeefdeadbeef">>},
    {<<"org_id">>, <<"abad1deaabad1deaabad1deaabad1dea">>},
    {<<"created_at">>, {{2012,4,25},{3,7,43.0}}},
    {<<"checksum">>, <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>},
    {<<"uploaded">>, false}],
   [{<<"sandbox_id">>, <<"deadbeefdeadbeefdeadbeefdeadbeef">>},
    {<<"org_id">>, <<"abad1deaabad1deaabad1deaabad1dea">>},
    {<<"created_at">>, {{2012,4,25},{3,7,43.0}}},
    {<<"checksum">>, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>},
    {<<"uploaded">>, true}],
   [{<<"sandbox_id">>, <<"deadbeefdeadbeefdeadbeefdeadbeef">>},
    {<<"org_id">>, <<"abad1deaabad1deaabad1deaabad1dea">>},
    {<<"created_at">>, {{2012,4,25},{3,7,43.0}}},
    {<<"checksum">>, <<"cccccccccccccccccccccccccccccccc">>},
    {<<"uploaded">>, false}],
   [{<<"sandbox_id">>, <<"deadbeefdeadbeefdeadbeefdeadbeef">>},
    {<<"org_id">>, <<"abad1deaabad1deaabad1deaabad1dea">>},
    {<<"created_at">>, {{2012,4,25},{3,7,43.0}}},
    {<<"checksum">>, <<"dddddddddddddddddddddddddddddddd">>},
    {<<"uploaded">>, true}]
  ].

sandbox_join_rows_to_record_test_() ->
    [
     {"Condenses several sandboxed checksum rows into a single sandbox record",
      fun() ->
              ?assertEqual(chef_sql:sandbox_join_rows_to_record(sandbox_rows()),
                           #chef_sandbox{id = <<"deadbeefdeadbeefdeadbeefdeadbeef">>,
                                         org_id = <<"abad1deaabad1deaabad1deaabad1dea">>,
                                         created_at = {{2012,4,25},{3,7,43.0}},
                                         checksums = [
                                                      {<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, false},
                                                      {<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>, true},
                                                      {<<"cccccccccccccccccccccccccccccccc">>, false},
                                                      {<<"dddddddddddddddddddddddddddddddd">>, true}
                                                     ]})
      end}
    ].
