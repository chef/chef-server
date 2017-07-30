%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%% @author Dave Parfitt <dparfitt@chef.io>
%% Copyright 2015 Chef Software, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(chef_wm_actions_queue_monitoring_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, {parse_transform, lager_transform}]).

-define(ORG_NAME, <<"testorg">>).
-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000002">>).

-define(CLIENT_NAME, <<"client1">>).
-define(CLIENT_NAME2, <<"client2">>).
-define(CLIENT_AUTHZ_ID, <<"00000000000000000000000000000003">>).
-define(ACTIONS_EXCHANGE, <<"actions">>).

-define(Q_SETTING(K, V),
        oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(K,V)).

init_per_suite(InitialConfig) ->
    UseFakeRabbit =
         case os:getenv("TRAVIS") of
             false -> false;
             _ -> true
         end,
    [{test_vhost, "/testing_vhost"},
     {test_password, "chef123"},
     {max_length, 10},
     {use_fake_rabbit, UseFakeRabbit}]
        ++ InitialConfig.

end_per_suite(_Config) ->
    ok.


init_per_testcase(_,  Config) ->
    % ensure rabbit starts from a clean slate
    case ?config(use_fake_rabbit, Config) of
        false -> teardown_rabbit(Config);
        true -> ok
    end,

    Config.

end_per_testcase(_, Config) ->
    case ?config(use_fake_rabbit, Config) of
        true ->
            catch(exit(whereis(fake_rabbit), kill)),
            meck:unload(bunnyc),
            meck:unload(chef_wm_rabbitmq_management);
        false -> ok
    end,
    application:set_env(oc_chef_wm, enable_actions, false),
    %% leave rabbit up for troubleshooting failed tests
    setup_helper:base_end_per_suite(Config),
    setup_helper:unmock_authz(),
    Config.

all() -> [basic_queue_monitor, queue_full_dont_start].


basic_queue_monitor(Config) ->
    setup_helper:mock_authz(?CLIENT_AUTHZ_ID),

    default_env_with_queue_monitor(Config),

    setup_rabbit(Config),

    _FinalConfig = setup_chef(Config),

    %% create 5 databags, queue should not be at capacity
    [ {ok, "201", _, _} = make_data_bag(?CLIENT_NAME, X) || X <- lists:seq(1,5)],
    Status = chef_wm_actions_queue_monitoring:status(),
    ?assertEqual(10, proplists:get_value(max_length, Status)),
    % make a sixth data bag
    make_data_bag(?CLIENT_NAME, 6),
    % make sure the timer is working
    ok = wait_until_next_check(),
    % wait until rabbit reports 6 messages in the queue
    ok = wait_until_queue_length(6),

    % queue should not be at capacity
    Status2 = chef_wm_actions_queue_monitoring:status(),
    ?assert(proplists:get_value(check_count, Status2) > 0),
    ?assertEqual(false, proplists:get_value(queue_at_capacity, Status2)),
    ?assertEqual(6, proplists:get_value(last_recorded_length, Status2)),
    % no messages dropped
    ?assertEqual(0, proplists:get_value(total_dropped, Status2)),

    make_data_bag(?CLIENT_NAME, 7),
    make_data_bag(?CLIENT_NAME, 8),
    make_data_bag(?CLIENT_NAME, 9),
    make_data_bag(?CLIENT_NAME, 10),

    % wait until rabbit reports 10 messages in the queue
    % there won't be 11 because max-length is set on the queue
    ok = wait_until_queue_length(10),
    Status3 = chef_wm_actions_queue_monitoring:status(),
    ?assert(proplists:get_value(check_count, Status3) > 0),
    ?assertEqual(true, proplists:get_value(queue_at_capacity, Status3)),
    ?assertEqual(10, proplists:get_value(last_recorded_length, Status3)),
    ?assertEqual(0, proplists:get_value(total_dropped, Status3)),

    % these messages should now be dropped as queue_at_capacity is true
    make_data_bag(?CLIENT_NAME, 11),
    make_data_bag(?CLIENT_NAME, 12),
    ok = wait_until_next_check(),
    Status4 = chef_wm_actions_queue_monitoring:status(),
    ?assert(proplists:get_value(check_count, Status4) > 0),
    ?assertEqual(true, proplists:get_value(queue_at_capacity, Status4)),
    ?assertEqual(10, proplists:get_value(last_recorded_length, Status4)),
    % data bags 11 and 12 were dropped
    ?assertEqual(2, proplists:get_value(total_dropped, Status4)),


    % don't drop messages from now on
    oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(drop_on_full_capacity, false),

    make_data_bag(?CLIENT_NAME, 13),
    make_data_bag(?CLIENT_NAME, 14),
    ok = wait_until_next_check(),
    Status5 = chef_wm_actions_queue_monitoring:status(),
    ?assert(proplists:get_value(check_count, Status5) > 0),
    ?assertEqual(true, proplists:get_value(queue_at_capacity, Status5)),
    ?assertEqual(10, proplists:get_value(last_recorded_length, Status5)),
    ?assertEqual(2, proplists:get_value(total_dropped, Status5)),


    get_alaska_msg(Config),
    get_alaska_msg(Config),
    get_alaska_msg(Config),
    get_alaska_msg(Config),
    get_alaska_msg(Config),

    wait_until_not_at_capacity(),
    Status6 = chef_wm_actions_queue_monitoring:status(),
    ?assert(proplists:get_value(check_count, Status6) > 0),
    ?assertEqual(false, proplists:get_value(queue_at_capacity, Status6)),
    ?assertEqual(5, proplists:get_value(last_recorded_length, Status6)),
    ?assertEqual(2, proplists:get_value(total_dropped, Status6)),
    ok.

queue_full_dont_start(Config) ->
    setup_helper:mock_authz(?CLIENT_AUTHZ_ID),
    default_env_with_queue_monitor(Config),
    MaxLength = ?config(max_length, Config),
    ?Q_SETTING(prevent_erchef_startup_on_full_capacity, true),

    setup_rabbit(Config),

    %% one more meck for this test
    case ?config(use_fake_rabbit, Config) of
        true ->
            meck:expect(chef_wm_rabbitmq_management, sync_check_queue_at_capacity,
                        fun(_PoolNameAtom, _Vhost, _Queue) ->
                                {MaxLength, MaxLength, true}
                        end);
        false -> ok
    end,

    ibrowse:start(),
    [ begin
          ct:pal("Publishing msg ~p to queue~n", [X]),
          put_alaska_msg(Config)
      end || X <- lists:seq(1,15)],
    ibrowse:stop(),

    %% TODO: There are about 500000000 reasons setup_chef could fail considering
    %% the amount of mocking and cargo culting in that path. The idea of this
    %% test is that it would only fail if we hit the queue at capacity check in
    %% the oc_chef_wm_sup. FURTHER, I'm pretty sure ?assert works via an
    %% exception which will then be caught, so this test basically always passes
    %% currently.
    try
        _FinalConfig = setup_chef(Config),
        ?assert(false)
    catch
        Error:Reason ->
            ct:pal("Erchef failed to start (this is expected): ~p~n~p~n", [Error, Reason])
    end,
    ok.

wait_until_not_at_capacity() ->
    wait_until(fun () ->
                CurrentStatus = chef_wm_actions_queue_monitoring:status(),
                false == proplists:get_value(queue_at_capacity, CurrentStatus)
               end, 600, 100).

wait_until_queue_length(N) ->
    wait_until(fun () ->
                CurrentStatus = chef_wm_actions_queue_monitoring:status(),
                N == proplists:get_value(last_recorded_length, CurrentStatus)
               end, 600, 100).

wait_until_next_check() ->
    InitialStatus = chef_wm_actions_queue_monitoring:status(),
    NextCheck = proplists:get_value(check_count, InitialStatus) + 1,
    wait_until(fun () ->
                CurrentStatus = chef_wm_actions_queue_monitoring:status(),
                NextCheck == proplists:get_value(check_count, CurrentStatus)
               end, 600, 100).


wait_until(Fun, Retry, Delay) when Retry > 0 ->
    Res = Fun(),
    case Res of
        true ->
            ok;
        _ when Retry == 1 ->
            {fail, Res};
        _ ->
            timer:sleep(Delay),
            wait_until(Fun, Retry-1, Delay)
    end.


default_rabbit_config(Config) ->
    Password = ?config(test_password, Config),
    BinPassword = list_to_binary(Password),
    [
        {management,
        [{user, <<"rabbitmgmt">>},
            {port, 15672},
            {password, BinPassword},
            % rabbitmq management http connection pool
            {rabbitmq_management_service,
            [{root_url, "http://127.0.0.1:15672/api"},
            {timeout, 30000},
            {init_count, 25},
            {max_count, 100},
            {cull_interval, {60, sec}},
            {max_age, {70, sec}},
            {max_connection_duration, {70, sec}},
            {ibrowse_options,
            [{connect_timeout, 10000},
                {basic_auth,
                {"rabbitmgmt",
                Password}}]}
            ]}]},
        {monitoring,
        [{queue_length_monitor_enabled, true},
            {queue_length_monitor_vhost, "/analytics"},
            {queue_length_monitor_queue, alaska },
            {queue_length_monitor_millis, 30000 },
            {queue_length_monitor_timeout_millis, 5000 },
            {drop_on_full_capacity, true },
            {prevent_erchef_startup_on_full_capacity, false},
            {queue_at_capacity_affects_overall_status, false}
        ]}
    ].



default_env_with_queue_monitor(Config) ->
    application:set_env(oc_chef_wm, rabbitmq, default_rabbit_config(Config)),

    ?Q_SETTING(queue_length_monitor_enabled, true),
    ?Q_SETTING(queue_length_monitor_vhost, ?config(test_vhost, Config)),
    ?Q_SETTING(queue_length_monitor_queue, "alaska"),
    ?Q_SETTING(queue_length_monitor_millis, 250),
    ?Q_SETTING(drop_on_full_capacity, true),
    ?Q_SETTING(prevent_erchef_startup_on_full_capacity, false),

    application:set_env(oc_chef_wm, enable_actions, true),
    application:set_env(oc_chef_wm, actions_host, "127.0.0.1"),
    application:set_env(oc_chef_wm, actions_port, 5672),
    application:set_env(oc_chef_wm, actions_user, <<"actions">>),
    application:set_env(oc_chef_wm, actions_password, list_to_binary(?config(test_password, Config))),
    application:set_env(oc_chef_wm, actions_vhost, list_to_binary(?config(test_vhost, Config))),
    application:set_env(oc_chef_wm, actions_exchange, ?ACTIONS_EXCHANGE),
    application:set_env(oc_chef_wm, actions_fqdn, <<"localhost">>).

setup_chef(InitialConfig) ->
    Config = chef_test_db_helper:start_db([{app, oc_chef_wm}|InitialConfig], "oc_chef_wm_itests"),
    Config2 = setup_helper:start_server(Config),
    make_org(?ORG_NAME, ?ORG_AUTHZ_ID),
    {OrgId, _} = chef_db:fetch_org_metadata(context(), ?ORG_NAME),
    {ok, PubKey} = file:read_file(filename:join([?config(data_dir, Config2), "..","spki_public.pem"])),
    {ok, AltPubKey} = file:read_file(filename:join([?config(data_dir, Config2), "..","public.pem"])),
    {ok, PrivateKeyRE} = re:compile(".*BEGIN (RSA )?PRIVATE KEY.*"),
    {ok, PubKeyRE} = re:compile(".*BEGIN (RSA )?PUBLIC KEY.*"),
    FinalConfig = [{org_id, OrgId},
                   {pubkey, PubKey},
                   {alt_pubkey, AltPubKey},
                   {pubkey_regex, PubKeyRE},
                   {privkey_regex, PrivateKeyRE}
                  ] ++ Config2,
    make_client(FinalConfig, ?CLIENT_NAME),
    FinalConfig.


setup_rabbit(Config) ->
    UseFakeRabbit = ?config(use_fake_rabbit, Config),
    case UseFakeRabbit of
        true -> setup_rabbit_fake(Config);
        false -> setup_rabbit_real(Config)
    end.

teardown_rabbit(Config) ->
    UseFakeRabbit = ?config(use_fake_rabbit, Config),
    case UseFakeRabbit of
      true -> ok;
      false ->
        Vhost = ?config(test_vhost, Config),
        rabbitmqctl(["delete_vhost", to_str("\"~p\"", [Vhost])])
    end.


setup_rabbit_fake(Config) ->
   start_fake_rabbit(?config(max_length, Config)),

    meck:new(bunnyc),
    meck:expect(bunnyc, start_link, fun(_, _, _, _) ->
                                               {ok, self()}
                                       end),

    meck:expect(bunnyc, publish,
                fun(_Server, _RoutingKey, _PersistentData) ->
                    %io:format(user, "CAUGHT BUNNYC:PUBLISH ~p ~p ~p~n",
                    %          [Server, RoutingKey, PersistentData]),
                    fake_rabbit_enqueue(),
                    ok
                end),


    meck:new(chef_wm_rabbitmq_management, [passthrough]),


    meck:expect(chef_wm_rabbitmq_management, get_max_length,
                fun(_PoolNameAtom, _Vhost) ->
                        ?config(max_length, Config)
                end),

    meck:expect(chef_wm_rabbitmq_management, get_current_length,
                fun(_PoolNameAtom, _Vhost, _Queue) ->
                        fake_rabbit_current_length()
                end),
    ok.



setup_rabbit_real(Config) ->
    ibrowse:start(),
    Vhost = ?config(test_vhost, Config),
    Password = ?config(test_password, Config),
    prereq("rabbitmqctl"),
    prereq("rabbitmq-plugins"),
    prereq("rabbitmq-server"),
    rabbitmqctl(["add_vhost", to_str("\"~p\"", [Vhost])]),
    rabbitmqctl(["add_user", "rabbitmgmt", Password]),
    rabbitmqctl(["add_user", "actions", Password]),

    all_permissions("/", "rabbitmgmt"),
    all_permissions(Vhost, "rabbitmgmt"),

    all_permissions("/", "actions"),
    all_permissions(Vhost, "actions"),

    %% This is a lie, as rabbitmq needs to be restarted after the
    %% plugin is installed
    cmd(["rabbitmq-plugins", "enable", "rabbitmq_management"]),

    rabbitmqctl(["set_user_tags", "rabbitmgmt", "administrator"]),

    MaxLength = ?config(max_length, Config),

    MaxLengthPolicy = lists:flatten(io_lib:format("'{\"max-length\":~p}'", [MaxLength])),

    rabbitmqctl(["set_policy", "-p", Vhost, "max_length",
                 "'(erchef|alaska|notifier.notifications|notifier_config)'",
                 MaxLengthPolicy , "--apply-to", "queues"]),

    rabbitmq_mgmt(escape_to_str("/api/queues/~s/alaska", Vhost), put,
                  "{\"auto_delete\":false,\"durable\":true,\"arguments\":[]}",
                  "rabbitmgmt", Password),
    rabbitmq_mgmt(escape_to_str("/api/exchanges/~s/actions", Vhost), put,
                  "{\"type\":\"topic\",\"auto_delete\":false,\"durable\":true,\"arguments\":[]}",
                  "rabbitmgmt", Password),
    rabbitmq_mgmt(escape_to_str("/api/bindings/~s/e/actions/q/alaska", Vhost), post,
                  "{\"routing_key\":\"#\",\"arguments\":[]}",
                  "rabbitmgmt", Password),
    % erchef is unhappy if ibrowse is already running
    ibrowse:stop(),
    ok.


put_alaska_msg(Config) ->
    UseFakeRabbit = ?config(use_fake_rabbit, Config),
    case UseFakeRabbit of
        true ->
            fake_rabbit_enqueue();
        false ->
            Password = ?config(test_password, Config),
            User = "rabbitmgmt",
            Vhost = ?config(test_vhost, Config),
            Path = escape_to_str("/api/exchanges/~s/actions/publish", Vhost),
            Body = "{\"properties\":{},\"routing_key\":\"my key\",\"payload\":\"my body\",\"payload_encoding\":\"string\"}",
            rabbitmq_mgmt(Path, post, Body, User, Password)
    end.


get_alaska_msg(Config) ->
    Password = ?config(test_password, Config),
    UseFakeRabbit = ?config(use_fake_rabbit, Config),
    case UseFakeRabbit of
        true -> fake_rabbit_dequeue();
        false ->
            User = "rabbitmgmt",
            Vhost = ?config(test_vhost, Config),
            Path = escape_to_str("/api/queues/~s/alaska/get", Vhost),
            Body = "{\"count\":1,\"requeue\":false,\"encoding\":\"auto\",\"truncate\":50000}",
            Url = "http://localhost:15672" ++ Path,
            % this request must use accept */*
            ibrowse:send_req(Url,
                            [{"accept", "*/*"},
                            {"content-type", "application/json"}],
                            post,
                            Body,
                            [{basic_auth, {User, Password}}])
    end.

% make an http call to the rabbitmq mgmt console
rabbitmq_mgmt(Path, Method, Body, User, Password) ->
    Url = "http://localhost:15672" ++ Path,
    ibrowse:send_req(Url,
                     [{"accept", "application/json"},
                      {"content-type", "application/json"}],
                     Method,
                     Body,
                     [{basic_auth, {User, Password}}]).



cmd(List) ->
    Command = space_join(List),
    ct:pal("Running Command: ~s~n", [Command]),
    Output = os:cmd(Command),
    ct:pal("Output: ~s~n", [Output]),
    Output.


% can we find the executable on the command line?
prereq(Cmd) ->
    case os:find_executable(Cmd) of
        false ->
            ct:pal("could not find executable: ~s~n", [Cmd]),
            ?assert(false);
        _ ->
            %%Found it!
            true
    end.


% it's like a join, but from a different universe
space_join(L) ->
    [ [Elt, " "] || Elt <- L ].


make_data_bag(Requestor, Id) ->
    DataBag =  to_str("{\"name\":\"foobar~p\"}",[Id]),
    Url = "http://localhost:8000/organizations/testorg/data",
    ibrowse:send_req(Url, [{"x-ops-userid", binary_to_list(Requestor)},
                           {"accept", "application/json"},
                           {"content-type", "application/json"}], post,
                     DataBag).


% Some helpers to keep noise out of the tes.
make_org(OrgName, OrgAuthzId) ->
    Org = chef_object:new_record(oc_chef_organization, ?API_MIN_VER, nil, OrgAuthzId,
                                 {[{<<"name">>, OrgName}, {<<"full_name">>, OrgName}]}),
    chef_db:create(Org, context(), OrgAuthzId).

make_client(Config, Name) ->
    OrgId = proplists:get_value(org_id, Config),
    PubKey = proplists:get_value(pubkey, Config),
    Client = chef_object:new_record(chef_client, ?API_MIN_VER, OrgId, ?CLIENT_AUTHZ_ID,
                                    {[{<<"name">>, Name},
                                      {<<"validator">>, true},
                                      {<<"admin">>, true},
                                      {<<"public_key">>, PubKey}]}),
    chef_db:create(Client, context(), ?CLIENT_AUTHZ_ID).


context() ->
    chef_db:make_context(?API_MIN_VER, <<"AB">>, no_header).


rabbitmqctl(Subcmds) ->
    cmd(["rabbitmqctl"] ++ Subcmds).

all_permissions(Vhost, User) ->
    rabbitmqctl(["set_permissions","-p",Vhost,User,"\".*\"", "\".*\"", "\".*\""]).

% format and flatten
to_str(P, Q) ->
  lists:flatten(io_lib:format(P, Q)).

% formant and flatten, url encode the second parameter
escape_to_str(Str, Val) ->
  lists:flatten(io_lib:format(Str, [http_uri:encode(Val)])).



% spawn a process that keeps a count of enqueue/dequeues
fake_rabbit(Max, Count) ->
    receive
        inc -> case Count == Max of
                   true -> fake_rabbit(Max, Count);
                   false -> fake_rabbit(Max, Count + 1)
               end;
        dec -> fake_rabbit(Max, Count -1);
        {current_count, Pid} -> Pid ! {current_count, Count},
                                fake_rabbit(Max, Count);
        die -> ok
    end.


start_fake_rabbit(MaxLength) ->
    Pid = spawn(?MODULE, fake_rabbit, [MaxLength, 0]),
    register(fake_rabbit, Pid).

fake_rabbit_enqueue() ->
    whereis(fake_rabbit) ! inc.

fake_rabbit_dequeue() ->
    whereis(fake_rabbit) ! dec.

fake_rabbit_current_length() ->
    whereis(fake_rabbit) ! {current_count, self()},
    receive
        {current_count, N} -> N
    end.
