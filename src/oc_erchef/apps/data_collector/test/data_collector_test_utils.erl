-module(data_collector_test_utils).

-include_lib("eunit/include/eunit.hrl").

-export([setup/1,
         setup_chef_secrets/0,
         cleanup/1,
         run_tests/2
        ]).

run_tests(MockedModules, Tests) ->
    {foreach,
     fun() -> data_collector_test_utils:setup(MockedModules) end,
     fun(_) -> data_collector_test_utils:cleanup(MockedModules) end,
     Tests
    }.

setup_chef_secrets() ->
    application:set_env(chef_secrets, provider, chef_secrets_mock_provider),
    application:set_env(chef_secrets, provider_config, []),
    {ok, FileContent} = file:read_file(filename:join(code:priv_dir(data_collector), "../test/secrets.json")),
    FakeSecretsData = jiffy:decode(FileContent),
    meck:new(chef_secrets_mock_provider, [non_strict]),
    meck:expect(chef_secrets_mock_provider, read, fun(_Config) -> {ok, FakeSecretsData} end),
    application:ensure_all_started(chef_secrets).

setup(MockedModules) ->
    [meck:new(M) || M <- MockedModules],
    Env = [
        {timeout, 3000},
        {init_count, 2},
        {max_count, 2},
        {cull_interval, {1, min}},
        {max_age, {70, sec}},
        {max_connection_duration, {70, sec}},
        {retry_on_conn_closed, true},
        {ibrowse_options, [{connect_timeout, 10000}]}
    ],
    [application:set_env(data_collector, Name, Value) || {Name, Value} <- Env],
    lager:start(),
    setup_chef_secrets(),
    application:start(pooler),
    application:start(ibrowse),
    application:start(opscoderl_httpc),
    ok.

cleanup(MockedModules) ->
    application:stop(data_collector),
    application:stop(chef_secrets),
    application:stop(opscoderl_httpc),
    application:stop(ibrowse),
    application:stop(pooler),
    [ ?assert(meck:validate(M)) || M <- MockedModules],
    meck:unload(),
    ok.
