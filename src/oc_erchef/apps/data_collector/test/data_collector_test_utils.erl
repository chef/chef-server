-module(data_collector_test_utils).

-include_lib("eunit/include/eunit.hrl").

-export([setup/1,
         cleanup/1,
         run_tests/2
        ]).

run_tests(MockedModules, Tests) ->
    {foreach,
     fun() -> data_collector_test_utils:setup(MockedModules) end,
     fun(_) -> data_collector_test_utils:cleanup(MockedModules) end,
     Tests
    }.

setup(MockedModules) ->
    [meck:new(M) || M <- MockedModules],
    Env = [
        {timeout, 3000},
        {init_count, 2},
        {max_count, 2},
        {cull_interval, {1, min}},
        {max_age, {70, sec}},
        {max_connection_duration, {70, sec}},
        {ibrowse_options, [{connect_timeout, 10000}]}
    ],
    [application:set_env(data_collector, Name, Value) || {Name, Value} <- Env],
    lager:start(),
    application:start(pooler),
    application:start(ibrowse),
    application:start(opscoderl_httpc),
    ok.

cleanup(MockedModules) ->
    application:stop(data_collector),
    application:stop(opscoderl_httpc),
    application:stop(ibrowse),
    application:stop(pooler),
    [ ?assert(meck:validate(M)) || M <- MockedModules],
    [ meck:unload(M) || M <- MockedModules],
    ok.
