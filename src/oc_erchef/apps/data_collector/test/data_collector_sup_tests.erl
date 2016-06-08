-module(data_collector_sup_tests).

-include_lib("eunit/include/eunit.hrl").

start_data_collector_no_config_test_() ->
    data_collector_test_utils:run_tests(
        [data_collector_http],
        [{
            "does not create http pool when not configured",
            fun() ->
                application:unset_env(data_collector, root_url),
                ?assertMatch(ok, application:start(data_collector)),
                ?assertMatch(ok, application:stop(data_collector))
            end
    }]).

start_data_collector_with_config_test_() ->
    data_collector_test_utils:run_tests(
        [data_collector_http],
        [{
            "creates http pool when configured",
            fun() ->
                application:set_env(data_collector, root_url, "http://data_collector"),
                meck:expect(data_collector_http, create_pool, 0, ok),
                ?assertMatch(ok, application:start(data_collector)),
                meck:expect(data_collector_http, delete_pool, 0, ok),
                ?assertMatch(ok, application:stop(data_collector))
            end
    }]).
