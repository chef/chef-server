-module(data_collector_tests).

-include_lib("eunit/include/eunit.hrl").

%% Validate that our oc_chef_wm_status:check_health() callbacks return properly
ping_with_valid_response_test_() ->
    data_collector_test_utils:run_tests(
        [data_collector_http],
        [{
            "returns ping if the response is valid",
            fun() ->
                meck:expect(data_collector_http, create_pool, 0, ok),
                meck:expect(data_collector_http, get, ["/"], ok),
                application:set_env(data_collector, root_url, "http://data_collector"),
                application:start(data_collector),
                ?assertEqual(pong, data_collector:ping())
            end
    }]).

ping_with_invalid_response_test_() ->
    data_collector_test_utils:run_tests(
        [data_collector_http],
        [{
            "returns pang if the response is invalid",
            fun() ->
                meck:expect(data_collector_http, create_pool, 0, ok),
                meck:expect(data_collector_http, get, ["/"], error),
                application:set_env(data_collector, root_url, "http://data_collector"),
                application:start(data_collector),
                ?assertEqual(pang, data_collector:ping())
            end
    }]).
