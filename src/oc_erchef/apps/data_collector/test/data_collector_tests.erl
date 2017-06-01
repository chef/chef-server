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

token_test_() ->
        [{
          "returns token as list when token is found",
          fun() ->
              meck:expect(chef_secrets, get, [<<"data_collector">>, <<"token">>], {ok, <<"secret token">>}),
              ?assertEqual("secret token", data_collector:token())
          end
         },
         {
          "returns undefined as atom when token is not found",
          fun() ->
              meck:expect(chef_secrets, get, [<<"data_collector">>, <<"token">>], {error, not_found}),
              ?assertEqual(undefined, data_collector:token())
          end
         }].

is_enabled_test_() ->
        [{
          "when root_url is set returns true",
          fun() ->
              application:set_env(data_collector, root_url, "http://data_collector"),
              ?assertEqual(true, data_collector:is_enabled())
          end
         },
         {
          "when root_url is unset returns false",
          fun() ->
              application:unset_env(data_collector, root_url, "http://data_collector"),
              ?assertEqual(false, data_collector:is_enabled())
          end
         }].
