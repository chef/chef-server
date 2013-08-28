-module(chef_wm_enforce_tests).

-include_lib("eunit/include/eunit.hrl").

max_size_test_() ->
    DefaultMaxSize = 1000000,
    TunedValue = 5,
    [
     {"Default tests",
      {foreach,
       fun() ->
               meck:new(wrq),
               application:unset_env(chef_wm, max_request_size)
       end,
       fun(_) ->
               meck:unload(wrq)
       end,
       [
        ?_test(max_size_success('POST', DefaultMaxSize)),
        ?_test(max_size_success('PUT', DefaultMaxSize)),
        ?_test(max_size_error('POST', DefaultMaxSize)),
        ?_test(max_size_error('PUT', DefaultMaxSize))
       ]
      }
     },
     {"Tuned tests",
      {foreach,
       fun() ->

               meck:new(wrq),
               application:set_env(chef_wm, max_request_size, TunedValue)
       end,
       fun(_) ->
               meck:unload(wrq),
               application:unset_env(chef_wm, max_request_size)
       end,
       [
        ?_test(max_size_success('POST', TunedValue)),
        ?_test(max_size_success('PUT', TunedValue)),
        ?_test(max_size_error('POST', TunedValue)),
        ?_test(max_size_error('PUT', TunedValue))
       ]
      }
     },
     {"Disabled tests",
      {foreach,
       fun() ->
               application:set_env(chef_wm, max_request_size, disabled)
       end,
       fun(_) ->
               application:unset_env(chef_wm, max_request_size)
       end,
       [
        fun(_) -> ?_test(disabled_max_size()) end
       ]
      }
     },
     {"Incorrect configuration tests",
      {foreach,
       fun() ->
               application:unset_env(chef_wm, max_request_size)
       end,
       fun(_) ->
               application:unset_env(chef_wm, max_request_size)
       end,
       [
        ?_test(config_error(0)),
        ?_test(config_error(-1)),
        ?_test(config_error(not_disabled))
       ]
      }
     }
    ].

max_size_success(Method, MaxSize) ->
    meck:expect(wrq, method, 1, Method),
    meck:expect(wrq, set_max_recv_body, 2, req),
    meck:expect(wrq, req_body, 1, ignored),
    ?assertEqual(req, chef_wm_enforce:max_size(req)),
    ?assert(meck:called(wrq, set_max_recv_body, [MaxSize, req])).

max_size_error(Method, MaxSize) ->
    meck:expect(wrq, method, 1, Method),
    meck:expect(wrq, set_max_recv_body, 2, req),
    meck:expect(wrq, req_body, fun(_) -> exit("request body too large") end),
    ErrorMessage = list_to_binary("JSON must be no more than " ++ integer_to_list(MaxSize) ++ " bytes."),
    ?assertThrow({too_big, ErrorMessage  }, chef_wm_enforce:max_size(req)),
    ?assert(meck:called(wrq, set_max_recv_body, [MaxSize, req])).

disabled_max_size() ->
    ?assertEqual(req, chef_wm_enforce:max_size(req)).

config_error(Val) ->
    application:set_env(chef_wm, max_request_size, Val),
    ?assertError(config_bad_type, chef_wm_enforce:max_size(req)).

max_size_when_get_should_return_req_test() ->
    meck:new(wrq),
    try
        meck:expect(wrq, method, 1, 'GET'),
        ?assertEqual(req, chef_wm_enforce:max_size(req)),
        ?assert(meck:called(wrq, method, [req]))
    after
        meck:unload(wrq)
    end.
