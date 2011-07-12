-module(chef_keyring_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> application:set_env(chef_common, keyring,
                                            [{webui, "../test/testkey.pem"}]),
                        chef_keyring:start_link() end).

lookup_test_() ->
    {foreach, ?SETUP,
     [fun() ->
              ?assertMatch({ok, <<"test.">>},
                             chef_keyring:get_key(webui)) end,
      fun() ->
              ?assertMatch({error, unknown_key},
                           chef_keyring:get_key(admin)) end]}.
