-module(chef_keyring_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> application:set_env(chef_common, keyring,
                                            [{webui, "../test/testkey.pem"}]),
                        chef_keyring:start_link() end).

lookup_test_() ->
    {foreach, ?SETUP,
     [fun() ->
              {ok, Key} = chef_keyring:get_key(webui),
              ?assertEqual(11, size(Key)),
              Key1 = tuple_to_list(Key),
              ['RSAPrivateKey', 'two-prime', FirstInt|_] = Key1,
              ?assert(is_integer(FirstInt)) end,
      fun() ->
              ?assertMatch({error, unknown_key},
                           chef_keyring:get_key(admin)) end]}.
