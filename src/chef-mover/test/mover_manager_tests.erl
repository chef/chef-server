-module(mover_manager_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("eunit/include/eunit.hrl").

%% HOAX_FIXTURE(fun Setup/0, fun Teardown/1) injects a new test generator named
%% hoax_fixture_tests_.  This fixture will include all zero arity functions in
%% the test module as tests.  The fixture is a foreach test where each test is
%% run within the context of setup/teardown.
?HOAX_FIXTURE(fun() -> mover_manager:start_link() end,
              fun(_Pid) -> mover_manager:stop() end).

set_concurency_should_return_error_when_not_integer() ->
    ?assertEqual({error, {badarg, -3}}, mover_manager:set_concurrency(-3)).

set_concurency_should_return_error_bad_state_when_not_working() ->
    ?assertEqual({error, bad_state}, mover_manager:set_concurrency(3)).

set_concurency_should_return_ok_modified_value_when_working_and_new_value() ->
    mover_manager:set_state(working),
    ?assertEqual({ok, modified, 0}, mover_manager:set_concurrency(3)).

set_concurency_should_return_ok_no_change_when_working_and_old_value() ->
    mover_manager:set_state(working),
    ?assertEqual({ok, modified, 0}, mover_manager:set_concurrency(3)),
    ?assertEqual({ok, no_change}, mover_manager:set_concurrency(3)).
