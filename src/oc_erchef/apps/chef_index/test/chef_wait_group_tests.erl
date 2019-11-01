-module(chef_wait_group_tests).
-include_lib("eunit/include/eunit.hrl").

start_link_returns_a_pid_test() ->
    {ok, Pid} = chef_wait_group:start_link(fun() -> ok end, []),
    ?assertEqual(true, erlang:is_process_alive(Pid)).

add_takes_a_jobid_and_arguments_test() ->
    {ok, Pid} = chef_wait_group:start_link(fun(MyArg) -> MyArg end, []),
    ?assertEqual(ok, chef_wait_group:add(Pid, test_id1, testarg)),
    ?assertEqual(ok, chef_wait_group:add(Pid, test_id2, [testarg])).

wait_returns_ok_and_the_responses_on_success_test() ->
    {ok, Pid} = chef_wait_group:start_link(fun(MyArg) -> MyArg end, []),
    chef_wait_group:add(Pid, test_id1, testarg),
    ?assertEqual({ok, [{test_id1, testarg}]}, chef_wait_group:wait(Pid)).

wait_returns_error_if_the_worker_fails_test() ->
    {ok, Pid} = chef_wait_group:start_link(fun() -> throw(badfun) end, []),
    chef_wait_group:add(Pid, test_id1, []),
    ?assertEqual({error, [], [{test_id1, {throw, badfun}}]}, chef_wait_group:wait(Pid)).

wait_returns_error_with_finished_jobs_and_failed_jobs_test() ->
    {ok, Pid} = chef_wait_group:start_link(fun(Arg) ->
                                                     Arg = item1,
                                                     ok
                                             end, []),
    chef_wait_group:add(Pid, test_id1, [item1]),
    chef_wait_group:add(Pid, test_id2, [item2]),
    ?assertEqual({error, [{test_id1, ok}], [{test_id2, {error, {badmatch, item1}}}]},
                 chef_wait_group:wait(Pid)).

wait_blocks_until_jobs_are_done_test() ->
    {ok, Pid} = chef_wait_group:start_link(fun(MyArg) ->
                                                     timer:sleep(50),
                                                     MyArg
                                             end, []),
    chef_wait_group:add(Pid, test_id1, testarg),
    ?assertEqual({ok, [{test_id1, testarg}]}, chef_wait_group:wait(Pid)).

gen_server_exits_after_wait_test() ->
    {ok, Pid} = chef_wait_group:start_link(fun() -> ok end, []),
    MonRef = erlang:monitor(process, Pid),
    chef_wait_group:wait(Pid),
    %% Test will timeout if the gen_server doesn't exit
    receive
        {'DOWN', MonRef, process, Pid, normal} -> ok
    end.
