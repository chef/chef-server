%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(opst_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% TEST SERVER CALLBACK FUNCTIONS
%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    ops_app:manual_start(),
    io:format("-----------------------_>~n~n"),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ops_app:manual_stop(),
    ok.

all(doc) ->
    ["This test is designed to validate the test opset configuration system"].

all() ->
    [creation_deletion,
     linking,
     value_manipulation].

%%====================================================================
%% TEST CASES
%%====================================================================

creation_deletion(doc) ->
    ["Describe the main purpose of this test case"];
creation_deletion(suite) ->
    [];
creation_deletion(Config) when is_list(Config) ->
    Name = genname(),
    Key = genname(),
    Value1 = genname(),
    ?assertMatch({ok, _Pid}, opset:create(Name, [{Key, Value1}])),
    ?assertMatch(true, opset:exists(Name)),
    ?assertMatch(Value1, opset:get_value(Key, Name)),
    Value2 = genname(),
    ?assertMatch(ok, opset:set_value(Key, Value2, Name)),
    ?assertMatch(ok, opset:delete(Name)),
    timer:sleep(100),
    ?assertMatch(false, opset:exists(Name)),
    ?assertMatch({ok, _}, opset:create(Name, [])),
    ?assertMatch(true, opset:exists(Name)),
    ?assertThrow({not_found, Name, Key},
                 opset:get_value(Key, Name)).
linking(doc) ->
    ["Describe the main purpose of this test case"];
linking(suite) ->
    [];
linking(Config) when is_list(Config) ->
    Name = genname(),
    Key1 = genname(),
    Value1 = genname(),
    Value2 = genname(),
    opset:create(Name, []),
    opset:link_config(Name),
    opset:set_value(Key1, Value1, Name),
    ConfPid = opset:config_pid(Name),
    receive
        {ConfPid, Name,[{set, [{Key1, Value1}]}]} ->
            ok
    after 10000 ->
            erlang:throw({?MODULE, ?LINE, did_not_receive_expected_exit})
    end,
    ?assertMatch(Value1, opset:get_value(Key1, Name)),
    opset:set_value(Key1, Value2, Name),
    receive
        {ConfPid, Name, [{set, [{Key1, Value2}]}]} ->
            ok
    after 10000 ->
            erlang:throw({?MODULE, ?LINE, did_not_receive_expected_exit})
    end,
    ?assertMatch(Value2, opset:get_value(Key1, Name)).

value_manipulation(doc) ->
    ["Check that values can be manipulated as expected"];
value_manipulation(suite) ->
    [];
value_manipulation(Config) when is_list(Config) ->
    Name = genname(),
    Key1 = genname(),
    Value1 = genname(),
    Value2 = genname(),
    Key2 = genname(),
    Key3 = genname(),
    Key4 = genname(),
    Value1_1 = genname(),
    Value2_1 = genname(),
    Value3 = genname(),
    Value4 = genname(),
    opset:create(Name, []),
    opset:link_config(Name),
    opset:set_value(Key1, Value1, Name),
    ConfPid = opset:config_pid(Name),
    receive
        {ConfPid, Name, [{set, [{Key1, Value1}]}]} ->
            ok
    after 10000 ->
            erlang:throw(did_not_receive_expected_exit)
    end,
    ?assertMatch(Value1, opset:get_value(Key1, Name)),
    opset:set_value(Key1, Value2, Name),
    receive
        {ConfPid, Name, [{set, [{Key1, Value2}]}]} ->
            ok
    after 10000 ->
            erlang:throw({?MODULE, ?LINE, did_not_receive_expected_exit})
    end,
    ?assertMatch(Value2, opset:get_value(Key1, Name)),
    NewConfig = [{Key1, Value1_1},
                 {Key2, Value2_1},
                 {Key3, Value3},
                 {Key4, Value4}],
    opset:set_values(NewConfig, Name),
    receive
        {ConfPid, Name, [{set, NewConfig}]} ->
            ok
    after 10000 ->
            erlang:throw({?MODULE, ?LINE, did_not_receive_expected_exit})
    end,
    %% We should time out on this one since we only expect one exit.
    receive
        {ConfPid, Name, _} ->
            erlang:throw({?MODULE, ?LINE, unexpected_exit})
    after 100 ->
            ok
    end,
    ?assertMatch(Value1_1, opset:get_value(Key1, Name)),
    ?assertMatch(Value2_1, opset:get_value(Key2, Name)),
    ?assertMatch(Value3, opset:get_value(Key3, Name)),
    ?assertMatch(Value4, opset:get_value(Key4, Name)),

    NewConfig1 = [{Key1, Value1},
                  {Key2, Value2}],
    opset:reset_config(NewConfig1, Name),
    receive
        {ConfPid, Name, [{delete, [{Key3, Value3},
                                   {Key4, Value4}]},
                         {set, NewConfig1}]}->
            ok
    after 10000 ->
            erlang:throw({?MODULE, ?LINE, did_not_receive_expected_exit})
    end,
    ?assertMatch(Value1, opset:get_value(Key1, Name)),
    ?assertMatch(Value2, opset:get_value(Key2, Name)),
    ?assertThrow({not_found, Name, Key3}, opset:get_value(Key3, Name)),
    ?assertThrow({not_found, Name, Key4}, opset:get_value(Key4, Name)).

%%====================================================================
%% Utilities
%%====================================================================
genname() ->
    erlang:list_to_atom(random_string(20)).

random_string(0) ->
    [];
random_string(Length) ->
    [random_char() | random_string(Length-1)].

random_char() ->
    random:uniform(95) + 31.
