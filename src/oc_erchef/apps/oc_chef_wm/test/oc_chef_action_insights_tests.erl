-module(oc_chef_action_insights_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

-record(state, {io_device = list_to_pid("<0.1.0>")}).

fixture_test_() ->
    hoax:fixture(?MODULE).

init_opens_file_for_appending() ->
    FileLoc = "/tmp/insights.log",
    IODevice = list_to_pid("<0.1.0>"),

    hoax:mock(envy,
        ?expect(get,
            ?withArgs([oc_chef_wm, insights_log, string]),
            ?andReturn(FileLoc))),
    hoax:mock(file,
        ?expect(open,
            ?withArgs([FileLoc, [append]]),
            ?andReturn({ok, IODevice}))),

    Expected = {ok, #state{}},
    Actual = oc_chef_action_insights:init([]),
    ?assertEqual(Expected, Actual),
    ?verifyAll.

terminate_closes_io_device() ->
    IODevice = list_to_pid("<0.1.0>"),

    hoax:mock(file,
        ?expect(close,
            ?withArgs([IODevice]),
            ?andReturn(ok))),
    Expected = ok,
    Actual = oc_chef_action_insights:terminate(reason, #state{}),
    ?assertEqual(Expected, Actual),
    ?verifyAll.

handle_cast_writes_request_to_file() ->
    %% Lazy way to not escape a bunch of strings
    InsightsEjson = {[{<<"Key">>, <<"Val">>}]},
    InsightsJson = jiffy:encode(InsightsEjson),
    IODevice = list_to_pid("<0.1.0>"),

    hoax:mock(file,
              ?expect(write,
                      ?withArgs([IODevice, [InsightsJson, "\n"]]),
                      ?andReturn(ok))),
    Expected = {noreply, #state{}},
    Actual = oc_chef_action_insights:handle_cast({ingest, InsightsJson}, #state{}),
    ?assertEqual(Expected, Actual),
    ?verifyAll.
