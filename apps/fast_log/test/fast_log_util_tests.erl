-module(fast_log_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include("fast_log.hrl").

iso8601_test() ->
    Now = calendar:universal_time(),
    {{Year, Month, Day}, {Hour, Min, Sec}} = Now,
    Fmt = "~4B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    TS = lists:flatten(io_lib:format(Fmt, [Year, Month, Day,
                                           Hour, Min, Sec])),
    ?assertMatch(TS, fast_log_util:time_iso8601(Now)).

log_level_test() ->
    ?assertMatch("DEBUG", fast_log_util:log_level(debug)),
    ?assertMatch("INFO", fast_log_util:log_level(info)),
    ?assertMatch("WARN", fast_log_util:log_level(warn)),
    ?assertMatch("ERR", fast_log_util:log_level(err)),
    ?assertMatch("UNKNOWN", fast_log_util:log_level(foobie)).

should_log_test_() ->
    {setup, fun() ->
                    ets:new(foo, [public, named_table]),
                    Config = #config{min_log=?LOG_INFO},
                    fast_log_util:put_config(foo, Config) end,
     fun(_) -> ok end,
     [{"Verify info logging when info is configured",
       fun() ->
               ?assert(fast_log_util:should_log(foo, info)) end},
      {"Verify no debug logging when info is configured",
       fun() ->
               ?assertNot(fast_log_util:should_log(foo, debug)) end},
      {"Verify error logging when info is configured",
      fun() ->
               ?assert(fast_log_util:should_log(foo, err)) end}]}.
