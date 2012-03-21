%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sx=4 et
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%% Additional testing for doest
%%% @end
%%%-------------------------------------------------------------------
-module(doest_tester).

-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

%%============================================================================
%% Public Api
%%============================================================================
run(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    Constraints = get_constraints(io:get_line(Device, "")),
    ok,
    ok,
    doest:solve(process_packages(read_packages(Device)), Constraints).

data1_test() ->
    ?assertMatch([{"app6","0.0.1"},
                  {"dep_pkg13","0.0.2"},
                  {"app13","0.0.1"},
                  {"dep_pkg2","0.0.5"},
                  {"dep_pkg1","0.0.2"},
                  {"dep_pkg7","0.1.2"},
                  {"app9","0.0.1"}],
                 run(fix_rebar_brokenness("data1.txt"))).

data2_test() ->
    ?assertMatch([{"app18","0.0.1"},
                  {"app4","0.0.1"},
                  {"app1","0.0.1"},
                  {"app6","0.0.1"},
                  {"dep_pkg13","0.0.2"},
                  {"app13","0.0.1"},
                  {"dep_pkg5","0.0.2"},
                  {"dep_pkg1","0.0.2"},
                  {"dep_pkg2","0.0.5"},
                  {"dep_pkg7","0.1.2"},
                  {"app9","0.0.1"},
                  {"dep_pkg16","1.0.2"}],
                 run(fix_rebar_brokenness("data2.txt"))).

data3_test() ->
    ?assertMatch([{"app68","0.0.1"},
                  {"app58","0.0.1"},
                  {"app48","0.0.7"},
                  {"app38","0.0.1"},
                  {"app28","0.0.1"},
                  {"app18","0.0.1"},
                  {"app4","0.0.1"},
                  {"app1","0.0.1"},
                  {"app6","0.0.1"},
                  {"dep_pkg13","0.0.2"},
                  {"app13","0.0.1"},
                  {"dep_pkg5","0.0.2"},
                  {"dep_pkg1","0.0.2"},
                  {"dep_pkg2","0.0.5"},
                  {"dep_pkg7","0.1.2"},
                  {"app9","0.0.1"},
                  {"dep_pkg16","1.0.2"}], run(fix_rebar_brokenness("data3.txt"))).

data4_test() ->
    ?assertMatch([{"dep_pkg20","0.0.2"},
                  {"app78","0.0.1"},
                  {"app68","0.0.1"},
                  {"app58","0.0.1"},
                  {"app48","0.0.7"},
                  {"app38","0.0.1"},
                  {"app28","0.0.1"},
                  {"app18","0.0.1"},
                  {"app4","0.0.1"},
                  {"app1","0.0.1"},
                  {"app6","0.0.1"},
                  {"dep_pkg13","0.0.2"},
                  {"app13","0.0.1"},
                  {"dep_pkg5","0.0.2"},
                  {"dep_pkg1","0.0.2"},
                  {"dep_pkg2","0.0.5"},
                  {"dep_pkg7","0.1.2"},
                  {"app9","0.0.1"},
                  {"dep_pkg16","1.0.2"}],
                 run(fix_rebar_brokenness("data4.txt"))).

data5_test() ->
    ?assertMatch([{"dep_pkg14","0.0.2"},
                  {"dep_pkg22","0.0.2"},
                  {"dep_pkg20","0.0.2"},
                  {"app78","0.0.1"},
                  {"app68","0.0.1"},
                  {"app58","0.0.1"},
                  {"app48","0.0.7"},
                  {"app38","0.0.1"},
                  {"app28","0.0.1"},
                  {"app18","0.0.1"},
                  {"app4","0.0.1"},
                  {"app1","0.0.1"},
                  {"app6","0.0.1"},
                  {"dep_pkg13","0.0.2"},
                  {"app13","0.0.1"},
                  {"dep_pkg5","0.0.2"},
                  {"dep_pkg1","0.0.2"},
                  {"dep_pkg2","0.0.5"},
                  {"dep_pkg7","0.1.2"},
                  {"app9","0.0.1"},
                  {"dep_pkg16","1.0.2"}],
                 run(fix_rebar_brokenness("data5.txt"))).

data6_test() ->
    ?assertMatch([{"app108","0.0.1"},
                  {"app98","0.0.1"},
                  {"app88","0.0.1"},
                  {"dep_pkg14","0.0.2"},
                  {"dep_pkg22","0.0.2"},
                  {"dep_pkg20","0.0.2"},
                  {"app78","0.0.1"},
                  {"app68","0.0.1"},
                  {"app58","0.0.1"},
                  {"app48","0.0.7"},
                  {"app38","0.0.1"},
                  {"app28","0.0.1"},
                  {"app18","0.0.1"},
                  {"app4","0.0.1"},
                  {"app1","0.0.1"},
                  {"app6","0.0.1"},
                  {"dep_pkg13","0.0.2"},
                  {"app13","0.0.1"},
                  {"dep_pkg5","0.0.2"},
                  {"dep_pkg1","0.0.2"},
                  {"dep_pkg2","0.0.5"},
                  {"dep_pkg7","0.1.2"},
                  {"app9","0.0.1"},
                  {"dep_pkg16","1.0.2"}],
                 run(fix_rebar_brokenness("data6.txt"))).

%%============================================================================
%% Internal Functions
%%============================================================================
fix_rebar_brokenness(Filename) ->
    Alt1 = filename:join(["./test", "data", Filename]),
    Alt2 = filename:join(["../test", "data", Filename]),
    case filelib:is_regular(Alt1) of
        true ->
            Alt1;
        false ->
            case filelib:is_regular(Alt2) of
                true ->
                    Alt2;
                false ->
                    io:format("~p~n", [Alt2]),
                    erlang:throw(unable_to_find_data_files)
            end
    end.

read_packages(Device) ->
    process_line(Device, io:get_line(Device, ""), []).

process_line(Device, eof, Acc) ->
    file:close(Device),
    Acc;
process_line(Device, [], Acc) ->
    process_line(Device, io:get_line(Device, ""),
                 Acc);
process_line(Device, "\n", Acc) ->
    process_line(Device, io:get_line(Device, ""),
                 Acc);
process_line(Device, [$\s | Rest], [{Pkg, Vsn, Deps} | Acc]) ->
    [DepPackage, Type,  DepVsn] = string:tokens(Rest, " \n"),
    Dep =
        case Type of
            "=" ->
                {DepPackage, DepVsn};
            ">=" ->
                {DepPackage, DepVsn, gte}
        end,
    process_line(Device, io:get_line(Device, ""),
                 [{Pkg, Vsn, [Dep | Deps]} | Acc]);
process_line(Device, Pkg, Acc) ->
    [Package, Vsn] = string:tokens(Pkg, " \n"),
    process_line(Device, io:get_line(Device, ""),
                 [{Package, Vsn, []} | Acc]).

process_packages(Pkgs) ->
    lists:foldl(fun({Pkg, Vsn, Constraints}, Dom0) ->
                        doest:add_package_version(Dom0, Pkg, Vsn, Constraints)
                end, doest:new(), Pkgs).

get_constraints(ConLine) ->
    AppVsns = string:tokens(ConLine, " \n"),
    lists:map(fun(AppCon) ->
                      parse_app(AppCon, [])
              end, AppVsns).

parse_app([$= | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
parse_app([$>, $= | Rest], Acc) ->
    {lists:reverse(Acc), Rest, gte};
parse_app([Else | Rest], Acc) ->
    parse_app(Rest, [Else | Acc]);
parse_app([], Acc) ->
    lists:reverse(Acc).




