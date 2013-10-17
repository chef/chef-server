%%%-------------------------------------------------------------------
%%% @author Oliver Ferrigni <>
%%% @copyright (C) 2013, Oliver Ferrigni
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2013 by Oliver Ferrigni <>
%%%-------------------------------------------------------------------
-module(oc_chef_group_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("oc_chef_authz/include/oc_chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.
end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].
 
all() -> 
    [fetch_group_sql, list_should_return_empty_list_when_no_groups].

list_should_return_empty_list_when_no_groups(_Config) ->
    ?assertEqual([], chef_db:list(#oc_chef_group{})),
    ok.

fetch_group_sql(_Config) ->
    OrgId = <<"77770000000000000000000000000000">>,
    Name = <<"admins">>,
    ReqId = <<"test-2-req-id">>,
    Ctx = oc_chef_authz:make_context(ReqId, darklaunch_stub),
    case oc_chef_authz_db:fetch_group_authz_id_sql(Ctx, OrgId, Name) of
        <<"66660000000000000000000000000000">> = V ->
            ct:pal("Found group with authz_id: ~p", [V]),
            ok;
        Bad ->
            erlang:error({unexpected_result, Bad})
    end.

