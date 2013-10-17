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
    suite_helper:init(Config).

end_per_suite(Config) ->
    suite_helper:stop(Config).
 
all() -> 
    [fetch_group_sql, list_should_return_empty_list_when_no_groups,
    list_should_return_group_name_when_group_exists].

list_should_return_empty_list_when_no_groups(_Config) ->
    OrgId = <<"77770000000000000000000000000001">>,
    ?assertEqual([], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    ok.

list_should_return_group_name_when_group_exists(_Config) ->
    OrgId = <<"77770000000000000000000000000000">>,
    ?assertEqual([<<"admins">>], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    ok.

fetch_group_sql(_Config) ->
    OrgId = <<"77770000000000000000000000000000">>,
    Name = <<"admins">>,    
    case chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = Name}) of
        #oc_chef_group{authz_id = <<"66660000000000000000000000000000">> = V} ->
            ct:pal("Found group with authz_id: ~p", [V]),
            ok;
        Bad ->
            erlang:error({unexpected_result, Bad})
    end.

