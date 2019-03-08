%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2015 Chef, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(chef_test_suite_helper).

-include_lib("eunit/include/eunit.hrl").
-include("../../../include/chef_types.hrl").

-export([
         context/0,

         set_env/2,
         random_bogus_port/0,
         stop_server/2,
         run_cmds/1,
         space_join/1,
         set_app_env/1,
         make_id/1,
         make_az_id/1,
         actor_id/0,

         make_orgs/0,
         org_name/0,
         other_org_name/0,

         the_org_id/0,
         other_org_id/0,

         create_record/1,
         list_records/1,
         update_record/1,
         fetch_record/1,
         delete_record/1,

         value_matches_regex/2
        ]).

-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000002">>).
-define(ORG_NAME, <<"testorg">>).

-define(OTHER_ORG_AUTHZ_ID, <<"99999999999999999999999999999999">>).
-define(OTHER_ORG_NAME, <<"other_testorg">>).



set_env(App, AppConfig) ->
    [ application:set_env(App, Key, Value) || {Key, Value} <- AppConfig ].

%% @doc If lucky, return an unused port. This is a cheat that opens a
%% UDP port letting the OS pick the port, captures that port, and then
%% closes the socket returning the port number. While not reliable,
%% this seems to work to obtain an "unused" port for setting up
%% services needed for testing.
random_bogus_port() ->
    {ok, S} = gen_udp:open(0, [binary, {active, once}]),
    {ok, Port} = inet:port(S),
    gen_udp:close(S),
    Port.

% NeededApps is a list of atoms of app to stop
stop_server(Config, NeededApps) ->
    [begin
         application:stop(App)
     end || App <- lists:reverse(NeededApps)],

    %% shut down the db if its on
    try chef_test_db_helper:stop_db(Config)
    catch
        _:_ -> ct:pal("No database to stop.~n")
    end,

    Config.

run_cmds(CMDS) ->
    [ begin
          CC = space_join(C),
          [CC, "\n", os:cmd(CC), "\n\n"]
      end || C <- CMDS ].

space_join(L) ->
    lists:flatten([ [Elt, " "] || Elt <- L ]).

set_app_env(stats_hero) ->
    set_env(stats_hero, [{estatsd_host, "localhost"},
                         {estatsd_port, random_bogus_port()},
                         {udp_socket_pool_size, 1}]);
set_app_env(pooler) ->
    application:set_env(pooler, pools,
                        [[{name, sqerl},
                          {max_count, 2},
                          {init_count, 2},
                          {start_mfa, {sqerl_client, start_link, []}}]]).

make_id(Prefix) when is_binary(Prefix) ->
    case size(Prefix) of
        Size when Size > 32 ->
            error(prefix_too_long_for_id);
        Size when Size =:= 32 ->
            Prefix;
        Size ->
            iolist_to_binary([Prefix, lists:duplicate(32 - Size, $0)])
    end;
make_id(Prefix) when is_list(Prefix) ->
    make_id(list_to_binary(Prefix)).

make_az_id(Prefix) when is_list(Prefix) ->
    make_az_id(list_to_binary(Prefix));

make_az_id(Prefix) ->
    make_id(<<"a11", Prefix/binary>>).

actor_id() ->
    make_az_id(<<"ffff">>).

make_orgs() ->
    make_org(),
    make_other_org(),
    OrgConfig = org_config(),
    confirm_org_setup(OrgConfig),
    OrgConfig.

make_org() ->
    Org = chef_object:new_record(oc_chef_organization, ?API_MIN_VER, nil, ?ORG_AUTHZ_ID,
                                 {[{<<"name">>, ?ORG_NAME}, {<<"full_name">>, ?ORG_NAME}]}),
    ok = chef_db:create(Org, context(), ?ORG_AUTHZ_ID).

make_other_org() ->
    Org = chef_object:new_record(oc_chef_organization, ?API_MIN_VER, nil, ?OTHER_ORG_AUTHZ_ID,
                                 {[{<<"name">>, ?OTHER_ORG_NAME}, {<<"full_name">>, ?OTHER_ORG_NAME}]}),
    ok = chef_db:create(Org, context(), ?OTHER_ORG_AUTHZ_ID).

org_config() ->
    {OrgId, _} = chef_db:fetch_org_metadata(context(), ?ORG_NAME),
    {OtherOrgID, _} = chef_db:fetch_org_metadata(context(), ?OTHER_ORG_NAME),
    [{org_id, OrgId}, {other_org_id, OtherOrgID}].

confirm_org_setup(Config) ->
    ExpectedOrgId = proplists:get_value(org_id, Config),
    ExpectedOtherOrgId = proplists:get_value(other_org_id, Config),

    {ActualMainOrg, _} = chef_db:fetch_org_metadata(context(), ?ORG_NAME),
    {ActualOtherOrg, _} = chef_db:fetch_org_metadata(context(), ?OTHER_ORG_NAME),
    ?assertEqual(ExpectedOrgId, ActualMainOrg),
    ?assertEqual(ExpectedOtherOrgId, ActualOtherOrg).


org_name() ->
    ?ORG_NAME.

other_org_name() ->
    ?OTHER_ORG_NAME.

context() ->
    chef_db:make_context(?API_MIN_VER, <<"AB">>).

the_org_id() ->
    make_id(<<"aa1">>).

other_org_id() ->
    make_id(<<"bb2">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common database queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_record(Record0) ->
    Record = chef_object:set_api_version(Record0, ?API_MIN_VER),
    Query = chef_object:create_query(Record),
    FieldsForInsert = chef_object:fields_for_insert(Record),
    chef_sql:create_object(Query, FieldsForInsert).

fetch_record(Record0) ->
    Record = chef_object:set_api_version(Record0, ?API_MIN_VER),
    Result = chef_sql:fetch_object(
               chef_object:fields_for_fetch(Record),
               element(1, Record),
               chef_object:find_query(Record),
               chef_object:record_fields(Record)
              ),
    case Result of
        {ok, OutRecord} when is_tuple(OutRecord) ->
            Final = chef_object:set_api_version(OutRecord, ?API_MIN_VER),
            {ok, Final};
        _ ->
            Result
    end.



update_record(Record0) ->
    Record = chef_object:set_api_version(Record0, ?API_MIN_VER),
    chef_sql:do_update(chef_object:update_query(Record), chef_object:fields_for_update(Record)).

%% Delete the database row associated with the Record. Note that the
%% corresponding database query must delete by id (e.g.,
%% "delete from table where id= $1"). Use a custom helper for other kinds of
%% delete queries.
delete_record(Record0) ->
    Record = chef_object:set_api_version(Record0, ?API_MIN_VER),
    Query = chef_object:delete_query(Record),
    Id = chef_object:id(Record),

    chef_sql:delete_object(Query, Id).

list_records(Record0) ->
    Record = chef_object:set_api_version(Record0, ?API_MIN_VER),
    chef_sql:fetch_object_names(Record).

value_matches_regex(undefined , _RE) ->
    false;
value_matches_regex(Value, RE) when is_list(RE) ->
    {ok, RealRE} = re:compile(RE),
    value_matches_regex(Value, RealRE);
value_matches_regex(Value, RE) ->
    case re:run(Value, RE) of
        {match, _} -> true;
        _ -> false
    end.

