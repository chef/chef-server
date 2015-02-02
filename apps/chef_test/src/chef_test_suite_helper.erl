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

-export([
         set_env/2,
         random_bogus_port/0,
	 stop_server/2,
	 run_cmds/1,
	 space_join/1,
	 ensure_started/1,
	 set_app_env/1,
	 make_id/1,
	 make_az_id/1,
	 actor_id/0,
	 the_org_id/0,
	 other_org_id/0
        ]).

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
         ct:pal("Stopping ~p~n", [App]),
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
    [ [Elt, " "] || Elt <- L ].

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        E ->
            E
    end.

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

the_org_id() ->
    make_id(<<"aa1">>).

other_org_id() ->
    make_id(<<"bb2">>).
