%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% @version 0.0.1
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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

-module(chef_db_test_utils).

-export([test_setup/0,
         test_cleanup/1,
         start_stats_hero/0,
         mock/1,
         mock/2,
         stats_hero_label/1,
         unmock/1,
         validate_modules/1
        ]).

-include_lib("eunit/include/eunit.hrl").


test_setup() ->
    start_stats_hero(),
    Server = {context,<<"test-req-id">>,{server,"localhost",5984,[],[]}},
    Superuser = <<"cb4dcaabd91a87675a14ec4f4a00050d">>,
    {Server, Superuser}.

start_stats_hero() ->
    application:set_env(stats_hero, estatsd_host, "localhost"),
    application:set_env(stats_hero, estatsd_port, dumb_random_port()),
    application:set_env(stats_hero, udp_socket_pool_size, 5),
    case application:start(stats_hero) of
        ok ->
            ok;
        {error, {already_started, stats_hero}} ->
            ok
    end.

dumb_random_port() ->
    {ok, Socket} = gen_udp:open(0),
    {ok, Port} = inet:port(Socket),
    gen_udp:close(Socket),
    Port.

test_cleanup(_State) ->
    case whereis(inet_gethost_native_sup) of
        P when is_pid(P) ->
            inet_gethost_native:terminate(shutdown, P);
        _ ->
            ok
    end,
    meck:unload(),
    ok.

%%@doc setup mocking for a list of modules.  This would normally be
%% called in the setup/0 method. You can optionally pass in a list of
%% meck options. See meck docs for details at
%% http://doc.erlagner.org/meck/meck.html#new-2
mock(Modules) ->
  mock(Modules, []).
mock(Modules, Opts) ->
    [ meck:new(M, Opts) || M <- Modules ].

%%@doc Unload a list of mocked modules
unmock(Modules) ->
    [ meck:unload(M) || M <-Modules ].

%% @doc Validate the state of the mock modules and raise
%% an eunit error if the modules have not been used according to
%% expectations
validate_modules(Modules) ->
    [?assert(meck:validate(M)) || M <- Modules].

%% copied from chef_wm_base to use for testing
stats_hero_label({chef_sql, Fun}) ->
    stats_hero_label0(rdbms, {chef_sql, Fun});
stats_hero_label({chef_otto, Fun}) ->
    stats_hero_label0(couchdb, {chef_otto, Fun});
stats_hero_label({chef_solr, Fun}) ->
    stats_hero_label0(solr, {chef_solr, Fun});
stats_hero_label({BadPrefix, Fun}) ->
    erlang:error({bad_prefix, {BadPrefix, Fun}}).

stats_hero_label0(Prefix, {Mod, Fun}) ->
    PrefixBin = erlang:atom_to_binary(Prefix, utf8),
    ModBin = erlang:atom_to_binary(Mod, utf8),
    FunBin = erlang:atom_to_binary(Fun, utf8),
    <<PrefixBin/binary, ".", ModBin/binary, ".", FunBin/binary>>.
