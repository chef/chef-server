%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% Copyright Chef Software, Inc. All Rights Reserved.
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
-module(chef_objects_test_utils).
-export([
         mock/1,
         mock/2,
         unmock/1,
         bcrypt_setup/0,
         bcrypt_cleanup/1,
         keygen_setup/0,
         keygen_cleanup/1,
         validate_modules/1,
         versioned_desc/2,
         make_deprecated_tests/1,
         make_non_deprecated_tests/1,
         make_all_versions_tests/1,
         make_versioned_test_range/3,
         read_file/1
        ]).

-include_lib("eunit/include/eunit.hrl").
-include("server_api_version.hrl").

%% helper functions for configuring mocking.

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

bcrypt_setup() ->
    application:set_env(bcrypt, default_log_rounds, 4),
    [ ensure_start(App) || App <- [crypto, bcrypt] ],
    ok.

bcrypt_cleanup(_) ->
    error_logger:tty(false),
    application:stop(bcrypt),
    error_logger:tty(true).

keygen_setup() ->
    error_logger:tty(false),
    [application:set_env(chef_authn, Field, Value) ||
     {Field, Value} <- [ {keygen_cache_size, 4}, {keygen_start_size, 4},
                         {keygen_timeout, 1000}, {keygen_size, 1024}] ],
    chef_keygen_worker_sup:start_link(),
    chef_keygen_cache:start_link().

keygen_cleanup(_) ->
    error_logger:tty(true).

ensure_start(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        Error ->
            error(Error)
    end.

versioned_desc(Version, Desc) ->
    iolist_to_binary( ["[v", integer_to_list(Version), "] ", Desc] ).

make_non_deprecated_tests(Generator) ->
    make_versioned_test_range(?API_DEPRECATED_VER + 1, ?API_MAX_VER, Generator).

make_deprecated_tests(Generator) ->
    T2 = make_versioned_test_range(?API_MIN_VER, ?API_DEPRECATED_VER, Generator),
    T2.

make_all_versions_tests(Generator) ->
    make_versioned_test_range(?API_MIN_VER, ?API_MAX_VER, Generator).

make_versioned_test_range(Min, Max, Generator) ->
    [Generator(Version) || Version <- lists:seq(Min, Max)].

read_file(File) ->
    case read_file(app, File) of
        {error, enoent} ->
            read_file(test, File);
        {ok, Bin} ->
            {ok, Bin}
    end.

read_file(app, File) -> %% Rebar3
    file:read_file(filename:join([".", "apps", "chef_objects", "test", File]));
read_file(test, File) -> %% Rebar2
    file:read_file(filename:join(["..", "test", File])).
