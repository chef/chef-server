%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_json_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%% Helper macro for validating plain old lists as run lists
-define(RUN_LIST(L), chef_json_validator:validate_json_by_regex_constraints(
                       [{<<"run_list">>, L}],
                       [{<<"run_list">>, is_run_list}]
                      )).

%% Helper macro for validating env_run_lists maps
-define(ENV_RUN_LIST(E), chef_json_validator:validate_json_by_regex_constraints(
                           [{<<"env_run_lists">>, E}],
                           [{<<"env_run_lists">>, is_run_list_map}]
                          )).

valid_run_list_test_() ->
    [
     {"Ensure that run list items are properly validated",
      fun() ->
              %% Empty list is valid
              ?assertEqual(ok, ?RUN_LIST([])),

              %% Needs to be a list of binaries, though
              ?assertMatch({bad_run_list, _}, ?RUN_LIST("FOO")),

              %% Not a list
              ?assertMatch({bad_run_list, _}, ?RUN_LIST(<<"recipe[foo], recipe[bar]">>)),

              %% item not binary
              ?assertMatch({bad_run_list, _}, ?RUN_LIST(["FOO"])),

              % Not a binary item (obviously)
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([123])),

              %% the most basic case should work
              ?assertEqual(ok, ?RUN_LIST([<<"recipe[foo]">>])),

              %% One rotten apple spoils the bunch
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[foo]">>, "BAD"])),

              %% Missing the closing bracket is a no-no
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[foo">>])),

              ?assertEqual(ok, ?RUN_LIST([<<"recipe[_thing]">>])),
              ?assertEqual(ok, ?RUN_LIST([<<"recipe[666-recipe-of-the-beast]">>])),
              ?assertEqual(ok, ?RUN_LIST([<<"recipe[this_is_valid]">>])),
              ?assertEqual(ok, ?RUN_LIST([<<"recipe[this_is_a_123_thing]">>])),
              ?assertEqual(ok, ?RUN_LIST([<<"recipe[this-also-works]">>])),

              ?assertEqual(ok, ?RUN_LIST([<<"recipe[webserver@1.0.0]">>])),
              ?assertEqual(ok, ?RUN_LIST([<<"recipe[webserver@1.0]">>])),

              %% Versions for a recipe must be MAJOR.MINOR or MAJOR.MINOR.PATCH
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[webserver@1]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[webserver@1.0.0.0]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[webserver@1.2.foo]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[webserver@1e0e0]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[webserver@1.0e0]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[webserver@1e0.0]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[webserver@1..0.0]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[webserver@1.0..0]">>])),

              ?assertEqual(ok, ?RUN_LIST([<<"recipe[webserver::foo@1.0.0]">>])),

              %% Illegal characters in recipe/role name
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[aaa%b]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[aaa b]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[aaa\tb]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[aaa\nb]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[aaa()b]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[aaa=b]">>])),

              %% Various pathological cases
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[Bl@a@]">>])),

              %% Role tests
              ?assertEqual(ok, ?RUN_LIST([<<"role[foo]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"role[foo@123]">>])),

              ?assertEqual(ok, ?RUN_LIST([<<"role[123foo]">>])),

              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"role[foo@1.0]">>])),
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"role[foobar::foo]">>])),

              %% Dots are OK
              ?assertMatch(ok, ?RUN_LIST([<<"role[dots.I.haz.them]">>])),

              %% Unqualified recipe tests
              ?assertEqual(ok, ?RUN_LIST([<<"foo">>])),
              ?assertEqual(ok, ?RUN_LIST([<<"_foo">>])),
              ?assertEqual(ok, ?RUN_LIST([<<"foo@1.0">>])),
              ?assertMatch(ok, ?RUN_LIST([<<"dots.i.haz.them">>])),

              %% This shouldn't work
              %% See https://tickets.corp.opscode.com/browse/ESPRINT-913
              ?assertMatch({bad_run_list, _}, ?RUN_LIST([<<"recipe[foo], recipe[bar]">>])),

              %% VALIDATE ALL THE THINGS!
              ?assertEqual(ok, ?RUN_LIST([
                                          <<"recipe[foo]">>,
                                          <<"recipe[webserver::foo]">>,
                                          <<"recipe[webserver::foo@1.0]">>,
                                          <<"recipe[awesomesauce::foo@001.234.82]">>,
                                          <<"role[app_server]">>,
                                          <<"role[other-server]">>,
                                          <<"monitoring">>,
                                          <<"monitoring::default">>,
                                          <<"monitoring::alerts@1.3.4">>,
                                          <<"website.com::base">>,
                                          <<"website.com">>,
                                          <<"recipe[website.com::base@1.2.3]">>,
                                          <<"recipe[website.com]">>
                                        ]))
      end}
    ].

%% This is not an exhaustive test, as all the run_list validation
%% details are tested above
valid_env_run_list_test_() ->
    [{"Ensure that env_run_lists are all valid",
     fun () ->
             %% Empty env_run_lists is valid
             ?assertEqual(ok, ?ENV_RUN_LIST({[]})),

             %% Must be an ejson map
             ?assertMatch({bad_run_lists, _}, ?ENV_RUN_LIST([])),

             %% Valid map is valid
             ?assertMatch(ok,
                          ?ENV_RUN_LIST({[{<<"foo">>, [<<"recipe[foo]">>,
                                                       <<"blah">>,
                                                       <<"role[server]">>]},
                                          {<<"bar">>, [<<"webserver">>,
                                                       <<"role[base]">>]}
                                         ]})),

             %% Invalid map is invalid
             ?assertMatch({bad_run_lists, _},
                          ?ENV_RUN_LIST({[{<<"foo">>, [<<"recipe[foo]">>,
                                                       <<"blah">>,
                                                       <<"role[server]">>]},
                                          {<<"bar">>, [<<"webserver">>,
                                                       <<"fake[not_good]">>]}
                                         ]}))
     end}].
