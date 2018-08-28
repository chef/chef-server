%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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

valid_run_list_item_test_() ->
    [
     {"Ensure that run list items are properly validated",
      fun() ->
              %% Empty list is not a valid item
              ?assertEqual(error, chef_json_validator:valid_run_list_item([])),

              %% String isn't valid
              ?assertMatch(error, chef_json_validator:valid_run_list_item("FOO")),

              %% Not an item
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[foo], recipe[bar]">>)),

              % Not a binary item (obviously)
              ?assertMatch(error, chef_json_validator:valid_run_list_item(123)),

              %% the most basic case should work
              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"recipe[foo]">>)),

              %% Missing the closing bracket is a no-no
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[foo">>)),

              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"recipe[_thing]">>)),
              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"recipe[666-recipe-of-the-beast]">>)),
              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"recipe[this_is_valid]">>)),
              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"recipe[this_is_a_123_thing]">>)),
              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"recipe[this-also-works]">>)),

              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"recipe[webserver@1.0.0]">>)),
              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"recipe[webserver@1.0]">>)),

              %% Versions for a recipe must be MAJOR.MINOR or MAJOR.MINOR.PATCH
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[webserver@1]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[webserver@1.0.0.0]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[webserver@1.2.foo]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[webserver@1e0e0]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[webserver@1.0e0]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[webserver@1e0.0]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[webserver@1..0.0]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[webserver@1.0..0]">>)),

              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"recipe[webserver::foo@1.0.0]">>)),

              %% Illegal characters in recipe/role name
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[aaa%b]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[aaa b]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[aaa\tb]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[aaa\nb]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[aaa()b]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[aaa=b]">>)),

              %% Various pathological cases
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"recipe[Bl@a@]">>)),

              %% Role tests
              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"role[foo]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"role[foo@123]">>)),

              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"role[123foo]">>)),

              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"role[foo@1.0]">>)),
              ?assertMatch(error, chef_json_validator:valid_run_list_item(<<"role[foobar::foo]">>)),

              %% Dots are OK
              ?assertMatch(ok, chef_json_validator:valid_run_list_item(<<"role[dots.I.haz.them]">>)),

              %% Unqualified recipe tests
              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"foo">>)),
              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"_foo">>)),
              ?assertEqual(ok, chef_json_validator:valid_run_list_item(<<"foo@1.0">>)),
              ?assertMatch(ok, chef_json_validator:valid_run_list_item(<<"dots.i.haz.them">>))
      end}
    ].
