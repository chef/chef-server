%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@chef.io>
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


-module(chef_regex_tests).

-include_lib("eunit/include/eunit.hrl").

match(Str, {RE, _Msg}) ->
    case re:run(Str, RE) of
        {match, _} -> ok;
        nomatch -> nomatch
    end.

regex_for_names_test_() ->
    NameTests = fun(Type) ->
        Regex = chef_regex:regex_for(Type),
        ?assertEqual(nomatch, match(<<"">>, Regex)),
        ?assertEqual(nomatch, match(<<"foo@bar">>, Regex)),

        %% single char cases
        ?assertEqual(ok, match(<<"f">>, Regex)),
        ?assertEqual(ok, match(<<"F">>, Regex)),
        ?assertEqual(ok, match(<<"_">>, Regex)),
        ?assertEqual(ok, match(<<"-">>, Regex)),
        ?assertEqual(ok, match(<<".">>, Regex)),

        ?assertEqual(ok, match(<<"foo">>, Regex)),
        ?assertEqual(ok, match(<<"FOO">>, Regex)),
        ?assertEqual(ok, match(<<"_foo">>, Regex)),
        ?assertEqual(ok, match(<<"123">>, Regex)),
        ?assertEqual(ok, match(<<"foo-bar">>, Regex)),
        ?assertEqual(ok, match(<<"foo.bar">>, Regex)),
        ?assertEqual(ok, match(<<"foo_bar-123.a">>, Regex))
    end,
    [
        {"Ensure cookbook names are properly matched",
         fun() -> NameTests(cookbook_name) end},
        {"Ensure environment names are properly matched",
         fun() -> NameTests(environment_name) end},
        {"Ensure recipe names are properly matched",
         fun() -> NameTests(recipe_name) end},
        {"Extra recipe checks",
         fun() ->
             Regex = chef_regex:regex_for(recipe_name),
             ?assertEqual(nomatch, match(<<"foo:bar">>, Regex)),
             ?assertEqual(nomatch, match(<<"foo::">>, Regex)),
             ?assertEqual(nomatch, match(<<"::baz">>, Regex)),
             ?assertEqual(nomatch, match(<<"foo::bar::baz">>, Regex)),

             ?assertEqual(ok, match(<<"foo::bar">>, Regex)),
             ?assertEqual(ok, match(<<"foo::FOO">>, Regex)),
             ?assertEqual(ok, match(<<"foo::_foo">>, Regex)),
             ?assertEqual(ok, match(<<"foo::123">>, Regex)),
             ?assertEqual(ok, match(<<"foo::foo-bar">>, Regex)),
             ?assertEqual(ok, match(<<"foo::foo.bar">>, Regex)),
             ?assertEqual(ok, match(<<"foo::foo_bar-123.a">>, Regex))
         end}
    ].
