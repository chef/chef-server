%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@chef.io>
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
    AltNameTests = fun(Type) ->
        Regex = chef_regex:regex_for(Type),
        ?assertEqual(ok, match(<<"a:b">>, Regex)),
        ?assertEqual(ok, match(<<":">>, Regex)),
        NameTests(Type)
    end,
    [
        {"Ensure node names are properly matched",
         fun() -> AltNameTests(node_name) end},
        {"Ensure key names are properly matched",
         fun() -> AltNameTests(key_name) end},
        {"Ensure data bag names are properly matched",
         fun() -> AltNameTests(data_bag_name) end},
        {"Ensure data bag item ids are properly matched",
         fun() -> AltNameTests(data_bag_item_id) end},
        {"Ensure role names are properly matched",
         fun() -> AltNameTests(role_name) end},
        {"Ensure cookbook names are properly matched",
         fun() -> NameTests(cookbook_name) end},
        {"Ensure environment names are properly matched",
         fun() -> NameTests(environment_name) end},
        {"Ensure recipe names are properly matched",
         fun() -> NameTests(recipe_name) end},
        {"Ensure client names are properly matched",
         fun() -> NameTests(client_name) end},
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

%% Username validation tests - CHEF-27822
%% Tests for relaxed username validation allowing RFC 5322 special characters and whitespace
regex_for_user_name_test_() ->
    Regex = chef_regex:regex_for(user_name),
    [
        {"Username validation - original allowed characters still work",
         fun() ->
             %% Lowercase letters, digits, hyphen, underscore
             ?assertEqual(ok, match(<<"alice">>, Regex)),
             ?assertEqual(ok, match(<<"bob123">>, Regex)),
             ?assertEqual(ok, match(<<"user-name">>, Regex)),
             ?assertEqual(ok, match(<<"user_name">>, Regex)),
             ?assertEqual(ok, match(<<"a1b2c3">>, Regex))
         end},
        
        {"Username validation - RFC 5322 special characters allowed",
         fun() ->
             %% @ sign (email-like)
             ?assertEqual(ok, match(<<"user@example.com">>, Regex)),
             ?assertEqual(ok, match(<<"first.last@domain">>, Regex)),
             
             %% Period
             ?assertEqual(ok, match(<<"user.name">>, Regex)),
             ?assertEqual(ok, match(<<"first.middle.last">>, Regex)),
             
             %% Plus sign
             ?assertEqual(ok, match(<<"user+tag">>, Regex)),
             ?assertEqual(ok, match(<<"email+filter@domain">>, Regex)),
             
             %% Other special characters from RFC 5322
             ?assertEqual(ok, match(<<"user!name">>, Regex)),
             ?assertEqual(ok, match(<<"user#name">>, Regex)),
             ?assertEqual(ok, match(<<"user$name">>, Regex)),
             ?assertEqual(ok, match(<<"user%name">>, Regex)),
             ?assertEqual(ok, match(<<"user&name">>, Regex)),
             ?assertEqual(ok, match(<<"user'name">>, Regex)),
             ?assertEqual(ok, match(<<"user*name">>, Regex)),
             ?assertEqual(ok, match(<<"user,name">>, Regex)),
             ?assertEqual(ok, match(<<"user/name">>, Regex)),
             ?assertEqual(ok, match(<<"user:name">>, Regex)),
             ?assertEqual(ok, match(<<"user;name">>, Regex)),
             ?assertEqual(ok, match(<<"user<name">>, Regex)),
             ?assertEqual(ok, match(<<"user=name">>, Regex)),
             ?assertEqual(ok, match(<<"user>name">>, Regex)),
             ?assertEqual(ok, match(<<"user?name">>, Regex)),
             ?assertEqual(ok, match(<<"user[name">>, Regex)),
             ?assertEqual(ok, match(<<"user\\name">>, Regex)),
             ?assertEqual(ok, match(<<"user]name">>, Regex)),
             ?assertEqual(ok, match(<<"user^name">>, Regex)),
             ?assertEqual(ok, match(<<"user`name">>, Regex)),
             ?assertEqual(ok, match(<<"user{name">>, Regex)),
             ?assertEqual(ok, match(<<"user|name">>, Regex)),
             ?assertEqual(ok, match(<<"user}name">>, Regex)),
             ?assertEqual(ok, match(<<"user~name">>, Regex))
         end},
        
        {"Username validation - whitespace and quote characters allowed",
         fun() ->
             %% Spaces allowed
             ?assertEqual(ok, match(<<" ">>, Regex)),
             ?assertEqual(ok, match(<<"user name">>, Regex)),
             ?assertEqual(ok, match(<<" user">>, Regex)),
             ?assertEqual(ok, match(<<"user ">>, Regex)),
             ?assertEqual(ok, match(<<"first last">>, Regex)),
             
             %% Tab allowed
             ?assertEqual(ok, match(<<"user\tname">>, Regex)),
             ?assertEqual(ok, match(<<"\tuser">>, Regex)),
             ?assertEqual(ok, match(<<"user\t">>, Regex)),
             
             %% Quote allowed
             ?assertEqual(ok, match(<<"user\"name">>, Regex)),
             ?assertEqual(ok, match(<<"\"user\"">>, Regex)),
             ?assertEqual(ok, match(<<"\"quoted username\"">>, Regex))
         end},
        
        {"Username validation - complex real-world examples",
         fun() ->
             ?assertEqual(ok, match(<<"user.name+tag@domain">>, Regex)),
             ?assertEqual(ok, match(<<"admin!#$%">>, Regex)),
             ?assertEqual(ok, match(<<"test_user-123@company.com">>, Regex)),
             ?assertEqual(ok, match(<<"first.last+filter@example.org">>, Regex)),
             ?assertEqual(ok, match(<<"user name">>, Regex)),
             ?assertEqual(ok, match(<<"user\ttab">>, Regex)),
             ?assertEqual(ok, match(<<"\"quoted\"">>, Regex))
         end},
        
        {"Username validation - invalid characters still rejected",
         fun() ->
             %% Empty string
             ?assertEqual(nomatch, match(<<"">>, Regex)),
             
             %% Newline
             ?assertEqual(nomatch, match(<<"user\nname">>, Regex)),
             
             %% Uppercase letters not allowed
             ?assertEqual(nomatch, match(<<"Alice">>, Regex)),
             ?assertEqual(nomatch, match(<<"BOB">>, Regex)),
             ?assertEqual(nomatch, match(<<"userNAME">>, Regex)),
             ?assertEqual(nomatch, match(<<"User Name">>, Regex))
         end}
    ].
