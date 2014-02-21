%% Copyright 2013 Opscode, Inc. All Rights Reserved.
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

-module(chef_index_sup_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/chef_solr.hrl").

is_list_of_binary_test_() ->
    AllBin = [<<"a">>, <<"b">>, <<"c">>, <<>>],
    BadLists = [
                {[<<"a">>, <<"b">>, nope], [nope]},
                {[<<"a">>, nope, <<"c">>], [nope]},
                {[nope, <<"b">>, <<"c">>], [nope]},
                {[nope1, <<"b">>, nope3], [nope1, nope3]},
                {[nope1, nope2, nope3], [nope1, nope2, nope3]}
               ],
    [?_assertEqual(true, chef_index_sup:is_list_of_binary(AllBin))]
        ++ [ ?_assertEqual({non_binary_items, Expect}, chef_index_sup:is_list_of_binary(In))
             || {In, Expect} <- BadLists ].
     
server_for_vhost_test() ->
    ?assertEqual('chef_index_queue/chef',
                 chef_index_sup:server_for_vhost(<<"/chef">>)).
