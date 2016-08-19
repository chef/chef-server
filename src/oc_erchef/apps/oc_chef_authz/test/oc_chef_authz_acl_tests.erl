%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%%
%% @author Marc A. Paradise <marc@chef.io>
%% Copyright 2016 Chef Software, Inc.
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
-module(oc_chef_authz_acl_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
fetch_actors_test_() ->
    {foreach,
     fun() ->
             meck:new(oc_chef_authz_db)
     end,
     fun(_) -> meck:unload([oc_chef_authz_db]) end,
     [
      {"bad_actor: actors that have neither client nor user authz ids",
       fun() ->
               meck:expect(oc_chef_authz_db, find_org_actors_by_name,
                           fun invalid_actor_data_response/2),
               FetchActorResponse = oc_chef_authz_acl:fetch_actors(<<"any">>,
                                                                   [<<"bob">>, <<"jane">>]),
               ?assertEqual({error, {bad_actor, [<<"bob">>, <<"jane">>]}}, FetchActorResponse)
       end},
      {"ambiguous_actor: actors that have both user and client authz ids",
       fun() ->
               meck:expect(oc_chef_authz_db, find_org_actors_by_name,
                           fun ambiguous_actor_data_response/2),
               FetchActorResponse = oc_chef_authz_acl:fetch_actors(<<"any">>, [<<"bob">>, <<"jane">>]),
               ?assertEqual({error, {ambiguous_actor, [<<"bob">>, <<"jane">>]}}, FetchActorResponse)
       end},
      {"ok: actors that have only a client authz id",
       fun() ->
               meck:expect(oc_chef_authz_db, find_org_actors_by_name,
                           fun valid_client_data_response/2),
               FetchActorResponse = oc_chef_authz_acl:fetch_actors(<<"any">>, [<<"bob">>, <<"jane">>]),
               ?assertEqual({ok, [<<"id-a">>, <<"id-a">>]}, FetchActorResponse)
       end},
      {"ok: actors that have only a user authz id",
       fun() ->
               meck:expect(oc_chef_authz_db, find_org_actors_by_name,
                           fun valid_user_data_response/2),
               FetchActorResponse = oc_chef_authz_acl:fetch_actors(<<"any">>, [<<"bob">>, <<"jane">>]),
               ?assertEqual({ok, [<<"id-a">>, <<"id-a">>]}, FetchActorResponse)
       end}
     ]}.

valid_user_data_response(_, Names) ->
    {ok, [{N, null, <<"id-a">>} || N <- Names]}.

valid_client_data_response(_, Names) ->
    {ok, [{N, <<"id-a">>, null} || N <- Names]}.

invalid_actor_data_response(_, Names) ->
    {ok, [{N, null, null} || N <- Names]}.

ambiguous_actor_data_response(_, Names) ->
    {ok, [{N, <<"id1">>, <<"id2">>} || N <- Names]}.

