%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2013-2015 Chef, Inc. All Rights Reserved.
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

-module(oc_chef_wm_containers_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("chef_types.hrl").
-include("oc_chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, {parse_transform, lager_transform}]).

-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000000">>).
-define(AUTHZ_ID, <<"00000000000000000000000000000001">>).
-define(CLIENT_NAME, <<"test-client">>).
-define(DEFAULT_HEADERS, [{"x-ops-userid", "test-client"},
                          {"accept", "application/json"}]).
-define(assertStatus(StatusCode, HttpcResponse), ?assertMatch({ok, {{_, StatusCode, _}, _, _}}, HttpcResponse)).

init_per_suite(Config) ->
    setup_helper:base_init_per_suite([{org_name, <<"org">>},
                                      {org_authz_id, ?ORG_AUTHZ_ID},
                                      {authz_id, ?AUTHZ_ID},
                                      {client_name, ?CLIENT_NAME}
                                      | Config]).

end_per_suite(Config) ->
    setup_helper:base_end_per_suite(Config).

all() ->
    [
     list_when_no_containers,
     list_when_created_containers,
     create_container,
     create_with_bad_name,
     delete_container,
     fetch_non_existant_container,
     fetch_existant_container,
     update_container
    ].

init_per_testcase(_, Config) ->
    setup_helper:mock_authz(?AUTHZ_ID),
    delete_all_containers(),
    Config.

end_per_testcase(_, _Config) ->
    setup_helper:unmock_authz(),
    ok.

delete_all_containers() ->
    Result = case sqerl:adhoc_delete("containers", all) of
                 {ok, Count} ->
                     Count;
                 Error ->
                     throw(Error)
             end,
    lager:info("Delete containers: ~p", [Result]),
    ok.

list_when_no_containers(_) ->
    Result = http_list_containers(),
    ?assertStatus(200, Result).

list_when_created_containers(_) ->
    Containers = ["foo", "bar"],
    [ http_create_container(Container) || Container <- Containers ],
    Response = http_list_containers(),
    ?assertStatus(200, Response),
    Ejson = chef_json:decode(respBody(Response)),
    {ContainerList} = Ejson,
    [ ?assertEqual(true, proplists:is_defined(list_to_binary(Container), ContainerList))
      || Container <- Containers ].

create_container(_) ->
    Result = http_create_container("foo"),
    ?assertStatus(201, Result).

create_with_bad_name(_) ->
    Result = http_create_container("name with spaces"),
    ?assertStatus(400, Result).

delete_container(_) ->
    http_create_container("foo"),
    Result = http_delete_container("foo"),
    ?assertStatus(200, Result).

fetch_non_existant_container(_) ->
    Result = http_fetch_container("bar"),
    ?assertStatus(404, Result),
    ?assertEqual([<<"Cannot load container bar">>], ej:get({"error"}, chef_json:decode(respBody(Result)))).

fetch_existant_container(_) ->
    http_create_container("foo"),
    Response = http_fetch_container("foo"),
    ?assertStatus(200, Response),
    Ejson = chef_json:decode(respBody(Response)),
    ?assertEqual(<<"foo">>, ej:get({"containername"}, Ejson)),
    ?assertEqual(<<"foo">>, ej:get({"containerpath"}, Ejson)).

update_container(_) ->
    http_create_container("foo"),
    UpdateJson = {[{<<"containername">>, <<"bar">>},
                   {<<"containerpath">>, <<"foo">>},
                   {<<"extra-data">>, <<"ignored">>}]},
    ?assertStatus(405, http_update_container("foo", UpdateJson)).

http_list_containers() ->
    Request = {"http://localhost:8000/organizations/org/containers", ?DEFAULT_HEADERS},
    httpc:request(get, Request, [], []).

http_fetch_container(Name) ->
    Request = {"http://localhost:8000/organizations/org/containers/" ++ Name, ?DEFAULT_HEADERS},
    httpc:request(get, Request, [], []).

http_create_container(Name) ->
    Url = "http://localhost:8000/organizations/org/containers",
    Body = chef_json:encode({[{<<"containername">>, list_to_binary(Name)}]}),
    Request = {Url, ?DEFAULT_HEADERS, "application/json", Body},
    httpc:request(post, Request, [], []).

http_delete_container(Name) ->
    Request = {"http://localhost:8000/organizations/org/containers/" ++ Name, ?DEFAULT_HEADERS},
    httpc:request(delete, Request, [], []).

http_update_container(Name, Ejson) ->
    Request = {"http://localhost:8000/organizations/org/containers/" ++ Name,
               ?DEFAULT_HEADERS,
               "application/json",
               chef_json:encode(Ejson)},
    httpc:request(put, Request, [], []).

respBody({ok, {{_, _, _}, _, Body}}) ->
    Body.
