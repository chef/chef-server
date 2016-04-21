%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_containers_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("chef_types.hrl").
-include("oc_chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(context, {reqid :: binary(),
                  otto_connection,
                  darklaunch = undefined}).

-compile([export_all, {parse_transform, lager_transform}]).

-define(ORG_ID, <<"00000000000000000000000000000000">>).
-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000000">>).
-define(AUTHZ_ID, <<"00000000000000000000000000000001">>).
-define(CLIENT_NAME, <<"test-client">>).

init_per_suite(Config) ->
    Config2 = setup_helper:start_server(Config),

    OrganizationRecord = chef_object:new_record(oc_chef_organization,
                                                ?ORG_ID,
                                                ?ORG_AUTHZ_ID,
                                                {[{<<"name">>, <<"org">>},
                                                  {<<"full_name">>, <<"org">>},
                                                  {<<"guid">>, <<"d540a2e7743112c732e4bb91ecc1df02">>}]}),
    Result2 = chef_db:create(OrganizationRecord,
                   #context{reqid = <<"fake-req-id">>},
                   <<"00000000000000000000000000000001">>),
    io:format("Organization Create Result ~p~n", [Result2]),

    %% create the test client
    %% {Pubkey, _PrivKey} = chef_wm_util:generate_keypair("name", "reqid"),
    ClientRecord = chef_object:new_record(chef_client,
                                          ?ORG_ID,
                                          ?AUTHZ_ID,
                                          {[{<<"name">>, ?CLIENT_NAME},
                                            {<<"validator">>, true},
                                            {<<"admin">>, true},
                                            {<<"public_key">>, <<"stub-pub">>}]}),
    io:format("ClientRecord ~p~n", [ClientRecord]),
    Result = chef_db:create(ClientRecord,
                   #context{reqid = <<"fake-req-id">>},
                   <<"00000000000000000000000000000001">>),
    io:format("Client Create Result ~p~n", [Result]),
    Config2.

end_per_suite(Config) ->
    Config2 = setup_helper:stop_server(Config),
    Config2.

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
    delete_all_containers(),
    Config.

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
    ?assertMatch({ok, "200", _, _} , Result),
    ok.

list_when_created_containers(_) ->
    Containers = ["foo", "bar"],
    [ http_create_container(Container) || Container <- Containers ],
    {ok, ResponseCode, _, ResponseBody} = http_list_containers(),
    ?assertEqual("200", ResponseCode),
    Ejson = ejson:decode(ResponseBody),
    {ContainerList} = Ejson,
    [ ?assertEqual(true, proplists:is_defined(list_to_binary(Container), ContainerList))
      || Container <- Containers ].

create_container(_) ->
    Result = http_create_container("foo"),
    ?assertMatch({ok, "201", _, _} , Result),
    ok.

create_with_bad_name(_) ->
    Result = http_create_container("name with spaces"),
    ?assertMatch({ok, "400", _, _}, Result),
    ok.

delete_container(_) ->
    http_create_container("foo"),
    Result = http_delete_container("foo"),
    ?assertMatch({ok, "200", _, _} , Result),
    ok.

fetch_non_existant_container(_) ->
    Result = {ok, _, _, ResponseBody} = http_fetch_container("bar"),
    ?assertMatch({ok, "404", _, ResponseBody} , Result),
    ?assertEqual([<<"Cannot load container bar">>], ej:get({"error"}, ejson:decode(ResponseBody))),
    ok.

fetch_existant_container(_) ->
    http_create_container("foo"),
    {ok, ResponseCode, _, ResponseBody} = http_fetch_container("foo"),
    ?assertMatch("200", ResponseCode),
    Ejson = ejson:decode(ResponseBody),
    ?assertEqual(<<"foo">>, ej:get({"containername"}, Ejson)),
    ?assertEqual(<<"foo">>, ej:get({"containerpath"}, Ejson)).

update_container(_) ->
    http_create_container("foo"),
    UpdateJson = {[{<<"containername">>, <<"bar">>},
                   {<<"containerpath">>, <<"foo">>},
                   {<<"extra-data">>, <<"ignored">>}]},

    ?assertMatch({ok, "405", _, _}, http_update_container("foo", UpdateJson)),
    ok.

http_list_containers() ->
    ibrowse:send_req("http://localhost:8000/organizations/org/containers",
                     [{"x-ops-userid", "test-client"},
                      {"accept", "application/json"}],
                     get).

http_fetch_container(Name) ->
    ibrowse:send_req("http://localhost:8000/organizations/org/containers/" ++ Name,
                     [{"x-ops-userid", "test-client"},
                      {"accept", "application/json"}],
                     get).

http_create_container(Name) ->
    ibrowse:send_req("http://localhost:8000/organizations/org/containers",
                     [{"x-ops-userid", "test-client"},
                      {"accept", "application/json"},
                      {"content-type", "application/json"}
                     ],post, ejson:encode({[{<<"containername">>, list_to_binary(Name)}]})).

http_delete_container(Name) ->
    ibrowse:send_req("http://localhost:8000/organizations/org/containers/" ++ Name,
                     [{"x-ops-userid", "test-client"},
                      {"accept", "application/json"}
                     ], delete).

http_update_container(Name, Ejson) ->
    ibrowse:send_req("http://localhost:8000/organizations/org/containers/" ++ Name,
                     [{"x-ops-userid", "test-client"},
                      {"accept", "application/json"},
                      {"content-type", "application/json"}
                     ], put, ejson:encode(Ejson)).
