%% @author Mark Anderson <mark@chef.io>
%% @version 0.0.1
%% @doc authorization - Interface to the opscode authorization servize
%%
%% This module is an Erlang port of the mixlib-authorization Ruby gem.
%%
%% Copyright 2011-2018 Chef Software, Inc.
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

-module(oc_chef_authz_tests).

-compile([export_all]).

%% -export([raw_request_test_helper/1,
%%          test_setup/0,
%%          test_cleanup/1]).

-include("oc_chef_authz.hrl").

%-define(setup, oc_chef_authz_tests).
-define(setup, test_utils).

-define(SUPERUSER,  <<"platform-superuser">>).
-define(test_org_name, <<"clownco">>).
-define(test_org_id,  <<"0aa7c5c35fbb4f1890c0a673511137af">>).
-define(test_org_admin, <<"clownco-org-admin">>).
-define(test_org_user1, <<"cooky">>).
-define(no_such_id, <<"deadbeefdeadbeefdeadbeefdeadbeef">>).
-define(authz_host, "http://localhost:5959").
-define(chef_host_name, "localhost").
-define(chef_host_port, 5984).
-define(AUTOMECK_FILE(TestName), test_utils:automeck_file(?MODULE, TestName)).


-include_lib("eunit/include/eunit.hrl").

%% user_lookup_test() ->
%%     test_utils:test_setup(),                    % starts stats_hero
%%     automeck:mocks(?AUTOMECK_FILE(user_lookup)),
%%     Context = chef_db:make_context(<<"testing">>), % req_id must be a binary
%%     ?assert(is_authz_id(oc_chef_authz:username_to_auth_id(Context, ?SUPERUSER))),
%%     meck:unload().

resource_test_() ->
    {foreach,
    fun() ->
            error_logger:tty(false),
            automeck:mocks(?AUTOMECK_FILE(resource)),
            test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({_Server, Superuser}) ->
          %% Resource creation
          {"Simple group create",
           fun() ->
               {ok, NewId} = oc_chef_authz:create_resource(Superuser, group),
               true = is_authz_id(NewId)
           end}
      end,
      fun({_Server, Superuser}) ->
              {"Simple group create by non superuser",
               fun() ->
                       {ok, Actor} = oc_chef_authz:create_resource(Superuser, actor),
                       {ok, Group} = oc_chef_authz:create_resource(Actor, group),
                       true = is_authz_id(Group)
               end}
      end,
      fun({_Server, _Superuser}) ->
              {"Simple group create by non-existient user",
               fun() ->
                       {ok, Group} = oc_chef_authz:create_resource(?no_such_id, group),
                       true = is_authz_id(Group)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"group delete",
               fun() ->
                       {ok, NewId} = oc_chef_authz:create_resource(Superuser, group),
                       true = is_authz_id(NewId),
                       ok = oc_chef_authz:delete_resource(Superuser, group, NewId)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"group delete without permission",
               fun() ->
                       {ok, NewId} = oc_chef_authz:create_resource(Superuser, group),
                       true = is_authz_id(NewId),
                       {error, forbidden} = oc_chef_authz:delete_resource(?no_such_id, group, NewId)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"group delete for non-group",
               fun() ->
                       {ok, NewId} = oc_chef_authz:create_resource(Superuser, actor),
                       true = is_authz_id(NewId),
                       {error, server_error} = oc_chef_authz:delete_resource(?no_such_id, group, NewId)
               end}
      end]}.

get_acl_from_resource_test_() ->
    {foreach,
     fun() ->
             error_logger:tty(false),
             automeck:mocks(?AUTOMECK_FILE(get_acl)),
             test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({_Server, Superuser}) ->
              {"get the acl for a newly created resource",
               fun() ->
                       {ok, GroupId} = oc_chef_authz:create_resource(Superuser, group),
                       {ok, Acl} = oc_chef_authz:get_acl_for_resource(Superuser, group, GroupId),
                       %%ACE should only contain the superuser
                       lists:foreach(fun(Ace) -> {_Method, #authz_ace{actors=[Superuser],groups=[]}} = Ace end, Acl)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"get the acl for a non existient resource",
               fun() ->
                       %% Should this be remapped?
                       {error, server_error} = oc_chef_authz:get_acl_for_resource(Superuser, group, ?no_such_id)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"get the acl for a resource you don't have rights to",
               fun() ->
                       {ok, GroupId} = oc_chef_authz:create_resource(Superuser, group),
                       {ok, ActorId} = oc_chef_authz:create_resource(Superuser, actor),
                       ?assertEqual({error, forbidden}, oc_chef_authz:get_acl_for_resource(ActorId, group, GroupId))
               end}
      end]}.

is_authorized_on_resource_test_() ->
    {foreach,
     fun() ->
             error_logger:tty(false),
             test_utils:test_setup()
     end,
     fun(_) ->
             meck:unload()
     end,
     [fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(is_authorized1)),
              {"check if the owner is authorized for grant on a newly created resource",
               fun() ->
                       {ok, ObjectId} = oc_chef_authz:create_resource(Superuser, object),
                       ?assert(oc_chef_authz:is_authorized_on_resource(Superuser, object, ObjectId, actor, Superuser, grant))
               end}
      end,
      fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(is_authorized1)),
              {"check that someone else is not authorized for grant on an newly created resource",
               fun() ->
                       {ok, ObjectId} = oc_chef_authz:create_resource(Superuser, object),
                       {ok, ActorId} = oc_chef_authz:create_resource(Superuser, actor),
                       ?assertNot(oc_chef_authz:is_authorized_on_resource(Superuser, object, ObjectId, actor, ActorId, grant))
               end}
      end,
      fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(is_authorized2)),
              {"check that someone else can query permissions on an newly created resource",
               fun() ->
                       {ok, ObjectId} = oc_chef_authz:create_resource(Superuser, object),
                       {ok, ActorId} = oc_chef_authz:create_resource(Superuser, actor),
                       {ok, GroupId} = oc_chef_authz:create_resource(Superuser, group),
                       ?assert(oc_chef_authz:is_authorized_on_resource(ActorId, object, ObjectId, actor, Superuser, grant)),
                       ?assert(oc_chef_authz:is_authorized_on_resource(ActorId, group, GroupId, actor, Superuser, grant))
               end}
      end,
      fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(is_authorized2)),
              {"queries on a nonexistient object fail",
               fun() ->
                       %% would expect not_found
                       ?assertNot(oc_chef_authz:is_authorized_on_resource(Superuser, object, ?no_such_id, actor, Superuser, grant))
               end}
      end,
      fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(is_authorized3)),
              {"queries on a object of the wrong type fail",
               fun() ->
                       {ok, ObjectId} = oc_chef_authz:create_resource(Superuser, object),
                       {ok, ActorId} = oc_chef_authz:create_resource(Superuser, actor),
                       %% would expect server_error
                       ?assertNot(oc_chef_authz:is_authorized_on_resource(Superuser, group, ObjectId, actor, ActorId, grant)),
                       ?assertNot(oc_chef_authz:is_authorized_on_resource(Superuser, object, ObjectId, group, ActorId, grant))
               end}
      end]}.

bulk_actor_is_authorized_test_() ->
    {foreach,
     fun() ->
             error_logger:tty(false),
             test_utils:test_setup(),
              start_apps(),
              meck:new(oc_httpc),
              Server = "http://127.0.0.1:9463",
              Superuser = <<"1d774f20735e25fa8a0e97d624b68346">>,
              application:set_env(oc_chef_authz, authz_superuser, Superuser),
              application:set_env(oc_chef_authz, authz_root_url, Server),
              application:set_env(oc_chef_authz, authz_service, [{root_url, Server}, {timeout, 10000000}] ),
              {Server, Superuser}
     end,
     fun(_) ->
             stop_apps(),
             meck:unload()
     end,
     [
      {"all allowed",
       fun() ->
               WantHeaders = [{"X-Ops-User-Id", "front-end-service"},
                              {"Accept", "application/json"},
                              {"Content-Type", "application/json"},
                              {"X-Ops-Requesting-Actor-Id", "abcabc123"},
                              {"X-Request-Id","test-req-id"}],
               BifrostBody = <<"{\"requestor_id\":\"abcabc123\","
                               "\"permission\":\"read\","
                               "\"type\":\"object\","
                               "\"collection\":[\"a\",\"b\"]}">>,
               meck:expect(oc_httpc, request,
                           fun(oc_chef_authz_http, "bulk", Headers, post, Body, 10000000)
                                 when Headers == WantHeaders andalso
                                      Body == BifrostBody ->
                                   {ok, "204", [], []}
                           end),
               Resources = [{<<"a">>, a_data}, {<<"b">>, b_data}],
               ?assertEqual(true,
                            oc_chef_authz:bulk_actor_is_authorized(<<"test-req-id">>, <<"abcabc123">>, object,
                                                                   Resources, read))
       end},

      {"all not allowed",
              fun() ->
               WantHeaders = [{"X-Ops-User-Id", "front-end-service"},
                              {"Accept", "application/json"},
                              {"Content-Type", "application/json"},
                              {"X-Ops-Requesting-Actor-Id", "abcabc123"},
                              {"X-Request-Id","test-req-id"}],
               BifrostBody = <<"{\"requestor_id\":\"abcabc123\","
                               "\"permission\":\"read\","
                               "\"type\":\"object\","
                               "\"collection\":[\"a\",\"b\"]}">>,
               meck:expect(oc_httpc, request,
                           fun(oc_chef_authz_http, "bulk", Headers, post, Body, 10000000)
                                 when Headers == WantHeaders andalso
                                      Body == BifrostBody ->
                                   {ok, "200", [], <<"{\"unauthorized\":[\"a\",\"b\"]}">>}
                           end),
               Resources = [{<<"a">>, a_data}, {<<"b">>, b_data}],
               ?assertEqual({false,[b_data, a_data]},
                            oc_chef_authz:bulk_actor_is_authorized(<<"test-req-id">>, <<"abcabc123">>, object,
                                                                   Resources, read))
       end},

      {"some not allowed",
       fun() ->
               %% this test exercises the internal get_data_for_id
               Ids = [ list_to_binary([$a + I]) || I <- lists:seq(0, 25) ],
               meck:expect(oc_httpc, request,
                           fun(oc_chef_authz_http, "bulk", _Headers, post, _Body, 10000000) ->
                                   {ok, "200", [], <<"{\"unauthorized\":[\"b\",\"l\",\"o\",\"t\"]}">>}
                           end),
               Resources = [ {I, binary_to_atom(<<I/binary, "_data">>, utf8)} || I <- Ids ],
               ?assertEqual({false,[t_data, o_data, l_data, b_data]},
                            oc_chef_authz:bulk_actor_is_authorized(<<"test-req-id">>,
                                                                   <<"abcabc123">>,
                                                                   object,
                                                                   Resources,
                                                                   read))
       end},

      {"bad response",
                     fun() ->
               WantHeaders = [{"X-Ops-User-Id", "front-end-service"},
                              {"Accept", "application/json"},
                              {"Content-Type", "application/json"},
                              {"X-Ops-Requesting-Actor-Id", "abcabc123"},
                              {"X-Request-Id","test-req-id"}],
               BifrostBody = <<"{\"requestor_id\":\"abcabc123\","
                               "\"permission\":\"read\","
                               "\"type\":\"object\","
                               "\"collection\":[\"a\",\"b\"]}">>,
               meck:expect(oc_httpc, request,
                           fun(oc_chef_authz_http, "bulk", Headers, post, Body, 10000000)
                                 when Headers == WantHeaders andalso
                                      Body == BifrostBody ->
                                   {ok, "200", [], <<"{\"not_expected\":[\"a\",\"b\"]}">>}
                           end),
               Resources = [{<<"a">>, a_data}, {<<"b">>, b_data}],
               ?assertEqual({error, unexpected_body},
                            oc_chef_authz:bulk_actor_is_authorized(<<"test-req-id">>, <<"abcabc123">>, object,
                                                                   Resources, read))
       end},

      {"server error",
                     fun() ->
               WantHeaders = [{"X-Ops-User-Id", "front-end-service"},
                              {"Accept", "application/json"},
                              {"Content-Type", "application/json"},
                              {"X-Ops-Requesting-Actor-Id", "abcabc123"},
                              {"X-Request-Id","test-req-id"}],
               BifrostBody = <<"{\"requestor_id\":\"abcabc123\","
                               "\"permission\":\"read\","
                               "\"type\":\"object\","
                               "\"collection\":[\"a\",\"b\"]}">>,
               meck:expect(oc_httpc, request,
                           fun(oc_chef_authz_http, "bulk", Headers, post, Body, 10000000)
                                 when Headers == WantHeaders andalso
                                      Body == BifrostBody ->
                                   {ok, "500", [], <<"boom">>}
                           end),
               Resources = [{<<"a">>, a_data}, {<<"b">>, b_data}],
               ?assertEqual({error, server_error},
                            oc_chef_authz:bulk_actor_is_authorized(<<"test-req-id">>, <<"abcabc123">>, object,
                                                                   Resources, read))
       end}
     ]}.

get_container_aid_for_object_test_() ->
    {foreach,
     fun() ->
             error_logger:tty(false),
             automeck:mocks(?AUTOMECK_FILE(container_aid)),
              test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({Context, _Superuser}) ->
              {"Can we get a real container",
               fun() ->
                       ObjectId = oc_chef_authz:get_container_aid_for_object(Context, ?test_org_id, node),
                       ?assert(is_authz_id(ObjectId))
               end}
      end]}.

create_entity_if_authorized_test_() ->
    {foreach,
     fun() ->
             error_logger:tty(false),
             test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
    [fun({Server, _Superuser}) ->
             automeck:mocks(?AUTOMECK_FILE(create_if_authorized1)),
             {"check if the admin user can create a new object (node)",
              fun() ->
                      AdminAID = <<"cf5d90545fbbac541225fbd9e73e94d9">>,
                      UserAID = <<"cf5d90545fbbac541225fbd9e73e4e42">>,
                      %% so bobo doesn't get created by setup test, probably only by running
                      %% features.
                      %% TODO: also need to decide on behavor for user_record_to_authz_id(not_found)
                      {ok, ObjectId} = oc_chef_authz:create_entity_if_authorized(Server, ?test_org_id, AdminAID, node),
                      ?assert(is_authz_id(ObjectId)),
                      %% the creator should have access
                      [ ?assert(oc_chef_authz:is_authorized_on_resource(AdminAID, object, ObjectId, actor,
                                                                     AdminAID, Method)) || Method <- ?access_methods],
                      %% a regular user should not
                      [ ?assertNot(oc_chef_authz:is_authorized_on_resource(UserAID, object, ObjectId, actor,
                                                                         UserAID, Method)) || Method <- ?access_methods]
              end}
     end,
     fun({Server, _Superuser}) ->
             automeck:mocks(?AUTOMECK_FILE(create_if_authorized2)),
             {"check that someone else is not authorized to create a new object",
              fun() ->
                      UserAID = <<"cf5d90545fbbac541225fbd9e73e4e42">>,
                      ?assertEqual({error, forbidden},
                                   oc_chef_authz:create_entity_if_authorized(Server, ?test_org_id, UserAID, node))
              end}
     end]}.

random_bogus_port() ->
    {ok, S} = gen_udp:open(0, [binary, {active, once}]),
    {ok, Port} = inet:port(S),
    gen_udp:close(S),
    Port.

needed_apps() ->
    [crypto, asn1, ibrowse, pooler, stats_hero, public_key, ssl, epgsql, sqerl, oc_chef_authz].

start_apps() ->
    error_logger:tty(false),
    application:set_env(oc_chef_authz, cleanup_batch_size, 100),
    application:set_env(oc_chef_authz, authz_superuser_id, <<"superuser">>),
    application:set_env(oc_chef_authz, cleanup_interval, 5000),
    application:set_env(stats_hero, estatsd_host, "localhost"),
    application:set_env(stats_hero, estatsd_port, random_bogus_port()),
    application:set_env(stats_hero, udp_socket_pool_size, 1),
    application:set_env(oc_chef_authz, authz_service,
                        [{root_url, "http://test-authz-service:2323"},
                         {timeout, 200}, {init_count, 5}, {max_count, 6}]),
    [ ok = ensure_started(A) || A <- needed_apps() ],
    ok.

stop_apps() ->
    [ application:stop(A) || A <- lists:reverse(needed_apps()) ],
    ok.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        E ->
            E
    end.

ping_test_() ->
    {foreach,
     fun() ->
             error_logger:tty(false),
             start_apps(),
             MockMods = [ibrowse, ibrowse_http_client],
             [ meck:new(M) || M <- MockMods ],
             MockMods
     end,
     fun(MockMods) ->
             [ meck:unload(M) || M <- MockMods ],
             stop_apps(),
             error_logger:tty(true),
             ok
     end,
     [
      {"ping pong",
       fun() ->
               FakePid = spawn(fun() -> ok end),
               meck:expect(ibrowse_http_client, start_link, fun(_) -> {ok, FakePid} end),
               meck:expect(ibrowse, send_req_direct,
                           fun(Pid, "http://test-authz-service:2323/_ping", _,
                               get, _, _, 200) when Pid =:= FakePid ->
                                   {ok, "200", [], <<"{\"status\":\"ok\"}">>}
                           end),
               ?assertEqual(pong, oc_chef_authz_http:ping())
       end},

      {"ping pang 500",
       fun() ->
               FakePid = spawn(fun() -> ok end),
               meck:expect(ibrowse_http_client, start_link, fun(_) -> {ok, FakePid} end),
               meck:expect(ibrowse, send_req_direct,
                           fun(Pid, "http://test-authz-service:2323/_ping", _,
                               get, _, _, 200) when Pid =:= FakePid ->
                                   {ok, "500", [], <<"{\"status\":\"NOT OK\"}">>}
                           end),
               ?assertEqual(pang, oc_chef_authz_http:ping())
       end},

      {"ping pang error",
       fun() ->
               FakePid = spawn(fun() -> ok end),
               meck:expect(ibrowse_http_client, start_link, fun(_) -> {ok, FakePid} end),
               meck:expect(ibrowse, send_req_direct,
                           fun(Pid, "http://test-authz-service:2323/_ping", _,
                               get, _, _, 200) when Pid =:= FakePid ->
                                   {error, req_timedout}
                           end),
               ?assertEqual(pang, oc_chef_authz_http:ping())
       end}
      ]}.

application_lifecycle_test() ->
    {setup,
     fun() ->
             error_logger:tty(false),
             Apps = [ibrowse, pooler],
             [ application:start(A) || A <- Apps ],
             Apps
     end,
     fun(Apps) ->
             [ application:stop(A) || A <- lists:reverse(Apps) ],
             error_logger:tty(true)
     end,
     [
      fun() ->
              ?assertEqual(ok, application:start(oc_chef_authz)),
              ?assertEqual(ok, application:stop(oc_chef_authz))
      end
     ]}.

%% helper for tests
is_authz_id(Id) when is_binary(Id) ->
    case re:run(Id, "[0-9a-f]*", []) of
        {match, _} -> true;
        match -> true;
        nomatch -> false
    end;
is_authz_id(_Id) -> false.
