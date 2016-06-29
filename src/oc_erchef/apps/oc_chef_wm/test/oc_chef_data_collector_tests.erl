%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Ryan Cragun <ryan@chef.io>
%% @author John Keiser <jkeiser@chef.io.
%%
%% Copyright 2016 Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_data_collector_tests).

-include("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

oc_chef_data_collector_notify_unsupported_entities_test_() ->
    States = [{"search", #search_state{}},
              {"principal", #principal_state{}},
              {"depsolver", #depsolver_state{}},
              {"sandbox", #sandbox_state{}},
              {"control", #control_state{}},
              {"policy", #policy_state{}},
              {"chef_role", #chef_role{}},
              {"chef_environment", #chef_environment{}}],
    [{"checking state " ++ ResourceName,
      fun() ->
              State = #base_state{resource_state=ResourceState},
              Ret = oc_chef_data_collector:notify(req, State),
              ?assertEqual(ok, Ret)
            end
     } || {ResourceName, ResourceState} <- States ].

oc_chef_data_collector_failed_actions_test_() ->
    ErrorCodes = [500, 400, 401, 402, 403, 404],
    MockedModules = [wrq, data_collector_http],
    {foreach,
     fun() -> oc_chef_wm_test_utils:setup(MockedModules),
              chef_index_test_utils:start_stats_hero()
     end,
     fun(_) -> oc_chef_wm_test_utils:cleanup(MockedModules) end,
     [{"skips notifying" ++ integer_to_list(ErrorCode) ++ "errors",
       fun() ->
               State = #base_state{resource_state=#key_state{}},
               meck:expect(wrq, method, fun(req) -> 'DELETE' end),
               meck:expect(wrq, response_code, fun(req) -> ErrorCode end),
               Ret = oc_chef_data_collector:notify(req, State),
               ?assertEqual(ok, Ret)
            end
     } || ErrorCode <- ErrorCodes]}.

oc_chef_data_collector_notify_test_() ->
    MockedModules = [chef_wm_util, wrq, data_collector_http],
    HostFQDN = <<"hostname.example.com">>,
    chef_index_test_utils:start_stats_hero(),
    ok = application:set_env(oc_chef_wm, actions_fqdn, HostFQDN),
    {foreach,
     fun() -> oc_chef_wm_test_utils:setup(MockedModules) end,
     fun(_) -> oc_chef_wm_test_utils:cleanup(MockedModules) end,
     [
      test_notify_acl() ++
      test_notify_association() ++
      test_notify_client() ++
      test_notify_cookbook() ++
      test_notify_environment() ++
      test_notify_group() ++
      test_notify_node() ++
      test_notify_organization() ++
      test_notify_role() ++
      test_notify_user() ++
      test_notify_key()
    ]}.

test_notify_acl() ->
    ResourceState = #acl_state{acl_data = {[{<<"grant">>, <<"permission">>}]}, type = organization},
    ExpectedMsgData = {"permission",
                       "grant",
                       [{<<"parent_name">>, undefined},
                        {<<"parent_type">>, <<"organization">>},
                        {<<"data">>, {[{<<"grant">>, <<"permission">>}]}}]},
    [ test_notify("acl", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'update'},
                                              {"PUT", 201, 'update'},
                                              {"POST", 201, 'create'},
                                              {"DELETE", 200, 'delete'}]].
test_notify_association() ->
    ResourceState = #association_state{data = {[{<<"grant">>, <<"permission">>}]},
                                       user = #chef_user{username = <<"rob">>},
                                       org_user_invite = #oc_chef_org_user_invite{}},
    ExpectedMsgData = {"user", "rob", [{<<"data">>, {[{<<"grant">>, <<"permission">>}]}}]},
    [ test_notify("association", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'associate'},
                                              {"PUT", 201, 'associate'},
                                              {"POST", 201, 'invite'}]].

test_notify_client() ->
    ResourceState = #client_state{client_data = {[{<<"name">>,<<"node-foo">>}]}},
    ExpectedMsgData = {"client",
                       "node-foo",
                       [{<<"data">>, {[{<<"name">>,<<"node-foo">>}]}}]},
    [ test_notify("client", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'update'},
                                              {"PUT", 201, 'update'},
                                              {"POST", 201, 'create'},
                                              {"DELETE", 200, 'delete'}]].

test_notify_cookbook() ->
    ResourceState = #cookbook_state{cookbook_name = db,
                                    cookbook_version = {6,6,6},
                                    cookbook_data= {[{<<"name">>, <<"db">>}]}},
    ExpectedMsgData = {"version",
                        "6.6.6",
                        [{<<"parent_type">>, <<"cookbook">>},
                         {<<"parent_name">>, <<"db">>},
                         {<<"data">>, {[{<<"name">>, <<"db">>}]}}]},
    [ test_notify("cookbook", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'update'},
                                              {"PUT", 201, 'create'},
                                              {"POST", 201, 'create'},
                                              {"DELETE", 200, 'delete'}]].

test_notify_environment() ->
    ResourceState = #environment_state{environment_data=entity_data()},
    ExpectedMsgData = {"environment", "db", [{<<"data">>, {[{<<"name">>,<<"db">>}]}}]},
    [ test_notify("environment", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'update'},
                                              {"PUT", 201, 'update'},
                                              {"POST", 201, 'create'},
                                              {"DELETE", 200, 'delete'}]].

test_notify_group() ->
    ResourceState = #group_state{group_data=entity_data(),
                                 group_authz_id='11111',
                                 oc_chef_group = #oc_chef_group{}},
    ExpectedMsgData = {"group", "db", [{<<"data">>, {[{<<"name">>,<<"db">>}]}}]},
    [ test_notify("group", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'update'},
                                              {"PUT", 201, 'update'},
                                              {"POST", 201, 'create'},
                                              {"DELETE", 200, 'delete'}]].

test_notify_node() ->
    ResourceState = #node_state{node_data=entity_data()},
    ExpectedMsgData = {"node", "db", [{<<"data">>, {[{<<"name">>,<<"db">>}]}}]},
    [ test_notify("node", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'update'},
                                              {"PUT", 201, 'update'},
                                              {"POST", 201, 'create'},
                                              {"DELETE", 200, 'delete'}]].

test_notify_organization() ->
    ResourceState = #organization_state{organization_data = {[{<<"name">>,<<"cmwest">>}]},
                                        oc_chef_organization = #oc_chef_organization{name = <<"cmwest">>}},
    ExpectedMsgData = {"organization", "cmwest", [{<<"data">>, {[{<<"name">>,<<"cmwest">>}]}}]},
    [ test_notify("organization", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'update'},
                                              {"PUT", 201, 'update'},
                                              {"POST", 201, 'create'},
                                              {"DELETE", 200, 'delete'}]].

test_notify_role() ->
    ResourceState = #role_state{role_data=entity_data()},
    ExpectedMsgData = {"role", "db", [{<<"data">>, {[{<<"name">>,<<"db">>}]}}]},
    [ test_notify("role", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'update'},
                                              {"PUT", 201, 'update'},
                                              {"POST", 201, 'create'},
                                              {"DELETE", 200, 'delete'}]].

test_notify_user() ->
    ResourceState = #user_state{user_data=entity_data()},
    ExpectedMsgData = {"user", "db", [{<<"data">>, {[{<<"name">>,<<"db">>}]}}]},
    [ test_notify("user", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'update'},
                                              {"PUT", 201, 'update'},
                                              {"POST", 201, 'create'},
                                              {"DELETE", 200, 'delete'}]].

test_notify_key() ->
    ExpectedMsgData = {"key",
                       "lucifer",
                        [{<<"parent_type">>, <<"mythical_being">>},
                         {<<"parent_name">>, <<"lucifer">>},
                         {<<"data">>, {[{<<"name">>, <<"lucifer">>}]}}]},
    ResourceState = #key_state{key_data={[{<<"name">>, <<"lucifer">>}]},
                               parent_name = lucifer,
                               type = mythical_being},
    [ test_notify("key", ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task)
      || {ReqMethod, ResponseCode, Task} <- [ {"PUT", 200, 'update'},
                                              {"PUT", 201, 'update'},
                                              {"POST", 201, 'create'},
                                              {"DELETE", 200, 'delete'}]].

test_notify(EntityType, ReqMethod, ResponseCode, ResourceState, ExpectedMsgData, Task) ->
    [{EntityType ++ " " ++ ReqMethod ++ " returning " ++ integer_to_list(ResponseCode) ++ " sends the " ++ EntityType ++ " to the data collector",
     fun() ->
        {ExpectedEntityType, ExpectedEntityName, ExpectedEntityData} = ExpectedMsgData,
        meck:expect(wrq, method, fun(req) -> list_to_atom(ReqMethod) end),
        meck:expect(wrq, response_code, fun(req) -> ResponseCode end),
        meck:expect(chef_wm_util, object_name, fun(_EntityType, req) -> undefined end),
        meck:expect(chef_wm_util, extract_from_path, fun(_Path, req) -> undefined end),
        meck:expect(uuid, uuid_to_string, fun(_UUID) -> "11111111-1111-1111-1111-111111111111" end),
        meck:expect(wrq, get_req_header,
            fun(Field, req) ->
                case Field of
                    "x-ops-timestamp" -> "2011-07-03T13:21:50Z";
                    "x-forwarded-for" -> "127.0.0.1";
                    "user-agent" -> "knife 11.10.0";
                    "x-remote-request-id" -> undefined
                end
            end),

        meck:expect(data_collector_http, post, fun(_Path, Msg) ->
            ?assertEqual(chef_json:encode(msg(Task,
                                              ExpectedEntityType,
                                              ExpectedEntityName,
                                              ExpectedEntityData)),
                         Msg),
            ok
        end),
        State = make_state(ResourceState),
        oc_chef_data_collector:notify(req, State)
     end
    }].

msg(Task, EntityType, EntityName, EntityData) ->
  {[{<<"message_type">>, <<"action">>},
    {<<"message_version">>, <<"0.1.1">>},
    {<<"organization_name">>, <<"cmwest">>},
    {<<"service_hostname">>, <<"hostname.example.com">>},
    {<<"recorded_at">>, <<"2011-07-03T13:21:50Z">>},
    {<<"remote_hostname">>, <<"127.0.0.1">>},
    {<<"request_id">>, <<"Xfh5mCQvjRgWDdlevrdyGt8M4lecXmN3gpGXrKKiUYqKdeD3">>},
    {<<"requestor_name">>, <<"rob">>},
    {<<"requestor_type">>, <<"user">>},
    {<<"user_agent">>, <<"knife 11.10.0">>},
    {<<"id">>, <<"11111111-1111-1111-1111-111111111111">>},
    {<<"task">>, Task},
    {<<"entity_type">>, list_to_binary(EntityType)},
    {<<"entity_name">>, list_to_binary(EntityName)}
  ] ++ EntityData}.

entity_data() ->
    {[{<<"name">>, <<"db">>}]}.

-spec make_state(ResourceState :: resource_state()) -> #base_state{}.
make_state(ResourceState) ->
    #base_state{
       resource_state=ResourceState,
       reqid = <<"Xfh5mCQvjRgWDdlevrdyGt8M4lecXmN3gpGXrKKiUYqKdeD3">>,
       requestor = #chef_requestor{name = <<"rob">>, type = <<"user">>},
       organization_name = <<"cmwest">>
    }.
