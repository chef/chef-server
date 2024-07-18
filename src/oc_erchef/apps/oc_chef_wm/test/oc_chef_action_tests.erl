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

-module(oc_chef_action_tests).

-include("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(VHOST, <<"/actions">>).

msg(Task) ->
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
      {<<"entity_type">>, <<"node">>},
      {<<"entity_name">>, <<"db">>}
     ]}.

msg_with_payload(Task) ->
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
    {<<"entity_type">>, <<"node">>},
    {<<"entity_name">>, <<"db">>},
    {<<"data">>, {[{<<"name">>, <<"db">>}]}}
   ]}.

task_for_cookbooks_test_() ->
    MockedModules = [wrq],
    State = #base_state{resource_state=#cookbook_state{}},
    {foreach,
     fun() -> oc_chef_wm_test_utils:setup(MockedModules) end,
     fun(_) -> oc_chef_wm_test_utils:cleanup(MockedModules) end,
     [
            {"PUT with 201 is create",
             fun() -> meck:expect(wrq, method, fun(req) -> 'PUT' end),
                      meck:expect(wrq, response_code, fun(req) -> 201 end),
                      Ret = oc_chef_action:task(req, State),
                      ?assertEqual(<<"create">>, Ret)
                end
            },
            {"PUT with 200 is update",
             fun() -> meck:expect(wrq, method, fun(req) -> 'PUT' end),
                      meck:expect(wrq, response_code, fun(req) -> 200 end),
                      Ret = oc_chef_action:task(req, State),
                      ?assertEqual(<<"update">>, Ret)
                end
            },
            {"DELETE with 200 is delete",
             fun() -> meck:expect(wrq, method, fun(req) -> 'DELETE' end),
                      meck:expect(wrq, response_code, fun(req) -> 200 end),
                      Ret = oc_chef_action:task(req, State),
                      ?assertEqual(<<"delete">>, Ret)
                end
            }
     ]
    }.

key_for_method_test() ->
    ?assertEqual(<<"create">>, oc_chef_action:key_for_method('POST')),
    ?assertEqual(<<"update">>, oc_chef_action:key_for_method('PUT')),
    ?assertEqual(<<"delete">>, oc_chef_action:key_for_method('DELETE')).

extract_entity_info_test_() ->
    MockedModules = [chef_wm_util],
    {foreach,
     fun() -> oc_chef_wm_test_utils:setup(MockedModules) end,
     fun(_) -> oc_chef_wm_test_utils:cleanup(MockedModules) end,
     [{"client entity info",
       fun() -> State = #client_state{client_data = {[{<<"name">>, <<"node-foo">>}]} },
                meck:expect(chef_wm_util,object_name, fun(client, req) -> undefined end),
                Ret = oc_chef_action:extract_entity_info(req, State),
                Expected = entity({[{<<"name">>, <<"node-foo">>}]}, <<"client">>, <<"node-foo">>),
               ?assertEqual(Expected, Ret)
             end},
     {"client entity info using name from path",
       fun() -> State = #client_state{client_data = undefined },
                meck:expect(chef_wm_util,object_name, fun(client, req) -> <<"node-foo">> end),
                Ret = oc_chef_action:extract_entity_info(req, State),
                Expected = entity(undefined, <<"client">>, <<"node-foo">>),
               ?assertEqual(Expected, Ret)
             end},
     {"cookbook entity info",
       fun() -> State = #cookbook_state{cookbook_name = <<"apache2">>,
                                        cookbook_version = {1,2,3},
                                        cookbook_data = {[{<<"name">>, <<"node-foo">>}]}},
                Ret = oc_chef_action:extract_entity_info(req, State),
                Expected = parent_entity({[{<<"name">>, <<"node-foo">>}]}, <<"cookbook">>, <<"apache2">>,
                                         <<"version">>, <<"1.2.3">>),
                ?assertEqual(Expected, Ret)
             end},
     {"data bag entity info",
       fun() -> State = #data_state{data_bag_name = <<"users">>,
                                    data_bag_item_name = undefined},
                Ret = oc_chef_action:extract_entity_info(req, State),
                Expected = entity({[{<<"name">>, <<"users">>}]}, <<"bag">>, <<"users">>),
                ?assertEqual(Expected, Ret)
             end},
     {"data bag item entity info",
       fun() -> State = #data_state{data_bag_name = <<"users">>,
                                    data_bag_item_name = <<"jdoe">>,
                                    data_bag_item_ejson = {[{<<"name">>, <<"node-foo">>}]}},
                Ret = oc_chef_action:extract_entity_info(req, State),
                Expected = parent_entity({[{<<"name">>, <<"node-foo">>}]}, <<"bag">>, <<"users">>,
                                         <<"item">>, <<"jdoe">>),
                ?assertEqual(Expected, Ret)
             end},
     {"environment entity info",
      fun() -> State = #environment_state{environment_data = {[{<<"name">>, <<"production">> }]} },
               meck:expect(chef_wm_util,object_name, fun(environment, req) -> undefined end),
               Ret = oc_chef_action:extract_entity_info(req, State),
               Expected = entity({[{<<"name">>, <<"production">>}]}, <<"environment">>, <<"production">>),
               ?assertEqual(Expected, Ret)
             end},
     {"group entity info",
      fun() -> State = #group_state{group_data = {[{<<"name">>, <<"sysadmins">> }]} },
               meck:expect(chef_wm_util,extract_from_path, fun(group_name, req) -> undefined end),
               Ret = oc_chef_action:extract_entity_info(req, State),
               Expected = entity({[{<<"name">>, <<"sysadmins">>}]}, <<"group">>, <<"sysadmins">>),
               ?assertEqual(Expected, Ret)
             end},
     {"node entity info",
      fun() -> State = #node_state{node_data = {[{<<"name">>, <<"node-foo">> }]} },
               meck:expect(chef_wm_util,object_name, fun(node, req) -> undefined end),
               Ret = oc_chef_action:extract_entity_info(req, State),
               Expected = entity({[{<<"name">>, <<"node-foo">>}]}, <<"node">>, <<"node-foo">>),
               ?assertEqual(Expected, Ret)
             end},
     {"role entity info",
      fun() -> State = #role_state{role_data = {[{<<"name">>, <<"webserver">> }]} },
               meck:expect(chef_wm_util,object_name, fun(role, req) -> undefined end),
               Ret = oc_chef_action:extract_entity_info(req, State),
               Expected = entity({[{<<"name">>, <<"webserver">>}]}, <<"role">>, <<"webserver">>),
               ?assertEqual(Expected, Ret)
             end},
     {"user entity info",
      fun() -> State = #user_state{user_data = {[{<<"name">>, <<"webserver">> }]} },
               meck:expect(chef_wm_util,object_name, fun(user, req) -> undefined end),
               Ret = oc_chef_action:extract_entity_info(req, State),
               Expected = entity({[{<<"name">>, <<"webserver">>}]}, <<"user">>, <<"webserver">>),
               ?assertEqual(Expected, Ret)
             end},
     {"keys entity info",
      fun() -> State = #key_state{key_data= {[{<<"name">>, <<"new-key">> }]}, parent_name = <<"bob">>, type = client},
               meck:expect(chef_wm_util,object_name, fun(key, req) -> undefined end),
               Ret = oc_chef_action:extract_entity_info(req, State),
               Expected = parent_entity({[{<<"name">>, <<"new-key">>}]}, <<"client">>, <<"bob">>, <<"key">>, <<"new-key">>),
               ?assertEqual(Expected, Ret)
      end},
     {"policy entity info",
      fun() -> State = #policy_state{policy_data= {[{<<"name">>, <<"expected_policy_name">>}]}},
               Stub = fun(policy, req) ->
                          <<"expected_policy_name">>;
                         (policy_group_asoc_name, req) ->
                          <<"expected_policy_group_name">>
                      end,
               meck:expect(chef_wm_util,object_name, Stub),
               Ret = oc_chef_action:extract_entity_info(req, State),
               Expected = parent_entity({[{<<"name">>, <<"expected_policy_name">>}]},
                                        <<"policy_group">>, <<"expected_policy_group_name">>,
                                        <<"policy">>, <<"expected_policy_name">>),
               ?assertEqual(Expected, Ret)
      end},
     {"cookbook artifact version entity info",
      fun() -> State = #cookbook_artifact_version_state{oc_chef_cookbook_artifact_version = #oc_chef_cookbook_artifact_version{
                  identifier = <<"abc123">>,
                  name = "nginx"},
                                                 cookbook_artifact_version_data = {[{<<"name">>, <<"nginx">>}]}},
               Ret = oc_chef_action:extract_entity_info(req, State),
               Expected = parent_entity({[{<<"name">>, <<"nginx">>}]}, <<"cookbook_artifact">>, <<"nginx">>,
                                         <<"cookbook_artifact_version">>, <<"abc123">>),
               ?assertEqual(Expected, Ret)
      end}
     ]
    }.

hostname_test_() ->
  HostFQDN = <<"hostname.example.com">>,
  ok = application:set_env(oc_chef_wm, actions_fqdn, HostFQDN),
  [{"gets fqdn correctly",
    fun() -> Ret = oc_chef_action:hostname(),
          ?assertEqual(<<"hostname.example.com">>, Ret)
      end
   }
  ].

maybe_add_remote_request_id_test_() ->
    [{"no remote request added when null",
      fun() -> Msg = msg(<<"create">>),
               Ret = oc_chef_action:maybe_add_remote_request_id(Msg, undefined),
               ?assertEqual(Msg, Ret)
      end
     },
     {"remote request added",
      fun() -> Msg = {[{<<"organization_name">>, <<"foo">>}]},
               ReqId = <<"1eff-dea9-dea9-dea9-jeffdea1-dea9">>,
               Expected = {[{<<"organization_name">>, <<"foo">>},
                            {<<"remote_request_id">>, ReqId}]},
               Ret = oc_chef_action:maybe_add_remote_request_id(Msg, ReqId),
               ?assertEqual(Expected, Ret)
      end
     }
    ].

maybe_add_data_test() ->
  [{"redacts password when <<\"password\">> key is present",
    fun() -> FullActionPayload = {[{<<"username">>, <<"alice">>},
                                   {<<"password">>, <<"changeme">>}]},
             Expected = {[{<<"data">>, {[{<<"username">>, <<"alice">>},
                                          <<"password">>, ?REDACTED_PASSWORD]} }]},
             Ret = oc_chef_action:maybe_add_data({[]}, FullActionPayload),
             ?assertEqual(Expected, Ret)
    end
   },
   {"no redacted password added when no <<\"password\">> key is present",
    fun() -> FullActionPayload = {[{<<"name">>, <<"db">>}]},
             Expected = {[{<<"data">>, {[{<<"name">>, <<"db">>}]} }]},
             Ret = oc_chef_action:maybe_add_data({[]}, FullActionPayload),
             ?assertEqual(Expected, Ret)
    end
   }
  ].

create_message_test_() ->
    HostFQDN = <<"hostname.example.com">>,
    MockedModules = [chef_wm_util, wrq],
    State = #base_state{requestor = #chef_requestor{name = <<"rob">>, type = <<"user">>},
                        reqid = <<"Xfh5mCQvjRgWDdlevrdyGt8M4lecXmN3gpGXrKKiUYqKdeD3">>,
                        organization_name = <<"cmwest">>,
                        resource_state=#node_state{node_data = {[{<<"name">>, <<"db">>}]} }},
    ok = application:set_env(oc_chef_wm, actions_fqdn, HostFQDN),
    {foreach,
     fun() -> oc_chef_wm_test_utils:setup(MockedModules),
              meck:expect(chef_wm_util,object_name, fun(node, req) -> undefined end),
              meck:expect(wrq, response_code, fun(req) -> 201 end),
              meck:expect(chef_wm_util,object_name, fun(node, req) -> undefined end),
              meck:expect(uuid, uuid_to_string, fun(_UUID) -> "11111111-1111-1111-1111-111111111111" end),
              meck:expect(wrq, get_req_header,
                          fun(Field, req) ->
                              case Field of
                                  "x-ops-timestamp" -> "2011-07-03T13:21:50Z";
                                  "x-forwarded-for" -> "127.0.0.1";
                                  "user-agent" -> "knife 11.10.0";
                                  "x-remote-request-id" -> undefined
                              end
                          end)
     end,
     fun(_) -> oc_chef_wm_test_utils:cleanup(MockedModules) end,
     [{"end to end client test, action create with payload",
       fun() ->
               ExpectedMsg = msg_with_payload(<<"create">>),
               meck:expect(wrq, method, fun(req) -> 'POST' end),
               Data = oc_chef_action:create_message(req, State),
               ?assertEqual(ExpectedMsg, chef_json:decode(Data))
       end
      },
      {"end to end client test, action delete with data",
       fun() ->
               ExpectedMsg = msg_with_payload(<<"delete">>),
               meck:expect(wrq, method, fun(req) -> 'DELETE' end),
               Data = oc_chef_action:create_message(req, State),
               ?assertEqual(ExpectedMsg, chef_json:decode(Data))
       end
      }
     ]
    }.

%%
%% Internal helper functions
%%
-spec make_state() -> #base_state{}.
make_state() ->
    #base_state{resource_state=#client_state{} }.

entity(FullActionPayload, EntityType, EntityName) ->
    {FullActionPayload, EntityType, [{<<"entity_type">>, EntityType},
                                     {<<"entity_name">>, EntityName}]}.

parent_entity(FullActionPayload, ParentType, ParentName, EntityType, EntityName) ->
    {FullActionPayload, EntityType, [{<<"entity_type">>, EntityType},
                                        {<<"entity_name">>, EntityName},
                                        {<<"parent_type">>, ParentType},
                                        {<<"parent_name">>, ParentName}]}.
