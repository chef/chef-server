%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author James Casey <james@chef.io>
%%
%% Copyright 2013-2018 Chef Software, Inc.
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

-module(oc_chef_action).

-include("oc_chef_wm.hrl").

-export([log_action/2]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(CHEF_ACTIONS_MESSAGE_VERSION, <<"0.1.1">>).

-spec log_action(Req :: wm_req(),
                 State :: #base_state{}) -> ok.
log_action(_Req, #base_state{resource_state = ResourceState}) when
        is_record(ResourceState, search_state);
        is_record(ResourceState, principal_state);
        is_record(ResourceState, depsolver_state) ->
    %% We skip this endpoints since they are read-only
    ok;
log_action(_Req, #base_state{resource_state = ResourceState}) when
        is_record(ResourceState, sandbox_state) ->
    %% TODO How do we handle sandbox upload ?
    ok;
log_action(_Req, #base_state{resource_state = ResourceState}) when
        is_record(ResourceState, chef_role);
        is_record(ResourceState, chef_environment) ->
    %% TODO - chef_wm_roles endpoint puts a chef_role{} directly
    %% into the resource_state record
    %% TODO - chef_wm_environments endpoint puts a chef_environment{} directly
    %% into the resource_state record
    ok;
log_action(Req, #base_state{resource_state = ResourceState,
                            resource_mod = ResourceMod} = State) when
        is_record(ResourceState, acl_state);
        is_record(ResourceState, association_state);
        is_record(ResourceState, client_state);
        is_record(ResourceState, cookbook_state);
        is_record(ResourceState, data_state);
        is_record(ResourceState, environment_state);
        is_record(ResourceState, group_state);
        is_record(ResourceState, organization_state);
        is_record(ResourceState, node_state);
        is_record(ResourceState, role_state);
        is_record(ResourceState, user_state) ->
    case wrq:method(Req) of
        'GET' ->
            ok;
        _ElseMethod -> %% POST, PUT, DELETE
            case ResourceMod of
                oc_chef_wm_authenticate_user ->  %% POST to authenticate_user should not be an action.
                    ok;
                _ElseMod ->
                    case wrq:response_code(Req) of
                        Code when Code =:= 200; Code =:= 201 ->
                            log_action0(Req, State);
                        _ElseStatus ->
                            ok
                    end
            end
    end;
log_action(Req, #base_state{resource_state = ResourceState} = State) ->
    ShouldLogUnEnabledActions = envy:get(oc_chef_wm, log_unenabled_actions, false, boolean),
    case ShouldLogUnEnabledActions of
        true -> Task = task(Req, State),
            lager:info("Action ~p not enabled for ~p\n", [Task, ResourceState]);
        false -> ok
    end.

-spec  log_action0(Req :: wm_req(),
                   State :: #base_state{}) -> ok.
log_action0(Req, #base_state{resource_state = ResourceState} = State) ->
    ShouldSendBody = envy:get(oc_chef_wm, enable_actions_body, true, boolean),
    {FullActionPayload, EntityType, EntitySpecificPayload} = extract_entity_info(Req, ResourceState),
    Payload = case ShouldSendBody of
        true -> FullActionPayload;
        false -> []
    end,
    Task = task(Req, State),
    MsgType = routing_key(EntityType, Task),
    Msg = construct_payload(Payload, Task, Req, State, EntitySpecificPayload),
    publish(MsgType, Msg).

%%
%% Internal functions
%%

-spec construct_payload(FullActionPayload :: [{binary(), binary()},...],
                        Task :: binary(),
                        Req :: wm_req(),
                        State :: #base_state{},
                        EntitySpecificPayload :: [{binary(), binary()},...]) -> binary().
construct_payload(FullActionPayload, Task,
                  Req, #base_state{reqid = RequestId,
                                   organization_name = OrgName,
                                   requestor = Requestor},
                  EntitySpecificPayload) ->
    Msg = {[{<<"message_type">>, <<"action">>},
            {<<"message_version">>, ?CHEF_ACTIONS_MESSAGE_VERSION},
            {<<"organization_name">>, get_org_name(OrgName)},
            %% Request Level Info
            {<<"service_hostname">>, hostname()},
            {<<"recorded_at">>, req_header("x-ops-timestamp", Req)},
            {<<"remote_hostname">>, req_header("x-forwarded-for", Req)},
            {<<"request_id">>, RequestId},
            {<<"requestor_name">>, requestor_name(Requestor)},
            {<<"requestor_type">>, requestor_type(Requestor)},
            {<<"user_agent">>, req_header("user-agent", Req)},
            {<<"id">>, list_to_binary(uuid:uuid_to_string(uuid:get_v4()))},
            %% Entity Level Info
            {<<"task">>, Task}
            | EntitySpecificPayload
           ]},
    Msg0 = maybe_add_remote_request_id(Msg, req_header("x-remote-request-id", Req)),
    Msg1 = maybe_add_data(Msg0, FullActionPayload),
    iolist_to_binary(chef_json:encode(Msg1)).

get_org_name(undefined) ->
    null;
get_org_name(OrgName) ->
    OrgName.

maybe_add_remote_request_id(Msg, undefined) ->
    Msg;
maybe_add_remote_request_id(Msg, RemoteRequestId) ->
    ej:set({<<"remote_request_id">>}, Msg, RemoteRequestId).

-spec routing_key(EntityType :: <<_:32,_:_*8>>, Method :: <<_:48>>) -> binary().
routing_key(EntityType, Method) ->
    iolist_to_binary([<<"erchef.">>, EntityType, <<".">>, Method]).

-spec publish(RoutingKey :: binary(),
              Msg :: binary()) -> ok.
publish(RoutingKey, Msg)->
    oc_chef_action_queue:publish(RoutingKey, Msg).

maybe_add_data(Msg, []) ->
    Msg;
maybe_add_data(Msg, undefined) ->
    ej:set({<<"data">>}, Msg, {[]});
maybe_add_data(Msg, FullActionPayload) ->
    ej:set({<<"data">>}, Msg, FullActionPayload).

%%Example of a permission object
%entity_name: grant,
%entity_type: permission,
%parent_name: pedant_testorg_private-chef_1072,
%parent_type: organization

extract_entity_info(Req, #acl_state{acl_data = {[{AceName, _}]} = FullActionPayload,
                                    type = ParentType}) ->
    ParentName = req_or_data_name(chef_wm_util:object_name(ParentType, Req), FullActionPayload),
    {FullActionPayload, <<"permission">>, [{<<"entity_type">>, <<"permission">>},
                                           {<<"entity_name">>, AceName},
                                           {<<"parent_name">>, ParentName},
                                           {<<"parent_type">>, parent_type(ParentType)}
                                          ]};
%%TODO Put the right data into the state for the association related endpoints
%% Currently #chef_user.username is populated for Accept, Dissociate and Invite
%% #org_user_invite.user_name is populated for Reject
%% Associaion data is populated for Accept.
extract_entity_info(_Req, #association_state{data = FullActionPayload,
                                             user = UserData,
                                             org_user_invite = OrgUserInvite}) ->
    Name = case UserData of
        % Cannot pattern match in header since the record can be undefined for some cases.
        undefined -> OrgUserInvite#oc_chef_org_user_invite.user_name;
        #chef_user{username = Username} -> Username
    end,
    {FullActionPayload, <<"user">>, [{<<"entity_type">>, <<"user">>},
                                     {<<"entity_name">>, Name}
                                    ]};
extract_entity_info(Req, #client_state{client_data = FullActionPayload}) ->
    Name = req_or_data_name(chef_wm_util:object_name(client, Req), FullActionPayload),
    {FullActionPayload, <<"client">>, [{<<"entity_type">>, <<"client">>},
                                       {<<"entity_name">>, Name}
                                      ]};
extract_entity_info(_Req, #cookbook_state{cookbook_name = Name,
                                          cookbook_version = Version,
                                          cookbook_data = FullActionPayload}) ->
    CbVersion = get_cookbook_version(Version),
    {FullActionPayload, <<"version">>, [{<<"entity_type">>, <<"version">>},
                                        {<<"entity_name">>, CbVersion},
                                        {<<"parent_type">>, <<"cookbook">>},
                                        {<<"parent_name">>, Name}
                                       ]};
extract_entity_info(_Req, #data_state{data_bag_name = DataBagName,
                                      data_bag_item_name = undefined}) ->
    FullActionPayload = {[{<<"name">>, DataBagName}]},
    {FullActionPayload, <<"bag">>, [{<<"entity_type">>, <<"bag">>},
                                    {<<"entity_name">>, DataBagName}
                                   ]};
extract_entity_info(_Req, #data_state{data_bag_name = DataBagName,
                                      data_bag_item_name = DataBagItemName,
                                      data_bag_item_ejson = FullActionPayload}) ->
    {FullActionPayload, <<"item">>, [{<<"entity_type">>, <<"item">>},
                                     {<<"entity_name">>, DataBagItemName},
                                     {<<"parent_type">>, <<"bag">>},
                                     {<<"parent_name">>, DataBagName}
                                    ]};
extract_entity_info(Req, #environment_state{environment_data = FullActionPayload}) ->
    Name = req_or_data_name(chef_wm_util:object_name(environment, Req), FullActionPayload),
    {FullActionPayload, <<"environment">>, [{<<"entity_type">>, <<"environment">>},
                                            {<<"entity_name">>, Name}
                                           ]};
extract_entity_info(Req, #group_state{group_data = FullActionPayload}) ->
    Name = req_or_data_name(chef_wm_util:extract_from_path(group_name, Req), FullActionPayload),
    CorrectedName = get_corrected_name(Name, "groupname", FullActionPayload),
    {FullActionPayload, <<"group">>, [{<<"entity_type">>, <<"group">>},
                                      {<<"entity_name">>, CorrectedName}
                                     ]};
extract_entity_info(Req, #node_state{node_data = FullActionPayload}) ->
    Name = req_or_data_name(chef_wm_util:object_name(node, Req), FullActionPayload),
    {FullActionPayload, <<"node">>, [{<<"entity_type">>, <<"node">>},
                                     {<<"entity_name">>, Name}
                                    ]};
extract_entity_info(_Req, #organization_state{organization_data = FullActionPayload,
                                              oc_chef_organization = #oc_chef_organization{name = Name}}) ->
    {FullActionPayload, <<"organization">>, [{<<"entity_type">>, <<"organization">>},
                                             {<<"entity_name">>, Name}
                                            ]};
extract_entity_info(Req, #role_state{role_data = FullActionPayload}) ->
    Name = req_or_data_name(chef_wm_util:object_name(role, Req), FullActionPayload),
    {FullActionPayload, <<"role">>, [{<<"entity_type">>, <<"role">>},
                                     {<<"entity_name">>, Name}
                                    ]};
extract_entity_info(Req, #user_state{user_data = FullActionPayload}) ->
    Name = req_or_data_name(chef_wm_util:object_name(user, Req), FullActionPayload),
    CorrectedName = get_corrected_name(Name, "username", FullActionPayload),
    {FullActionPayload, <<"user">>, [{<<"entity_type">>, <<"user">>},
                                     {<<"entity_name">>, CorrectedName}
                                    ]}.

get_corrected_name(undefined, NameKey, FullActionPayload) ->
    ej:get({NameKey}, FullActionPayload);
get_corrected_name(Name, _NameKey, _FullActionPayload) ->
    Name.

-spec parent_type(ParentType :: atom()) -> binary().
parent_type(data_bag) ->
  <<"bag">>;
parent_type(ParentType) ->
  list_to_binary(atom_to_list(ParentType)).

-spec requestor_name(Requestor:: #chef_client{} | #chef_user{}) -> binary().
requestor_name(#chef_client{name = Name}) ->
    Name;
requestor_name(#chef_user{username = Name}) ->
    Name.

-spec requestor_type(Requestor:: #chef_client{} | #chef_user{}) -> <<_:32,_:_*16>>.
requestor_type(#chef_client{}) ->
    <<"client">>;
requestor_type(#chef_user{}) ->
    <<"user">>.

%% We have to do special casing for cookbook versions since it uses PUT
%% for both update and create.  We can distinguish between the two by the response
%% codes (201 for create, 200 for update)
-spec task(Req :: wm_req(),
           State :: #base_state{}) -> <<_:48>>.
task(Req, #base_state{resource_state=#cookbook_state{}}) ->
    case wrq:method(Req) of
        'PUT' ->
            case wrq:response_code(Req) of
                201 ->
                    %% actually should be a POST
                    key_for_method('POST');
                200 ->
                    key_for_method('PUT')
            end;
        Else ->
            key_for_method(Else)
    end;
%'DELETE' request could mean both dissociate or reject,
%hence we look into the log message to identify.
task(Req, #base_state{resource_state=#association_state{}, log_msg = LogMsg}) ->
    case wrq:method(Req) of
        'POST' ->
            <<"invite">>;
        'PUT' ->
            <<"associate">>;
        'DELETE' ->
            case element(1, LogMsg) of
                removed -> <<"dissociate">>;
                invite_deleted -> <<"reject">>
            end
    end;
task(Req, _State)->
    key_for_method(wrq:method(Req)).

-spec key_for_method('POST'|'PUT'|'DELETE') -> <<_:48>>.
key_for_method('DELETE') ->
    <<"delete">>;
key_for_method('POST') ->
    <<"create">>;
key_for_method('PUT') ->
    <<"update">>.

req_or_data_name(undefined, Data) ->
    ej:get({<<"name">>}, Data);
req_or_data_name(Name, _Data) when is_binary(Name) ->
    Name.

-spec hostname() -> binary().
hostname() ->
    envy:get(oc_chef_wm, actions_fqdn, binary).


get_cookbook_version({Major, Minor, Patch} = Version) when Major >=0, Minor >=0, Patch >=0 ->
    chef_cookbook_version:version_to_binary(Version).

req_header(Name, Req) ->
    case wrq:get_req_header(Name, Req) of
        undefined ->
            undefined;
        Header ->
            iolist_to_binary(Header)
    end.

