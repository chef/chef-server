%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author James Casey <james@chef.io>
%%
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

-module(oc_chef_action).

-include("oc_chef_wm.hrl").

-export([create_message/2]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-define(CHEF_ACTIONS_MESSAGE_VERSION, <<"0.1.1">>).

-spec create_message(Req :: wm_req(), State :: #base_state{}) -> binary().
create_message(Req, #base_state{resource_state = ResourceState} = State) ->
    {Payload, _EntityType, EntitySpecificPayload} = extract_entity_info(Req, ResourceState),
    Task = task(Req, State),
    construct_payload(Payload, Task, Req, State, EntitySpecificPayload).

%%
%% Internal functions
%%
-spec construct_payload(FullActionPayload :: [{binary(), binary()},...],
                        Task :: binary(),
                        Req :: wm_req(),
                        State :: #base_state{},
                        EntitySpecificPayload :: [{binary(), binary()},...]) -> binary().
construct_payload(FullActionPayload, Task, Req,
                  #base_state{ requestor = #chef_client{name = Name} } = State,
                  EntitySpecificPayload) ->
    % There is one special case in which we're required to use a chef_client record instead of chef_requestor;
    % work around that by forcing the type we expect in construct_payload.
    construct_payload(FullActionPayload, Task, Req,
                      State#base_state{requestor = #chef_requestor{type = <<"client">>, name = Name}},
                      EntitySpecificPayload);
construct_payload(FullActionPayload, Task,
                  Req, #base_state{reqid = RequestId,
                                   organization_name = OrgName,
                                   requestor = #chef_requestor{ type = RequestorType, name = RequestorName } },
                  EntitySpecificPayload) ->
    Msg = {[{<<"message_type">>, <<"action">>},
            {<<"message_version">>, ?CHEF_ACTIONS_MESSAGE_VERSION},
            {<<"organization_name">>, get_org_name(OrgName)},
            %% Request Level Info
            {<<"service_hostname">>, hostname()},
            {<<"recorded_at">>, req_header("x-ops-timestamp", Req)},
            {<<"remote_hostname">>, req_header("x-forwarded-for", Req)},
            {<<"request_id">>, RequestId},
            {<<"requestor_name">>, RequestorName},
            {<<"requestor_type">>, RequestorType},
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

maybe_add_data(Msg, []) ->
    Msg;
maybe_add_data(Msg, undefined) ->
    ej:set({<<"data">>}, Msg, {[]});
maybe_add_data(Msg, FullActionPayload) ->
    ej:set({<<"data">>}, Msg, maybe_scrub_password(FullActionPayload)).

maybe_scrub_password(Msg) ->
    case ej:get({"password"}, Msg) of
        undefined -> Msg;
        _ -> ej:set({<<"password">>}, Msg, ?REDACTED_PASSWORD)
    end.

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
                                    ]};
extract_entity_info(Req, #key_state{key_data = FullActionPayload, parent_name = ParentName, type = ParentType}) ->
    Name = req_or_data_name(chef_wm_util:object_name(key, Req), FullActionPayload),
    {FullActionPayload, <<"key">>, [{<<"entity_type">>, <<"key">>},
                                    {<<"entity_name">>, Name },
                                    {<<"parent_type">>, atom_to_binary(ParentType, utf8)},
                                    {<<"parent_name">>, ParentName}
                                   ]};
extract_entity_info(Req, #policy_state{policy_data = FullActionPayload}) ->
    PolicyName = chef_wm_util:object_name(policy, Req),
    PolicyGroupName = chef_wm_util:object_name(policy_group_asoc_name, Req),
    {FullActionPayload, <<"policy">>, [{<<"entity_type">>, <<"policy">>},
                                     {<<"entity_name">>, PolicyName},
                                     {<<"parent_type">>, <<"policy_group">>},
                                     {<<"parent_name">>, PolicyGroupName}
                                    ]};
extract_entity_info(_Req, #cookbook_artifact_version_state{oc_chef_cookbook_artifact_version = CBAInfo,
                                                           cookbook_artifact_version_data = FullActionPayload}) ->
    Name = iolist_to_binary(oc_chef_cookbook_artifact_version:name(CBAInfo)),
    Identifier = iolist_to_binary(oc_chef_cookbook_artifact_version:identifier(CBAInfo)),
    {FullActionPayload, <<"cookbook_artifact_version">>, [{<<"entity_type">>, <<"cookbook_artifact_version">>},
                                        {<<"entity_name">>, Identifier},
                                        {<<"parent_type">>, <<"cookbook_artifact">>},
                                        {<<"parent_name">>, Name}
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
task(Req, _State) ->
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
