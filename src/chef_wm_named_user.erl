%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_wm_named_user).

-include("chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2,
                        post_is_create/2]}]).

-mixin([{?BASE_RESOURCE, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).

-export([
         allowed_methods/2,
         delete_resource/2,
         from_json/2,
         to_json/2
       ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #user_state{}}.

request_type() ->
  "users".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request(Method, Req, State) when Method == 'GET';
                                          Method == 'DELETE' ->
    {Req, State};
validate_request('PUT', Req, #base_state{resource_state = UserState} = State) ->
    %% Name = chef_wm_util:object_name(user, Req),
    Body = wrq:req_body(Req),
    %% FIXME: need to validate no name change since we aren't going to support rename for
    %% users.
    {ok, User} = chef_user:parse_binary_json(Body),
    {Req, State#base_state{resource_state = UserState#user_state{user_data = User}}}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           resource_state = UserState}=State) ->
    UserName = chef_wm_util:object_name(user, Req),
    case chef_db:fetch_user(DbContext, UserName) of
        not_found ->
            Message = chef_wm_util:not_found_message(user, UserName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = user_not_found}};
        #chef_user{authz_id = AuthzId} = User ->
            UserState1 = UserState#user_state{chef_user = User},
            State1 = State#base_state{resource_state = UserState1},
            {{object, AuthzId}, Req, State1}
    end.

from_json(Req, #base_state{reqid = RequestId,
                           resource_state =
                               #user_state{chef_user = User,
                                           user_data = UserData}} = State) ->
    case maybe_generate_key_pair(UserData, RequestId) of
        {UserData1, PublicKey} ->
            chef_wm_base:update_from_json(Req, State, User#chef_user{ public_key = PublicKey }, UserData1);
        UserData1 ->
            chef_wm_base:update_from_json(Req, State, User, UserData1)
    end.

to_json(Req, #base_state{resource_state =
                             #user_state{chef_user = User},
                         organization_name = OrgName} = State) ->
    EJson = chef_user:assemble_user_ejson(User, OrgName),
    Json = ejson:encode(EJson),
    {Json, Req, State}.

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #user_state{
                                   chef_user = User},
                                 organization_name = OrgName} = State) ->
    ok = chef_object_db:delete(DbContext, User, RequestorId),
    EJson = chef_user:assemble_user_ejson(User, OrgName),
    Req1 = chef_wm_util:set_json_body(Req, EJson),
    {true, Req1, State}.

%% If the request contains "private_key":true, then we will generate a new key pair. In
%% this case, we'll add the new public and private keys into the EJSON since
%% update_from_json will use it to set the response.
maybe_generate_key_pair(UserData, RequestId) ->
    Name = ej:get({<<"name">>}, UserData),
    case ej:get({<<"private_key">>}, UserData) of
        true ->
            {PublicKey, PrivateKey} = chef_wm_util:generate_keypair(Name, RequestId),
            {chef_user:set_key_pair(UserData,
                                   {public_key, PublicKey},
                                   {private_key, PrivateKey}), PublicKey};
        _ ->
            UserData
    end.

% TODO: this could stand refactoring: I'm sure there is stuff re-used by other
% endpoints and possibly unused code here
error_message(Msg) when is_list(Msg) ->
    error_message(iolist_to_binary(Msg));
error_message(Msg) when is_binary(Msg) ->
    {[{<<"error">>, [Msg]}]}.

malformed_request_message(#ej_invalid{type = json_type, key = Key}, _Req, _State) ->
    case Key of
        undefined -> error_message([<<"Incorrect JSON type for request body">>]);
        _ ->error_message([<<"Incorrect JSON type for ">>, Key])
    end;
malformed_request_message(#ej_invalid{type = missing, key = Key}, _Req, _State) ->
    error_message([<<"Required value for ">>, Key, <<" is missing">>]);
malformed_request_message({invalid_key, Key}, _Req, _State) ->
    error_message([<<"Invalid key ">>, Key, <<" in request body">>]);
malformed_request_message(invalid_json_body, _Req, _State) ->
    error_message([<<"Incorrect JSON type for request body">>]);
malformed_request_message(#ej_invalid{type = exact, key = Key, msg = Expected},
                          _Req, _State) ->
    error_message([Key, <<" must equal ">>, Expected]);
malformed_request_message(#ej_invalid{type = string_match, msg = Error},
                          _Req, _State) ->
    error_message([Error]);
malformed_request_message(#ej_invalid{type = object_key, key = Object, found = Key},
                          _Req, _State) ->
    error_message([<<"Invalid key '">>, Key, <<"' for ">>, Object]);
% TODO: next two tests can get merged (hopefully) when object_map is extended not
% to swallow keys
malformed_request_message(#ej_invalid{type = object_value, key = Object, found = Val},
                          _Req, _State) when is_binary(Val) ->
    error_message([<<"Invalid value '">>, Val, <<"' for ">>, Object]);
malformed_request_message(#ej_invalid{type = object_value, key = Object, found = Val},
                          _Req, _State) ->
    error_message([<<"Invalid value '">>, io_lib:format("~p", [Val]),
                   <<"' for ">>, Object]);
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).


%% update_from_json(#wm_reqdata{} = Req, #base_state{chef_db_context = DbContext,
%%                                                   requestor_id = ActorId}=State,
%%                  OrigObjectRec, ObjectEjson) ->
%%     ObjectRec = chef_object:update_from_ejson(OrigObjectRec, ObjectEjson),
%%     case OrigObjectRec =:= ObjectRec of
%%         true ->
%%             State1 = State#base_state{log_msg = ignore_update_for_duplicate},
%%             {true, chef_wm_util:set_json_body(Req, ObjectEjson), State1};
%%         false ->
%%             UpdateFun = chef_db:update_fun(ObjectRec),
%%             case chef_db:UpdateFun(DbContext, ObjectRec, ActorId) of
%%                 ok ->
%%                     {true, chef_wm_util:set_json_body(Req, ObjectEjson), State};
%%                 not_found ->
%%                     %% We will get this if no rows were affected by the query. This could
%%                     %% happen if the object is deleted in the middle of handling this
%%                     %% request. In this case, we return 404 just as we would if the client
%%                     %% retried.
%%                     State1 = State#base_state{log_msg = not_found},
%%                     Msg = chef_wm_util:not_found_message(chef_object:type_name(ObjectRec),
%%                                                            chef_object:name(ObjectRec)),
%%                     Req1 = chef_wm_util:set_json_body(Req, Msg),
%%                     {{halt, 404}, Req1, State1};
%%                 {conflict, _} ->
%%                     Name = chef_object:name(ObjectRec),
%%                     RecType = erlang:element(1,ObjectRec),
%%                     LogMsg = {RecType, name_conflict, Name},
%%                     ConflictMsg = conflict_message(Name),
%%                     {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg),
%%                      State#base_state{log_msg = LogMsg}};
%%                 Why ->
%%                     State1 = State#base_state{log_msg = Why},
%%                     {{halt, 500}, Req, State1}
%%             end
%%     end.

%% conflict_message(Name) ->
%%     Msg = iolist_to_binary([<<"User '">>, Name, <<"' already exists">>]),
%%     {[{<<"error">>, [Msg]}]}.
