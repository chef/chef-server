%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Tyler Cloke <tyler@chef.io>
%% @author Marc Paradise <marc@chef.io>
%%
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_wm_named_key).

-include("oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-mixin([{oc_chef_wm_keys, [auth_info/2,
                           validate_request/3,
                           conflict_message/1,
                           route_args/2]}]).

%% chef_wm behavior callbacks
-behavior(chef_wm).
-export([init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         finalize_update_body/3]).

-export([allowed_methods/2,
         delete_resource/2,
         auth_info/3,
         from_json/2,
         to_json/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #key_state{}}.

request_type() ->
    "keys".

%% /users/:user/keys/:key OR
%% /organizations/:org/users/:user/keys/:key
allowed_methods(Req, #base_state{resource_args = user} = State) ->
    case wrq:path_info(organization_id, Req) of
        %% /users/:user/keys/:key
        undefined ->
           {['GET', 'DELETE', 'PUT'], Req, State};
        %% For /orgs/:org/users/:user/keys/:key, we've only implemented GET so far.
        _ ->
            {['GET', 'DELETE'], Req, State}
    end;
%% /organizations/:org/clients/:client/keys/:key
allowed_methods(Req, State) ->
    {['GET', 'DELETE', 'PUT'], Req, State}.

% auth requirements are shared between this module and oc_chef_wm_key,
% but we additionally need to verify that the user isn't attempting to remove the
% key currently used for authentication, and that the requested key record exists.
% before we attempt any operation on it.
auth_info(Method, Req, #base_state{chef_db_context = Ctx,
                                   requestor = #chef_requestor{id = RequestorId, key_name = AuthenticatedKeyName},
                                   resource_state = #key_state{parent_id = ParentId} = ResourceState} = State) ->

    Name = chef_wm_util:object_name(key, Req),
    case {Name, RequestorId, Method} of
        {AuthenticatedKeyName, ParentId, 'DELETE'} ->
            key_in_use_response(Name, Req, State);
        {AuthenticatedKeyName, ParentId, 'PUT'} ->
            % TODO consider allowing updates to authenticating key
            % that wouldn't break auth:
            % - not to an expired date or date to expire soon
            % - not to a new public_key.
            key_in_use_response(Name, Req, State);
        _ ->
            case chef_db:fetch(#chef_key{id = ParentId, key_name = Name}, Ctx) of
                not_found ->
                    key_not_found_response(Name, Req, State);
                #chef_key{} = Key ->
                    ResourceState1 = ResourceState#key_state{chef_key = Key},
                    oc_chef_wm_keys:auth_info(Method, Req, State#base_state{resource_state = ResourceState1})
            end
    end.

key_in_use_response(Name, Req, #base_state{resource_state = #key_state{parent_name = ParentName}} = State) ->
    % This is an action forbidden by policy, but the usual "you don't have permissions to X" doesn't apply -
    % use a custom message.
    Message = iolist_to_binary(["The key '", Name, "' was used to authenticate this request and cannot be modified or deleted."]),
    {{halt, 403, Message}, Req, State#base_state{log_msg={key_in_use_for_request, {ParentName, Name}}}}.

key_not_found_response(Name, Req, #base_state{resource_state = #key_state{parent_name = ParentName}} = State) ->
    Message = chef_wm_util:not_found_message(key, {ParentName, Name}),
    Req1 = chef_wm_util:set_json_body(Req, Message),

    {{halt, 404}, Req1, State#base_state{log_msg={key_not_found, {ParentName, Name}}}}.
to_json(Req, #base_state{resource_state = #key_state{chef_key = Key}} = State) ->
    EJ = chef_key:ejson_from_key(Key),
    {chef_json:encode(EJ), Req, State}.

from_json(Req, #base_state{resource_state = #key_state{key_data = EJ}} = State) ->
    chef_key_base:maybe_generate_key_pair(EJ, fun(Result) -> handle_keypair(Req, State, Result) end).

handle_keypair(Req, State, keygen_timeout) ->
    {{halt, 503}, Req, State#base_state{log_msg = keygen_timeout}};
handle_keypair(Req, State, {undefined, undefined}) ->
   update_from_json(Req, State);
handle_keypair(Req, #base_state{resource_state = #key_state{chef_key = Key} = KeyState} = State,
               {PublicKey, PrivateKey}) ->
    KeyState2 = KeyState#key_state{generated_private_key = PrivateKey,
                                   chef_key = Key#chef_key{public_key = PublicKey} },
    update_from_json(Req, State#base_state{resource_state = KeyState2}).

update_from_json(Req, #base_state{resource_state = #key_state{chef_key = Key, key_data = EJ}} = State) ->
    oc_chef_wm_base:update_from_json(Req, State, Key, EJ).

finalize_update_body(_Req, #base_state{resource_state = #key_state{generated_private_key = undefined}}, BodyEJ) ->
    % create_key could have been 'false' - make sure it's gone.
    ej:delete({<<"create_key">>}, BodyEJ);
finalize_update_body(_Req, #base_state{resource_state = #key_state{chef_key = #chef_key{public_key = PubKey}, generated_private_key = GenPrivKey}}, BodyEJ) ->
    EJ1 = ej:delete({<<"create_key">>}, BodyEJ),
    EJ2 = ej:set({<<"private_key">>}, EJ1, GenPrivKey),
    ej:set({<<"public_key">>}, EJ2, PubKey).

delete_resource(Req, #base_state{requestor_id = RequestorId, chef_db_context = Ctx,
                                 resource_state = #key_state{chef_key = Key}} = State) ->
    ok = oc_chef_wm_base:delete_object(Ctx, Key, RequestorId),
    EJ = chef_key:ejson_from_key(Key),
    Req1 = chef_wm_util:set_json_body(Req, EJ),
    {true, Req1, State}.

malformed_request_message(missing_required_field, _Req, _State) ->
    {[{<<"error">>, <<"At least one of 'name', 'expiration_date', or 'public_key' must be specified">>}]};
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
