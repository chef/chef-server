%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
%% Copyright 2014 Chef Software, Inc
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

-module(oc_chef_wm_key_base).
-include("oc_chef_wm.hrl").

-export([create_object_with_embedded_key_data/2,
         update_object_embedded_key_data_v0/4,
         finalize_create_body/3]).


-spec update_object_embedded_key_data_v0(wm_req(), #base_state{},
                                         #chef_client{} | #chef_user{}, jiffy:json_value()) ->
    chef_wm_create_update_response().
update_object_embedded_key_data_v0(Req, State, ObjectRec, EJ) ->
    case chef_key_base:maybe_generate_key_pair(EJ) of
        keygen_timeout ->
            {{halt, 503}, Req, State#base_state{log_msg = keygen_timeout}};
        EJWithKeys->
            oc_chef_wm_base:update_from_json(Req, State, ObjectRec, EJWithKeys)
    end.

-spec create_object_with_embedded_key_data(wm_req(), #base_state{}) -> chef_wm_create_update_response().
create_object_with_embedded_key_data(Req, #base_state{server_api_version = ?API_v0,
                                                      key_context = #key_context{object_ej = EJ}} = State) ->
    KeyData = case chef_key_base:cert_or_key(EJ) of
                  {undefined, _} ->
                      chef_keygen_cache:get_key_pair();
                  {PubKey, _PubKeyVersion} ->
                      {PubKey, undefined}
              end,
    create_object(KeyData, Req, State);
create_object_with_embedded_key_data(Req, #base_state{key_context = #key_context{object_ej = EJ}} = State) ->
    chef_key_base:maybe_generate_key_pair(EJ,
                                          fun(Result) ->
                                                  create_object(Result, Req, State)
                                          end).
-spec create_object(keygen_timeout |
                    {binary(), binary()} |
                    {binary(), undefined} |
                    {undefined, undefined},
                    wm_req(), #base_state{}) -> chef_wm_create_update_response().
create_object(keygen_timeout, Req, State) ->
    {{halt, 503}, Req, State#base_state{log_msg = keygen_timeout}};
create_object({PublicKey, PrivateKey},
              Req, #base_state{server_api_version = ?API_v0,
                               key_context = #key_context{object_ej = ObjectEJ, type = Type,
                                                          object_authz_id = AuthzId} = KeyContext} = State) ->
    EJ1 = chef_key_base:set_public_key(ObjectEJ, PublicKey),
    KeyEJ =  {[{<<"private_key">>, PrivateKey},
               {<<"public_key">>, PublicKey}]},
    KeyContext1 = KeyContext#key_context{ object_ej = EJ1, key_ej = KeyEJ },
    oc_chef_wm_base:create_from_json(Req,
                                     State#base_state{ key_context = KeyContext1},
                                     object_type(Type) , {authz_id, AuthzId}, EJ1);
create_object({undefined, undefined},
              Req, #base_state{key_context = #key_context{type = Type, object_authz_id = AuthzId,
                                                          object_ej = EJ}} = State ) ->
    % No public key was provided, and no key requested to be generated. Continue without additional action.
    oc_chef_wm_base:create_from_json(Req, State, object_type(Type), {authz_id, AuthzId}, EJ);
create_object({PublicKey, PrivateKey},
              Req,
              #base_state{server_api_version = Version, organization_guid = OrgId,
                          requestor_id = ActorId, chef_db_context = Ctx,
                          key_context = #key_context{type = OwnerType, object_ej = OwnerEJ,
                                                     object_name = OwnerName,
                                                     object_authz_id = OwnerAuthzId} = KeyContext} = State) ->
    OrgId2 = safe_org_id(OrgId),
    % Create the ID for the object ahead of time, we'll need it in our key.
    ObjectId = chef_object_base:make_org_prefix_id(OrgId2, OwnerName),
    #chef_key{key_name = KeyName} = Key = chef_object:new_record(chef_key, Version, OrgId2, unset, {ObjectId, chef_key:default_key_ejson(PublicKey)}),
    case chef_db:create(Key, Ctx, ActorId) of
        ok ->
            URI = oc_chef_wm_routes:route(chef_key_base:key_owner_type(OwnerType), Req, [ {object_name, OwnerName}, {name, KeyName} ]),
            KeyEJ = ej:set({<<"uri">>}, chef_key:ejson_from_key(Key), URI),
            KeyEJ2 = case PrivateKey of
                undefined -> KeyEJ;
                _ -> ej:set({<<"private_key">>}, KeyEJ, PrivateKey)
            end,
            KeyContext2 = KeyContext#key_context{key_ej = KeyEJ2},
            % By specifying the id in the EJ we'll ensure the object itself doesn't try to create it.
            OwnerEJ1 = ej:set({<<"id">>}, OwnerEJ, ObjectId),
            OwnerEJ2 = ej:delete({<<"create_key">>}, OwnerEJ1),
            oc_chef_wm_base:create_from_json(Req, State#base_state{key_context = KeyContext2},
                                            object_type(OwnerType), {authz_id, OwnerAuthzId}, OwnerEJ2);
        _What ->
            {{halt, 500}, Req, State#base_state{ log_msg = failed_to_save_new_key }}
    end.

safe_org_id(undefined) -> ?OSC_ORG_ID;
safe_org_id(OrgId) -> OrgId.


-spec finalize_create_body(wm_req(), #base_state{}, jiffy:json_value()) -> jiffy:json_value().
finalize_create_body(_Req, #base_state{key_context = #key_context{key_ej = undefined}}, BodyEJ) ->
    BodyEJ;
finalize_create_body(_Req, #base_state{key_context = #key_context{key_ej = EJToEmbed}}, BodyEJ) ->
    ej:set({<<"chef_key">>}, BodyEJ, EJToEmbed).

object_type(Type) ->
    list_to_existing_atom("chef_" ++ atom_to_list(Type)).

