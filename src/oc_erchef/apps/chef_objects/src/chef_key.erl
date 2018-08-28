%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
% @author Marc Paradise <marc@chef.io>
%% Copyright 2015-2018 Chef Software, Inc. All Rights Reserved.
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

-module(chef_key).

-include_lib("mixer/include/mixer.hrl").
-include("chef_types.hrl").

-behaviour(chef_object).

-export([authz_id/1,
         is_indexed/1,
         ejson_for_indexing/2,
         update_from_ejson/2,
         set_created/2,
         fields_for_update/1,
         fields_for_fetch/1,
         ejson_from_list/2,
         ejson_from_key/1,
         record_fields/1,
         list/2,
         set_updated/2,
         set_api_version/2,
         new_record/4,
         default_key_ejson/1,
         name/1,
         id/1,
         org_id/1,
         type_name/1,
         delete/2,
         parse_binary_json/2,
         fields_for_insert/1,
         create_query/1,
         update_query/1,
         delete_query/1,
         find_query/1,
         bulk_get_query/1,
         list_query/1
        ]).

-mixin([{chef_object_default_callbacks, [fetch/2, update/2]}]).

-ifdef(TEST).
-compile(export_all).
-endif.

authz_id(#chef_key{}) ->
    undefined.

is_indexed(_ObjectRec) ->
    false.

ejson_for_indexing(#chef_key{}, _) ->
    error(not_indexed).

-spec update_from_ejson(#chef_key{}, ejson_term()) -> #chef_key{}.
update_from_ejson(#chef_key{key_name = OldName, key_version = OldPubKeyVersion,
                            public_key = OldPubKey, expires_at = OldExpirationDate} = Key, EJ) ->
    {NewVersion, NewPubKey} = case ej:get({<<"public_key">>}, EJ) of
                                undefined ->
                                  {OldPubKeyVersion, OldPubKey};
                                PK ->
                                  {safe_key_version(PK), PK}
                              end,
    NewExpiration = case ej:get({<<"expiration_date">>}, EJ) of
                      undefined ->
                        OldExpirationDate;
                      Exp ->
                        chef_object_base:parse_date(Exp)
                    end,
    Key#chef_key{key_name = ej:get({<<"name">>}, EJ, OldName),
                 key_version = NewVersion,
                 public_key = NewPubKey,
                 expires_at = NewExpiration,
                 % We need to preserve the old name for any update to be applied
                 old_name = OldName}.

set_created(#chef_key{} = Key, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Key#chef_key{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

set_updated(#chef_key{} = Key, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Key#chef_key{updated_at = Now, last_updated_by = ActorId}.

fields_for_update(#chef_key{id = Id, key_name = NewName, old_name = OldName,
                            key_version = PubKeyVersion, public_key = PublicKey,
                            expires_at = ExpirationDate, updated_at = UpdatedAt,
                            last_updated_by = ActorId}) ->
  [Id, OldName, NewName, PublicKey, PubKeyVersion, ExpirationDate, ActorId, UpdatedAt].


fields_for_fetch(#chef_key{id = Id, key_name = KeyName}) ->
  [Id, KeyName].

record_fields(_ApiVersion) ->
  record_info(fields, chef_key).

ejson_from_list(KeysList, URIDecorator) ->
  [ {[{<<"uri">>, URIDecorator(Name)},
      {<<"name">>, Name},
      {<<"expired">>, Expired}]} || [Name, Expired] <- KeysList ].

ejson_from_key(#chef_key{key_name = Name, public_key = PublicKey, expires_at = NativeExpirationDate}) ->
    ExpirationDate = case NativeExpirationDate of
        ?INFINITY_TIMESTAMP -> <<"infinity">>;
        _ -> list_to_binary(ec_date:format("Y-m-dTH:i:sZ", NativeExpirationDate))
    end,
    {[{<<"name">>, Name},
      {<<"public_key">>, PublicKey},
      {<<"expiration_date">>, ExpirationDate}]}.

list(#chef_key{id = Id} = Key, CallbackFun) when is_binary(Id) ->
    CallbackFun({list_query(Key), [Id], rows}).

find_query(_ObjectRec) ->
    find_key_by_id_and_name.

new_record(ApiVersion, _OrgId, _AuthzId, {Id, KeyData}) ->
    PubKey = ej:get({<<"public_key">>}, KeyData),
    %% return a more useful error if key_version fails
    PubKeyVersion = safe_key_version(PubKey),
    Expires = chef_object_base:parse_date(ej:get({<<"expiration_date">>}, KeyData)),
    #chef_key{server_api_version = ApiVersion, id = Id,
              key_name = ej:get({<<"name">>}, KeyData),
              public_key = PubKey, key_version = PubKeyVersion,
              expires_at = Expires}.

default_key_ejson(PublicKey ) ->
    {[{<<"name">>, <<"default">>},
      {<<"public_key">>, PublicKey},
      {<<"expiration_date">>, <<"infinity">>}]}.

safe_key_version(PublicKey) ->
    try chef_key_base:key_version(PublicKey) of
        Result -> Result
    catch
        _:_ -> throw(invalid_public_key)
    end.

name(#chef_key{key_name = KeyName}) ->
    KeyName.

id(#chef_key{id = Id}) ->
    Id.

org_id(#chef_key{}) ->
    undefined.

type_name(#chef_key{}) ->
    key.

list_query(_ObjectRec) ->
    list_keys_for_actor.

create_query(_ObjectRec) ->
    insert_key_for_actor.

parse_binary_json(Bin, update) ->
  EJ = chef_json:decode(Bin),
  % Expiration date, name, and either public_key or create_key must be present
  % If both public_key andcreate_key are present it is an error.

  OneOf = [<<"expiration_date">>, <<"name">>, <<"public_key">>, <<"create_key">>],
  case lists:filter(fun(X) -> X =/= undefined end, [ej:get({Field}, EJ) || Field <- OneOf]) of
      [] ->
          throw(missing_required_field);
      _ ->
          validate_name(opt, EJ),
          chef_key_base:validate_public_key_fields(opt, EJ, key, update),
          validate_expiration_date(opt, EJ)
  end;
parse_binary_json(Bin, create) ->
  EJ = chef_json:decode(Bin),
  validate_name(req, EJ),
  chef_key_base:validate_public_key_fields(req, EJ, key, create),
  validate_expiration_date(req, EJ).

validate_expiration_date(Required, EJ) ->
  case {Required, ej:get({<<"expiration_date">>}, EJ)} of
    {opt, undefined} -> EJ;
    _ -> chef_object_base:validate_date_field(EJ, <<"expiration_date">>)
  end.

validate_name(Req, EJ) ->
    chef_object_base:validate_ejson(EJ, {[ {{Req, <<"name">>}, {string_match, chef_regex:regex_for(key_name)}} ]}).

update_query(_ObjectRec) ->
    update_key_by_id_and_name.

delete_query(_ObjectRec) ->
    delete_key_by_id_and_name.

delete(#chef_key{id = Id, key_name = Name} = Rec, CallbackFun) ->
    CallbackFun({delete_query(Rec), [Id, Name]}).

fields_for_insert(#chef_key{id = Id, key_name = KeyName,
                           public_key = PublicKey, key_version = KeyVersion,
                           expires_at = ExpiresAt, last_updated_by = LastUpdatedBy,
                           created_at = CreatedAt , updated_at = UpdatedAt}) ->
    [Id, KeyName, PublicKey, KeyVersion, ExpiresAt,
     LastUpdatedBy, CreatedAt, UpdatedAt].

bulk_get_query(_ObjectRec) ->
    error(unsupported).

set_api_version(ObjectRec, Version) ->
    ObjectRec#chef_key{server_api_version = Version}.
