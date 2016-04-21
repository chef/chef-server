%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Mark Mzyk <mmzyk@chef.io>
%% @author Marc Paradise <marc@chef.io>
%% Copyright 2011-2014 Chef, Inc. All Rights Reserved.
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
%
-module(chef_user).

-export([
         assemble_user_ejson/2,
         authz_id/1,
         ejson_for_indexing/2,
         fetch/2,
         fields_for_fetch/1,
         fields_for_update/1,
         id/1,
         is_indexed/0,
         name/1,
         username_from_ejson/1,
         new_record/3,
         org_id/1,
         parse_binary_json/1,
         parse_binary_json/3,
         password_data/1,
         record_fields/0,
         set_created/2,
         set_password_data/2,
         set_updated/2,
         type_name/1,
         update_from_ejson/2,
         validate_user_name/1,
         serialized_field_value/2
        ]).

%% database named queries
-export([
         bulk_get_query/0,
         create_query/0,
         delete_query/0,
         find_query/0,
         list_query/0,
         update_query/0
        ]).

-export([ list/2 ]).

-include_lib("mixer/include/mixer.hrl").
-mixin([{chef_object,[ {default_update/2, update} ]}]).

-include("chef_types.hrl").

-include_lib("ej/include/ej.hrl").

-behaviour(chef_object).

%% Whitelist of fields we will persist to serialized_object.  Fields not in the list
%% will not be persisted in create and update operations.
-define(JSON_SERIALIZABLE, [<<"display_name">>,
                            <<"first_name">>,
                            <<"last_name">>,
                            <<"middle_name">>,
                            <<"city">>,
                            <<"country">>,
                            <<"twitter_account">>,
                            <<"image_file_name">>]).

-spec authz_id(#chef_user{}) -> object_id().
authz_id(#chef_user{authz_id = AuthzId}) ->
    AuthzId.

-spec name(#chef_user{}) -> binary().
name(#chef_user{username = Name}) ->
    Name.

-spec id(#chef_user{}) -> object_id().
id(#chef_user{id = Id}) ->
    Id.

org_id(#chef_user{}) ->
    error(not_valid_for_chef_user).

type_name(#chef_user{}) ->
    user.

-spec username_from_ejson(ejson_term()) -> binary().
username_from_ejson(UserData) ->
    case ej:get({<<"name">>}, UserData) of
        undefined ->
            ej:get({<<"username">>}, UserData);
        Name ->
            Name
    end.

serialized_field_value(FieldName, User) when is_list(FieldName) ->
    serialized_field_value(list_to_binary(FieldName), User);
serialized_field_value(FieldName, User) when is_atom(FieldName) ->
    serialized_field_value(atom_to_binary(FieldName, utf8), User);
serialized_field_value(FieldName, #chef_user{serialized_object = SerializedObject}) ->
    EJ = chef_json:decode(SerializedObject),
    ej:get({FieldName}, EJ).

-spec new_record(object_id(), object_id(), ejson_term()) -> #chef_user{}.
new_record(OrgId, AuthzId, Data) ->
    {HashPass, Salt, HashType} = case ej:get({<<"password">>}, Data) of
        undefined ->
            {null, null, null};
        Password ->
            chef_password:encrypt(Password)
    end,
    UserData = ej:delete({<<"password">>}, Data),
    Name = username_from_ejson(UserData),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    Email = value_or_null({<<"email">>}, UserData),
    ExtAuthUid = value_or_null({<<"external_authentication_uid">>}, UserData),
    EnableRecovery = ej:get({<<"recovery_authentication_enabled">>}, UserData) =:= true,
    {PublicKey, PubkeyVersion} = chef_object_base:cert_or_key(UserData),
    SerializedObject = { whitelisted_values(UserData, ?JSON_SERIALIZABLE) },
    #chef_user{id = Id,
               authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
               username = Name,
               email = Email,
               pubkey_version = PubkeyVersion,
               public_key = PublicKey,
               hashed_password = HashPass,
               salt = Salt,
               hash_type = HashType,
               external_authentication_uid = ExtAuthUid,
               recovery_authentication_enabled = EnableRecovery,
               admin = false,
               serialized_object = chef_json:encode(SerializedObject)
    }.

value_or_null(Key, Data) ->
    Value = ej:get(Key, Data),
    case Value of
        undefined ->
            null;
        _ ->
            Value
    end.

password_validator() ->
    {fun_match, {fun valid_password/1, string, <<"Password must have at least 6 characters">>}}.

public_key_spec() ->
    {[
        {{opt,<<"public_key">>}, {fun_match, {fun chef_object_base:valid_public_key/1, string,
                                              <<"Public Key must be a valid key.">>}}} ]}.
user_spec(common) ->
    {[
        {<<"display_name">>, string}, %% FIXME as an always-required field this belongs in the schema
        {{opt,<<"first_name">>},  string},  %% Note that remaining fields are serialized via serialized_object and
        {{opt,<<"last_name">>},   string} , %% are/were used by other Chef components.
        {{opt,<<"middle_name">>}, string},  %% FIXME these should be retained by the components that need them
        {{opt,<<"twitter_account">>}, string},
        {{opt,<<"city">>}, string},
        {{opt,<<"country">>}, string},
        {{opt,<<"external_authentication_uid">>}, string }
    ]};
user_spec(create) ->
    {[ {{opt,<<"password">>},  password_validator()} ]};
user_spec(update) ->
    {[ {{opt,<<"password">>},  password_validator()},
       {{opt,<<"recovery_authentication_enabled">>}, boolean },
       {{opt,<<"private_key">>}, boolean } ]}.

local_auth_user_spec(common) ->
    {[{<<"email">>,            {fun_match, {fun valid_email/1, string, <<"email must be valid">>}}}]};
local_auth_user_spec(create) ->
    {[ {<<"password">>, password_validator()} ]};
local_auth_user_spec(update) ->
    {[ {{opt,<<"password">>},  password_validator()} ]}.



valid_email(EMail) when is_binary(EMail) ->
    valid_email(binary_to_list(EMail));
valid_email(EMail) ->
    %% * We are not caching this because updating users is a fairly infrequent operation.
    %%   Note that we are compiling it, since the 'caseless' option is only available to a
    %%   compiled regex.
    %% * This will allow most valid emails. It will fail in some valid cases, such as if someone
    %%   submitted: me+"my address"@somehost.com - which is technically valid.
    RE = "^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*" ++
         "|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]" ++
         "|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")" ++
         "@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?" ++
         "|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}" ++
         "(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:" ++
         "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]" ++
         "|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)" ++
         "\\])$",
    {ok, MP} = re:compile(RE, [caseless]),
    case re:run(EMail, MP) of
        {match, _} ->
            ok;
        nomatch ->
            error
    end.

valid_password(Password) when is_binary(Password) andalso byte_size(Password) >= 6 ->
    ok;
valid_password(_Password) ->
    error.

%% Returns the subset of permitted fields that are present in in the request.
whitelisted_values(EJ, Permitted) ->
    [{X, ej:get({X}, EJ) } || X <- Permitted, ej:get({X}, EJ)  =/= undefined ].

assemble_user_ejson(#chef_user{username = Name,
                               public_key = KeyOrCert,
                               email = Email,
                               serialized_object = SerializedObject},
                    _OrgId) ->
    EJ = chef_json:decode(SerializedObject ),
    % public_key can mean either public key or cert.
    % if it's a cert, we need to extract the public key -
    % we don't want to hand the cert back on user GET.
    RealPubKey = chef_object_base:extract_public_key(KeyOrCert),
    % Where external auth is enable, email may be null/undefined
    Email2 = case Email of
        undefined -> <<"">>;
        null -> <<"">>;
        _ -> Email
    end,

    User1 = [{<<"username">>, Name},
             {<<"public_key">>, RealPubKey},
             {<<"email">>, Email2}],
    User2 = whitelisted_values(EJ, ?JSON_SERIALIZABLE ++
                               [ <<"recovery_authentication_enabled">>, <<"external_authentication_uid">>] ),

    { User1 ++ User2 }.


%% @doc Convert a binary JSON string representing a Chef User into an
%% EJson-encoded Erlang data structure.
-spec parse_binary_json(binary()) -> {ok, ej:json_object()}. % or throw
parse_binary_json(Bin) ->
    parse_binary_json(Bin, create, undefined).

-spec parse_binary_json(binary(), create | update, #chef_user{} | undefined) -> {ok, ej:json_object()}. % or throw
parse_binary_json(Bin, Operation, User) ->
    EJ = chef_object_base:delete_null_public_key(chef_json:decode(Bin)),
    EJson = case ej:get({<<"private_key">>}, EJ) of
        true ->
            ej:delete({<<"public_key">>}, EJ);
        _ ->
            validate_user(EJ, public_key_spec()),
            EJ
    end,

    %% If user is invalid, an error is thrown
    validate_user(EJson, user_spec(common)),
    validate_user(EJson, user_spec(Operation)),

    %% IF user is internally authenticated, some additional fields
    %% (common to both ops) are required. In the case where
    %% external auth id is not provided in the ejson/request,
    %% we'll use the existing data in the user record itself.
    case external_auth_uid(EJson, User) of
        undefined ->
            validate_user(EJson, local_auth_user_spec(common)),
            validate_user(EJson, local_auth_user_spec(Operation));
         _ ->
            ok
    end,
    {ok, EJson}.

external_auth_uid(EJson, #chef_user{external_authentication_uid = ExtAuthUid}) ->
    case ej:get({<<"external_authentication_uid">>}, EJson) of
        undefined ->
            undefined_or_value(ExtAuthUid);
        Value ->
            Value
    end;
external_auth_uid(EJson, _) ->
    ej:get({<<"external_authentication_uid">>}, EJson).

undefined_or_value(null) -> undefined;
undefined_or_value(Value) -> Value.

%%-spec validate_user(ejson_term(), ejson_term()) -> {ok, ejson_term()}. % or throw
validate_user(User, Spec) ->
  validate_user_name(User),
  case ej:valid(Spec, User) of
    ok ->
      {ok, User};
    BadSpec ->
      throw(BadSpec)
  end.

% Our user spec does not include 'username' because one of
% 'name'|'username' may be present. Check for either/or here,
% and use the ej:valid function to ensure consistent error
% formatting in case of problem.
validate_user_name(User) ->
  RE = chef_regex:regex_for(user_name),
  UserNameSpec = {[ {<<"username">>, {string_match, RE}} ]},
  NameSpec = {[ {<<"name">>, {string_match, RE}} ]},
  case ej:valid(UserNameSpec, User) of
    ok -> ok;
    #ej_invalid{type = missing} ->
      case ej:valid(NameSpec, User) of
        ok ->
          ok;
        BadSpec ->
          throw(BadSpec#ej_invalid{key = <<"username">>})
      end;
    BadSpec ->
      throw(BadSpec)
 end.

password_data(#chef_user{hashed_password = HashedPassword,
                                         salt = Salt,
                                         hash_type = HashType}) ->
        {HashedPassword, Salt, HashType}.

set_password_data(#chef_user{}=User, {HashedPassword, Salt, HashType}) ->
        User#chef_user{hashed_password = HashedPassword,
                       salt = Salt,
                       hash_type = HashType}.

%% @doc Return a new `chef_user()' record updated according to the specified EJSON
%% terms. This provides behavior similar to chef_objects:update_from_ejson()
-spec update_from_ejson(#chef_user{}, ejson_term()) -> #chef_user{}.
update_from_ejson(#chef_user{} = User, UserEJson) ->
    User1 = case ej:get({<<"password">>}, UserEJson) of
        NewPassword when is_binary(NewPassword) ->
            chef_user:set_password_data(User, chef_password:encrypt(NewPassword));
        _ ->
            User
    end,
    Name = username_from_ejson(UserEJson),
    Email = ej:get({<<"email">>}, UserEJson),
    ExternalAuthenticationUid = value_or_null({<<"external_authentication_uid">>}, UserEJson),
    RecoveryAuthenticationEnabled = ej:get({<<"recovery_authentication_enabled">>}, UserEJson) =:= true,
    {Key, Version} = chef_object_base:cert_or_key(UserEJson),

    User2 = case Key of
        undefined ->
            User1;
        _ ->
            User1#chef_user{
                pubkey_version= Version,
                % Note: public_key is now potentially a certificate...
                public_key = Key
            }
    end,
    SerializedObject0 = { whitelisted_values(UserEJson, ?JSON_SERIALIZABLE) },
    SerializedObject1 = merge_user_data(chef_json:decode(User2#chef_user.serialized_object),
                                        SerializedObject0),
    User2#chef_user{username = Name,
                    email = Email,
                    external_authentication_uid = ExternalAuthenticationUid,
                    recovery_authentication_enabled = RecoveryAuthenticationEnabled,
                    serialized_object = chef_json:encode(SerializedObject1)}.

merge_user_data(User, {ModData}) ->
    % If value in ModData is null, delete from user, otherwise replace or insert the value.
    lists:foldl(fun({Key,null}, AccIn) ->
                        ej:delete({Key}, AccIn);
                   ({_Key, undefined}, AccIn) ->
                        % Shouldn't happen, but just in case - don't insert value 'undefined'
                        % it's not valid.
                          AccIn;
                   ({Key, Value}, AccIn) ->
                        ej:set({Key}, AccIn, Value)
                end,
                User, ModData).


-spec set_created(#chef_user{}, object_id()) -> #chef_user{}.
set_created(#chef_user{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_user{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

-spec set_updated(#chef_user{}, object_id()) -> #chef_user{}.
set_updated(#chef_user{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_user{updated_at = Now, last_updated_by = ActorId}.

create_query() ->
    insert_user.

update_query() ->
    update_user_by_id.

delete_query() ->
    delete_user_by_id.

find_query() ->
    error(unsupported).

list_query() ->
    list_users.

bulk_get_query() ->
    bulk_get_users.

is_indexed() ->
    false.

fetch(#chef_user{username = undefined, external_authentication_uid = AuthUid}, CallbackFun) ->
    fetch_user(find_user_by_external_authentication_uid, AuthUid, CallbackFun);
fetch(#chef_user{username = UserName}, CallbackFun) ->
    fetch_user(find_user_by_username, UserName, CallbackFun).

fetch_user(Query, KeyValue, CallbackFun) ->
    CallbackFun({Query, [KeyValue],
                 {first_as_record, [chef_user, record_fields()]}}).

ejson_for_indexing(#chef_user{}, _) ->
    error(not_indexed).

fields_for_fetch(_) ->
    error(unsupported).

fields_for_update(#chef_user{last_updated_by = LastUpdatedBy,
                             updated_at      = UpdatedAt,
                             pubkey_version  = PublicKeyVersion,
                             public_key      = PublicKey,
                             hashed_password = HashedPassword,
                             salt            = Salt,
                             hash_type       = HashType,
                             serialized_object = SerializedObject,
                             external_authentication_uid = ExternalAuthenticationUid,
                             recovery_authentication_enabled = RecoveryAuthEnabled,
                             email           = Email,
                             username        = UserName,
                             id              = Id }) ->
     [false, PublicKeyVersion, PublicKey, HashedPassword, Salt, HashType, SerializedObject,
     ExternalAuthenticationUid, RecoveryAuthEnabled =:= true, Email, UserName,  LastUpdatedBy, UpdatedAt, Id].

record_fields() ->
    record_info(fields, chef_user).

list(#chef_user{external_authentication_uid = ExtAuthUid}, CallbackFun) when ExtAuthUid =/= undefined ->
    CallbackFun({list_users_by_ext_auth_uid, [ExtAuthUid], [username]});
list(#chef_user{email = undefined}, CallbackFun) ->
    CallbackFun({list_query(), [], [username]});
list(#chef_user{email = EMail}, CallbackFun) ->
    CallbackFun({list_users_by_email, [EMail], [username]}).
