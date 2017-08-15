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

-include_lib("mixer/include/mixer.hrl").
-include_lib("ej/include/ej.hrl").
-include("chef_types.hrl").

-export([assemble_user_ejson/2,
         authz_id/1,
         ejson_for_indexing/2,
         fetch/2,
         fields_for_fetch/1,
         fields_for_update/1,
         fields_for_insert/1,
         id/1,
         is_indexed/1,
         name/1,
         username_from_ejson/1,
         new_record/4,
         org_id/1,
         parse_binary_json/2,
         parse_binary_json/4,
         password_data/1,
         record_fields/1,
         set_created/2,
         set_password_data/2,
         set_updated/2,
         set_api_version/2,
         type_name/1,
         update_from_ejson/2,
         validate_user_name/1,
         serialized_field_value/2,
         list/2
        ]).

%% database named queries
-export([
         bulk_get_query/1,
         create_query/1,
         delete_query/1,
         find_query/1,
         list_query/1,
         update_query/1
        ]).

-mixin([{chef_object_default_callbacks, [ update/2 ]}]).

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

-spec new_record(api_version(), object_id(), object_id(), ejson_term()) -> #chef_user{}.
new_record(?API_v0 = ApiVersion, OrgId, AuthzId, Data) ->
    User = common_new_record(ApiVersion, AuthzId, Data),
    #chef_user{username = UserName} = User,
    Id = chef_object_base:make_org_prefix_id(OrgId, UserName),
    {PublicKey, PubKeyVersion} = chef_key_base:cert_or_key(Data),
    User#chef_user{id = Id, public_key = PublicKey,
                   pubkey_version = PubKeyVersion,
                   authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id) };
new_record(ApiVersion, OrgId, AuthzId, Data) ->
    User = common_new_record(ApiVersion, AuthzId, Data),
    Id = case ej:get({<<"id">>}, Data) of
            undefined ->
                #chef_user{username = UserName} = User,
                chef_object_base:make_org_prefix_id(OrgId, UserName);
            UserId ->
                UserId
         end,
    User#chef_user{id = Id,
                   authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id) }.


common_new_record(ApiVersion, AuthzId, Data) ->
    {HashPass, Salt, HashType} = case ej:get({<<"password">>}, Data) of
        undefined ->
            {null, null, null};
        Password ->
            chef_password:encrypt(Password)
    end,
    UserData = ej:delete({<<"password">>}, Data),
    Name = username_from_ejson(Data),
    Email = value_or_null({<<"email">>}, UserData),
    ExtAuthUid = value_or_null({<<"external_authentication_uid">>}, UserData),
    EnableRecovery = ej:get({<<"recovery_authentication_enabled">>}, UserData) =:= true,
    SerializedObject = { whitelisted_values(UserData, ?JSON_SERIALIZABLE) },
    #chef_user{server_api_version = ApiVersion,
               username = Name,
               authz_id = AuthzId,
               email = Email,
               hashed_password = HashPass,
               salt = Salt,
               hash_type = HashType,
               external_authentication_uid = ExtAuthUid,
               recovery_authentication_enabled = EnableRecovery,
               serialized_object = chef_json:encode(SerializedObject)
    }.

undef_as_null(Value) ->
    case Value of
        undefined ->
            null;
        V ->
            V
    end.

value_or_existing(Key, Data, Existing) ->
    case value_or_null(Key, Data) of
        null ->
            undef_as_null(Existing);
        Value ->
            Value
    end.

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

assemble_user_ejson(#chef_user{server_api_version = ?API_v0,
                               public_key = KeyOrCert} = User, _OrgId) ->
    % public_key can mean either public key or cert.
    % if it's a cert, we need to extract the public key -
    % we don't want to hand the cert back on user GET.
    { common_user_ejson(User) ++
      [{<<"public_key">>, chef_key_base:extract_public_key(KeyOrCert)}] };
assemble_user_ejson(User, _OrgId) ->
    { common_user_ejson(User) }.

common_user_ejson(#chef_user{username = Name,
                             email = Email,
                             serialized_object = SerializedObject}) ->
    EJ = chef_json:decode(SerializedObject),
    % Where external auth is enable, email may be null/undefined
    Email2 = case Email of
        undefined -> <<"">>;
        null -> <<"">>;
        _ -> Email
    end,
    User1 = [{<<"username">>, Name}, {<<"email">>, Email2}],
    User2 = whitelisted_values(EJ, ?JSON_SERIALIZABLE ++
                               [ <<"recovery_authentication_enabled">>, <<"external_authentication_uid">>] ),
    User1 ++ User2.

%% @doc Convert a binary JSON string representing a Chef User into an
%% EJson-encoded Erlang data structure.
-spec parse_binary_json(api_version(), binary()) -> {ok, jiffy:json_value()}. % or throw
parse_binary_json(ApiVersion, Bin) ->
    parse_binary_json(ApiVersion, Bin, create, undefined).

-spec parse_binary_json(api_version(), binary(), create | update, #chef_user{} | undefined) -> {ok, jiffy:json_value()}. % or throw
parse_binary_json(?API_v0, Bin, Operation, User) ->
    EJ = delete_null_public_key(chef_json:decode(Bin)),
    EJ1 = case ej:get({<<"private_key">>}, EJ) of
        true ->
            ej:delete({<<"public_key">>}, EJ);
        _ ->
            chef_object_base:validate_ejson(EJ, {[ chef_key_base:public_key_spec(opt) ]}),
            EJ
    end,
    common_user_validation(EJ1, User, Operation);
parse_binary_json(_ApiVersion, Bin, Operation, User) ->
    % We no longer accept key, this will fail if public_key, create_key, or private_key
    % indicators are present.
    EJ = chef_json:decode(Bin),
    chef_key_base:validate_public_key_fields(opt, EJ, user, Operation),
    common_user_validation(EJ, User, Operation),
    {ok, EJ}.

%% If user is invalid, an error is thrown
common_user_validation(EJ, User, Operation) ->
    validate_user_name(EJ),
    chef_object_base:validate_ejson(EJ, user_spec(common)),
    chef_object_base:validate_ejson(EJ, user_spec(Operation)),

    %% IF user is internally authenticated, some additional fields
    %% (common to both ops) are required. In the case where
    %% external auth id is not provided in the ejson/request,
    %% we'll use the existing data in the user record itself.
    case external_auth_uid(EJ, User) of
        undefined ->
            chef_object_base:validate_ejson(EJ, local_auth_user_spec(common)),
            chef_object_base:validate_ejson(EJ, local_auth_user_spec(Operation));
         _ ->
            ok
    end,
    % Ensure the user can't set this - we make use of it in some cases by setting it internally
    EJ1 = ej:delete({<<"id">>}, EJ),
    {ok, EJ1}.

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

%% Hack to get null public_key accepted as undefined
-spec delete_null_public_key(jiffy:json_value()) -> jiffy:json_value().
delete_null_public_key(Ejson) ->
    case ej:get({<<"public_key">>}, Ejson) of
        null ->
            ej:delete({<<"public_key">>}, Ejson);
        _ ->
            Ejson
    end.

%% Our user spec does not include 'username' because one of
%% 'name'|'username' may be present. Check for either/or here,
%% and use the ej:valid function to ensure consistent error
%% formatting in case of problem.
%%
%% Prefer 'name' over 'username' since that is what we prefer in the
%% function username_from_ejson/1.
validate_user_name(User) ->
    RE = chef_regex:regex_for(user_name),
    NameSpec = {[ {<<"name">>, {string_match, RE}} ]},
    UserNameSpec = {[ {<<"username">>, {string_match, RE}} ]},
    case ej:valid(NameSpec, User) of
        ok -> ok;
        #ej_invalid{type = missing} ->
            case ej:valid(UserNameSpec, User) of
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
update_from_ejson(#chef_user{server_api_version = ?API_v0} = User, UserEJson) ->
    User1 = update_from_ejson_common(User, UserEJson),
    {Key, Version} = chef_key_base:cert_or_key(UserEJson),
    case Key of
        undefined ->
            User1;
        _ ->
            User1#chef_user{
                pubkey_version= Version,
                % Note: public_key is now potentially a certificate...
                public_key = Key
            }
    end;
update_from_ejson(#chef_user{} = User, UserEJson) ->
    update_from_ejson_common(User, UserEJson).

update_from_ejson_common(User, UserEJson) ->
    User1 = case ej:get({<<"password">>}, UserEJson) of
        NewPassword when is_binary(NewPassword) ->
            chef_user:set_password_data(User, chef_password:encrypt(NewPassword));
        _ ->
            User
    end,
    Name = username_from_ejson(UserEJson),
    Email = ej:get({<<"email">>}, UserEJson),
    ExternalAuthenticationUid = value_or_existing({<<"external_authentication_uid">>},
                                                  UserEJson,
                                                  User#chef_user.external_authentication_uid),
    RecoveryAuthenticationEnabled = value_or_existing({<<"recovery_authentication_enabled">>},
                                                      UserEJson,
                                                      User#chef_user.recovery_authentication_enabled) =:= true,
    SerializedObject0 = { whitelisted_values(UserEJson, ?JSON_SERIALIZABLE) },
    SerializedObject1 = merge_user_data(chef_json:decode(User1#chef_user.serialized_object),
                                        SerializedObject0),
    User1#chef_user{username = Name,
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

create_query(#chef_user{server_api_version = ?API_v0}) ->
    insert_user_v0;
create_query(_ObjectRec) ->
    insert_user.

update_query(#chef_user{server_api_version = ?API_v0}) ->
    update_user_by_id_v0;
update_query(_ObjectRec) ->
    update_user_by_id.

delete_query(_ObjectRec) ->
    delete_user_by_id.

find_query(_ObjectRec) ->
    error(unsupported).

list_query(_ObjectRec) ->
    list_users.

bulk_get_query(_ObjectRec) ->
    error(unsupported).

is_indexed(_ObjectRec) ->
    false.

fetch(#chef_user{server_api_version = ApiVersion,
                 username = undefined, external_authentication_uid = AuthUid} = Record, CallbackFun) ->
    fetch_user(external_auth_id_query(ApiVersion, ldap_case_sensitivity()), Record, AuthUid, CallbackFun);
fetch(#chef_user{server_api_version = ?API_v0, username = UserName} = Record, CallbackFun) ->
    fetch_user(find_user_by_username_v0, Record, UserName, CallbackFun);
fetch(#chef_user{username = UserName} = Record, CallbackFun) ->
    fetch_user(find_user_by_username, Record, UserName, CallbackFun).

ldap_case_sensitivity() ->
    LdapConfig = envy:get(oc_chef_wm, ldap, [], list),
    proplists:get_value(case_sensitive_login_attribute, LdapConfig, false).

external_auth_id_query(?API_v0, true) ->
    find_user_by_sensitive_external_authentication_uid_v0;
external_auth_id_query(?API_v0, _NotSensitive) ->
    find_user_by_external_authentication_uid_v0;
external_auth_id_query(_Not0, true) ->
    find_user_by_sensitive_external_authentication_uid;
external_auth_id_query(_Not0, _NotSensitive) ->
    find_user_by_external_authentication_uid.

fetch_user(Query, Record, KeyValue, CallbackFun) ->
    CallbackFun({Query, [KeyValue],
                 {first_as_record, [chef_user, record_fields(Record)]}}).

ejson_for_indexing(#chef_user{}, _) ->
    error(not_indexed).

fields_for_fetch(_) ->
    error(unsupported).

fields_for_update(#chef_user{server_api_version = ?API_v0,
                             last_updated_by = LastUpdatedBy,
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
     [PublicKeyVersion, PublicKey, HashedPassword, Salt,
      HashType, SerializedObject, ExternalAuthenticationUid, RecoveryAuthEnabled =:= true,
      Email, UserName,  LastUpdatedBy, UpdatedAt, Id];
fields_for_update(#chef_user{last_updated_by = LastUpdatedBy,
                             updated_at      = UpdatedAt,
                             hashed_password = HashedPassword,
                             salt            = Salt,
                             hash_type       = HashType,
                             serialized_object = SerializedObject,
                             external_authentication_uid = ExternalAuthenticationUid,
                             recovery_authentication_enabled = RecoveryAuthEnabled,
                             email           = Email,
                             username        = UserName,
                             id              = Id }) ->
     [HashedPassword, Salt, HashType, SerializedObject,
     ExternalAuthenticationUid, RecoveryAuthEnabled =:= true,
     Email, UserName,  LastUpdatedBy, UpdatedAt, Id].

fields_for_insert(#chef_user{server_api_version = ?API_v0} = User) ->
    chef_object_default_callbacks:fields_for_insert(User);
fields_for_insert(#chef_user{id = Id, authz_id = AuthzId, username = UserName,
                             email = Email, hashed_password = HashedPassword,
                             salt = Salt, hash_type = HashType, last_updated_by = LUB,
                             created_at = CreatedAt, updated_at = UpdatedAt,
                             external_authentication_uid = ExtAuthUID,
                             recovery_authentication_enabled = RecAuthEnabled,
                             serialized_object = SerializedObject} = User) ->
    Fields = [Id, AuthzId, UserName, Email, HashedPassword, Salt, HashType,
              LUB, CreatedAt, UpdatedAt, ExtAuthUID, RecAuthEnabled, SerializedObject],
    case lists:any(fun chef_object_default_callbacks:is_undefined/1, Fields) of
          true -> error({undefined_in_record, User});
          false -> ok
    end,
    Fields.


record_fields(_ApiVersion) ->
    % Note that we're not versioning this - the extra fields won't cause problems.
    record_info(fields, chef_user).

list(#chef_user{external_authentication_uid = ExtAuthUid}, CallbackFun) when ExtAuthUid =/= undefined ->
    CallbackFun({list_users_by_ext_auth_uid, [ExtAuthUid], [username]});
list(#chef_user{email = undefined} = User, CallbackFun) ->
    CallbackFun({list_query(User), [], [username]});
list(#chef_user{email = EMail}, CallbackFun) ->
    CallbackFun({list_users_by_email, [EMail], [username]}).

set_api_version(ObjectRec, Version) ->
    ObjectRec#chef_user{server_api_version = Version}.
