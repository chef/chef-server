%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Mark Mzyk <mmzyk@opscode.com
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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
         fields_for_fetch/1,
         fields_for_update/1,
         id/1,
         is_indexed/0,
         name/1,
         org_id/1,
         new_record/3,
         parse_binary_json/1,
         parse_binary_json/2,
         password_data/1,
         record_fields/0,
         set_created/2,
         set_password_data/2,
         set_updated/2,
         type_name/1,
         update_from_ejson/2
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

-export([
         list/2
         ]).

-include("chef_types.hrl").

%% fields:
 %% username/name - in webui, name has _ inserted for , (periods) so should check that there are no periods in the name here
 %% password - no default -
 %%   cannot be blank, must be 6 chars
 %% admin - default is false

-define(DEFAULT_FIELD_VALUES,
        [
          {<<"admin">>, false}
        ]).


-behaviour(chef_object).

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

%% TODO: this doesn't need an argument
type_name(#chef_user{}) ->
    user.

-spec new_record(object_id(), object_id(), {ejson_term(), {binary(), binary(), binary()}}) -> #chef_user{}.
    %% This only works for Open Source Users currently
new_record(OrgId, AuthzId, {UserData, {HashPass, Salt, HashType}}) ->
Name = ej:get({<<"name">>}, UserData),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    Email = value_or_null({<<"email">>}, UserData),
    Admin = ej:get({<<"admin">>}, UserData) =:= true,
    {PublicKey, _PubkeyVersion} = cert_or_key(UserData),
    #chef_user{id = Id,
               authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
               username = Name,
               email = Email,
               public_key = PublicKey,
               hashed_password = HashPass,
               salt = Salt,
               hash_type = HashType,
               external_authentication_uid = null, %% Not used in open source user
               recovery_authentication_enabled = false, %% Not used in open source user
               admin = Admin
    }.

value_or_null(Key, Data) ->
 Value = ej:get(Key, Data),
  case Value of
    undefined ->
      null;
    _ ->
      Value
  end.

cert_or_key(Payload) ->
    %% Some consumers of the API, such as webui, will generate a
    %% JSON { public_key: null } to mean, "do not change it". By
    %% default, null is treated as a defined, and will erase the
    %% public_key in the database. We use value_or_undefined() to
    %% convert all null into undefined.
    Cert = value_or_undefined({<<"certificate">>}, Payload),
    PublicKey = value_or_undefined({<<"public_key">>}, Payload),
    %% Take certificate first, then public_key
    case Cert of
        undefined ->
            {PublicKey, ?KEY_VERSION};
        _ ->
            {Cert, ?CERT_VERSION}
    end.

value_or_undefined(Key, Data) ->
  case ej:get(Key, Data) of
    null ->
      undefined;
    Value ->
      Value
  end.

user_spec(create) ->
  {[
    {<<"name">>, {string_match, chef_regex:regex_for(user_name)}},
    {<<"password">>, {fun_match, {fun valid_password/1, string, <<"Password must have at least 6 characters">>}}},
    {{opt,<<"admin">>}, boolean},
    {{opt,<<"public_key">>}, {fun_match, {fun chef_object_base:valid_public_key/1, string, <<"Public Key must be a valid key.">>}}}
   ]};
user_spec(update) ->
  {[
    {<<"name">>, {string_match, chef_regex:regex_for(user_name)}},
    {{opt,<<"password">>}, {fun_match, {fun valid_password/1, string, <<"Password must have at least 6 characters">>}}},
    {{opt,<<"private_key">>}, boolean},
    {{opt,<<"admin">>}, boolean},
    {{opt,<<"public_key">>}, {fun_match, {fun chef_object_base:valid_public_key/1, string, <<"Public Key must be a valid key.">>}}}
   ]}.

valid_password(Password) when is_binary(Password) andalso byte_size(Password) >= 6 ->
  ok;
valid_password(_Password) ->
  error.



assemble_user_ejson(#chef_user{username = Name,
                               public_key = PubKey,
                               admin = Admin},
                    _OrgId) ->
    {[{<<"name">>, Name},
      {<<"public_key">>, PubKey},
      {<<"admin">>, Admin}]}.

%% @doc Convert a binary JSON string representing a Chef User into an
%% EJson-encoded Erlang data structure.
-spec parse_binary_json(binary()) -> {ok, ej:json_object()}. % or throw
parse_binary_json(Bin) ->
    parse_binary_json(Bin, create).

-spec parse_binary_json(binary(), create | update) -> {ok, ej:json_object()}. % or throw
parse_binary_json(Bin, Operation) ->
  User = chef_object_base:delete_null_public_key(chef_json:decode(Bin)),
  %% If user is invalid, an error is thown
  validate_user(User, user_spec(Operation)),
  %% Set default values after validating input, so admin can be set to false
  %% if it is not present
  User1 = set_default_values(User, ?DEFAULT_FIELD_VALUES),
  {ok, User1}.

set_default_values(User, Defaults) ->
  lists:foldl(fun({Key, Default}, Current) ->
                case ej:get({Key}, Current) of
                    undefined ->
                      ej:set({Key}, Current, Default);
                    _ -> Current
                end
              end,
              User,
              Defaults).

%%-spec validate_user(ejson_term(), ejson_term()) -> {ok, ejson_term()}. % or throw
validate_user(User, Spec) ->
  case ej:valid(Spec, User) of
    ok ->
      {ok, User};
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

%% TODO: This is transient code and will be deprecated/removed in the future
-spec update_from_ejson(#chef_user{}, {ejson_term(), {binary(), binary(), binary()}}) -> #chef_user{}.
%% @doc Return a new `chef_user()' record updated according to the specified EJSON
%% terms. This provides behavior similar to chef_objects:update_from_ejson()
update_from_ejson(#chef_user{} = User, {UserData, PasswordData}) ->
    Name = ej:get({<<"name">>}, UserData),
    IsAdmin = ej:get({<<"admin">>}, UserData) =:= true,

    {Key, _Version} = chef_object_base:cert_or_key(UserData),
    UserWithPassword = chef_user:set_password_data(User, PasswordData),
    case Key of
        undefined ->
            UserWithPassword#chef_user{username = Name,
                admin = IsAdmin
            };
        _ ->
            UserWithPassword#chef_user{username = Name,
                admin = IsAdmin,
                public_key = Key
            }
    end.

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
    find_user_by_username.

list_query() ->
    list_users.

bulk_get_query() ->
    bulk_get_users.

is_indexed() ->
    false.

ejson_for_indexing(#chef_user{}, _) ->
    error(not_indexed).

fields_for_update(#chef_user{last_updated_by = LastUpdatedBy,
                             updated_at      = UpdatedAt,
                             admin           = IsAdmin,
                             public_key      = PublicKey,
                             hashed_password = HashedPassword,
                             salt            = Salt,
                             hash_type       = HashType,
                             id              = Id }) ->
    [IsAdmin =:= true, PublicKey, HashedPassword, Salt, HashType, LastUpdatedBy, UpdatedAt, Id].

fields_for_fetch(#chef_user{username = UserName}) ->
    [UserName].

record_fields() ->
    record_info(fields, chef_user).

list(#chef_user{}, CallbackFun) ->
    CallbackFun(list_query(), [], [username]).
    
