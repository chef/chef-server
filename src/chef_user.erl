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

-export([assemble_user_ejson/2,
         parse_binary_json/1,
         parse_binary_json/2,
         password_data/1,
         set_password_data/2,
         update_from_ejson/2]).

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

user_spec(create) ->
  {[
    {<<"name">>, {string_match, chef_regex:regex_for(user_name)}},
    {<<"password">>, {fun_match, {fun valid_password/1, string, <<"Password must have at least 6 characters">>}}},
    {{opt,<<"admin">>}, boolean}
   ]};
user_spec(update) ->
  {[
    {<<"name">>, {string_match, chef_regex:regex_for(user_name)}},
    {{opt,<<"password">>}, {fun_match, {fun valid_password/1, string, <<"Password must have at least 6 characters">>}}},
    {{opt,<<"private_key">>}, boolean},
    {{opt,<<"admin">>}, boolean}
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
  User = chef_json:decode(Bin),
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

    {Key, _Version} = chef_object:cert_or_key(UserData),
    UserWithPassword = chef_user:set_password_data(User, PasswordData),
    UserWithPassword#chef_user{username = Name,
                               admin = IsAdmin,
                               public_key = Key
                              }.

