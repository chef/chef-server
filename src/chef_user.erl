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
         set_key_pair/3,
         set_public_key/2]).

-include("chef_types.hrl").

%% fields:
 %% username/name - in webui, name has _ inserted for , (periods) so should check that there are no periods in the name here
 %% password - no default -
 %%   cannot be blank, must be 6 chars
 %% openid - default is none - no restrictions on what it can be set to

-define(DEFAULT_FIELD_VALUES,
        [
          {<<"openid">>, null},
          {<<"admin">>, false}
        ]).

user_spec() ->
  {[
    {<<"name">>, {string_match, chef_regex:regex_for(user_name)}},
    {<<"password">>, {fun_match, {fun valid_password/1, string, <<"Password must have at least 6 characters">>}}},
    {{opt,<<"admin">>}, boolean},
    {{opt, <<"openid">>}, string}
   ]}.

valid_password(Password) when is_binary(Password) andalso byte_size(Password) >= 6 ->
  ok;
valid_password(_Password) ->
  error.

assemble_user_ejson(#chef_user{username = Name,
                               public_key = PubKey,
                               external_authentication_uid = OpenId,
                               admin = Admin},
                    _OrgId) ->
    {[{<<"name">>, Name},
      {<<"public_key">>, PubKey},
      {<<"openid">>, OpenId},
      {<<"admin">>, Admin}]}.

%% @doc Convert a binary JSON string representing a Chef User into an
%% EJson-encoded Erlang data structure.
-spec parse_binary_json( binary()) -> {ok, ejson_term() }. % or throw
parse_binary_json(Bin) ->
  User = ejson:decode(Bin),
  %% If user is invalid, an error is thown
  validate_user(User, user_spec()),
  %% Set default values after validating input, so openid can be validated as string
  %% but set to null if it is not present (null fails string validation)
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

%% @doc Add public and private key data to `UserEjson'. This function infers
%% the key type and puts the public key data in iether a `certificate' or
%% `public_key' field. The private key will be placed in the `private_key'
%% field.
-spec set_key_pair(ej:json_object(), {public_key, binary()}, {private_key, binary()}) -> ej:json_object().
set_key_pair(UserEjson, {public_key, PublicKey}, {private_key, PrivateKey}) ->
    UserEjson1 = set_public_key(UserEjson, PublicKey),
    ej:set({<<"private_key">>}, UserEjson1, PrivateKey).

%% @doc Sets either the `certificate' or `public_key' field of
%% `UserEjson' depending on the value of `PublicKey'.
-spec set_public_key(ej:json_object(), binary()) -> ej:json_object().
set_public_key(UserEjson, PublicKey) ->
  case key_version(PublicKey) of
        ?KEY_VERSION ->
            ej:set({<<"public_key">>}, UserEjson, PublicKey);
        ?CERT_VERSION ->
            ej:set({<<"certificate">>}, UserEjson, PublicKey)
    end.

%% Shameless ripped key_version code from chef_client -
%% needs to be put in a common place if it is going to end up in
%% both user and client - putting here to more further along for now

%% Determine the "pubkey_version" of a key or certificate in PEM
%% format. Certificates are version 1. Public keys in either PKCS1 or
%% SPKI format are version 0. The PKCS1 format is deprecated, but
%% supported for read. We will only generate certs or SPKI packaged
%% keys.
key_version(<<"-----BEGIN CERTIFICATE", _Bin/binary>>) ->
    %% cert
    ?CERT_VERSION;
key_version(<<"-----BEGIN PUBLIC KEY", _Bin/binary>>) ->
    %% SPKI
    ?KEY_VERSION;
key_version(<<"-----BEGIN RSA PUBLIC KEY", _Bin/binary>>) ->
    %% PKCS1
    ?KEY_VERSION.



