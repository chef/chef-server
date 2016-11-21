%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
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

-module(chef_key_base).

-include("chef_types.hrl").

-export([maybe_generate_key_pair/1,
         maybe_generate_key_pair/2,
         set_key_pair/3,
         set_public_key/2,
         valid_public_key/1,
         validate_public_key_fields/4,
         public_key_spec/1,
         cert_or_key/1,
         extract_public_key/1,
         key_version/1,
         key_owner_type/1]).

-spec maybe_generate_key_pair(ej:json_object(),
                              fun(( keygen_timeout | not_requested | {binary(),binary()}) -> term())) -> term().
maybe_generate_key_pair(EJ, ContinuationFun) ->
    KeyPair = case ej:get({<<"create_key">>}, EJ) of
        true -> chef_keygen_cache:get_key_pair();
         _ -> {ej:get({<<"public_key">>}, EJ), undefined}
    end,
    ContinuationFun(KeyPair).

-spec maybe_generate_key_pair(ej:json_object()) -> ej:json_object() | keygen_timeout.
%% @doc Conditionally generate and add key pair data.
%%
%% If the request data contains "private_key":true, then we will generate a new key pair. In
%% this case, we'll add the new public and private keys into the EJSON since
%% update_from_json will use it to set the response.
%%
%% Note that this is now used by only ?API_v0 functions,
%% and can be removed when API_v0 is removed.
maybe_generate_key_pair(Data) ->
    case ej:get({<<"private_key">>}, Data) of
        true ->
            case chef_keygen_cache:get_key_pair() of
                {PublicKey, PrivateKey} ->
                    set_key_pair(Data, {public_key, PublicKey}, {private_key, PrivateKey});
                keygen_timeout ->
                    keygen_timeout
            end;
        _ ->
            Data
    end.


%% @doc Add public and private key data to `EJ'. This function infers
%% the key type and puts the public key data in either a `certificate' or
%% `public_key' field.
%% If a private key is defined, then the private key will be placed in the `private_key'
%% field.
-spec set_key_pair(ej:json_object(), {public_key, binary()}, {private_key, binary()}) -> ej:json_object().
set_key_pair(EJ, {public_key, PublicKey}, {private_key, PrivateKey}) ->
    EJ1 = set_public_key(EJ, PublicKey),
    case PrivateKey of
        undefined ->
            EJ1;
        _ ->
            ej:set({<<"private_key">>}, EJ1, PrivateKey)
    end.


%% @doc Sets either the `certificate' or `public_key' field of
%% `EJ' depending on the value of `PublicKey'.
-spec set_public_key(ej:json_object(), null | binary()) -> ej:json_object().
set_public_key(EJ, null) ->
    ej:set({<<"public_key">>}, EJ, null);
set_public_key(EJ, PublicKey) ->
   case key_version(PublicKey) of
        ?KEY_VERSION ->
          EJ1 = ej:set({<<"public_key">>}, EJ, PublicKey),
          ej:delete({<<"certificate">>}, EJ1);
        ?CERT_VERSION ->
          EJ1 = ej:set({<<"certificate">>}, EJ, PublicKey),
          ej:delete({<<"public_key">>}, EJ1)
    end.


%% Determine the "pubkey_version" of a key or certificate in PEM
%% format. Certificates are version 1. Public keys in either PKCS1 or
%% SPKI format are version 0. The PKCS1 format is deprecated, but
%% supported for read. We will only generate certs or SPKI packaged
%% keys.
-spec key_version(<<_:64,_:_*8>>) -> 0 | 1.
key_version(null) ->
    null;
key_version(<<"-----BEGIN CERTIFICATE", _Bin/binary>>) ->
    %% cert
    ?CERT_VERSION;
key_version(<<"-----BEGIN PUBLIC KEY", _Bin/binary>>) ->
    %% SPKI
    ?KEY_VERSION;
key_version(<<"-----BEGIN RSA PUBLIC KEY", _Bin/binary>>) ->
    %% PKCS1
    ?KEY_VERSION.

-spec valid_public_key(<<_:64, _:_*8>>) -> ok | error.
valid_public_key(PublicKey) ->
    case has_public_key_header(PublicKey) of
        true ->
            case chef_authn:extract_public_key(PublicKey) of
                {error, bad_key} ->
                    error;
                _ ->
                    ok
            end;
        false ->
            error
    end.

-spec has_public_key_header(binary()) -> true | false.
has_public_key_header(<<"-----BEGIN PUBLIC KEY", _/binary>>) ->
    true;
has_public_key_header(<<"-----BEGIN RSA PUBLIC KEY", _/binary>>) ->
    true;
has_public_key_header(_) ->
    false.

-spec public_key_spec( req | opt ) -> term().
public_key_spec(OptOrRequired) ->
    {{OptOrRequired,<<"public_key">>}, {fun_match, {fun valid_public_key/1, string,
                                            <<"Public Key must be a valid key.">>}}}.

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

extract_public_key(null) -> null;
extract_public_key(<<"null">>) -> null;
extract_public_key(Data) ->
    case key_version(Data) of
        ?KEY_VERSION ->
            Data;
        ?CERT_VERSION ->
            chef_authn:extract_pem_encoded_public_key(Data)
    end.

value_or_undefined(Key, Data) ->
  case ej:get(Key, Data) of
    null ->
      undefined;
    Value ->
      Value
  end.

%% For clients
% Will ensure that if required, only one of 'create_key' or 'public_key' is present,
% and that the one present is valid.
-spec validate_public_key_fields(opt|req,  ej:json_object(), key|user|client, create|update) -> {ok, ej:json_object()}. % or throw
% but we want USER and CLIENT to reject all key-related fields on UPDATE.


validate_public_key_fields(Req, EJ, key, _CreateOrUpdate) ->
    fail_if_present(<<"private_key">>, EJ, private_key_field_not_supported),
    % Simple case first - if one or both are provided and are not valid, handle it.
    chef_object_base:validate_ejson(EJ, {[ {{opt, <<"create_key">>}, boolean},
                                           chef_key_base:public_key_spec(opt) ]}),
    % Now we know that if present, they're good (validate_ejson will throw)
    case {Req, ej:get({<<"public_key">>}, EJ), ej:get({<<"create_key">>}, EJ)} of
        {req, undefined, undefined} ->
            % One is required, both missing.
            throw(create_or_pubkey_missing);
        {_, PK, Gen} when PK /= undefined andalso
                          Gen /= undefined andalso
                          Gen /= false ->
            % one or both are present and valid - but if it's both, it's a problem.
            throw(create_and_pubkey_specified);
        _ ->
            % Only one is present and it looks good OR key is optional
            % and both are missing. In both cases, we're fine.
            {ok, EJ}
    end;
validate_public_key_fields(Req, EJ, _ObjectType, create) ->
    % Any object that supports ancillary key creation at time of object creation
    % validates the same as does a chef_key
    % as a 'chef_key' on object creation
    validate_public_key_fields(Req, EJ, key, create);
validate_public_key_fields(_Req, EJ, _ObjectType, update) ->
    % any object that supports ancillary key creation at time of object creation
    % does NOT support updating it at a later time.
    fail_if_present(<<"private_key">>, EJ, private_key_field_not_supported),
    fail_if_present(<<"create_key">>, EJ, key_management_not_supported),
    fail_if_present(<<"public_key">>, EJ, key_management_not_supported),
    {ok, EJ}.


fail_if_present(FieldName, EJ, Response) ->
    case ej:get({FieldName}, EJ) of
        undefined -> ok;
        _ -> throw(Response)
    end.

key_owner_type(Owner) when Owner =:= client;
                           Owner =:= chef_client ->
    client_key;
key_owner_type(Owner) when Owner =:= user;
                           Owner =:= chef_user ->
    user_key;
key_owner_type(Owner) when Owner =:= org_user;
                           Owner =:= chef_org_user ->
    org_user_key.
