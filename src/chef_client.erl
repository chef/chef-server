%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80-*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
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
-module(chef_client).

-export([
         add_authn_fields/2,

         assemble_client_ejson/2,
         oc_assemble_client_ejson/2,
         osc_assemble_client_ejson/2,

         oc_parse_binary_json/2,
         oc_parse_binary_json/3,

         osc_parse_binary_json/2,
         osc_parse_binary_json/3,

         parse_binary_json/2,
         parse_binary_json/3,

         set_key_pair/3,
         set_public_key/2
        ]).

-include_lib("ej/include/ej.hrl").
-include("chef_types.hrl").
-include("chef_osc_defaults.hrl").

-define(DEFAULT_FIELD_VALUES,
        [
         {<<"json_class">>, <<"Chef::ApiClient">>},
         {<<"chef_type">>, <<"client">>},
         {<<"validator">>, false},
         {<<"admin">>, false}
        ]).

-define(OC_DEFAULT_FIELD_VALUES, [
                                  {<<"validator">>, false},
                                  {<<"private_key">>, false}
                                 ]).

-define(ASSEMBLE_CLIENT, osc_assemble_client_ejson).

-define(PARSE_BINARY, osc_parse_binary_json).

-ifdef(OC_CHEF).
-undef(ASSEMBLE_CLIENT).
-define(ASSEMBLE_CLIENT, oc_assemble_client_ejson).

-undef(PARSE_BINARY).
-define(PARSE_BINARY, oc_parse_binary_json).
-endif.

-spec add_authn_fields(ejson_term(), binary()) -> ejson_term().
%% @doc Add in the generated public key along with other authn related
%% fields to the EJson encoded request body. Return the modified EJson
%% structure
add_authn_fields(ClientData, PublicKey) ->
    lists:foldl(fun({Key, Value}, EJson) ->
                    ej:set({Key}, EJson, Value)
                end,
                ClientData,
                [
                    {<<"pubkey_version">>, key_version(PublicKey)},
                    {<<"public_key">>, PublicKey}
                ]).

%% @doc creates the json body for clients
-spec assemble_client_ejson(#chef_client{}, binary()) -> ejson_term().
assemble_client_ejson(Client, OrgName) ->
    ?ASSEMBLE_CLIENT(Client, OrgName).

%% @doc creates the json body for clients
-spec oc_assemble_client_ejson(#chef_client{}, binary()) -> ejson_term().
oc_assemble_client_ejson(#chef_client{name = Name, validator = Validator,
                                      public_key = PublicKey}, OrgName) ->
    Values = [{<<"name">>, value_or_default(Name, <<"">>)},
              {<<"clientname">>, value_or_default(Name, <<"">>)},
              {<<"validator">>, Validator =:= true},
              {<<"orgname">>, OrgName}],
    case PublicKey of
        undefined ->
            {Values};
        _ ->
            case key_version(PublicKey) of
                0 ->
                    {[{<<"public_key">>, PublicKey} | Values]};
                1 ->
                    {[{<<"certificate">>, PublicKey} | Values]}
            end
    end.

%% @doc creates the json body for clients
-spec osc_assemble_client_ejson(#chef_client{}, binary() | ?OSC_ORG_NAME) -> ejson_term().
osc_assemble_client_ejson(#chef_client{name = Name,
                                   admin = IsAdmin,
                                   validator = IsValidator,
                                   public_key = PublicKey}, _OrgName) ->
    Values = [{<<"name">>, value_or_default(Name, <<"">>)},
              {<<"admin">>, IsAdmin =:= true},
              {<<"validator">>, IsValidator =:= true},
              {<<"json_class">>, <<"Chef::ApiClient">>},
              {<<"chef_type">>, <<"client">>}],
    case PublicKey of
        undefined ->
            {Values};
        AKey ->
            {[{<<"public_key">>, AKey} | Values]}
    end.

parse_binary_json(Bin, ReqName) ->
    ?PARSE_BINARY(Bin, ReqName, not_found).

parse_binary_json(Bin, ReqName, CurrentClient) ->
    ?PARSE_BINARY(Bin, ReqName, CurrentClient).

osc_parse_binary_json(Bin, ReqName) ->
    osc_parse_binary_json(Bin, ReqName, not_found).

-spec osc_parse_binary_json(binary(), binary() | undefined,
                            not_found | #chef_client{}) -> {'ok', {[{_, _}]}}. % or throw
%% @doc Convert a binary JSON string representing a Chef Client into an
%% EJson-encoded Erlang data structure, using passed defaults
%% @end
osc_parse_binary_json(Bin, ReqName, CurrentClient) ->
    Client = osc_set_values_from_current_client(chef_json:decode_body(Bin), CurrentClient),
    Client1 = set_default_values(Client, ?DEFAULT_FIELD_VALUES),
    {Name, FinalClient} = osc_destination_name(Client1, ReqName),
    valid_name(Name),
    validate_client(FinalClient, Name, osc).

oc_parse_binary_json(Bin, ReqName) ->
    oc_parse_binary_json(Bin, ReqName, not_found).

-spec oc_parse_binary_json(binary(), binary() | undefined,
                           not_found | #chef_client{}) -> {'ok', {[{_, _}]}}. % or throw
%% @doc Convert a binary JSON string representing a Chef Client into an
%% EJson-encoded Erlang data structure, using passed defaults
%% @end
oc_parse_binary_json(Bin, ReqName, CurrentClient) ->
    Client = oc_set_values_from_current_client(chef_json:decode_body(Bin), CurrentClient),
    Client1 = set_default_values(Client, ?OC_DEFAULT_FIELD_VALUES),
    {Name, FinalClient} = oc_destination_name(Client1, ReqName),
    valid_name(Name),
    validate_client(FinalClient, Name, oc).

%% @doc Add public and private key data to `ClientEjson'. This function infers
%% the key type and puts the public key data in iether a `certificate' or
%% `public_key' field. The private key will be placed in the `private_key'
%% field.
-spec set_key_pair(ej:json_object(), {public_key, binary()}, {private_key, binary()}) -> ej:json_object().
set_key_pair(ClientEjson, {public_key, PublicKey}, {private_key, PrivateKey}) ->
    ClientEjson1 = set_public_key(ClientEjson, PublicKey),
    ej:set({<<"private_key">>}, ClientEjson1, PrivateKey).

%% @doc Sets either the `certificate' or `public_key' field of
%% `ClientEjson' depending on the value of `PublicKey'.
-spec set_public_key(ej:json_object(), binary()) -> ej:json_object().
set_public_key(ClientEjson, PublicKey) ->
    case key_version(PublicKey) of
        ?KEY_VERSION ->
            ej:set({<<"public_key">>}, ClientEjson, PublicKey);
        ?CERT_VERSION ->
            ej:set({<<"certificate">>}, ClientEjson, PublicKey)
    end.

validate_client(Client, Name, osc) ->
    validate_client(Client, osc_client_spec(Name));
validate_client(Client, Name, oc) ->
    validate_client(Client, oc_client_spec(Name)).

validate_client(Client, Spec) ->
    case ej:valid(Spec, Client) of
        ok -> {ok, Client};
        Bad -> throw(Bad)
    end.

osc_set_values_from_current_client(Client, not_found) ->
    Client;
osc_set_values_from_current_client(Client, #chef_client{admin = IsAdmin,
                                                        validator = IsValidator,
                                                        public_key = PublicKey}) ->
    C = set_default_values(Client, [{<<"admin">>, IsAdmin},
                                    {<<"validator">>, IsValidator}]),
    set_public_key(C, PublicKey).

oc_set_values_from_current_client(Client, not_found) ->
    Client;
oc_set_values_from_current_client(Client, #chef_client{validator = IsValidator,
                                                       public_key = Cert}) ->
    C = set_default_values(Client, [{<<"validator">>, IsValidator}]),
    set_public_key(C, Cert).

set_default_values(Client, Defaults) ->
    lists:foldl(fun({Key, Default}, Current) ->
                        case ej:get({Key}, Current) of
                            undefined -> ej:set({Key}, Current, Default);
                            _ -> Current
                        end
                end,
                Client, Defaults).

osc_client_spec(Name) ->
    {[
      {{opt, <<"name">>}, Name},
      {{opt, <<"admin">>}, boolean},
      {{opt, <<"validator">>}, boolean},
      {{opt, <<"private_key">>}, boolean},
      {{opt, <<"json_class">>}, <<"Chef::ApiClient">>},
      {{opt, <<"chef_type">>}, <<"client">>}
     ]}.

oc_client_spec(Name) ->
    {[
      {<<"name">>, Name},
      {<<"clientname">>, Name},
      {{opt, <<"validator">>}, boolean},
      {{opt, <<"private_key">>}, boolean}
     ]}.

%% If name is not in JSON, use the name from the URL. Otherwise, use the name in the JSON.
osc_destination_name(Client, ReqName) ->
    Name = ej:get({<<"name">>}, Client),
    case {Name, ReqName} of
        {undefined, undefined} ->
            throw({both_missing, <<"name">>, <<"URL-NAME">>});
        {undefined, _} ->
            %% If the name is ommitted from the JSON then use what's in the URL
            {ReqName, ej:set({<<"name">>}, Client, ReqName)};
        {_, _} ->
            %% If both are present, leave as-is, because the user may be
            %% renaming (from name in URL to name in JSON).
            {Name, Client}
    end.

-spec oc_destination_name(ej:json_object(), binary() | undefined) -> {binary(), ej:json_object()} | no_return().
oc_destination_name(Client, ReqName) ->
    % Since either name or clientname is required (but not both), if only one is
    % passed, we will use it to set the other one
    Name = ej:get({<<"name">>}, Client),
    ClientName = ej:get({<<"clientname">>}, Client),
    % TODO: Can this be simplified?  Several of these cases are duplicates; doesn't
    % seem to be a case where reordering really solves the issue
    case {Name, ClientName, ReqName} of
        {undefined, undefined, undefined} ->
            %% this is a POST with no name and no clientname is body
            throw({both_missing, <<"name">>, <<"clientname">>});
        {undefined, undefined, ReqName} ->
            %% a PUT with only name found in URL, set both in body
            {ReqName, ej:set({<<"name">>}, ej:set({<<"clientname">>},
                                                  Client, ReqName), ReqName)};
        {Name, Name, _} ->
            %% POST or PUT with with name == clientname
            {Name, Client};
        {Name, undefined, _} ->
            %% POST or PUT with only name, set clientname
            {Name, ej:set({<<"clientname">>}, Client, Name)};
        {undefined, ClientName, _} ->
            %% POST or PUT with only clientname, set name
            {ClientName, ej:set({<<"name">>}, Client, ClientName)};
        {_, _, _} ->
            throw({client_name_mismatch})
    end.

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

valid_name(Name) ->
    {Regex, Msg} = chef_regex:regex_for(client_name),
    case re:run(Name, Regex) of
        nomatch ->
            throw({bad_client_name, Name, Msg});
        _ ->
            ok
    end.

value_or_default(undefined, Default) ->
    Default;
value_or_default(Value, _) ->
    Value.
