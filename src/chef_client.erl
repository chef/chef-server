%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80-*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
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
         parse_binary_json/2,
         parse_binary_json/3
        ]).

-include_lib("ej/include/ej.hrl").
-include("chef_types.hrl").

-define(DEFAULT_FIELD_VALUES,
        [
         {<<"validator">>, false},
         {<<"admin">>, false},
         {<<"private_key">>, false},
         {<<"certificate">>, <<"">>}
        ]).

%% @doc creates the json body for clients
-spec assemble_client_ejson(#chef_client{}, binary()) -> ejson_term().
assemble_client_ejson(#chef_client{name = Name, validator = Validator,
                                   public_key = PublicKey}, OrgName) ->
    {[{<<"name">>, Name},
      {<<"clientname">>, Name},
      {<<"validator">>, Validator},
      {<<"certificate">>, PublicKey},
      {<<"orgname">>, OrgName}]}.

-spec parse_binary_json(binary(), binary() | undefined) -> {'ok', {[{_, _}]}}. % or throw
%% @doc Convert a binary JSON string representing a Chef Client into an
%% EJson-encoded Erlang data structure, with no passed defaults
%% @end
parse_binary_json(Bin, ReqName) ->
    parse_binary_json(Bin, ReqName, not_found).

-spec parse_binary_json(binary(), binary() | undefined, not_found | [_]) -> {'ok', {[{_, _}]}}. % or throw
%% @doc Convert a binary JSON string representing a Chef Client into an
%% EJson-encoded Erlang data structure, using passed defaults
%% @end
parse_binary_json(Bin, ReqName, Defaults) ->
    Client = chef_json:decode_body(Bin),
    NewClient = case Defaults of
                    not_found ->
                        set_default_values(Client);
                    Values ->
                        set_default_values(Client, Values)
                end,
    {Name, FinalClient} = set_name_values(NewClient, ReqName),
    valid_name(Name),
    validate_client(FinalClient, Name).

client_spec(Name) ->
    {[
      {<<"name">>, Name},
      {<<"clientname">>, Name},
      {{opt, <<"admin">>}, boolean},
      {{opt, <<"validator">>}, boolean},
      {{opt, <<"private_key">>}, boolean}
     ]}.

set_default_values(Client) ->
    set_default_values(Client, ?DEFAULT_FIELD_VALUES).
set_default_values(Client, Defaults) ->
    lists:foldl(fun({Key, Default}, Current) ->
                        case ej:get({Key}, Current) of
                            undefined ->
                                ej:set({Key}, Current, Default);
                            _ -> Current
                        end
                end,
                Client,
                Defaults).

-spec set_name_values(ej:json_object(), binary() | undefined) -> {binary(), ej:json_object()} | no_return().
set_name_values(Client, ReqName) ->
    % Since either name or clientname is required (but not both), if only one is
    % passed, we will use it to set the other one
    Name = ej:get({<<"name">>}, Client),
    ClientName = ej:get({<<"clientname">>}, Client),
    % TODO: Can this be simplified?  Several of these cases are duplicates; doesn't
    % seem to be a case where reordering really solves the issue
    case {Name, ClientName, ReqName} of
        {undefined, undefined, _} ->
            throw({both_missing, <<"name">>, <<"clientname">>});
        {Name, Name, undefined} ->
            {Name, Client};
        {Name, Name, Name} ->
            {Name, Client};
        {Name, Name, _} ->
            {ReqName, ej:set({<<"name">>}, ej:set({<<"clientname">>},
                                                  Client, ReqName), ReqName)};
        {Name, undefined, undefined} ->
            {Name, ej:set({<<"clientname">>}, Client, Name)};
        {Name, undefined, Name} ->
            {Name, ej:set({<<"clientname">>}, Client, Name)};
        {Name, undefined, _} ->
            {ReqName, ej:set({<<"name">>}, ej:set({<<"clientname">>},
                                                  Client, ReqName), ReqName)};
        {undefined, ClientName, undefined} ->
            {ClientName, ej:set({<<"name">>}, Client, ClientName)};
        {undefined, ClientName, ClientName} ->
            {ClientName, ej:set({<<"name">>}, Client, ClientName)};
        {undefined, ClientName, _} ->
            {ReqName, ej:set({<<"name">>}, ej:set({<<"clientname">>},
                                                  Client, ReqName), ReqName)};
        {_, _, _} ->
            throw({client_name_mismatch})
    end.

validate_client(Client, Name) ->
    case ej:valid(client_spec(Name), Client) of
        ok -> {ok, Client};
        Bad -> throw(Bad)
    end.

valid_name(Name) ->
    {Regex, Msg} = chef_regex:regex_for(client_name),
    case re:run(Name, Regex) of
        nomatch ->
            throw({bad_client_name, Name, Msg});
        _ ->
            ok
    end.

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

%% Determine the "pubkey_version" of a key or certificate in PEM
%% format. Certificates are version 1. Public keys in either PKCS1 or
%% SPKI format are version 0. The PKCS1 format is deprecated, but
%% supported for read. We will only generate certs or SPKI packaged
%% keys.
key_version(<<"-----BEGIN CERTIFICATE", _Bin/binary>>) ->
    %% cert
    1;
key_version(<<"-----BEGIN PUBLIC KEY", _Bin/binary>>) ->
    %% SPKI
    0;
key_version(<<"-----BEGIN RSA PUBLIC KEY", _Bin/binary>>) ->
    %% PKCS1
    0.

