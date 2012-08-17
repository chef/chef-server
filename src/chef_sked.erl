%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
%%
%% @doc A utility module for manipulating OSC Chef objects from the
%% console.  Sked is Swedish for spoon. Does not work for OPC/OHC.
%%
%% @end
-module(chef_sked).

-include_lib("chef_objects/include/chef_types.hrl").
-include_lib("chef_objects/include/chef_osc_defaults.hrl").
-include_lib("chef_certgen/include/chef_certgen.hrl").

-export([create_client/3,
         create_client/4,
         create_default_environment/0,
         create_default_environment/1]).

%% An authz id used as the requestor id for operations performed by
%% this helper module.
-define(CHEF_SKED_AUTHZ_ID, <<"00000000000000000000000000005ced">>).

%% The key size to use when generating an RSA key pair.
-define(KEY_BITS, 2048).

%% @doc Create a client generating a new RSA key pair and return the
%% private key.
create_client(Name, IsValidator, IsAdmin) ->
    KeyPair = chef_certgen:rsa_generate_keypair(?KEY_BITS),
    case create_client(Name, IsValidator, IsAdmin,
                       KeyPair#rsa_key_pair.public_key) of
        ok ->
            {ok, KeyPair#rsa_key_pair.private_key};
        Error ->
            Error
    end.

%% @doc Create a client with the specified `PublicKey'.
create_client(Name, IsValidator, _IsAdmin, PublicKey) ->
    Id = chef_object:make_org_prefix_id(?OSC_ORG_ID, Name),
    Client = #chef_client{id = Id,
                          %% stub authz ID
                          authz_id = Id,
                          org_id = ?OSC_ORG_ID,
                          name = Name,
                          validator = IsValidator =:= true,
                          public_key = PublicKey,
                          pubkey_version = key_version(PublicKey)},
    Ctx = chef_db:make_context(make_req_id()),
    chef_db:create_client(Ctx, Client, ?CHEF_SKED_AUTHZ_ID).

%% @doc Create the _default environment assuming the default RDBMS of PostgreSQL.
create_default_environment() ->
    create_default_environment(pgsql).

%% @doc Create the _default environment with compression determined by
%% `DbType'.
create_default_environment(DbType) ->
    Name = <<"_default">>,
    Json = <<"{\"name\":\"_default\",\"description\":\"The default Chef environment\","
             "\"cookbook_versions\":{},\"json_class\":\"Chef::Environment\","
             "\"chef_type\":\"environment\","
             "\"default_attributes\":{},"
             "\"override_attributes\":{}}">>,
    Data = chef_db_compression:compress(DbType, chef_environment, Json),
    Id = chef_object:make_org_prefix_id(?OSC_ORG_ID, Name),
    Env = #chef_environment{id = Id,
                            authz_id = Id,
                            org_id = ?OSC_ORG_ID,
                            name = Name,
                            serialized_object = Data},
    Ctx = chef_db:make_context(make_req_id()),
    chef_db:create_environment(Ctx, Env, ?CHEF_SKED_AUTHZ_ID).
        
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

%% Create a binary string used as the request identifier.
make_req_id() ->
    TS = os:timestamp(),
    Rand = base64:encode(crypto:rand_bytes(6)),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time(TS),
    iolist_to_binary(io_lib:format("~s-~4w-~2..0w-~2..0w-~2..0w:~2..0w:~2..0w-~s",
                                   [?MODULE,
                                    Year, Month, Day,
                                    Hour, Minute, Second,
                                    Rand])).
