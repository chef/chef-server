%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Seth Chisamore <schisamo@opscode.com>
%% @doc A utility module for manipulating OSC Chef objects from the
%% console.  Sked is Swedish for spoon. Does not work for OPC/OHC.
%%
%% @end
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

-module(chef_sked).

-include("chef_wm.hrl").

-export([create_client/4,
         create_user/4,
         create_default_environment/0]).

%% An authz id used as the requestor id for operations performed by
%% this helper module.
-define(CHEF_SKED_AUTHZ_ID, <<"00000000000000000000000000005ced">>).

%% The key size to use when generating an RSA key pair.
-define(KEY_BITS, 2048).

%% @doc Create a client generating a new RSA key pair and return the
%% private key.
create_client(Name, IsValidator, IsAdmin, create_key) ->
    {PubKey, PrivKey} = generate_key_pair(),
    case create_client(Name, IsValidator, IsAdmin, PubKey) of
        ok ->
            {ok, PrivKey};
        Error ->
            Error
    end;
%% @doc Create a client with the specified `PublicKey'.
create_client(Name, IsValidator, IsAdmin, PublicKey) ->
    Ejson = {[{<<"name">>, Name},
              {<<"validator">>, IsValidator =:= true},
              {<<"admin">>, IsAdmin =:= true},
              {<<"public_key">>, PublicKey}]},
    create_from_json(chef_client, Ejson).

create_user(Name, Password, IsAdmin, create_key) ->
    {PubKey, PrivKey} = generate_key_pair(),
    case create_user(Name, Password, IsAdmin, PubKey) of
        ok ->
            {ok, PrivKey};
        Error ->
            Error
    end;

create_user(Name, Password, IsAdmin, PublicKey) ->
    PasswordData = chef_password:encrypt(Password),
    Ejson = {[{<<"name">>, Name},
              {<<"admin">>, IsAdmin =:= true},
              {<<"public_key">>, PublicKey}]},
    create_from_json(chef_user, {Ejson, PasswordData}).

%% @doc Create the _default environment
create_default_environment() ->
    Ejson = {[{<<"name">>, <<"_default">>},
              {<<"description">>, <<"The default Chef environment">>},
              {<<"cookbook_versions">>, {[]}},
              {<<"json_class">>, <<"Chef::Environment">>},
              {<<"chef_type">>, <<"environment">>},
              {<<"default_attributes">>, {[]}},
              {<<"override_attributes">>, {[]}}]},
    create_from_json(chef_environment, Ejson).

%% @doc Thin wrapper used to create objects from ejson tuples.
%% FIXME: It would be nice to deal stricly with records!
create_from_json(RecType, ObjectEjson) ->
    DbContext = chef_db:make_context(make_req_id()),
    %% ObjectEjson should already be normalized. Record creation does minimal work and does
    %% not add or update any fields.
    ObjectRec = chef_object:new_record(RecType, ?OSC_ORG_ID, unset, ObjectEjson),
    %% We send the object data to solr for indexing *first*. If it fails, we'll error out on
    %% a 500 and client can retry. If we succeed and the db call fails or conflicts, we can
    %% safely send a delete to solr since this is a new object with a unique ID unknown to
    %% the world.
    ok = chef_object_db:add_to_solr(ObjectRec, ObjectEjson),

    case chef_db:create(ObjectRec, DbContext, ?CHEF_SKED_AUTHZ_ID) of
        {conflict, Msg} ->
            %% ignore return value of solr delete, this is best effort.
            chef_object_db:delete_from_solr(ObjectRec),
            {conflict, Msg};
        ok ->
            ok;
        What ->
            %% ignore return value of solr delete, this is best effort.
            chef_object_db:delete_from_solr(ObjectRec),
            What
    end.

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

generate_key_pair() ->
    case chef_keygen_cache:get_key_pair() of
        keygen_timeout ->
            erlang:error({chef_sked, "unable to generate key pair", keygen_timeout});
        {PubKey, PrivKey} ->
            {PubKey, PrivKey}
    end.
