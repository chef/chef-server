%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80-*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@chef.io>
%% @author Seth Falcon <seth@chef.io>
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
         authz_id/1,
         add_authn_fields/2,
         assemble_client_ejson/2,
         ejson_for_indexing/2,
         fields_for_fetch/1,
         fields_for_update/1,
         id/1,
         is_indexed/0,
         name/1,
         org_id/1,
         new_record/3,
         oc_assemble_client_ejson/2,
         oc_parse_binary_json/2,
         oc_parse_binary_json/3,
         osc_assemble_client_ejson/2,
         osc_parse_binary_json/2,
         osc_parse_binary_json/3,
         parse_binary_json/2,
         parse_binary_json/3,
         record_fields/0,
         set_created/2,
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

-include_lib("mixer/include/mixer.hrl").
-mixin([{chef_object,[
                      {default_fetch/2, fetch},
                      {default_update/2, update}
                     ]}]).

-export([
         list/2
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
                                  {<<"json_class">>, <<"Chef::ApiClient">>},
                                  {<<"chef_type">>, <<"client">>},
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

-behaviour(chef_object).

-spec name(#chef_client{}) -> binary().
name(#chef_client{name = Name}) ->
    Name.

-spec id(#chef_client{}) -> object_id().
id(#chef_client{id = Id}) ->
    Id.

-spec org_id(#chef_client{}) -> object_id().
org_id(#chef_client{org_id = OrgId}) ->
    OrgId.

%% TODO: this doesn't need an argument
type_name(#chef_client{}) ->
    client.

authz_id(#chef_client{authz_id = AuthzId})->
    AuthzId.

is_indexed() ->
    true.

ejson_for_indexing(#chef_client{}, Client) ->
    Client.

update_from_ejson(#chef_client{} = Client, ClientData) ->
    Name = ej:get({<<"name">>}, ClientData),
    IsAdmin = ej:get({<<"admin">>}, ClientData) =:= true,
    IsValidator = ej:get({<<"validator">>}, ClientData) =:= true,
    %% Take public_key first, then certificate
    {Key, Version} = cert_or_key(ClientData),
    case Key of
        undefined ->
            Client#chef_client{name = Name,
                admin = IsAdmin,
                validator = IsValidator};
        _ ->
            Client#chef_client{name = Name,
                admin = IsAdmin,
                validator = IsValidator,
                public_key = Key,
                pubkey_version = Version}
    end.

-spec set_created(#chef_client{}, object_id()) -> #chef_client{}.
set_created(#chef_client{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_client{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

-spec set_updated(#chef_client{}, object_id()) -> #chef_client{}.
set_updated(#chef_client{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_client{updated_at = Now, last_updated_by = ActorId}.

create_query() ->
    insert_client.

update_query() ->
    update_client_by_id.

delete_query() ->
    delete_client_by_id.

find_query() ->
    find_client_by_orgid_name.

list_query() ->
    list_clients_for_org.

bulk_get_query() ->
    bulk_get_clients.

-spec new_record(object_id(), object_id(), ejson_term()) -> #chef_client{}.
new_record(OrgId, AuthzId, ClientData) ->
    Name = ej:get({<<"name">>}, ClientData),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    Validator = ej:get({<<"validator">>}, ClientData) =:= true,
    Admin = ej:get({<<"admin">>}, ClientData) =:= true,
    {PublicKey, PubkeyVersion} = cert_or_key(ClientData),
    #chef_client{id = Id,
                 authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
                 org_id = OrgId,
                 name = Name,
                 validator = Validator,
                 admin = Admin,
                 public_key = PublicKey,
                 pubkey_version = PubkeyVersion}.

fields_for_update(#chef_client{last_updated_by = LastUpdatedBy,
                               updated_at = UpdatedAt,
                               name = Name,
                               public_key = PublicKey,
                               pubkey_version = PubkeyVersion,
                               admin = IsAdmin,
                               validator = IsValidator,
                               id = Id}) ->
    [LastUpdatedBy, UpdatedAt, Name,
     PublicKey, PubkeyVersion,
     IsValidator =:= true,
     IsAdmin =:= true, Id].

fields_for_fetch(#chef_client{org_id = OrgId,
                              name = Name}) ->
    [OrgId, Name].

record_fields() ->
    record_info(fields, chef_client).

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
                    {<<"pubkey_version">>, chef_object_base:key_version(PublicKey)},
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
              {<<"orgname">>, OrgName},
              {<<"json_class">>, <<"Chef::ApiClient">>},
              {<<"chef_type">>, <<"client">>}],
    case PublicKey of
        undefined ->
            {Values};
        _ ->
            {[{<<"public_key">>, chef_object_base:extract_public_key(PublicKey)} | Values]}
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
    Client = osc_set_values_from_current_client(chef_object_base:delete_null_public_key(chef_json:decode_body(Bin)), CurrentClient),
    Client1 = chef_object_base:set_default_values(Client, ?DEFAULT_FIELD_VALUES),
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
    Client1 = chef_object_base:set_default_values(Client, ?OC_DEFAULT_FIELD_VALUES),
    {Name, FinalClient} = oc_destination_name(Client1, ReqName),
    valid_name(Name),
    validate_client(FinalClient, Name, oc).

validate_client(Client, Name, osc) ->
    validate_client(Client, osc_client_spec(Name)),
    validate_admin_xor_validator(Client);
validate_client(Client, Name, oc) ->
    validate_client(Client, oc_client_spec(Name)).

validate_client(Client, Spec) ->
    case ej:valid(Spec, Client) of
        ok -> {ok, Client};
        Bad -> throw(Bad)
    end.

% On OSC, clients can either be an admin or a validator, but not both
validate_admin_xor_validator(Client) ->
    IsAdmin = ej:get({<<"admin">>}, Client),
    IsValidator = ej:get({<<"validator">>}, Client),
    case {IsAdmin, IsValidator} of
        {true, true} ->
            chef_object_base:throw_invalid_fun_match(<<"Client can be either an admin or a validator, but not both.">>);
        {_, _} ->
            {ok, Client}
    end.

osc_set_values_from_current_client(Client, not_found) ->
    Client;
osc_set_values_from_current_client(Client, #chef_client{admin = IsAdmin,
                                                        validator = IsValidator,
                                                        public_key = PublicKey}) ->
    C = chef_object_base:set_default_values(Client, [{<<"admin">>, IsAdmin},
                                                     {<<"validator">>, IsValidator}]),
    case chef_object_base:cert_or_key(C) of
        {undefined, _} ->
            chef_object_base:set_public_key(C, PublicKey);
        {_NewPublicKey, _} ->
            C
    end.

oc_set_values_from_current_client(Client, not_found) ->
    Client;
oc_set_values_from_current_client(Client, #chef_client{validator = IsValidator,
                                                       public_key = Cert}) ->
    C = chef_object_base:set_default_values(Client, [{<<"validator">>, IsValidator}]),
    case chef_object_base:cert_or_key(C) of
        {undefined, _} ->
            chef_object_base:set_public_key(C, Cert);
        {_NewPublicKey, _} ->
            C
    end.

osc_client_spec(Name) ->
    {[
      {{opt, <<"name">>}, Name},
      {{opt, <<"admin">>}, boolean},
      {{opt, <<"validator">>}, boolean},
      {{opt, <<"private_key">>}, boolean},
      {{opt, <<"json_class">>}, <<"Chef::ApiClient">>},
      {{opt, <<"chef_type">>}, <<"client">>},
      {{opt, <<"public_key">>}, {fun_match, {fun chef_object_base:valid_public_key/1, string, <<"Public Key must be a valid key.">>}}}
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

cert_or_key(Payload) ->
    %% Some consumers of the API, such as webui, will generate a
    %% JSON { public_key: null } to mean, "do not change it". By
    %% default, null is treated as a defined, and will erase the
    %% public_key in the database. We use value_or_undefined() to
    %% convert all null into undefined.
    Cert = value_or_undefined({<<"certificate">>}, Payload),
    PublicKey = value_or_undefined({<<"public_key">>}, Payload),
    %% Take certificate first, then public_key
    case PublicKey of
        undefined ->
            {Cert, ?CERT_VERSION};
        _ ->
            {PublicKey, ?KEY_VERSION}
    end.

value_or_undefined(Key, Data) ->
  case ej:get(Key, Data) of
    null ->
      undefined;
    Value ->
      Value
  end.
-spec(list(#chef_client{}, chef_object:select_callback()) -> chef_object:select_return()).
list(#chef_client{org_id = OrgId}, CallbackFun) ->
    CallbackFun({list_query(), [OrgId], [name]}).


