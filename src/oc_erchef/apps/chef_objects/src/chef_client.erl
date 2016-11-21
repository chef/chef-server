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

-include_lib("ej/include/ej.hrl").
-include("chef_types.hrl").
-include("chef_osc_defaults.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         authz_id/1,
         assemble_client_ejson/2,
         ejson_for_indexing/2,
         fields_for_fetch/1,
         fields_for_update/1,
         id/1,
         is_indexed/1,
         name/1,
         org_id/1,
         new_record/4,
         parse_binary_json/3,
         record_fields/1,
         set_created/2,
         set_updated/2,
         set_api_version/2,
         type_name/1,
         update_from_ejson/2,
         list/2,
         fields_for_insert/1
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

-mixin([{chef_object_default_callbacks, [ fetch/2, update/2 ]}]).

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

is_indexed(_ObjectRec) ->
    true.

ejson_for_indexing(#chef_client{}, Client) ->
    Client.

update_from_ejson(#chef_client{server_api_version = ?API_v0} = Client, ClientData) ->
    Name = ej:get({<<"name">>}, ClientData),
    IsAdmin = ej:get({<<"admin">>}, ClientData) =:= true,
    IsValidator = ej:get({<<"validator">>}, ClientData) =:= true,
    %% Take public_key first, then certificate
    {Key, Version} = chef_key_base:cert_or_key(ClientData),
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
    end;
update_from_ejson(#chef_client{} = Client, ClientData) ->
    Name = ej:get({<<"name">>}, ClientData),
    IsValidator = ej:get({<<"validator">>}, ClientData) =:= true,
    Client#chef_client{name = Name,
                       validator = IsValidator}.


-spec set_created(#chef_client{}, object_id()) -> #chef_client{}.
set_created(#chef_client{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_client{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

-spec set_updated(#chef_client{}, object_id()) -> #chef_client{}.
set_updated(#chef_client{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_client{updated_at = Now, last_updated_by = ActorId}.

create_query(#chef_client{server_api_version = ?API_v0}) ->
    insert_client_v0;
create_query(_ObjectRec) ->
    insert_client.

update_query(#chef_client{server_api_version = ?API_v0}) ->
    update_client_by_id_v0;
update_query(_ObjectRec) ->
    update_client_by_id.

delete_query(_ObjectRec) ->
    delete_client_by_id.

find_query(#chef_client{server_api_version = ?API_v0}) ->
    find_client_by_orgid_name_v0;
find_query(_ObjectRec) ->
    find_client_by_orgid_name.

list_query(_ObjectRec) ->
    list_clients_for_org.

bulk_get_query(#chef_client{server_api_version = ?API_v0}) ->
    bulk_get_clients_v0;
bulk_get_query(_ObjectRec) ->
    bulk_get_clients.

-spec new_record(api_version(), object_id(), object_id(), ejson_term()) -> #chef_client{}.
new_record(?API_v0 = ApiVersion, OrgId, AuthzId, ClientData) ->
    Name = ej:get({<<"name">>}, ClientData),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    Validator = ej:get({<<"validator">>}, ClientData) =:= true,
    Admin = ej:get({<<"admin">>}, ClientData) =:= true,
    {PublicKey, PubkeyVersion} = chef_key_base:cert_or_key(ClientData),
    #chef_client{server_api_version = ApiVersion,
                 id = Id,
                 authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
                 org_id = OrgId,
                 name = Name,
                 validator = Validator,
                 admin = Admin,
                 public_key = PublicKey,
                 pubkey_version = PubkeyVersion};
new_record(ApiVersion, OrgId, AuthzId, ClientData) ->
    Name = ej:get({<<"name">>}, ClientData),
    % this is only present if we put it there - we filter it out from incoming data.
    Id = case ej:get({<<"id">>}, ClientData) of
            undefined ->
                chef_object_base:make_org_prefix_id(OrgId, Name);
            ClientId ->
                ClientId
         end,
    Validator = ej:get({<<"validator">>}, ClientData) =:= true,
    #chef_client{server_api_version = ApiVersion,
                 id = Id,
                 authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
                 org_id = OrgId,
                 name = Name,
                 validator = Validator}.

fields_for_update(#chef_client{server_api_version = ?API_v0,
                               last_updated_by = LastUpdatedBy,
                               updated_at = UpdatedAt,
                               name = Name,
                               public_key = PublicKey,
                               pubkey_version = PubkeyVersion,
                               admin = IsAdmin,
                               validator = IsValidator,
                               id = Id}) ->
    [LastUpdatedBy, UpdatedAt, Name, PublicKey, PubkeyVersion,
     IsValidator =:= true, IsAdmin =:= true, Id];
fields_for_update(#chef_client{last_updated_by = LastUpdatedBy,
                               updated_at = UpdatedAt,
                               name = Name,
                               validator = IsValidator,
                               id = Id}) ->
    [LastUpdatedBy, UpdatedAt, Name, IsValidator =:= true, Id].

fields_for_fetch(#chef_client{org_id = OrgId,
                              name = Name}) ->
    [OrgId, Name].

fields_for_insert(#chef_client{server_api_version = ?API_v0} = Client) ->
    chef_object_default_callbacks:fields_for_insert(Client);
fields_for_insert(#chef_client{id = Id,
                               authz_id = AuthzId,
                               org_id = OrgId,
                               name = Name,
                               validator = IsValidator,
                               last_updated_by = LastUpdatedBy,
                               created_at = CreatedAt, updated_at = UpdatedAt}) ->
    [Id, AuthzId, OrgId, Name, IsValidator =:= true, LastUpdatedBy, CreatedAt, UpdatedAt].

record_fields(_ApiVersion) ->
    record_info(fields, chef_client).

%% @doc creates the json body for clients
-spec assemble_client_ejson(#chef_client{}, binary()) -> ejson_term().
assemble_client_ejson(#chef_client{server_api_version = ?API_v0, public_key = PublicKey} = Client, OrgName) ->
    Values = base_client_ejson(Client, OrgName),
    case PublicKey of
        undefined ->
            {Values};
        _ ->
            {[{<<"public_key">>, chef_key_base:extract_public_key(PublicKey)} | Values]}
    end;
assemble_client_ejson(Client, OrgName) ->
    { base_client_ejson(Client, OrgName) }.

base_client_ejson(#chef_client{name = Name, validator = Validator}, OrgName) ->
    [{<<"name">>, value_or_default(Name, <<"">>)},
     {<<"clientname">>, value_or_default(Name, <<"">>)},
     {<<"validator">>, Validator =:= true},
     {<<"orgname">>, OrgName},
     {<<"json_class">>, <<"Chef::ApiClient">>},
     {<<"chef_type">>, <<"client">>}].

%% @doc Convert a binary JSON string representing a Chef Client into an
%% EJson-encoded Erlang data structure, using passed defaults
%% @end

-spec parse_binary_json(api_version(), binary(), undefined | #chef_client{}) -> {'ok',ej:json_object()}. % or throw
parse_binary_json(ApiVersion, Bin, undefined) ->
    validate_json(ApiVersion, Bin, undefined, undefined);
parse_binary_json(ApiVersion, Bin, #chef_client{name = ReqName} = CurrentClient) ->
    validate_json(ApiVersion, Bin, ReqName, CurrentClient).

validate_json(ApiVersion, Bin, ReqName, CurrentClient) ->
    ClientEJ0  = set_values_from_current_client(chef_json:decode_body(Bin), CurrentClient),
    ClientEJ1 = chef_object_base:set_default_values(ClientEJ0, default_field_values(ApiVersion)),
    % we use this if it gets populated, so let's not let a caller inject it.
    ClientEJ2 = ej:delete({<<"id">>}, ClientEJ1),
    {Name, ClientEJ3} = destination_name(ClientEJ2, ReqName),
    valid_name(Name),
    % TODO - open bug here: clientname and name are both accepted but we ultimately only
    % validate one of them.
    chef_object_base:validate_ejson(ClientEJ3, client_spec(ApiVersion, Name)),
    validate_key(ApiVersion, ClientEJ3, CurrentClient).

validate_key(?API_v0 , ClientData, _Client) ->
    % Under APIv0, we do not validate public key, which does allow
    % a certificate to be passed in. The certificate is still handled correctly
    % later, for auth purposes and retrieving key data
    {ok, ClientData};
validate_key(_, ClientData, undefined)  ->
    chef_key_base:validate_public_key_fields(opt, ClientData, client, create);
validate_key(_, ClientData, _Client)  ->
    chef_key_base:validate_public_key_fields(opt, ClientData, client, update).

set_values_from_current_client(Client, #chef_client{server_api_version = ?API_v0, validator = IsValidator, public_key = Cert}) ->
    C = chef_object_base:set_default_values(Client, [{<<"validator">>, IsValidator}]),
    case chef_key_base:cert_or_key(C) of
        {undefined, _} ->
            chef_key_base:set_public_key(C, Cert);
        {_NewPublicKey, _} ->
            C
    end;
set_values_from_current_client(Client, #chef_client{validator = IsValidator}) ->
    chef_object_base:set_default_values(Client, [{<<"validator">>, IsValidator}]);
set_values_from_current_client(Client, undefined) ->
    Client.

client_spec(?API_v0, Name) ->
    {[
      {<<"name">>, Name},
      {<<"clientname">>, Name},
      {{opt, <<"validator">>}, boolean},
      {{opt, <<"private_key">>}, boolean}
     ]};
client_spec(_ApiVersion, Name) ->
    {[
      {<<"name">>, Name},
      {<<"clientname">>, Name},
      {{opt, <<"public_key">>}, string},
      {{opt, <<"validator">>}, boolean}
     ]}.


-spec destination_name(ej:json_object(), binary() | undefined) -> {binary(), ej:json_object()} | no_return().
destination_name(Client, ReqName) ->
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
            %% TODO coverage checks show that our pedant tests don't cover this. Is it valid?
            {ReqName, ej:set({<<"name">>}, ej:set({<<"clientname">>}, Client, ReqName), ReqName)};
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

-spec(list(#chef_client{}, chef_object:select_callback()) -> chef_object:select_return()).
list(#chef_client{org_id = OrgId} = Rec, CallbackFun) ->
    CallbackFun({list_query(Rec), [OrgId], [name]}).

default_field_values(?API_v0) ->
     [ {<<"json_class">>, <<"Chef::ApiClient">>},
       {<<"chef_type">>, <<"client">>},
       {<<"validator">>, false},
       {<<"private_key">>, false}
     ];
default_field_values(_) ->
     [ {<<"json_class">>, <<"Chef::ApiClient">>},
       {<<"chef_type">>, <<"client">>},
       {<<"validator">>, false}
     ].

set_api_version(ObjectRec, Version) ->
    ObjectRec#chef_client{server_api_version = Version}.
