%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @author Marc Paradise <marc@chef.io>
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

-module(chef_key).

-behaviour(chef_object).

-export([authz_id/1,
         is_indexed/0,
         ejson_for_indexing/2,
         update_from_ejson/2,
         set_created/2,
         fields_for_update/1,
         fields_for_fetch/1,
         ejson_from_list/2,
         ejson_from_key/1,
         record_fields/0,
         list/2,
         set_updated/2,
         new_record/3,
         name/1,
         id/1,
         org_id/1,
         type_name/1,
         delete/2,
         parse_binary_json/2
        ]).

%% database named queries
-export([
         create_query/0,
         update_query/0,
         delete_query/0,
         find_query/0,
         bulk_get_query/0,
         list_query/0
        ]).

-include_lib("mixer/include/mixer.hrl").
-mixin([{chef_object,[
                     {default_fetch/2, fetch},
                     {default_update/2, update}
                    ]}]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("../../include/chef_types.hrl").

authz_id(#chef_key{}) ->
    undefined.

is_indexed() ->
    false.

ejson_for_indexing(#chef_key{}, _) ->
    error(not_indexed).

update_from_ejson(_, _) ->
    error(need_to_implement).

set_created(#chef_key{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_key{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

set_updated(#chef_key{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_key{updated_at = Now, last_updated_by = ActorId}.

fields_for_update(#chef_key{}) ->
    %% NOTE: we must do the same date parse here that we do in flatten
    error(need_to_implement).

fields_for_fetch(#chef_key{id = Id, key_name = KeyName}) ->
    [Id, KeyName].

record_fields() ->
    record_info(fields, chef_key).

ejson_from_list(KeysList, URIDecorator) ->
    [ {[{<<"uri">>, URIDecorator(Name)},
        {<<"name">>, Name},
        {<<"expired">>, Expired}]} || [Name, Expired] <- KeysList ].

ejson_from_key(#chef_key{key_name = Name, public_key = PublicKey, expires_at = UnparsedExpirationDate}) ->
    ExpirationDate = case UnparsedExpirationDate of
        ?INFINITY_TIMESTAMP -> <<"infinity">>;
        _ -> list_to_binary(ec_date:format("Y-m-dTH:i:sZ", UnparsedExpirationDate))
    end,
    {[{<<"name">>, Name},
      {<<"public_key">>, PublicKey},
      {<<"expiration_date">>, ExpirationDate}]}.

list(#chef_key{id = Id}, CallbackFun) when is_binary(Id) ->
    CallbackFun({list_query(), [Id], rows}).

find_query() ->
    find_key_by_id_and_name.

new_record(_OrgId, _AuthzId, {Id, KeyData}) ->
    PubKey = ej:get({<<"public_key">>}, KeyData),
    %% return a more useful error if key_version fails
    PubKeyVersion = try chef_object_base:key_version(PubKey) of
        Result -> Result
    catch
        _:_ -> throw(invalid_public_key)
    end,
    Expires = chef_object_base:parse_expiration(ej:get({<<"expiration_date">>}, KeyData)),
    #chef_key{ id = Id, key_name = ej:get({<<"name">>}, KeyData),
               public_key = PubKey, key_version = PubKeyVersion,
               expires_at = Expires}.

name(#chef_key{key_name = KeyName}) ->
    KeyName.

id(#chef_key{id = Id}) ->
    Id.

org_id(#chef_key{}) ->
    undefined.

type_name(#chef_key{}) ->
    key.

list_query() ->
    list_keys_for_actor.

create_query() ->
    insert_key_for_actor.

parse_binary_json(_Bin, #chef_key{} = _ExistingObject) ->
    % TODO according to rfc23, updates of individual fields are supported,
    % so we'll validate each as optional and then verify that at least one is present.
    error(unsupported);
parse_binary_json(Bin, undefined) ->
    EJ = chef_json:decode(Bin),

    % validate public_key field
    chef_object_base:validate_ejson(EJ, chef_object_base:public_key_spec(req)),

    % validate name field
    chef_object_base:validate_ejson(EJ, {[
                                           {<<"name">>, {string_match, chef_regex:regex_for(key_name)}}
                                         ]}),

    % validate expiration_date and make is safe for sqerl
    chef_object_base:validate_and_sanitize_date_field(EJ, <<"expiration_date">>).

update_query() ->
    error(need_to_implement).

delete_query() ->
    delete_key_by_id_and_name.

delete(#chef_key{id = Id, key_name = Name}, CallbackFun) ->
    CallbackFun({delete_query(), [Id, Name]}).

bulk_get_query() ->
    error(unsupported).

