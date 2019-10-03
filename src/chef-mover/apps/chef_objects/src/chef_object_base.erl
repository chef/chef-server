%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
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


%% @doc General utility module for common functions that operate on
%% "Chef Objects", such as nodes, roles, etc.
-module(chef_object_base).

-include("chef_types.hrl").
-include_lib("ej/include/ej.hrl").

-export([
         cert_or_key/1,
         delete_null_public_key/1,
         depsolver_constraints/1,
         extract_public_key/1,
         key_version/1,
         make_guid/0,
         make_org_prefix_id/1,
         make_org_prefix_id/2,
         maybe_stub_authz_id/2,
         normalize_run_list/1,
         parse_constraint/1,
         set_created/2,
         set_key_pair/3,
         set_public_key/2,
         set_updated/2,
         strictly_valid/3,
         sql_date/1,
         throw_invalid_fun_match/1,
         valid_public_key/1,
         set_default_values/2
        ]).

%% In order to fully test things
-ifdef(TEST).
-compile([export_all]).
-endif.

-spec set_created(Object :: chef_object() |
                            #chef_user{} |
                            #chef_sandbox{} |
                            #chef_cookbook_version{},
                  ActorId :: object_id()) -> chef_object() |
                                             #chef_sandbox{} |
                                             #chef_cookbook_version{}.
%% @doc Set the `created_at' and, if appropriate, the `updated_at' and `last_updated_by'
%% fields of a `chef_object()' record type.
set_created(#chef_data_bag{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_data_bag{created_at = Now, updated_at = Now, last_updated_by = ActorId};
set_created(#chef_data_bag_item{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_data_bag_item{created_at = Now, updated_at = Now, last_updated_by = ActorId};
set_created(#chef_environment{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_environment{created_at = Now, updated_at = Now, last_updated_by = ActorId};
set_created(#chef_client{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_client{created_at = Now, updated_at = Now, last_updated_by = ActorId};
set_created(#chef_node{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_node{created_at = Now, updated_at = Now, last_updated_by = ActorId};
set_created(#chef_role{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_role{created_at = Now, updated_at = Now, last_updated_by = ActorId};
set_created(#chef_user{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_user{created_at = Now, updated_at = Now, last_updated_by = ActorId};
set_created(#chef_sandbox{}=Object, _ActorId) ->
    Now = sql_date(now),
    Object#chef_sandbox{created_at = Now};
set_created(#chef_cookbook_version{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_cookbook_version{created_at = Now, updated_at = Now,
                                 last_updated_by = ActorId}.


-spec set_updated(chef_object() |
                  #chef_user{} |
                  #chef_cookbook_version{},
                  object_id()) -> chef_object().
%% @doc Set the `updated_at' and `last_updated_by' fields of a `chef_object()' record type
%% (if appropriate, otherwise no-op that returns the same record provided as argument).
set_updated(#chef_data_bag{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_data_bag{updated_at = Now, last_updated_by = ActorId};
set_updated(#chef_data_bag_item{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_data_bag_item{updated_at = Now, last_updated_by = ActorId};
set_updated(#chef_environment{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_environment{updated_at = Now, last_updated_by = ActorId};
set_updated(#chef_client{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_client{updated_at = Now, last_updated_by = ActorId};
set_updated(#chef_node{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_node{updated_at = Now, last_updated_by = ActorId};
set_updated(#chef_role{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_role{updated_at = Now, last_updated_by = ActorId};
set_updated(#chef_user{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_user{updated_at = Now, last_updated_by = ActorId};
set_updated(#chef_cookbook_version{} = Object, ActorId) ->
    Now = sql_date(now),
    Object#chef_cookbook_version{updated_at = Now, last_updated_by = ActorId}.


-spec sql_date(now | {non_neg_integer(), non_neg_integer(), non_neg_integer()}) -> binary().
%% @doc Convert an Erlang timestamp (see `os:timestamp/0') to DATETIME friendly format.
sql_date(now) ->
    sql_date(os:timestamp());
sql_date({_,_,_} = TS) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
    iolist_to_binary(io_lib:format("~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [Year, Month, Day, Hour, Minute, Second])).

%% @doc Generate a list of depsolver constraints from either an Environment record or from
%% the dependencies JSON from a cookbook (passing in an EJson hash is also possible, though
%% more as an implementation detail).
%%
%% If given a JSON binary, it is assumed that the string represents ONLY a dependencies /
%% constraint hash (i.e., it's just a mapping of name to constraint string).  It is assumed
%% that the given input (either JSON or Environment) have been previously validated.
-spec depsolver_constraints(#chef_environment{}
                            | binary()   % JSON string
                            | {[{Name::binary(), ConstraintString::binary()}]}) %% EJson hash
                           -> [ chef_depsolver:constraint() ].
depsolver_constraints(#chef_environment{serialized_object=SerializedObject}) ->
    EJson = chef_db_compression:decompress_and_decode(SerializedObject),
    %% The "cookbook_versions" key is required for Environments, and will always be present
    Constraints = ej:get({<<"cookbook_versions">>}, EJson),
    depsolver_constraints(Constraints);
depsolver_constraints(JSON) when is_binary(JSON) ->
    Constraints = chef_json:decode(JSON),
    depsolver_constraints(Constraints);
depsolver_constraints({Constraints}) when is_list(Constraints) ->
    [ process_constraint_for_depsolver(Constraint)
      || Constraint <- Constraints ].

%% @doc Convert a cookbook name / version constraint string pair into a valid depsolver
%% constraint.  Mainly ensures the types of the various components are correct, as depsolver
%% works mainly with strings and atoms, instead of binaries.
-spec process_constraint_for_depsolver({binary(), binary()}) -> {binary(), binary(), '<' | '<=' | '=' | '>' | '>=' | '~>'}.
process_constraint_for_depsolver({Name, ConstraintString}) ->
    {Comparator, Version} = parse_constraint(ConstraintString),
    {chef_cookbook_version:base_cookbook_name(Name), Version, Comparator}.

%% @doc Given a version constraint string (e.g., `<<">= 1.5.0">>'), extract the comparison
%% operator and version and present them as a paired tuple.
-spec parse_constraint(Constraint :: binary()) -> {Operator :: comparison_operator(), Version :: binary()} | error.
parse_constraint(<<"< ", Version/binary>>) ->
    {'<', Version};
parse_constraint(<<"> ", Version/binary>>) ->
    {'>', Version};
parse_constraint(<<"<= ", Version/binary>>) ->
    {'<=', Version};
parse_constraint(<<">= ", Version/binary>>) ->
    {'>=', Version};
parse_constraint(<<"~> ", Version/binary>>) ->
    {'~>', Version};
parse_constraint(<<"= ", Version/binary>>) ->
    {'=', Version};
parse_constraint(<<Version/binary>>) ->
    %% no constraint, implied =
    {'=', Version};
parse_constraint(_) ->
    error.

%% @doc
%% Create a GUID with an org-specifc prefix for nameless objects.
%%
%% See make_org_prefix_id/2 for further details.
-spec make_org_prefix_id(object_id()) -> object_id().
make_org_prefix_id(OrgId) ->
    %% The GUIDs we generate incorporate an object's name.  Most times, the objects we'll
    %% want to create GUIDs for will have names of their own; this is not the case for
    %% sandboxes, at least, which are only identified by a GUID.  To keep things simple,
    %% we'll just generate a random "name" for them, and then pass this along to the
    %% "normal" GUID creation machinery.
    %%
    %% It will still be prefixed with an org-specific prefix, though, just like our other
    %% GUIDs.
    FakeName = crypto:strong_rand_bytes(32),  %% Picked 32 for the hell of it
    make_org_prefix_id(OrgId, FakeName).

-spec make_org_prefix_id(<<_:256>>, string()|binary()) -> <<_:256>>.
%% @doc Create a guid with org-specific prefix
%%
%% We use the last 48 bits of the org guid as the prefix for the object guid.  The remainder
%% of the object guid is the first 80 bits of the MD5 hash of org name, object name, and six
%% random bytes.
%%
%% We could also add in object type, but the random bytes should take care of same name
%% different type situations just as well and also solves race condition issues with
%% multiple requests for the same name.
%%
make_org_prefix_id(OrgId, Name) ->
    %% assume couchdb guid where trailing part has uniqueness
    <<_:20/binary, OrgSuffix:12/binary>> = OrgId,
    Bin = iolist_to_binary([OrgId, Name, crypto:strong_rand_bytes(6)]),
    <<ObjectPart:80, _/binary>> = crypto:md5(Bin),
    iolist_to_binary(io_lib:format("~s~20.16.0b", [OrgSuffix, ObjectPart])).

make_guid() ->
    Raw = crypto:strong_rand_bytes(16),
    <<Guid:128>> = Raw,
    iolist_to_binary(io_lib:format("~32.16.0b", [Guid])). %% 128 bits/16 bytes/32 hex chars

%% If the incoming authz id is the atom 'unset', use the object's id as ersatz authz id.
maybe_stub_authz_id(unset, ObjectId) ->
    ObjectId;
maybe_stub_authz_id(AuthzId, _ObjectId) ->
    AuthzId.

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

extract_public_key(Data) ->
    case key_version(Data) of
        ?KEY_VERSION ->
            Data;
        ?CERT_VERSION ->
            chef_authn:extract_pem_encoded_public_key(Data)
    end.

%% Hack to get null public_key accepted as undefined
-spec delete_null_public_key(json_object()) -> json_object().
delete_null_public_key(Ejson) ->
    case ej:get({<<"public_key">>}, Ejson) of
        null ->
            ej:delete({<<"public_key">>}, Ejson);
        _ ->
            Ejson
    end.


%% @doc Returns a normalized version of `RunList`.  All implicitly-declared recipes (e.g.,
%% "foo::bar") are made explicit (e.g., "recipe[foo::bar]").  Already explicit recipes and
%% roles (which are always explicit) are unchanged.
%%
%% Exact duplicates are removed following the normalization process.  Semantic duplicates
%% (such as "recipe[foo]" and "recipe[foo::default]") are preserved.
-spec normalize_run_list(RunList :: [binary()]) -> [binary()].
normalize_run_list(RunList) ->
    deduplicate_run_list([normalize_item(Item) || Item <- RunList]).

%% @doc Explicitly qualify a run list item.  Items already marked as "recipe[...]" or
%% "role[...]" remain unchanged, while all other input is taken to be a recipe, and is
%% wrapped as "recipe[ITEM]".
%%
%% It is assumed that only legal run list items will be input to this function (i.e., the
%% run lists they are part of have already been validated).
%%
%% NOTE: About the spec here, `<<_:40,_:_*8>>` is the notation for a binary string that is
%% at least 5 bytes long (8 bits * 5 = 40).  This comes from Dialyzer inferring that the
%% smallest possible return value for this function would be <<"role[">>, which (while true)
%% is rather unhelpful.  We can't specify a return value of `binary()`, however, because
%% that is an underspecification, which conflicts with our Dialyzer setting of -Wunderspecs;
%% we want to keep that because it's a generally useful setting... just not when dealing
%% with Erlang's lack of a true string data type :(
-spec normalize_item(binary()) -> <<_:40,_:_*8>>.
normalize_item(<<"role[",_Item/binary>>=Role) ->
    Role;
normalize_item(<<"recipe[",_Item/binary>>=Recipe) ->
    Recipe;
normalize_item(Recipe) when is_binary(Recipe) ->
    <<"recipe[", Recipe/binary, "]">>.

%% @doc Removes duplicates from a run list, preserving order.  Intended for use with
%% already-normalized run lists.
%% @end
%%
%% NOTE: The spec for this function is the way it is for the same reasons as
%% `normalize_item/1`.  See the documentation for that function for the gory details.
%%
%% TODO: This would be a good candidate for a 'chef_common' module function; it's copied
%% from chef_wm_depsolver:remove_dups/1.
-spec deduplicate_run_list([<<_:40,_:_*8>>]) -> list().
deduplicate_run_list(L) ->
    WithIdx = lists:zip(L, lists:seq(1, length(L))),
    [ Elt || {Elt, _} <- lists:ukeysort(2, lists:ukeysort(1, WithIdx)) ].

value_or_undefined(Key, Data) ->
  case ej:get(Key, Data) of
    null ->
      undefined;
    Value ->
      Value
  end.

%% These type specs are taken from ej. They are not in an exportable form
%% They are reproduced here to make dialyzer work for strictly_valid()
%% Perhaps, that means strictly_valid() should be moved into ej
-type ej_string_match() :: {'string_match', {re:mp(), _}}.
-type ej_fun_match() :: {fun_match, {fun((json_term()) -> ok | error),
                                        ej_json_type_name(), _}}.
-type ej_array_map() :: {array_map, ej_json_val_spec()}.

-type ej_object_map() :: {object_map, {{keys, ej_json_val_spec()},
                                       {values, ej_json_val_spec()}}}.

-type ej_json_spec() :: {[ej_json_spec_rule()]} | ej_object_map().
-type ej_json_spec_rule() :: {ej_json_key_spec(), ej_json_val_spec()}.
-type ej_json_key_spec() :: binary() | {opt, binary()}.
-type ej_json_val_spec() :: binary()             |
                            ej_json_type_name()  |
                            ej_string_match()    |
                            ej_fun_match()       |
                            ej_array_map()       |
                            ej_object_map()      |
                            {[ej_json_val_spec()]}.

%% Call this instead of ej:valid() if you want to validate specs
%% and check for invalid top-level keys. This will not check for
%% invalid keys beyond the top-level.
-spec strictly_valid(Constraints :: ej_json_spec(), ValidKeys :: [binary()],  Ejson :: json_object()) -> ok | #ej_invalid{}.
strictly_valid(Constraints, ValidKeys, Ejson) ->
    case allowed_keys(ValidKeys, Ejson) of
        ok ->
            ej:valid(Constraints, Ejson)
        % allowed_keys will throw, not return
    end.

allowed_keys(_ValidKeys, []) ->
    ok;
allowed_keys(ValidKeys, {List}) when is_list(List) ->
    allowed_keys(ValidKeys, List);
allowed_keys(ValidKeys, [{Item, _}|Rest]) ->
    case lists:member(Item, ValidKeys) of
        true -> allowed_keys(ValidKeys, Rest);
        _ ->
            throw({invalid_key, Item})
    end.

%% @doc Add public and private key data to `UserEjson'. This function infers
%% the key type and puts the public key data in iether a `certificate' or
%% `public_key' field.
%% If a private key is defined, then the private key will be placed in the `private_key'
%% field.
-spec set_key_pair(ej:json_object(), {public_key, binary()}, {private_key, binary()}) -> ej:json_object().
set_key_pair(UserEjson, {public_key, PublicKey}, {private_key, PrivateKey}) ->
    UserEjson1 = set_public_key(UserEjson, PublicKey),
    case PrivateKey of
        undefined ->
            UserEjson1;
        _ ->
            ej:set({<<"private_key">>}, UserEjson1, PrivateKey)
    end.

%% @doc Sets either the `certificate' or `public_key' field of
%% `UserEjson' depending on the value of `PublicKey'.
-spec set_public_key(ej:json_object(), binary()) -> ej:json_object().
set_public_key(UserEjson, PublicKey) ->
  case key_version(PublicKey) of
        ?KEY_VERSION ->
          UserEjson1 = ej:set({<<"public_key">>}, UserEjson, PublicKey),
          ej:delete({<<"certificate">>}, UserEjson1);
        ?CERT_VERSION ->
          UserEjson1 = ej:set({<<"certificate">>}, UserEjson, PublicKey),
          ej:delete({<<"public_key">>}, UserEjson1)
    end.


%% Determine the "pubkey_version" of a key or certificate in PEM
%% format. Certificates are version 1. Public keys in either PKCS1 or
%% SPKI format are version 0. The PKCS1 format is deprecated, but
%% supported for read. We will only generate certs or SPKI packaged
%% keys.
-spec key_version(<<_:64,_:_*8>>) -> 0 | 1.
key_version(<<"-----BEGIN CERTIFICATE", _Bin/binary>>) ->
    %% cert
    ?CERT_VERSION;
key_version(<<"-----BEGIN PUBLIC KEY", _Bin/binary>>) ->
    %% SPKI
    ?KEY_VERSION;
key_version(<<"-----BEGIN RSA PUBLIC KEY", _Bin/binary>>) ->
    %% PKCS1
    ?KEY_VERSION.

%% OSC will only accept public keys. It will not accept certificates
-spec has_public_key_header(<<_:64,_:_*8>>) -> true | false.
has_public_key_header(<<"-----BEGIN PUBLIC KEY", _/binary>>) ->
    true;
has_public_key_header(<<"-----BEGIN RSA PUBLIC KEY", _/binary>>) ->
    true;
has_public_key_header(_) ->
    false.

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

%% @doc throws an ej_invalid for fun_match. Useful for bespoke validation functions
%% that needs to return an error message back to the API client.
-spec throw_invalid_fun_match(binary()) -> none().
throw_invalid_fun_match(Message) ->
    throw(#ej_invalid{type = fun_match, msg = Message}).



%% Walks through ejson term and set default values
%% Factored out from monkey copied code in most objects
-spec set_default_values( ejson_term(), list({binary(), any()}) ) -> ejson_term().
set_default_values(Object, Defaults) ->
    lists:foldl(fun({Key, Default}, Current) ->
                        case ej:get({Key}, Current) of
                            undefined ->
                                ej:set({Key}, Current, Default);
                            _ -> Current
                        end
                end,
                Object,
                Defaults).
