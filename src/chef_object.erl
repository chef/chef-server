%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
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


%% @doc General utility module for common functions that operate on
%% "Chef Objects", such as nodes, roles, etc.
-module(chef_object).

-include("chef_types.hrl").
-include_lib("ej/include/ej.hrl").

-export([cert_or_key/1,
         delete_null_public_key/1,
         depsolver_constraints/1,
         ejson_for_indexing/2,
         id/1,
         key_version/1,
         make_org_prefix_id/1,
         make_org_prefix_id/2,
         name/1,
         new_record/4,
         normalize_run_list/1,
         parse_constraint/1,
         set_created/2,
         set_key_pair/3,
         set_public_key/2,
         set_updated/2,
         strictly_valid/3,
         throw_invalid_fun_match/1,
         type_name/1,
         update_from_ejson/2,
         valid_public_key/1
        ]).

%% In order to fully test things
-ifdef(TEST).
-compile([export_all]).
-endif.

%% @doc Create a new Chef object record of type specified by `RecType'. This function will
%% generate a unique id for the object using `make_org_prefix_id/2'. If `AuthzId' is the
%% atom 'unset', then the object's generated id will be used as a placeholder authorization
%% identifier.
-spec new_record(RecType :: chef_object_name() | chef_cookbook_version,
                 OrgId :: object_id(),
                 AuthzId :: object_id() | unset,
                 ObjectEjson :: ejson_term() |
                                binary() |
                                {binary(), ejson_term()} |
                                {ejson_term(), _}) ->
                        chef_object() | #chef_cookbook_version{}.
new_record(chef_environment, OrgId, AuthzId, EnvData) ->
    Name = ej:get({<<"name">>}, EnvData),
    Id = make_org_prefix_id(OrgId, Name),
    Data = chef_db_compression:compress(chef_environment, chef_json:encode(EnvData)),
    #chef_environment{id = Id,
                      authz_id = maybe_stub_authz_id(AuthzId, Id),
                      org_id = OrgId,
                      name = Name,
                      serialized_object = Data};
new_record(chef_client, OrgId, AuthzId, ClientData) ->
    Name = ej:get({<<"name">>}, ClientData),
    Id = make_org_prefix_id(OrgId, Name),
    Validator = ej:get({<<"validator">>}, ClientData) =:= true,
    Admin = ej:get({<<"admin">>}, ClientData) =:= true,
    {PublicKey, PubkeyVersion} = cert_or_key(ClientData),
    #chef_client{id = Id,
                 authz_id = maybe_stub_authz_id(AuthzId, Id),
                 org_id = OrgId,
                 name = Name,
                 validator = Validator,
                 admin = Admin,
                 public_key = PublicKey,
                 pubkey_version = PubkeyVersion};
new_record(chef_data_bag, OrgId, AuthzId, Name) ->
    Id = make_org_prefix_id(OrgId, Name),
    #chef_data_bag{id = Id,
                   authz_id = maybe_stub_authz_id(AuthzId, Id),
                   org_id = OrgId,
                   name = Name};
new_record(chef_data_bag_item, OrgId, _AuthzId, {BagName, ItemData}) ->
    ItemName = ej:get({<<"id">>}, ItemData),
    Id = make_org_prefix_id(OrgId, <<BagName/binary, ItemName/binary>>),
    Data = chef_db_compression:compress(chef_data_bag_item, chef_json:encode(ItemData)),
    #chef_data_bag_item{id = Id,
                        org_id = OrgId,
                        data_bag_name = BagName,
                        item_name = ItemName,
                        serialized_object = Data
                       };
new_record(chef_node, OrgId, AuthzId, NodeData) ->
    Name = ej:get({<<"name">>}, NodeData),
    Environment = ej:get({<<"chef_environment">>}, NodeData),
    Id = make_org_prefix_id(OrgId, Name),
    Data = chef_db_compression:compress(chef_node, chef_json:encode(NodeData)),
    #chef_node{id = Id,
               authz_id = maybe_stub_authz_id(AuthzId, Id),
               org_id = OrgId,
               name = Name,
               environment = Environment,
               serialized_object = Data};
new_record(chef_user, OrgId, AuthzId, {UserData, {HashPass, Salt, HashType}}) ->
    %% This only works for Open Source Users currently
    Name = ej:get({<<"name">>}, UserData),
    Id = make_org_prefix_id(OrgId, Name),
    Email = value_or_null({<<"email">>}, UserData),
    Admin = ej:get({<<"admin">>}, UserData) =:= true,
    {PublicKey, _PubkeyVersion} = cert_or_key(UserData),
    #chef_user{id = Id,
               authz_id = maybe_stub_authz_id(AuthzId, Id),
               username = Name,
               email = Email,
               public_key = PublicKey,
               hashed_password = HashPass,
               salt = Salt,
               hash_type = HashType,
               external_authentication_uid = null, %% Not used in open source user
               recovery_authentication_enabled = false, %% Not used in open source user
               admin = Admin
    };
new_record(chef_role, OrgId, AuthzId, RoleData) ->
    Name = ej:get({<<"name">>}, RoleData),
    Id = make_org_prefix_id(OrgId, Name),
    Data = chef_db_compression:compress(chef_role, chef_json:encode(RoleData)),
    #chef_role{id = Id,
               authz_id = maybe_stub_authz_id(AuthzId, Id),
               org_id = OrgId,
               name = Name,
               serialized_object = Data};
new_record(chef_cookbook_version, OrgId, AuthzId, CBVData) ->
    %% name for a cookbook_version is actually cb_name-cb_version which is good for ID
    %% creation
    Name = ej:get({<<"name">>}, CBVData),
    Id = make_org_prefix_id(OrgId, Name),
    {Major, Minor, Patch} = chef_cookbook:parse_version(ej:get({<<"metadata">>, <<"version">>},
                                                        CBVData)),

    Metadata0 = ej:get({<<"metadata">>}, CBVData),

    MAttributes = compress_maybe(ej:get({<<"attributes">>}, Metadata0, {[]}),
                                 cookbook_meta_attributes),

    %% Do not compress the deps!
    Deps = chef_json:encode(ej:get({<<"dependencies">>}, Metadata0, {[]})),

    LongDesc = compress_maybe(ej:get({<<"long_description">>}, Metadata0, <<"">>),
                              cookbook_long_desc),

    Metadata = compress_maybe(lists:foldl(fun(Key, MD) ->
                                                  ej:delete({Key}, MD)
                                          end, Metadata0, [<<"attributes">>,
                                                           <<"dependencies">>,
                                                           <<"long_description">>]),
                              cookbook_metadata),

    Data = compress_maybe(ej:delete({<<"metadata">>}, CBVData),
                          chef_cookbook_version),
    #chef_cookbook_version{id = Id,
                           authz_id = maybe_stub_authz_id(AuthzId, Id),
                           org_id = OrgId,
                           name = ej:get({<<"cookbook_name">>}, CBVData),
                           major = Major,
                           minor = Minor,
                           patch = Patch,
                           frozen = ej:get({<<"frozen?">>}, CBVData, false),
                           meta_attributes = MAttributes,
                           meta_deps = Deps,
                           meta_long_desc = LongDesc,
                           metadata = Metadata,
                           checksums = chef_cookbook:extract_checksums(CBVData),
                           serialized_object = Data}.

compress_maybe(Data, cookbook_long_desc) ->
    chef_db_compression:compress(cookbook_long_desc, Data);
compress_maybe(Data, Type) ->
    chef_db_compression:compress(Type, chef_json:encode(Data)).

-spec ejson_for_indexing(chef_indexable_object() | #chef_data_bag_item{},
                         ejson_term() | {ejson_term(), _}) -> ejson_term().
%% @doc Return EJSON terms appropriate for sending to opscode-expander for
%% indexing. Although the EJSON data is embedded in the ChefRecord, it is stored in a
%% possibly compressed form. To avoid double work, we pass both the Chef object record
%% (mostly for dispatch, but also used in the case of data_bag_item to obtain data_bag name)
%% and the object EJSON as inputs. The returned EJSON is only suitable for sending to the
%% queue for indexing.
ejson_for_indexing(#chef_data_bag_item{data_bag_name = BagName,
                                       item_name = ItemName}, Item) ->
    %% See Chef::DataBagItem#to_hash
    %% We basically set data_bag and chef_type key against the original data bag item.
    ItemName = ej:get({<<"id">>}, Item),
    ej:set({<<"data_bag">>}, ej:set({<<"chef_type">>}, Item, <<"data_bag_item">>), BagName);
ejson_for_indexing(#chef_data_bag{}, <<Name/binary>>) ->
    %% We do not currently index data_bag objects in solr. This is here to allow us to take
    %% advantage of shared code paths which expect a valid convert function to get indexable
    %% data. The return value is not used, but is nominally valid:
    {[{<<"name">>, Name},
      {<<"chef_type">>, <<"data_bag">>},
      {<<"json_class">>, <<"Chef::DataBag">>}]};
ejson_for_indexing(#chef_cookbook_version{}, _CBVersion) ->
    %% FIXME: cleanup how we handle non-indexed objects
    %% cookbook_versions don't get indexed.
    {[]};
ejson_for_indexing(#chef_user{}, _) ->
    %% FIXME: we don't index users, so this is a dummy value
    {[]};
ejson_for_indexing(#chef_node{name = Name, environment = Environment}, Node) ->
    Defaults = ej:get({<<"default">>}, Node, ?EMPTY_EJSON_HASH),
    Normal = ej:get({<<"normal">>}, Node, ?EMPTY_EJSON_HASH),
    Override = ej:get({<<"override">>}, Node, ?EMPTY_EJSON_HASH),
    %% automatic may not always be present
    Automatic = ej:get({<<"automatic">>}, Node, ?EMPTY_EJSON_HASH),
    DefaultNormal = chef_deep_merge:merge(Defaults, Normal),
    DefaultNormalOverride = chef_deep_merge:merge(DefaultNormal, Override),
    {Merged} = chef_deep_merge:merge(DefaultNormalOverride, Automatic),
    RunList = ej:get({<<"run_list">>}, Node, []),
    %% We transform to a dict to ensure we override the top-level keys
    %% with the appropriate values and don't introduce any duplicate
    %% keys
    NodeDict = dict:from_list(Merged),
    TopLevelDict = dict:from_list([{<<"name">>, Name},
                                   {<<"chef_type">>, <<"node">>},
                                   %% FIXME: nodes may have environment in the db, but not in JSON
                                   %% or not set at all (pre-environments nodes).
                                   {<<"chef_environment">>, Environment},
                                   {<<"recipe">>, extract_recipes(RunList)},
                                   {<<"role">>, extract_roles(RunList)},
                                   {<<"run_list">>, RunList}]),
    NodeDict1 = dict:merge(fun(_Key, TopVal, _AttrVal) ->
                                   TopVal
                           end, TopLevelDict, NodeDict),
    {dict:to_list(NodeDict1)};
ejson_for_indexing(#chef_role{}, Role) ->
    EnvironmentRunLists0 = ej:get({<<"env_run_lists">>}, Role, ?EMPTY_EJSON_HASH),
    EnvironmentRunLists = ej:delete({<<"_default">>}, EnvironmentRunLists0),
    ej:set({<<"env_run_lists">>}, Role, EnvironmentRunLists);
ejson_for_indexing(#chef_environment{}, Environment) ->
    Environment;
ejson_for_indexing(#chef_client{}, Client) ->
    Client.

extract_recipes(RunList) ->
    [ binary:part(Item, {0, byte_size(Item) - 1})
      || <<"recipe[", Item/binary>> <- RunList ].

extract_roles(RunList) ->
    [ binary:part(Item, {0, byte_size(Item) - 1})
      || <<"role[", Item/binary>> <- RunList ].

-spec update_from_ejson(chef_object() | #chef_cookbook_version{},
                        ejson_term()) -> chef_object().
%% @doc Return a new `chef_object()' record updated according to the specified EJSON
%% terms. Data normalization on the EJSON should occur before making this call. Fields in
%% the EJSON that exist in the record are updated. The serialized_object record field is
%% updated with appropriately compressed data. No sanity checks are made; you can "rename"
%% an object record with this function.
update_from_ejson(#chef_environment{} = Env, EnvData) ->
    Name = ej:get({<<"name">>}, EnvData),
    Data = chef_db_compression:compress(chef_environment, chef_json:encode(EnvData)),
    Env#chef_environment{name = Name, serialized_object = Data};
update_from_ejson(#chef_client{} = Client, ClientData) ->
    Name = ej:get({<<"name">>}, ClientData),
    IsAdmin = ej:get({<<"admin">>}, ClientData) =:= true,
    IsValidator = ej:get({<<"validator">>}, ClientData) =:= true,
    %% Take certificate first, then public_key
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
    end;
update_from_ejson(#chef_data_bag{} = DataBag, DataBagData) ->
    %% here for completeness
    Name = ej:get({<<"name">>}, DataBagData),
    DataBag#chef_data_bag{name = Name};
update_from_ejson(#chef_data_bag_item{} = DataBagItem, DataBagItemData) ->
    Name = ej:get({<<"id">>}, DataBagItemData),
    DataBagItemJson = chef_json:encode(DataBagItemData),
    Data = chef_db_compression:compress(chef_data_bag_item, DataBagItemJson),
    DataBagItem#chef_data_bag_item{item_name = Name, serialized_object = Data};
update_from_ejson(#chef_node{} = Node, NodeJson) ->
    Name = ej:get({<<"name">>}, NodeJson),
    %% We expect that the insert_autofill_fields call will insert default when necessary
    Environment = ej:get({<<"chef_environment">>}, NodeJson),
    Data = chef_db_compression:compress(chef_node, chef_json:encode(NodeJson)),
    Node#chef_node{name = Name, environment = Environment, serialized_object = Data};
update_from_ejson(#chef_role{} = Role, RoleData) ->
    Name = ej:get({<<"name">>}, RoleData),
    Data = chef_db_compression:compress(chef_role, chef_json:encode(RoleData)),
    Role#chef_role{name = Name, serialized_object = Data};
update_from_ejson(#chef_cookbook_version{org_id = OrgId,
                                         authz_id = AuthzId,
                                         frozen = FrozenOrig} = CookbookVersion,
                  CookbookVersionData) ->
    UpdatedVersion = new_record(chef_cookbook_version, OrgId, AuthzId, CookbookVersionData),
    %% frozen is immutable once it is set to true
    Frozen = FrozenOrig =:= true orelse UpdatedVersion#chef_cookbook_version.frozen,
    CookbookVersion#chef_cookbook_version{frozen            = Frozen,
                                          meta_attributes   = UpdatedVersion#chef_cookbook_version.meta_attributes,
                                          meta_deps         = UpdatedVersion#chef_cookbook_version.meta_deps,
                                          meta_long_desc    = UpdatedVersion#chef_cookbook_version.meta_long_desc,
                                          metadata          = UpdatedVersion#chef_cookbook_version.metadata,
                                          checksums         = UpdatedVersion#chef_cookbook_version.checksums,
                                          serialized_object = UpdatedVersion#chef_cookbook_version.serialized_object}.


-spec id(chef_object() | #chef_user{}) -> object_id().
%% @doc Return the `id' field from a `chef_object()' record type.
id(#chef_node{id = Id}) ->
    Id;
id(#chef_role{id = Id}) ->
    Id;
id(#chef_user{id = Id}) ->
    Id;
id(#chef_environment{id = Id}) ->
    Id;
id(#chef_client{id = Id}) ->
    Id;
id(#chef_data_bag{id = Id}) ->
    Id;
id(#chef_data_bag_item{id = Id}) ->
    Id;
id(#chef_cookbook_version{id = Id}) ->
    Id.

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

-spec name(chef_object() | #chef_user{}) -> binary() | {binary(), binary()}.
%% @doc Return the `name' field from a `chef_object()' record type. For `data_bag_items' the
%% return value is a tuple of `{BagName, ItemName}',
%% for a chef_user, the username is returned.
name(#chef_node{name = Name}) ->
    Name;
name(#chef_role{name = Name}) ->
    Name;
name(#chef_user{username = Name}) ->
    Name;
name(#chef_environment{name = Name}) ->
    Name;
name(#chef_client{name = Name}) ->
    Name;
name(#chef_data_bag{name = Name}) ->
    Name;
name(#chef_data_bag_item{data_bag_name = BagName, item_name = ItemName}) ->
    {BagName, ItemName};
name(#chef_cookbook_version{name = Name}) ->
    Name.

-spec type_name(chef_object() | #chef_user{}) -> chef_type() | cookbook_version | user.
%% @doc Return the common type name of a `chef_object()' record. For example, the common
%% type name of a `chef_node{}' record is `node'.
type_name(#chef_data_bag{}) ->
    data_bag;
type_name(#chef_data_bag_item{}) ->
    data_bag_item;
type_name(#chef_environment{}) ->
    environment;
type_name(#chef_client{}) ->
    client;
type_name(#chef_node{}) ->
    node;
type_name(#chef_role{}) ->
    role;
type_name(#chef_user{}) ->
    user;
type_name(#chef_cookbook_version{}) ->
    cookbook_version.


%% TODO: Why don't we also have a 'delete_fun/1' function?


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
                           -> [ depsolver:constraint() ].
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
-spec process_constraint_for_depsolver({_, binary()}) -> {_, binary(), '<' | '<=' | '=' | '>' | '>=' | '~>'}.
process_constraint_for_depsolver({Name, ConstraintString}) ->
    {Comparator, Version} = parse_constraint(ConstraintString),
    {Name, Version, Comparator}.

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
    FakeName = crypto:rand_bytes(32),  %% Picked 32 for the hell of it
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
    Bin = iolist_to_binary([OrgId, Name, crypto:rand_bytes(6)]),
    <<ObjectPart:80, _/binary>> = crypto:md5(Bin),
    iolist_to_binary(io_lib:format("~s~20.16.0b", [OrgSuffix, ObjectPart])).

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

value_or_null(Key, Data) ->
 Value = ej:get(Key, Data),
  case Value of
    undefined ->
      null;
    _ ->
      Value
  end.

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
            ej:set({<<"public_key">>}, UserEjson, PublicKey);
        ?CERT_VERSION ->
            ej:set({<<"certificate">>}, UserEjson, PublicKey)
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
