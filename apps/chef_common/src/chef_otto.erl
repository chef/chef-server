%%%
%%% License:: Apache License, Version 2.0
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @copyright Copyright 2011 Opscode, Inc.
%%% @version 0.0.2
%%% @end
-module(chef_otto).

-export([
         fetch_user/2,
         fetch_all_users/1,
         fetch_org/2,
         fetch_org_id/2,
         fetch_client/3,
         fetch_user_or_client_cert/3,
         fetch_auth_join/2,
         fetch_orgs_for_user/2,
         is_user_in_org/3,
         connect/0,
         connect/2,
         bulk_get/3,
         data_bag_exists/3
         ]).

-include_lib("couchbeam/include/couchbeam.hrl").

-type http_port() :: non_neg_integer().
-type db_key() :: binary() | string().

-include("chef_otto.hrl").

-define(gv(Key, PList), proplists:get_value(Key, PList)).

-spec connect() -> couchbeam:server().
connect() ->
    {ok, Host} = application:get_env(chef_common, couchdb_host),
    {ok, Port} = application:get_env(chef_common, couchdb_port),
    connect(Host, Port).

-spec connect(string(), http_port()) -> couchbeam:server().
connect(Host, Port) ->
    couchbeam:server_connection(Host, Port, "", []).

-spec fetch_user(couchbeam:server(), db_key()) -> [tuple()]
                                                    | {user_not_found,
                                                       not_in_view}
                                                    | {user_not_found,
                                                       {no_doc, binary()}}.
%% @doc Return the user document for the username specified by `User'
%%
fetch_user(Server, User) when is_binary(User) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_user_design, "by_username"},
                                [{key, User}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->
            UserId = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, UserId) of
                {error, not_found} -> {user_not_found, {no_doc, UserId}};
                {ok, {UserDoc}}    -> UserDoc
            end;
        {ok, []} ->
            {user_not_found, not_in_view}
    end;
fetch_user(Server, User) when is_list(User) ->
    fetch_user(Server, list_to_binary(User)).


-spec is_user_in_org(couchbeam:server(), db_key(), db_key()) -> boolean() | {error, any()}.
%% @doc Return true if `User' is in `Org' and false otherwise.
is_user_in_org(Server, User, Org) when is_binary(Org) ->
    case fetch_orgs_for_user(Server, User) of
        Orgs when is_list(Orgs) ->
            lists:member(Org, Orgs);
        Error ->
            Error
    end;
is_user_in_org(Server, User, Org) when is_list(Org) ->
    is_user_in_org(Server, User, list_to_binary(Org)).

-spec fetch_orgs_for_user(couchbeam:server(), db_key()) -> [binary()].
%% @doc Return the list of organization names that username `User' is associated with
%%
fetch_orgs_for_user(Server, User) when is_binary(User) ->
    case fetch_user(Server, User) of
        {user_not_found, Why} ->
            {user_not_found, Why};
        UserDoc ->
            UserId = ?gv(<<"_id">>, UserDoc),
        {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
        {ok, View} = couchbeam:view(Db, {?organization_user_design,
                                         "by_organizations_for_user"},
                                    [{key, UserId}, {include_docs, true}]),
        OrgAccountIds = case couchbeam_view:fetch(View) of
                            {ok, {Res}} ->
                                Rows = ?gv(<<"rows">>, Res),
                                lists:map(fun({Row}) ->
                                              {Doc} = ?gv(<<"doc">>, Row),
                                              ?gv(<<"organization">>, Doc)
                                          end, Rows);
                            {error, Why} ->
                                {error, Why}
                        end,
        Orgs = bulk_get(Server, ?user_db, OrgAccountIds),
        [ ?gv(<<"name">>, Org) || Org <- Orgs ]
    end;
fetch_orgs_for_user(Server, User) when is_list(User) ->
    fetch_orgs_for_user(Server, list_to_binary(User)).

-spec fetch_all_users(couchbeam:server()) -> [term()] | {error, term()}.
%% Return a list of all user documents
fetch_all_users(Server) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_user_design, "by_username"},
                                [{include_docs, true}]),
    case couchbeam_view:fetch(View) of
        {ok, {Res}} ->
            ?gv(<<"rows">>, Res);
        {error, Why} ->
            {error, Why}
    end.

-spec fetch_org_id(couchbeam:server(), binary()) -> binary() | not_found.
%% @doc Return the org GUID for a given organization name.
fetch_org_id(Server, OrgName) when is_binary(OrgName) ->
    case fetch_org(Server, OrgName) of
        {org_not_found, _} -> not_found;
        Org when is_list(Org) -> ?gv(<<"guid">>, Org)
    end.

-spec fetch_org(couchbeam:server(), binary()) ->
    [tuple()]
        | {org_not_found, not_in_view}
        | {org_not_found, {no_doc, binary()}}.
%% @doc Return the organization document for a given organization name.
fetch_org(Server, OrgName) when is_binary(OrgName) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_org_design, "by_name"},
                                [{key, OrgName}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->
            OrgDocId = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, OrgDocId) of
                {error, not_found} -> {org_not_found, {no_doc, OrgDocId}};
                {ok, {OrgDoc}} -> OrgDoc
            end;
        {ok, []} ->
            {org_not_found, not_in_view}
    end;
fetch_org(Server, OrgName) when is_list(OrgName) ->
    fetch_org(Server, list_to_binary(OrgName)).

-spec fetch_client(couchbeam:server(), binary() | not_found,
                   binary() | string()) ->
                          [tuple()] | {not_found, client}
                              | {not_found, org}.
fetch_client(Server, OrgId, ClientName)
  when is_binary(ClientName), is_binary(OrgId) ->
    ChefDb = [<<"chef_">>, OrgId],
    {ok, Db} = couchbeam:open_db(Server, ChefDb, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_client_design, "by_clientname"},
                                [{key, ClientName}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->
            ClientId = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, ClientId) of
                {error, not_found} -> {not_found, client};
                {ok, {ClientDoc}} -> ClientDoc
            end;
        {ok, []} -> {not_found, client}
    end;
fetch_client(Server, OrgId, ClientName) when is_list(ClientName), is_binary(OrgId) ->
    fetch_client(Server, OrgId, list_to_binary(ClientName));
fetch_client(_Server, not_found, _ClientName) ->
    {not_found, org}.

-spec fetch_user_or_client_cert(couchbeam:server(), db_key(), db_key()) ->
                                       [{atom(), atom() | binary()}]
                                           | {'not_found', 'client' | 'org'}.
%% @doc Given a name and an org, find either a user or a client.
%% 
%% Looks for a user first, then a client.  The fact that users and
%% clients are in the same name space is a known limitation of the
%% system.  We examine the user/client record and return the
%% certificate containing the actor's public key.  Some legacy records
%% are present in the database that do not have certificates and have
%% only a public key.  For these cases, we return the key tagged with
%% 'key' instead of 'cert'.
fetch_user_or_client_cert(Server, OrgName, ClientName)
  when is_binary(OrgName), is_binary(ClientName) ->
    case fetch_user(Server, ClientName) of
        {user_not_found, _} ->
            OrgId = fetch_org_id(Server, OrgName),
            case fetch_client(Server, OrgId, ClientName) of
                {not_found, What} ->
                    {not_found, What};
                Client when is_list(Client) ->
                    [find_key_data(Client), {type, client}, {org_guid, OrgId}]
            end;
        UserDoc ->
            [find_key_data(UserDoc), {type, user}]
    end;
fetch_user_or_client_cert(Server, OrgName, ClientName)
  when is_list(OrgName), is_list(ClientName) ->
    fetch_user_or_client_cert(Server, list_to_binary(OrgName),
                              list_to_binary(ClientName)).

-spec find_key_data([{binary(), term()}]) ->
                           {key_data, {cert, binary()} | {key, binary()}}.
%% Some of Our user data lacks a certificate and has a public key
%% instead.  We look first for certificate, then for public_key.  If
%% both are not found, we'll crash with some detail of the badly
%% formed user record.
%% 
find_key_data(ClientOrUser) ->
    Cert = ?gv(<<"certificate">>, ClientOrUser),
    Key = ?gv(<<"public_key">>, ClientOrUser),
    KeyData = case {Cert, Key} of
                  {Cert, _} when is_binary(Cert) ->
                      {cert, Cert};
                  {undefined, Key} when is_binary(Key) ->
                      {key, Key};
                  _ ->
                      error_logger:error_report({error, {no_cert_or_key,
                                                         ClientOrUser}}),
                      erlang:error({error, no_cert_or_key})
              end,
    {key_data, KeyData}.

-spec bulk_get(couchbeam:server(), string(), [binary()]) ->
    [[tuple()]] | [].
bulk_get(Server, DbName, Ids) ->
    {ok, Db} = couchbeam:open_db(Server, DbName, []),
    {ok, View} = couchbeam:all_docs(Db, [{keys, Ids}, {include_docs, true}]),
    DocCollector = fun({Row}, Acc) ->
                           case ?gv(<<"doc">>, Row) of
                               {Doc} ->
                                   [Doc|Acc];
                               _ ->
                                   Acc
                           end end,
    lists:reverse(couchbeam_view:fold(View, DocCollector)).


-spec fetch_auth_join(couchbeam:server(), db_key()) -> [tuple()]
                                                    | {not_found, term()}.
fetch_auth_join(Server, ObjectId) when is_binary(ObjectId) ->
    {ok, Db} = couchbeam:open_db(Server, ?auth_join_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_join_design, "by_user_object_id"},
                                [{key, ObjectId}, {include_docs, true}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} -> ?gv(<<"doc">>, Row);
        Why -> {not_found, Why}
    end;
fetch_auth_join(Server, ObjectId) when is_list(ObjectId) ->
    fetch_auth_join(Server, list_to_binary(ObjectId)).


-spec data_bag_exists(couchbeam:server(), binary(), binary() | string()) ->
                             [tuple()] | {not_found, client}
                                 | {not_found, org}.
%% @doc Return true if there is a data bag named `DataBag' in the
%% specified org.
%% 
%% 
data_bag_exists(Server, OrgId, DataBag)
  when is_binary(DataBag), is_binary(OrgId) ->
    ChefDb = [<<"chef_">>, OrgId],
    {ok, Db} = couchbeam:open_db(Server, ChefDb, []),
    {ok, View} = couchbeam:view(Db, {"data_bags", "all_id"},
                                [{key, DataBag}]),
    case couchbeam_view:first(View) of
        {ok, {_Row}} -> true;
        {ok, []} -> false
    end;
data_bag_exists(Server, OrgId, DataBag) when is_list(DataBag), is_binary(OrgId) ->
    data_bag_exists(Server, OrgId, list_to_binary(DataBag)).
