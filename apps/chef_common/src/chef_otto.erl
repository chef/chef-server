%
% License:: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
%     http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.0.2
% @end
-module(chef_otto).

-export([
         fetch_user/2,
         fetch_all_users/1,
         fetch_org/2,
         fetch_org_id/2,
         fetch_client/3,
         fetch_user_or_client_cert/3,
         fetch_auth_join/2,
         connect/0,
         connect/2,
         bulk_get/3
         ]).

%% This should include the couchbeam.hrl file and refer to the 'server' record.
-type couchbeam_server() :: {'server',
			     string(),
			     integer(),
			     string(),
			     [] | [{atom(),any()}]}.
-type http_port() :: non_neg_integer().
-type db_key() :: binary() | string().

-include("chef_otto.hrl").

-define(gv(Key, PList), proplists:get_value(Key, PList)).

-spec connect() -> couchbeam_server().
connect() ->
    connect("localhost", 5984).

-spec connect(string(), http_port()) -> couchbeam_server().
connect(Host, Port) ->
    couchbeam:server_connection(Host, Port, "", []).

-spec fetch_user(couchbeam_server(), db_key()) -> [tuple()]
                                                    | {user_not_found,
                                                       not_in_view}
                                                    | {user_not_found,
                                                       {no_doc, binary()}}.
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

-spec fetch_all_users(couchbeam_server()) -> any(). % TODO: Figure out how to tighten the return value.
fetch_all_users(Server) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_user_design, "by_username"},
                                [{include_docs, true}]),
    case couchbeam_view:fetch(View) of
        {ok, {Res}} ->
            ?gv(<<"rows">>, Res);
        Error ->
            Error
    end.



-spec fetch_org_id(couchbeam_server(), binary()) -> binary() | not_found.
fetch_org_id(Server, OrgName) when is_binary(OrgName) ->
    case fetch_org(Server, OrgName) of
        {org_not_found, _} -> not_found;
        Org when is_list(Org) -> ?gv(<<"guid">>, Org)
    end.

-spec fetch_org(couchbeam_server(), binary()) ->
    [tuple()]
        | {org_not_found, not_in_view}
        | {org_not_found, {no_doc, binary()}}.
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

-spec fetch_client(couchbeam_server(), binary() | not_found,
                   binary() | string()) -> [tuple()] | not_found.
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
                % {client_not_found, {no_doc, ClientId}}
                {error, not_found} -> not_found;
                {ok, {ClientDoc}} -> ClientDoc
            end;
        {ok, []} -> not_found
    end;
fetch_client(Server, OrgId, ClientName) when is_list(ClientName), is_binary(OrgId) ->
    fetch_client(Server, OrgId, list_to_binary(ClientName));
fetch_client(_Server, not_found, _ClientName) ->
    not_found.

% FIXME: do we want to distinguish between client not found and org not found?
-spec fetch_user_or_client_cert(couchbeam_server(), db_key(), db_key()) ->
    [tuple()] | not_found.
fetch_user_or_client_cert(Server, OrgName, ClientName)
  when is_binary(OrgName), is_binary(ClientName) ->
    case fetch_user(Server, ClientName) of
        {user_not_found, _} ->
            OrgId = fetch_org_id(Server, OrgName),
            case fetch_client(Server, OrgId, ClientName) of
                not_found -> not_found;
                Client when is_list(Client) ->
                    Cert = ?gv(<<"certificate">>, Client),
                    [{cert, Cert}, {type, client}, {org_guid, OrgId}]
            end;
        UserDoc ->
            [{cert, ?gv(<<"certificate">>, UserDoc)}, {type, user}]
    end;
fetch_user_or_client_cert(Server, OrgName, ClientName)
  when is_list(OrgName), is_list(ClientName) ->
    fetch_user_or_client_cert(Server, list_to_binary(OrgName),
                              list_to_binary(ClientName)).


-spec bulk_get(couchbeam_server(), string(), [binary()]) ->
    [[tuple()]].
bulk_get(Server, DbName, Ids) ->
    {ok, Db} = couchbeam:open_db(Server, DbName, []),
    {ok, View} = couchbeam:all_docs(Db, [{keys, Ids}, {include_docs, true}]),
    DocCollector = fun({Row}, Acc) ->
                           {Doc} = ?gv(<<"doc">>, Row),
                           [Doc|Acc]
                   end,
     couchbeam_view:fold(View, DocCollector).


-spec fetch_auth_join(couchbeam_server(), db_key()) -> [tuple()]
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
