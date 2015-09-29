%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% Copyright 2014 Chef Software, Inc. All Rights Reserved.
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

%% @doc Chef object flatten/expand and POSTing to Solr
%%
%% This module implements Chef object flatten/expand using the same
%% algorithm as `chef-expander' and handles POSTing updates (both adds
%% and deletes) to Solr.
%%
%% @end
-module(chef_index_expand).

-export([make_command/5,
         post_multi/2,
         post_single/2,
         post_to_solr/1,
         send_item/1,
         send_delete/1,
         doc_for_index/4,
         doc_for_delete/3]).

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-define(SEP, <<"_">>).
-define(FIELD(Name, Value), [<<"<field name=\"">>, Name, <<"\">">>, Value, <<"</field>">>]).
-define(XML_HEADER, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>).

-define(ADD_S, <<"<add>">>).
-define(ADD_E, <<"</add>">>).
-define(UPDATE_S, <<"<update>">>).
-define(UPDATE_E, <<"</update>">>).
-define(DOC_S, <<"<doc>">>).
-define(DOC_E, <<"</doc>">>).

%% Keys expected in command JSON.
-define(K_ACTION, <<"action">>).
-define(K_PAYLOAD, <<"payload">>).

%% Keys expected in payload JSON
-define(K_ITEM, <<"item">>).
-define(K_ID, <<"id">>).
-define(K_TYPE, <<"type">>).
-define(K_DATABASE, <<"database">>).
-define(K_ENQUEUED_AT, <<"enqueued_at">>).
-define(K_DATA_BAG_ITEM, <<"data_bag_item">>).
-define(K_DATA_BAG, <<"data_bag">>).

%% A mapping of the metadata command JSON keys to metadata field names
%% for XML.
chef_object_type(Index) when is_binary(Index) -> {data_bag_item, Index};
chef_object_type(Index) when is_atom(Index)   -> Index.

%% Solr
%%
%% <?xml version="1.0" encoding="UTF-8"?>
%% <update>
%%   <add>
%%     <doc>
%%       <field name="X_CHEF_id_CHEF_X">a1</field>
%%       <field name="X_CHEF_database_CHEF_X">chef_db1</field>
%%       <field name="X_CHEF_type_CHEF_X">role</field>
%%       <field name="content">X_CHEF_database_CHEF_X__=__chef_db1 X_CHEF_id_CHEF_X__=__a1 X_CHEF_type_CHEF_X__=__role key1__=__value1 key2__=__value2</field>
%%     </doc>
%%     <doc>
%%       <field name="X_CHEF_id_CHEF_X">a2</field>
%%       <field name="X_CHEF_database_CHEF_X">chef_db1</field>
%%       <field name="X_CHEF_type_CHEF_X">role</field>
%%       <field name="content">X_CHEF_database_CHEF_X__=__chef_db1 X_CHEF_id_CHEF_X__=__a2 X_CHEF_type_CHEF_X__=__role key1__=__value1 key2__=__value2</field>
%%     </doc>
%%   </add>
%%   <delete>
%%     <id>a3</id>
%%   </delete>
%%   <delete>
%%     <id>a4</id>
%%   </delete>
%% </update>
%%
%% Cloudsearch
%%
%% <?xml version="1.0" encoding="UTF-8"?>
%% <batch>
%%   <add id="a1">
%%     <field name="x_chef_id_chef_x">a1</field>
%%     <field name="x_chef_database_chef_x">chef_db1</field>
%%     <field name="x_chef_type_chef_x">role</field>
%%     <field name="content">x_chef_database_chef_x__=__chef_db1 x_chef_id_chef_x__=__a1 x_chef_type_chef_x__=__role key1__=__value1 key2__=__value2</field>
%%   </add>
%%   <add id="a1">
%%     <field name="x_chef_id_chef_x">a1</field>
%%     <field name="x_chef_database_chef_x">chef_db1</field>
%%     <field name="x_chef_type_chef_x">role</field>
%%     <field name="content">x_chef_database_chef_x__=__chef_db1 x_chef_id_chef_x__=__a1 x_chef_type_chef_x__=__role key1__=__value1 key2__=__value2</field>
%%   </add>
%%   <delete id="a3" />
%%   <delete id="a4" />
%% </batch>
%%

%%
%% Synchronous API
%%
%% doc_for_index, doc_for_delete, send_item, and send_delete
%% can be used to directly update the search index without
%% using the gen_server.
%%

%% @doc Create a fully expanded XML representation of the object for
%% addition. This document will need to be wrapped in
%% <update><add>DOC_HERE</add></update> before it is sent to solr.
doc_for_index(Index, Id, OrgId, Ejson) ->
    Command = make_command(add, Index, Id, OrgId, Ejson),
    make_doc_for_add(Command, chef_object_type(Index)).

doc_for_delete(_Index, Id, _OrgId) ->
    make_doc_for_del(chef_solr:search_provider(), Id).

update_payload(ToAdd, ToDel) ->
    update_payload(ToAdd, ToDel, chef_solr:search_provider()).

update_payload(ToAdd, ToDel, SearchProvider) ->
    [?XML_HEADER,
     start_tag(SearchProvider),
     ToDel,
     add_block(ToAdd, SearchProvider),
     end_tag(SearchProvider)
    ].

start_tag(solr) ->
    ?UPDATE_S;
start_tag(cloudsearch) ->
    <<"<batch>">>.

end_tag(solr) ->
    ?UPDATE_E;
end_tag(cloudsearch) ->
    <<"</batch>">>.

add_block([], _) ->
    [];
add_block(ToAdd, solr) ->
    [?ADD_S, ToAdd, ?ADD_E];
add_block(ToAdd, cloudsearch) ->
    ToAdd.

%% @doc Send a single document to solr directly.
send_item(Doc) ->
    post_to_solr(update_payload([Doc], [])).

send_delete(Doc) ->
    post_to_solr(update_payload([], [Doc])).

%% --- start copy from chef_index (chef_index_queue) ---

%% @doc Create a "command" EJSON term given Chef object attributes
%% `Type', `ID', `DatabaseName', and `Item'. The returned EJSON has
%% the same form as we use to place on the RabbitMQ queue for indexing
%% and that chef-expander expects to find for processing. The
%% `DatabaseName' can be either an OrgId or "chef_1deadbeef". The
%% `Item' should be the EJSON representation for the object
%% appropriate for indexing. In particular, this means a deep merged
%% structure for node objects.
%%
%% The code here is largely copied from the `chef_index' repo and the
%% `chef_index_queue' module, but isn't tied to rabbitmq client
%% libraries or actual queue publishing.
-spec make_command(Action :: add | delete,
                   Type :: binary() | atom(),
                   ID :: binary(),
                   DatabaseName :: binary() | string(),
                   Item :: term()) -> term().   % both term() are EJSON
make_command(Action, Type, ID, DatabaseName, Item) ->
  package_for_set(to_bin(Action), to_bin(Type), ID, normalize_db_name(DatabaseName), Item).

package_for_set(Action, Type, ID, DatabaseName, Item) ->
  InnerEnvelope = inner_envelope(Type, ID, DatabaseName, Item),
  {[{<<"action">>, Action},
    {<<"payload">>, InnerEnvelope}]}.

inner_envelope(Type, ID, DatabaseName, Item) ->
  {[{<<"type">>, Type},
    {<<"id">>, ID},
    {<<"database">>, DatabaseName},
    {<<"item">>, Item}, %% DEEP MERGED NODE
    {<<"enqueued_at">>, unix_time()}
  ]}.

unix_time() ->
  {MS, S, _US} = os:timestamp(),
  (1000000 * MS) + S.

normalize_db_name(S) when is_list(S) ->
    normalize_db_name(iolist_to_binary(S));
normalize_db_name(<<"chef_", _/binary>>=Name) ->
    Name;
normalize_db_name(OrgId) ->
    <<"chef_", OrgId/binary>>.
%% --- end copy from chef_index ---

%% @doc Given a list of command EJSON terms, as returned by {@link
%% make_command/4}, perform the appropriate flatten/expand operation
%% and POST the result to Solr as a single update.
-spec post_multi(list(), atom() | binary()) -> ok | {error, {_, _}}.
post_multi(Commands, Index) ->
    case handle_commands(Commands, chef_object_type(Index)) of
        {[], []} ->
            ok;
        {ToAdd, ToDel} ->
            Doc = update_payload(ToAdd, ToDel),
            post_to_solr(Doc)
    end.

%% @doc Given a command EJSON term as returned by {@link
%% make_command/4}, flatten/expand and POST to Solr.
-spec post_single(term(), atom() | binary()) -> ok | {error, {_, _}}.
post_single(Command, Index) ->
    post_multi([Command], Index).

%% @doc Return tuple of `{ToAdd, ToDel}' where `ToAdd' and `ToDel' are
%% iolists of XML data appropriate for including in an
%% `<update>...</update>' doc and POSTing to Solr.
-spec handle_commands(list(), atom() | {data_bag_item, binary}) -> {list(), list()}.
handle_commands(Commands, Index) ->
    lists:foldl(fun(C, {Adds, Deletes}) ->
                        case ej:get({?K_ACTION}, C) of
                            <<"add">> ->
                                {[make_doc_for_add(C, Index) | Adds], Deletes};
                            <<"delete">> ->
                                {Adds, [make_doc_for_del(C) | Deletes]};
                            _ ->
                                {Adds, Deletes}
                        end
                end, {[], []}, Commands).

%% @doc Post iolist `Doc' to Solr's `/update' endpoint at
%% `SolrUrl'.
%%
%% The atom `ok' is returned if Solr responds with a 2xx
%% status code. Otherwise, an error tuple is returned.
-spec post_to_solr(iolist()) -> ok | {error, {_, _}}.
post_to_solr(Doc) ->
    %% Note: we should try to enhance ibrowse to allow sending an
    %% iolist to avoid having to do iolist_to_binary here.
    DocBin = iolist_to_binary(Doc),
    {ok, Code, _Head, Body} = chef_index_http:request(chef_solr:update_url(), post, DocBin),
    case Code of
        [$2|_Rest] ->
            ok;
        _ ->
            {error, {Code, Body}}
    end.

make_doc_for_del(Command) ->
    Payload = ej:get({?K_PAYLOAD}, Command),
    Id = ej:get({?K_ID}, Payload),
    make_doc_for_del(chef_solr:search_provider(), Id).

make_doc_for_del(solr, Id) ->
    [<<"<delete><id>">>,
     Id,
     <<"</id></delete>">>];
make_doc_for_del(cloudsearch, Id) ->
    [<<"<delete id=\"">>,
     Id,
     <<"\" />">>].

make_doc_for_add(Command, ObjType) ->
    make_doc_for_add(Command, ObjType, chef_solr:search_provider()).

make_doc_for_add(Command, ObjType, SearchProvider) ->
    Payload = ej:get({?K_PAYLOAD}, Command),
    TypeField = ?FIELD(chef_solr:type_field(), get_object_type(ObjType)),
    MetaFieldsPL =  [ {Key, ej:get({Key0}, Payload)} || {Key0, Key} <- meta_fields() ] ++ [{chef_solr:type_field(), get_object_type(ObjType)}],
    MetaFields = [?FIELD(Name, ej:get({Key}, Payload)) || {Key, Name} <- meta_fields() ] ++ [TypeField],
    Id = ej:get({<<"id">>}, Payload),
    [doc_start(SearchProvider, Id),
     MetaFields,
     maybe_data_bag_field(ObjType),
     make_content(Payload, MetaFieldsPL),
     doc_end(SearchProvider)].

meta_fields() ->
    [{?K_ID, chef_solr:id_field()},
     {?K_DATABASE, chef_solr:database_field()}].

get_object_type({ObjectType, _}) ->
    get_object_type(ObjectType);
get_object_type(ObjectType) ->
    list_to_binary(atom_to_list(ObjectType)).

doc_start(solr, _Id) ->
    ?DOC_S;
doc_start(cloudsearch, Id) ->
    [<<"<add id=\"">>,
     Id,
     <<"\">">>].

doc_end(solr) ->
    ?DOC_E;
doc_end(cloudsearch) ->
    <<"</add>">>.

%% @doc If we have a `data_bag_item' object, return a Solr field
%% `data_bag', otherwise empty list.

maybe_data_bag_field({_, DataBagName}) ->
    ?FIELD(<<"data_bag">>, xml_text_escape(DataBagName));
maybe_data_bag_field(_) ->
    [].

%% @doc Extract the Chef object content, flatten/expand, and return an
%% iolist of the `content' field.
make_content(Payload, MetaFieldsPL) ->
    %% The Ruby code in chef-expander adds the database name, id, and
    %% type as fields. So we do the same.
    {Item0} = ej:get({?K_ITEM}, Payload),
    Item = {MetaFieldsPL ++ Item0},
    ?FIELD(<<"content">>, flatten(Item)).

%% @doc Main interface to flatten/expand for Chef object EJSON. Given
%% an EJSON term representing a Chef object, returns an iolist of the
%% flatten/expanded key value pairs.
%%
%% Key/value pairs are delimited with `__=__'. Nested key structure is
%% handled by joining the key path into a single key separated by
%% `_'. The final result is passed through {@link lists:usort/1} to
%% remove duplicate entries. This is similar to de-duping that the
%% Ruby implementation carries out.
%%
%% Keys and values receive basic XML escaping for the characters `<',
%% `&', and `>'.
-spec flatten(term()) -> iolist().
flatten(Obj) ->
    unique_expand([], Obj, []).

unique_expand(Keys, Obj, Acc) ->
    lists:usort(expand(Keys, Obj, Acc)).

expand(Keys, {PL} = Obj, Acc) when is_list(PL) ->
    expand_obj(Keys, Obj, Acc);
expand(Keys, Array, Acc) when is_list(Array) ->
    expand_list(Keys, Array, Acc);
expand(Keys, String, Acc) when is_binary(String) ->
    add_kv_pair(Keys, String, Acc);
expand(Keys, Int, Acc) when is_integer(Int) ->
    I = list_to_binary(integer_to_list(Int)),
    add_kv_pair(Keys, I, Acc);
expand(Keys, Flt, Acc) when is_float(Flt) ->
    %% trying to match closest to Ruby implementation, we can't use
    %% ~f.
    F = iolist_to_binary(io_lib:format("~p", [Flt])),
    add_kv_pair(Keys, F, Acc);
expand(Keys, true, Acc) ->
    add_kv_pair(Keys, <<"true">>, Acc);
expand(Keys, false, Acc) ->
    add_kv_pair(Keys, <<"false">>, Acc);
expand(_Keys, undefined, _Acc) ->
    throw({undefined_value_in_expander, "You cannot pass a key with an undefined value to chef_index_expander:expand/3."});
expand(Keys, null, Acc) ->
    add_kv_pair(Keys, <<"">>, Acc).

expand_list(Keys, List, Acc) ->
    lists:foldl(fun(Item, MyAcc) ->
                        expand(Keys, Item, MyAcc)
                end, Acc, List).

expand_obj(Keys, {PL}, Acc) ->
    lists:foldl(fun({K, V}, MyAcc) ->
                        MyAcc1 = expand(Keys, K, MyAcc),
                        expand([K|Keys], V, MyAcc1)
                end, Acc, PL).

add_kv_pair([], _Value, Acc) ->
    Acc;
add_kv_pair([K], Value, Acc) ->
    [encode_pair(K, Value) | Acc];
add_kv_pair([K|_]=Keys, Value, Acc) ->
    [encode_pair(join_keys(Keys, ?SEP), Value),
     encode_pair(K, Value) | Acc].

%% @doc Encode a single key/value pair for indexing. This means XML
%% text escaping `K' and `V' and building an iolist with the
%% appropriate separator.
encode_pair(K, V) ->
    [xml_text_escape(chef_solr:transform_data(K)),
     chef_solr:kv_sep(),
     xml_text_escape(chef_solr:transform_data(V)),
     <<" ">>].

%% @doc Return an iolist such that `Sep' is added between each element
%% of `Keys'. Does not flatten.  Used to construct the flattened key
%% paths. Note that `Keys' is expected to arrive in deepest key first
%% order and the iolist returned will be in deepest key last order
%% (thus suitable for printing).
join_keys(Keys, Sep) ->
    join_keys(Keys, Sep, []).

join_keys([], _Sep, Acc) ->
    Acc;
join_keys([K], _Sep, Acc) ->
    [K | Acc];
join_keys([K | Rest], Sep, Acc) ->
    join_keys(Rest, Sep, [Sep, K | Acc]).

%% @doc Given a binary or list of binaries, replace occurances of `<',
%% `&', `"', and `>' with the corresponding entity code such that the
%% resulting binary or list of binaries is suitable for inclusion as
%% text in an XML element.
%%
%% We cheat and simply process the binaries byte at a time. This
%% should be OK for UTF-8 binaries, but relies on multi-byte
%% characters not starting with the same value as those we are
%% searching for to escape.  Note that technically we don't need to
%% escape `>' nor `"', symmetry and matching of a pre-existing Ruby
%% implementation suggest otherwise.
-spec xml_text_escape(binary()|[binary()]) -> binary()|[binary()].
xml_text_escape(BinStr) ->
    iolist_to_binary(xml_text_escape1(BinStr)).

xml_text_escape1(BinStr) when is_binary(BinStr) ->
    efast_xs:escape(BinStr);

xml_text_escape1(BinList) when is_list(BinList) ->
    [ xml_text_escape1(B) || B <- BinList ].

to_bin({_, B}) ->
    to_bin(B);
to_bin(B) when is_binary(B) ->
    B;
to_bin(S) when is_list(S) ->
    iolist_to_binary(S);
to_bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8).
