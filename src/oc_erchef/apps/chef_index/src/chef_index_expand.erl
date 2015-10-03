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

-export([send_item/1,
         send_delete/1,
         doc_for_index/4,
         doc_for_delete/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-record(chef_idx_expand_doc, {type,
                              id,
                              database,
                              item}).
-define(SEP, <<"_">>).
-define(FIELD(Name, Value), [<<"<field name=\"">>, Name, <<"\">">>, Value, <<"</field>">>]).
-define(XML_HEADER, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>).

-define(ADD_S, <<"<add>">>).
-define(ADD_E, <<"</add>">>).
-define(UPDATE_S, <<"<update>">>).
-define(UPDATE_E, <<"</update>">>).
-define(DOC_S, <<"<doc>">>).
-define(DOC_E, <<"</doc>">>).

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
    make_doc_for_add(make_record(Index, Id, OrgId, Ejson)).

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
    chef_solr:update(update_payload([Doc], [])).

send_delete(Doc) ->
    chef_solr:update(update_payload([], [Doc])).

%% @doc Create a chef_idx_expand_doc given Chef object attributes
%% `Type', `ID', `DatabaseName', and `Item'.
%%
-spec make_record(Type :: binary() | atom(),
                   ID :: binary(),
                   DatabaseName :: binary() | string(),
                   Item :: term()) -> #chef_idx_expand_doc{}.   % both term() are EJSON
make_record(Type, ID, DatabaseName, Item) ->
    #chef_idx_expand_doc{
       type = Type,
       id = ID,
       database = normalize_db_name(DatabaseName),
       item = Item
      }.

normalize_db_name(S) when is_list(S) ->
    normalize_db_name(iolist_to_binary(S));
normalize_db_name(<<"chef_", _/binary>>=Name) ->
    Name;
normalize_db_name(OrgId) ->
    <<"chef_", OrgId/binary>>.

make_doc_for_del(solr, Id) ->
    [<<"<delete><id>">>,
     Id,
     <<"</id></delete>">>];
make_doc_for_del(cloudsearch, Id) ->
    [<<"<delete id=\"">>,
     Id,
     <<"\" />">>].

make_doc_for_add(Command) ->
    make_doc_for_add(Command, chef_solr:search_provider()).

make_doc_for_add(Command = #chef_idx_expand_doc{id = Id, type=Type},
                 SearchProvider) ->
    MetaFieldsPL = meta_fields(Command),
    MetaFields = [ ?FIELD(Name, Value) || {Name, Value} <- MetaFieldsPL ],
    [doc_start(SearchProvider, Id),
     MetaFields,
     maybe_data_bag_field(Type),
     make_content(Command, MetaFieldsPL),
     doc_end(SearchProvider)].

meta_fields(#chef_idx_expand_doc{id = Id, database = Database, type=Type}) ->
    [{chef_solr:id_field(), Id},
     {chef_solr:database_field(), Database},
     {chef_solr:type_field(), get_object_type(Type)}].

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
make_content(#chef_idx_expand_doc{item={Item0}}, MetaFieldsPL) ->
    %% The Ruby code in chef-expander adds the database name, id, and
    %% type as fields. So we do the same.
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
