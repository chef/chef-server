%% @doc Chef object flatten/expand and POSTing to Solr
%%
%% This module implements Chef object flatten/expand using the same
%% algorithm as `chef-expander' and handles POSTing updates (both adds
%% and deletes) to Solr. You will interact with four functions:
%%
%% <ol>
%% <li>{@link init_items/1}</li>
%% <li>{@link add_item/5}</li>
%% <li>{@link delete_item/4}</li>
%% <li>{@link send_items/1}</li>
%% </ol>
%%
%% Start by initializing an item context object using {@link
%% init_items/1} to which you pass the URL for the Solr instance you
%% want to work with.
%%
%% Next, use {@link add_item/5} to add/update items in the
%% index. These items will go through the flatten/expand process. If
%% you want to stage an item delete, use {@link delete_item/4}. Both
%% of these functions take an item context object and return a
%% possibly updated context. It is important to keep track of the
%% possibly modified context to use for your next call. This API
%% allows this module to handle the flatten/expand and post in
%% different ways. For example, the flatten/expand can be done inline,
%% accumulating the result in the context, or the context can contain
%% a pid and the work can be done async and in parallel.
%%
%% Finally, call {@link send_items/1} passing the accumulated context
%% object. Calling this function triggers the actual POST to solr.
%%
%% @end
-module(chef_index_expand).

-export([
         add_item/5,
         delete_item/4,
         init_items/1,
         make_command/5,
         post_multi/1,
         post_single/1,
         send_items/1
        ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(SEP, <<"_">>).
-define(KV_SEP, <<"__=__">>).

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
-define(META_FIELDS, [{?K_ID, <<"X_CHEF_id_CHEF_X">>},
                      {?K_DATABASE, <<"X_CHEF_database_CHEF_X">>},
                      {?K_TYPE, <<"X_CHEF_type_CHEF_X">>}]).

-record(idx_exp_ctx, {
          to_add = [],
          to_del = [],
          solr_url
         }).

-opaque index_expand_ctx() :: #idx_exp_ctx{}.
-export_type([index_expand_ctx/0]).

%% @doc Create a new index expand context.
-spec init_items(string()) -> index_expand_ctx().
init_items(SolrUrl) ->
    #idx_exp_ctx{solr_url = SolrUrl}.

%% @doc Add an EJSON item to the provided index expand context. The
%% backing implementation will flatten/expand `Ejson' either inline
%% (blocking) or async/parallel (in which case this function returns
%% immediately).
-spec add_item(index_expand_ctx(), binary(), ej:json_object(),
               binary() | atom(), binary()) -> index_expand_ctx().
add_item(#idx_exp_ctx{to_add = Added} = Ctx, Id, Ejson, Index, OrgId) ->
    %% TODO: we don't really need the intermediate "command" object.
    Command = make_command(add, Index, Id, OrgId, Ejson),
    Doc = make_doc_for_add(Command),
    Ctx#idx_exp_ctx{to_add = [Doc | Added]}.

%% @doc Add `Id' to the list of items to delete from solr.
-spec delete_item(index_expand_ctx(),
                  binary(),
                  binary() | atom(), binary()) -> index_expand_ctx().
delete_item(#idx_exp_ctx{to_del = Deleted} = Ctx, Id, Index, OrgId) ->
    Command = make_command(delete, Index, Id, OrgId, {[]}),
    Doc = make_doc_for_del(Command),
    Ctx#idx_exp_ctx{to_del = [Doc | Deleted]}.

%% @doc Send items accumulated in the index expand context to
%% Solr. The URL used to talk to Solr is embedded in the context
%% object and determined when `{@link init_items/1}' was called.
-spec send_items(index_expand_ctx()) -> ok | {error, {_, _}}.
send_items(#idx_exp_ctx{to_add = Added, to_del = Deleted, solr_url = Url}) ->
    case {Added, Deleted} of
        {[], []} ->
            ok;
        {ToAdd, ToDel} ->
            Doc = [?XML_HEADER,
                   ?UPDATE_S,
                   ToDel,
                   value_or_empty(ToAdd, [?ADD_S, ToAdd, ?ADD_E]),
                   ?UPDATE_E],
            post_to_solr(Doc, Url)
    end.

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
-spec post_multi(list()) -> ok | {error, {_, _}}.
post_multi(Commands) ->
    case handle_commands(Commands) of
        {[], []} ->
            ok;
        {ToAdd, ToDel} ->
            Doc = [?XML_HEADER,
                   ?UPDATE_S,
                   ToDel,
                   value_or_empty(ToAdd, [?ADD_S, ToAdd, ?ADD_E]),
                   ?UPDATE_E],
            post_to_solr(Doc)
    end.

value_or_empty([], _V) ->
    [];
value_or_empty([_|_], V) ->
    V.

%% @doc Given a command EJSON term as returned by {@link
%% make_command/4}, flatten/expand and POST to Solr.
-spec post_single(term()) -> ok | {error, {_, _}}.
post_single(Command) ->
    post_multi([Command]).

%% @doc Return tuple of `{ToAdd, ToDel}' where `ToAdd' and `ToDel' are
%% iolists of XML data appropriate for including in an
%% `<update>...</update>' doc and POSTing to Solr.
-spec handle_commands(list()) -> {list(), list()}.
handle_commands(Commands) ->
    lists:foldl(fun(C, {Adds, Deletes}) ->
                        case ej:get({?K_ACTION}, C) of
                            <<"add">> ->
                                {[make_doc_for_add(C) | Adds], Deletes};
                            <<"delete">> ->
                                {Adds, [make_doc_for_del(C) | Deletes]};
                            _ ->
                                {Adds, Deletes}
                        end
                end, {[], []}, Commands).

post_to_solr(Doc) ->
    post_to_solr(Doc, solr_url()).

%% @doc Post iolist `Doc' to Solr's `/update' endpoint at
%% `SolrUrl'.
%%
%% The atom `ok' is returned if Solr responds with a 2xx
%% status code. Otherwise, an error tuple is returned.
-spec post_to_solr(iolist(), string()) -> ok | {error, {_, _}}.
post_to_solr(Doc, SolrUrl) ->
    Headers = [{"Content-Type", "text/xml"}],
    %% Note: we should try to enhance ibrowse to allow sending an
    %% iolist to avoid having to do iolist_to_binary here.
    DocBin = iolist_to_binary(Doc),
    {ok, Code, _Head, Body} = ibrowse:send_req(SolrUrl, Headers, post, DocBin),
    case Code of
        "2" ++ _Rest ->
            %% FIXME: add logging and timing
            ok;
        _ ->
            %% FIXME: add logging, timing
            {error, {Code, Body}}
    end.

solr_url() ->
    case application:get_env(chef_index, solr_url) of
        {ok, Url} ->
            Url ++ "/update";
        undefined ->
            "http://localhost:8983/update"
    end.

make_doc_for_del(Command) ->
    Payload = ej:get({?K_PAYLOAD}, Command),
    [<<"<delete><id>">>,
     ej:get({?K_ID}, Payload),
     <<"</id></delete>">>].

make_doc_for_add(Command) ->
    Payload = ej:get({?K_PAYLOAD}, Command),
    [?DOC_S,
     [ ?FIELD(Name, ej:get({Key}, Payload)) || {Key, Name} <- ?META_FIELDS ],
     maybe_data_bag_field(Payload),
     make_content(Payload),
     ?DOC_E].

%% @doc If we have a `data_bag_item' object, return a Solr field
%% `data_bag', otherwise empty list.
maybe_data_bag_field(Payload) ->
    case ej:get({?K_TYPE}, Payload) of
        ?K_DATA_BAG_ITEM ->
            ?FIELD(<<"data_bag">>,
                   xml_text_escape(ej:get({?K_ITEM, ?K_DATA_BAG}, Payload)));
        _ ->
            []
    end.

%% @doc Extract the Chef object content, flatten/expand, and return an
%% iolist of the `content' field.
make_content(Payload) ->
    %% The Ruby code in chef-expander adds the database name, id, and
    %% type as fields. So we do the same.
    Meta = [ {Key, ej:get({Key0}, Payload)} || {Key0, Key} <- ?META_FIELDS ],
    {Item0} = ej:get({?K_ITEM}, Payload),
    Item = {Meta ++ Item0},
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
    [xml_text_escape(K), ?KV_SEP, xml_text_escape(V), <<" ">>].

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
xml_text_escape(BinStr) ->
    iolist_to_binary(xml_text_escape1(BinStr)).

xml_text_escape1(BinStr) when is_binary(BinStr) ->
    [ escape_char(C) || <<C>> <= BinStr ];
xml_text_escape1(BinList) when is_list(BinList) ->
    [ xml_text_escape1(B) || B <- BinList ].

escape_char($<) ->
    "&lt;";
escape_char($&) ->
    "&amp;";
escape_char($>) ->
    "&gt;";
escape_char($") ->
    "&quot;";
escape_char(C) ->
    C.

to_bin(B) when is_binary(B) ->
    B;
to_bin(S) when is_list(S) ->
    iolist_to_binary(S);
to_bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8).

