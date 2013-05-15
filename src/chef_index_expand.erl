-module(chef_index_expand).

-export([
         make_command/5,
         post_multi/1,
         post_single/1
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
-spec post_multi(list()) -> ok | skip | {error, string(), term()}.
post_multi(Commands) ->
    case separate_add_delete(Commands) of
        {[], []} ->
            skip;
        {ToAdd, ToDel} ->
            Deletes = [ handle_command(C) || C <- ToDel ],
            Adds = [ make_doc_for_add(C) || C <- ToAdd ],
            Doc = [?XML_HEADER,
                   <<"<update>">>,
                   Deletes,
                   <<"<add>">>,
                   Adds,
                   <<"</add>">>,
                   <<"</update>">>],
            post_to_solr(Doc)
    end.

separate_add_delete(Commands) ->
    lists:foldl(fun(C, {Adds, Deletes}) ->
                        case ej:get({?K_ACTION}, C) of
                            <<"add">> ->
                                {[C | Adds], Deletes};
                            <<"delete">> ->
                                {Adds, [C | Deletes]};
                            _ ->
                                {Adds, Deletes}
                        end
                end, {[], []}, Commands).

%% @doc Given a command EJSON term as returned by {@link
%% make_command/4}, flatten/expand and POST to Solr.
-spec post_single(term()) -> ok | skip | {error, string(), term()}.
post_single(Command) ->
    case handle_command(Command) of
        skip ->
            skip;
        SubDoc ->
            Doc = [?XML_HEADER, SubDoc],
            post_to_solr(Doc)
    end.

%% @doc Post iolist `Doc' to Solr's `/update' URL. The atom `ok' is
%% returned if Solr responds with a 2xx status code. Otherwise, an
%% error tuple is returned.
post_to_solr(Doc) ->
    Headers = [{"Content-Type", "text/xml"}],
    %% Note: we should try to enhance ibrowse to allow sending an
    %% iolist to avoid having to do iolist_to_binary here.
    DocBin = iolist_to_binary(Doc),
    {ok, Code, _Head, Body} = ibrowse:send_req(solr_url(), Headers, post, DocBin),
    case Code of
        "2" ++ _Rest ->
            %% FIXME: add logging and timing
            ok;
        _ ->
            %% FIXME: add logging, timing
            {error, Code, Body}
    end.

solr_url() ->
    case application:get_env(mover, solr_url) of
        {ok, Url} ->
            Url ++ "/update";
        undefined ->
            "http://localhost:8983/update"
    end.

%% @doc Return an iolist of XML data appropriate for POSTing to
%% Solr. If `Command' has an unexpected format, the atom `skip' is
%% returned.
%%
%% There are two supported actions: "add" and "delete". The
%% appropriate iolist is returned that can be used to construct a
%% single or multi update XML doc for POSTing to solr.
-spec handle_command(term()) -> iolist() | skip.
handle_command(Command) ->
    handle_command(ej:get({?K_ACTION}, Command), Command).

handle_command(<<"add">>, Command) ->
    [?ADD_S, make_doc_for_add(Command), ?ADD_E];
handle_command(<<"delete">>, Command) ->    
            Payload = ej:get({?K_PAYLOAD}, Command),
            [<<"<delete><id>">>,
                  ej:get({?K_ID}, Payload),
                  <<"</id></delete>\n">>];
handle_command(_, _) ->    
    skip.

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
    ?FIELD(<<"content">>, flatten(ej:get({?K_ITEM}, Payload))).

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
    F = iolist_to_binary(io_lib:format("~.15g", [Flt])),
    add_kv_pair(Keys, F, Acc);
expand(Keys, true, Acc) ->
    add_kv_pair(Keys, <<"true">>, Acc);
expand(Keys, false, Acc) ->
    add_kv_pair(Keys, <<"false">>, Acc);
expand(Keys, null, Acc) ->
    add_kv_pair(Keys, <<"null">>, Acc).

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
%% `&', and `>' with the corresponding entity code such that the
%% resulting binary or list of binaries is suitable for inclusion as
%% text in an XML element.
%%
%% We cheat and simply process the binaries byte at a time. This
%% should be OK for UTF-8 binaries, but relies on multi-byte
%% characters not starting the same value as those we are searching
%% for to escape.  Note that technically we don't need to escape '>',
%% but symmetry.
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
escape_char(C) ->
    C.

to_bin(B) when is_binary(B) ->
    B;
to_bin(S) when is_list(S) ->
    iolist_to_binary(S);
to_bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8).

