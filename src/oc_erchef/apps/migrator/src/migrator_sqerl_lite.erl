-module(migrator_sqerl_lite).
% This badly named module takes a single logical transaction after it has been parsed by
% migrator_decode:decode
%-behaviour(gen_server).

% %This connection handles talking to the remote server. Connection info is hard-coded right now.
%
% This has two exports:
%   prepare/2 (Statement),
%   execute/2(TXTerm, Arguments).
%
-export([build_query/1, join/2]).
         %% API functions
%         start_link/0,
         %prepare/3,
         %unprepare/3,
         %execute/3,

         %% gen_server behaviour
         %init/1,
         %handle_call/3,
         %handle_cast/2,
         %terminate/2,
         %handle_info/2,
         %code_change/3 ]).

%-record(prepared_statement,
        %{name :: string(),
         %input_types  :: any(),
         %output_fields :: any(),
         %stmt :: any() } ).

-export([apply_tx/1]).

% apply_tx takes a logical transaction as converted by migrator_decoder:decode/1 and does the following:
%   * check to see if a prepared statement is inthe cache
%   * if no:
%       * assembles query for this combination of entity/tx type/fields
%       * prepares and caches the query
%   * apply prepared statement with values from the logical transaction
% Why?
%   - it wasn't significantly more work than just assembling the query inline, and
%     this avoids having to worry about escaping data for direct insert.
%   - less tx overhead on the DB because the statement is preparsed.
apply_tx(noop) ->
  ok;
apply_tx(Transaction)
  gen_server:call(?MODULE, {apply, Transaction}).
cached_statement( {tx_end, _TXID}) ->
  ok;
apply_tx({tx_start, _TXID}) ->
  ok;
apply_tx({_Table, _Operation, noop}) ->
  % TODO: Some function calls appear to generate a placeholder TX that makes no change. We need to
  % look more closely at this, since it's not consistent with the replication slot
  % only shipping actual changes.  More notes on this in migrator_decode:decode_transaction2.
  ok;
apply_tx(Transaction) ->
  gen_server:call(?MODULE, {apply, Transaction}).

% query cache can move to a different gen_server, or even an ets table.
% init - connect to 'remote' server.






%execute(SQL, Parameters, #state{cn=Cn}=State) when is_binary(SQL) ->
    %TParameters = input_transforms(Parameters),
    %case epgsql:equery(Cn, SQL, TParameters) of
        %{ok, Columns, Rows} ->
            %{{ok, format_result(Columns, Rows)}, State};
        %{ok, Count} ->
            %{{ok, Count}, State};
        %{ok, Count, Columns, Rows} ->
            %{{ok, {Count, format_result(Columns, Rows)}}, State};
        %{error, ?EPGSQL_TIMEOUT_ERROR} ->
            %{{error, timeout}, State};
        %{error, Error} ->
            %{{error, Error}, State}
    %end.




%%% { <<"clients">>, <<"INSERT">>, [ {<<"id">>, <<"da11fd5c59ad7cd575c4d53f6836168a">>},
%%%                                  {<<"org_id">>, <<"b22a18ce74e549b0ccb7da11fd5c59ad">>},
%%%                                  {<<"authz_id">>, <<"c3250a4fc08c482076abbaf6ed0b0ead">>},
%%%                                  {<<"public_key">> <<"...snip...">>},
%%%                                  {<<"validator">>, <<"false">>},
%%%                                  {<<"last_updated_by">>, <<"f1cd13cb541436c290c2b4eb3685f6c2">>},
%%%                                  {<<"created_at">>,<<"2017-06-05 18:06:47">>},
%%%                                  {<<"updated_at">>,<<"2017-06-05 18:06:47">>},
%%%                                  {<<"pubkey_version">>,0},
%%%                                  {<<"admin">>,false} ] }
%execute_prepared({#prepared_statement{} = PrepStmt, Statements}, Parameters,
                 %#state{cn = Cn, ctrans = CTrans} = State) ->
    %Stmt = PrepStmt#prepared_statement.stmt,
    %TParameters = input_transforms(Parameters, PrepStmt, State),
    %Result = try epgsql:execute_batch(Cn, [{Stmt, TParameters}]) of
        %[{ok, Count}|_] when is_integer(Count) ->
            %% returned for update, delete, so sync db
            %{ok, Count};
        %[{ok, RowData}|_] when is_list(RowData) ->
            %Rows = unpack_rows(PrepStmt, RowData),
            %TRows = sqerl_transformers:by_column_name(Rows, CTrans),
            %{ok, TRows};
        %[{ok, Count, RowData}|_] when is_list(RowData), is_integer(Count) ->
            %Rows = unpack_rows(PrepStmt, RowData),
            %TRows = sqerl_transformers:by_column_name(Rows, CTrans),
            %{ok, Count, TRows};
        %Unexpected ->
            %handle_error_response(Cn, Unexpected)
        %catch _:X ->
            %handle_error_response(Cn, X)
        %end,
    %{Result, State#state{statements = Statements}};
%execute_prepared(Error, _Parameters, State) ->
    %{Error, State}.

%handle_error_response(Cn, Response) ->
    %epgsql:sync(Cn),
    %handle_error_response(Response).

%handle_error_response([{error, ?EPGSQL_TIMEOUT_ERROR}|_]) ->
    %{error, timeout};
%handle_error_response({error, ?EPGSQL_TIMEOUT_ERROR}) ->
    %{error, timeout};
%handle_error_response({error, Error}) ->
    %{error, Error};
%handle_error_response([{error, Error}|_]) ->
    %{error, Error};
%handle_error_response(Other) ->
    %{error, Other}.


%%% @doc Prepare a new statement.
%-spec prepare(atom(), binary(), #state{}) -> {ok, #state{}}.
%prepare(Name, SQL, #state{cn=Cn, statements=Statements}=State) ->
    %{ok, UpdatedStatements} = load_statement(Cn, Name, SQL, Statements),
    %UpdatedState = State#state{statements=UpdatedStatements},
    %{ok, UpdatedState}.

%-spec unprepare(atom(), [], #state{}) -> {ok, #state{}}.
%unprepare(Name, _, State) ->
    %unprepare(Name, State).

%-spec unprepare(atom(), #state{}) -> {ok, #state{}}.
%unprepare(Name, #state{cn=Cn, statements=Statements}=State) ->
    %{ok, UpdatedStatements} = unload_statement(Cn, Name, Statements),
    %UpdatedState = State#state{statements=UpdatedStatements},
    %{ok, UpdatedState}.

%%% @see sqerl_adhoc:select/4.
%-spec sql_parameter_style() -> dollarn.
%sql_parameter_style() -> dollarn.

%is_connected(#state{cn=Cn}=State) ->
    %case catch epgsql:squery(Cn, ?PING_QUERY) of
        %{ok, _, _} ->
            %{true, State};
        %_ ->
            %false
    %end.

%connect() ->
    %case epgsql:connect(Host, User, Pass, Opts) of
        %{ok, Connection} ->
            %%% Link to pid so if this process dies we clean up
            %%% the socket
            %%% erlang:link(Connection),
            %set_statement_timeout(Connection, Timeout),
            %{ok, #state{cn=Connection, statements=Prepared, ctrans=CTrans, default_timeout=Timeout}};
        %%% I [jd] can't find any evidence of this clause in the wg/epgsql
        %%%{error, {syntax, Msg}} ->
        %%%    {stop, {syntax, Msg}};
        %{error, Error} ->
            %ErrorMsg = sqerl_pgsql_errors:translate(Error),
            %error_logger:error_msg("Unable to start database connection: ~p~n", [ErrorMsg]),
            %ErrorMsg
    %end.

%prepare_statement(Connection, Name, SQL) when is_atom(Name) ->
    %case epgsql:parse(Connection, atom_to_list(Name), SQL, []) of
        %{ok, Statement} ->
            %{ok, {statement, SName, Desc, DataTypes}} = epgsql:describe(Connection, Statement),
            %ColumnData = [ {CN, CT} || {column, CN, CT, _, _, _} <- Desc ],
            %P = #prepared_statement{
              %name = SName,
              %input_types = DataTypes,
              %output_fields = ColumnData,
              %stmt = Statement},
            %{ok, P};
        %{error, {error, error, _ErrorCode, Msg, Position}} ->
            %{error, {syntax, {Msg, Position}}};
        %Error ->
            %%% TODO: Discover what errors can flow out of this, and write tests.
            %{error, Error}
    %end.

%-spec unprepare_statement(connection(), atom()) -> ok.
%unprepare_statement(Connection, Name) when is_atom(Name) ->
    %SQL = list_to_binary([<<"DEALLOCATE ">>, atom_to_binary(Name, latin1)]),
    %%% Have to do squery here (execute/3 uses equery which will try to prepare)
    %{ok, _, _} = epgsql:squery(Connection, SQL),
    %ok.


%%%%
%%%% Data format conversion
%%%%


%extract_column_names({result_column_data, Columns}) ->
    %%% For column data coming from a query result
    %[Name || {column, Name, _Type, _Size, _Modifier, _Format} <- Columns];
%extract_column_names({prepared_column_data, ColumnData}) ->
    %%% For column data coming from a prepared statement
    %[Name || {Name, _Type} <- ColumnData].

%transform(timestamp, {datetime, X}) ->
    %X;
%transform(timestamp, X) when is_binary(X) ->
    %sqerl_transformers:parse_timestamp_to_datetime(X);
%transform(_Type, X) ->
    %X.

%input_transforms(Data, #prepared_statement{input_types=Types}, _State) ->
    %[ transform(T, E) || {T,E} <- lists:zip(Types, Data) ].

%input_transforms(Parameters) ->
    %[transform(Parameter) || Parameter <- Parameters].

%transform({datetime, X}) -> X;
%transform(X) -> X.

%set_statement_timeout(Connection, Timeout) ->
    %SQL = list_to_binary(
            %lists:flatten(io_lib:format("set statement_timeout=~p", [Timeout]))),
    %epgsql:squery(Connection, SQL).
