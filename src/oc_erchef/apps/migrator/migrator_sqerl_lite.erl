
-module(migrator_sqerl_lite).

-behaviour(sqerl_client).

-include("sqerl.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% sqerl_client callbacks
-export([init/1,
         prepare/3,
         unprepare/3,
         execute/3,
         is_connected/1,
         sql_parameter_style/0]).

-define(EPGSQL_TIMEOUT_ERROR, {error,error,<<"57014">>,
                                  <<"canceling statement due to statement timeout">>,
                                  []}).
-ifdef(TEST).
-compile([export_all]).
-endif.

-record(state,  {cn = undefined :: pid() | undefined,
                 statements = dict:new() :: sqerl_dict(),
                 ctrans :: sqerl_dict() | undefined,
                 default_timeout = 0 :: non_neg_integer() }).

-record(prepared_statement,
        {name :: string(),
         input_types  :: any(),
         output_fields :: any(),
         stmt :: any() } ).

-type connection() :: pid().

execute_prepared({#prepared_statement{} = PrepStmt, Statements}, Parameters,
                 #state{cn = Cn, ctrans = CTrans} = State) ->
    Stmt = PrepStmt#prepared_statement.stmt,
    TParameters = input_transforms(Parameters, PrepStmt, State),
    % epsql:execute_batch pipelines the multiple calls into epgsql we had to make for
    % bind, execute, sync.
    Result = try epgsql:execute_batch(Cn, [{Stmt, TParameters}]) of
        % NOTE we ignore additional data for now, but this opens up the possibility of accepting a list of
        % prepared statements and returning separate responses for each in a single call.
        [{ok, Count}|_] when is_integer(Count) ->
            % returned for update, delete, so sync db
            {ok, Count};
        [{ok, RowData}|_] when is_list(RowData) ->
            Rows = unpack_rows(PrepStmt, RowData),
            TRows = sqerl_transformers:by_column_name(Rows, CTrans),
            {ok, TRows};
        [{ok, Count, RowData}|_] when is_list(RowData), is_integer(Count) ->
            Rows = unpack_rows(PrepStmt, RowData),
            TRows = sqerl_transformers:by_column_name(Rows, CTrans),
            {ok, Count, TRows};
        Unexpected ->
            handle_error_response(Cn, Unexpected)
        catch _:X ->
            handle_error_response(Cn, X)
        end,
    {Result, State#state{statements = Statements}};
execute_prepared(Error, _Parameters, State) ->
    {Error, State}.

handle_error_response(Cn, Response) ->
    epgsql:sync(Cn),
    handle_error_response(Response).

handle_error_response([{error, ?EPGSQL_TIMEOUT_ERROR}|_]) ->
    {error, timeout};
handle_error_response({error, ?EPGSQL_TIMEOUT_ERROR}) ->
    {error, timeout};
handle_error_response({error, Error}) ->
    {error, Error};
handle_error_response([{error, Error}|_]) ->
    {error, Error};
handle_error_response(Other) ->
    {error, Other}.


%% @doc Prepare a new statement.
-spec prepare(atom(), binary(), #state{}) -> {ok, #state{}}.
prepare(Name, SQL, #state{cn=Cn, statements=Statements}=State) ->
    {ok, UpdatedStatements} = load_statement(Cn, Name, SQL, Statements),
    UpdatedState = State#state{statements=UpdatedStatements},
    {ok, UpdatedState}.

-spec unprepare(atom(), [], #state{}) -> {ok, #state{}}.
unprepare(Name, _, State) ->
    unprepare(Name, State).

-spec unprepare(atom(), #state{}) -> {ok, #state{}}.
unprepare(Name, #state{cn=Cn, statements=Statements}=State) ->
    {ok, UpdatedStatements} = unload_statement(Cn, Name, Statements),
    UpdatedState = State#state{statements=UpdatedStatements},
    {ok, UpdatedState}.

%% @see sqerl_adhoc:select/4.
-spec sql_parameter_style() -> dollarn.
sql_parameter_style() -> dollarn.

is_connected(#state{cn=Cn}=State) ->
    case catch epgsql:squery(Cn, ?PING_QUERY) of
        {ok, _, _} ->
            {true, State};
        _ ->
            false
    end.

connect() ->
    case epgsql:connect(Host, User, Pass, Opts) of
        {ok, Connection} ->
            %% Link to pid so if this process dies we clean up
            %% the socket
            %% erlang:link(Connection),
            set_statement_timeout(Connection, Timeout),
            {ok, #state{cn=Connection, statements=Prepared, ctrans=CTrans, default_timeout=Timeout}};
        %% I [jd] can't find any evidence of this clause in the wg/epgsql
        %%{error, {syntax, Msg}} ->
        %%    {stop, {syntax, Msg}};
        {error, Error} ->
            ErrorMsg = sqerl_pgsql_errors:translate(Error),
            error_logger:error_msg("Unable to start database connection: ~p~n", [ErrorMsg]),
            ErrorMsg
    end.

prepare_statement(Connection, Name, SQL) when is_atom(Name) ->
    case epgsql:parse(Connection, atom_to_list(Name), SQL, []) of
        {ok, Statement} ->
            {ok, {statement, SName, Desc, DataTypes}} = epgsql:describe(Connection, Statement),
            ColumnData = [ {CN, CT} || {column, CN, CT, _, _, _} <- Desc ],
            P = #prepared_statement{
              name = SName,
              input_types = DataTypes,
              output_fields = ColumnData,
              stmt = Statement},
            {ok, P};
        {error, {error, error, _ErrorCode, Msg, Position}} ->
            {error, {syntax, {Msg, Position}}};
        Error ->
            %% TODO: Discover what errors can flow out of this, and write tests.
            {error, Error}
    end.

-spec unprepare_statement(connection(), atom()) -> ok.
unprepare_statement(Connection, Name) when is_atom(Name) ->
    SQL = list_to_binary([<<"DEALLOCATE ">>, atom_to_binary(Name, latin1)]),
    %% Have to do squery here (execute/3 uses equery which will try to prepare)
    {ok, _, _} = epgsql:squery(Connection, SQL),
    ok.


%%%
%%% Data format conversion
%%%

%% @doc Format results of a query to expected standard (list of proplists).
format_result(Columns, Rows) ->
    %% Results from simple queries return Columns, Rows
    %% Columns are records
    %% Rows are tuples
    Names = extract_column_names({result_column_data, Columns}),
    unpack_rows(Names, Rows).

unpack_rows(#prepared_statement{output_fields=ColumnData}, Rows) ->
    %% Takes in a prepared statement record that
    %% holds column data that holds column names
    Columns = extract_column_names({prepared_column_data, ColumnData}),
    unpack_rows(Columns, Rows);
unpack_rows(ColumnNames, Rows) ->
    %% Takes in a list of colum names
    [lists:zip(ColumnNames, tuple_to_list(Row)) || Row <- Rows].

%% @doc Extract column names from column data.
%% Column data comes in two forms: as part of a result set,
%% or as part of a prepared statement.
%% With column data from a result set, call as
%% extract_column_names({result_column_data, Columns}).
%% With column data from a prepared statement, call as
%% extract_column_names({prepared_column_data, ColumnData}).
-spec extract_column_names({atom(), [#column{}]}) -> [any()].
extract_column_names({result_column_data, Columns}) ->
    %% For column data coming from a query result
    [Name || {column, Name, _Type, _Size, _Modifier, _Format} <- Columns];
extract_column_names({prepared_column_data, ColumnData}) ->
    %% For column data coming from a prepared statement
    [Name || {Name, _Type} <- ColumnData].

%%%
%%% Simple hooks to support coercion inputs to match the type expected by pgsql
%%%
transform(timestamp, {datetime, X}) ->
    X;
transform(timestamp, X) when is_binary(X) ->
    sqerl_transformers:parse_timestamp_to_datetime(X);
transform(_Type, X) ->
    X.

%% @doc Transform input for prepared statements.
%% Prepared statements have type data which we use
%% to transform the input.
input_transforms(Data, #prepared_statement{input_types=Types}, _State) ->
    [ transform(T, E) || {T,E} <- lists:zip(Types, Data) ].

%% transform (e.g. datetime type).
-spec input_transforms(list()) -> list().
input_transforms(Parameters) ->
    [transform(Parameter) || Parameter <- Parameters].

%% @doc Transform input data where applicable.
-spec transform(any()) -> any().
transform({datetime, X}) -> X;
transform(X) -> X.

-spec set_statement_timeout(connection(), integer()) -> term().
set_statement_timeout(Connection, Timeout) ->
    SQL = list_to_binary(
            lists:flatten(io_lib:format("set statement_timeout=~p", [Timeout]))),
    epgsql:squery(Connection, SQL).
