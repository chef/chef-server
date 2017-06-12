

{ok, _Fields, Data} = epgsql:squery(C2, "select * from pg_logical_slot_get_changes('regression_slot',NULL, NULL, 'include-xids','1','force-binary','0', 'skip-empty-xacts', '1')").
Decoded = [ migrator_decode:decode(R) || {_, _, R} <- AllData ].

%% @doc Unload statement: unprepare in DB, then update statement
%% state dict.
%% Returns {ok, UpdatedDict}.
-spec unload_statement(connection(), atom(), sqerl_dict()) -> {ok, sqerl_dict()}.
unload_statement(Connection, Name, Dict) ->
        unprepare_statement(Connection, Name),
        {ok, pqc_remove(Name, Dict)}.
-spec pqc_remove(atom(), sqerl_dict()) -> sqerl_dict().
pqc_remove(Name, Cache) ->
    dict:erase(Name, Cache).


%% @doc Call DB to unprepare a previously prepared statement.
-spec unprepare_statement(connection(), atom()) -> ok.
unprepare_statement(Connection, Name) when is_atom(Name) ->
    SQL = list_to_binary([<<"DEALLOCATE ">>, atom_to_binary(Name, latin1)]),
    %% Have to do squery here (execute/3 uses equery which will try to prepare)
    {ok, _, _} = epgsql:squery(Connection, SQL),
    ok.


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

