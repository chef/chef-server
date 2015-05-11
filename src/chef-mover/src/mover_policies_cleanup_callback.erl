-module(mover_policies_cleanup_callback).

-export([
     migration_init/0,
     migration_complete/0,
     migration_type/0,
     supervisor/0,
     migration_start_worker_args/2,
     error_halts_migration/0,
     reconfigure_object/2,
     migration_action/2,
     next_object/0,
     needs_account_dets/0
    ]).

-record(container, { id, authz_id} ).

migration_init() ->
    mv_oc_chef_authz_http:create_pool(),
    Query = <<"select id, authz_id from containers where name in ('policies', 'policy_groups', 'cookbook_artifacts')">>,
    Results = case sqerl:select(Query, [], rows_as_records, [container, record_info(fields, container)]) of
                  {ok, none} -> [];
                  {ok, Any} -> Any
              end,
    Final = case file:consult("/tmp/reprocess") of
        {ok, Reprocess} ->
            Reprocess2 = [ {container, Id, AuthzId} ||  { Id, AuthzId } <- Reprocess ],
            Reprocess2 ++ Results;
        _ ->
            % No file, so no reprocessing
            Results
    end,
    mover_transient_migration_queue:initialize_queue(?MODULE, Final).

migration_action(#container{id = Id, authz_id = AuthzId}, _) ->
    case mv_oc_chef_authz:delete_resource(superuser, container, AuthzId) of
        {error, Reason} ->
            lager:error("Did not process {~p,~p}. due to ~p", [Id, AuthzId, Reason]);
        _ ->
            % Failures will be logged from authz, but in any event we'll need to clean up the database record as well.
            case sqerl:execute(<<"delete from containers where id = $1">>, [Id]) of
                {ok, _} ->
                    ok;
                {error, Error} ->
                    lager:warning("Unexpected error deleting container {~p, ~p}. from database: ~p", [Id, AuthzId, Error])
            end
    end.

migration_complete() ->
    mv_oc_chef_authz_http:delete_pool().

supervisor() ->
    mover_transient_worker_sup.

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

error_halts_migration() ->
    false.

reconfigure_object(_ObjectId, _AcctInfo) ->
    no_op.

needs_account_dets() ->
    false.

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_type() ->
    <<"policies_containers_cleanup">>.


