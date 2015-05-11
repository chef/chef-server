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

-record(container, { id, authz_id, su_aid} ).

migration_init() ->
    mv_oc_chef_authz_http:create_pool(),
    {ok, SuperUserAuthzId} = sqerl:select(<<"select authz_id from users where username = 'pivotal'">>, [], first_as_scalar, [authz_id]),
    Query = <<"select id, authz_id, $1::character(32) as su_aid from containers where name in ('policies', 'policy_groups', 'cookbook_artifacts')">>,
    {ok, Results} = sqerl:select(Query, [SuperUserAuthzId], rows_as_records, [container, record_info(fields, container)]),
    mover_transient_migration_queue:initialize_queue(?MODULE, Results).

migration_action(#container{id = Id, authz_id = AuthzId, su_aid = RequestorId}, _) ->
    mv_oc_chef_authz:delete_resource(superuser, container, AuthzId),
    % Failures will be logged from authz, but in any event we'll need to clean up the database record as well.
    case sqerl:execute(<<"delete from containers where id = $1">>, [Id]) of
        {ok, _} ->
            ok;
        {error, Error} ->
            lager:warning("Unexpected error deleting container ~p from database: ~p", [Id, Error])
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


