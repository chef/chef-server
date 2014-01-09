-module(mover_phase_1_migrator_callback).

-export([
         migration_start_worker_args/3,
         migration_action/2
         ]).

%Return any initial state not related to the object id
migration_start_worker_args(Object, AcctInfo, ProcessorFun) ->
    [Object, AcctInfo].

migration_action(_, _) ->
    ok.
