-module(mover_transient_migration_queue_test).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

?HOAX_FIXTURE(
   fun() ->
           mover_transient_migration_queue:start_link()
   end,
   fun(Pid) ->
           Pid
   end).

length_when_initialize_queue_size_3_should_return_3() ->
    mover_transient_migration_queue:initialize_queue(?MODULE, [1,2,3]),
    ?assertEqual(3, mover_transient_migration_queue:length(?MODULE)).

next_when_initialize_queue_size_3_should_return_3_items() ->
    mover_transient_migration_queue:initialize_queue(?MODULE, [1,2,3]),
    ?assertEqual(1, mover_transient_migration_queue:next(?MODULE)),
    ?assertEqual(2, mover_transient_migration_queue:next(?MODULE)),
    ?assertEqual(3, mover_transient_migration_queue:next(?MODULE)).

initialize_queue_overwrites_queue() ->
    mover_transient_migration_queue:initialize_queue(?MODULE, [1,2,3]),
    ?assertEqual(3, mover_transient_migration_queue:length(?MODULE)),
    mover_transient_migration_queue:initialize_queue(?MODULE, [4,5]),
    ?assertEqual(2, mover_transient_migration_queue:length(?MODULE)).
