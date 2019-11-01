-module(mover_reindex_dry_run_prep_migration_callback_tests).

-include_lib("moser/include/moser.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

?HOAX_FIXTURE(fun() ->
                  mover_transient_migration_queue:start_link()
              end,
              fun(Pid) ->
                  Pid
              end).

-define(ORG,#org_info{org_name = org_name, account_info = account_info}).
-define(EXPANDED_ORG,#org_info{org_name = org_name, account_info = account_info, org_id = org_id}).

migration_init_should_fail_if_in_dry_run() ->
    application:set_env(mover, dry_run, true),
    ?assertError({badmatch, true}, mover_reindex_dry_run_prep_migration_callback:migration_init()).

migration_init_should_initialize_queue_with_orgs() ->
    application:set_env(mover, dry_run, false),
    mock(moser_acct_processor, ?expect(open_account, ?withArgs([]), ?andReturn(dets_handle))),
    mock(moser_acct_processor, ?expect(all_orgs, ?withArgs([dets_handle]),
                                       ?andReturn([
                                                   #org_info{org_name = <<"name1">>},
                                                   #org_info{org_name = <<"name2">>}
                                                  ]))),

    mover_reindex_dry_run_prep_migration_callback:migration_init(),

    ?assertEqual(2, mover_transient_migration_queue:length(mover_reindex_dry_run_prep_migration_callback)).

migration_start_worker_args_include_both_org_and_account_info_in_list() ->
    ?assertEqual([org,account_info], mover_reindex_dry_run_prep_migration_callback:migration_start_worker_args(org, account_info)).

migration_action_should_insert_org_return_ok_on_successful_insert() ->
    mock(moser_acct_processor, ?expect(expand_org_info, ?withArgs([?ORG]),
                                       ?andReturn(?EXPANDED_ORG))),
    mock(moser_state_tracker, ?expect(insert_one_org, ?withArgs([?EXPANDED_ORG, <<"solr4_migration_dry_run">>]),
                                                                ?andReturn(ok))),
    ?assertEqual(ok, mover_reindex_dry_run_prep_migration_callback:migration_action(org_name, account_info)).

migration_action_should_insert_org_return_ok_on_duplicate_key() ->
    mock(moser_acct_processor, ?expect(expand_org_info, ?withArgs([?ORG]),
                                       ?andReturn(?EXPANDED_ORG))),
    mock(moser_state_tracker, ?expect(insert_one_org, ?withArgs([?EXPANDED_ORG, <<"solr4_migration_dry_run">>]),
                                                                ?andReturn({error,
                                                                           {error, error, <<"23505">>, msg, detail}}))),
    mock(moser_state_tracker, ?expect(force_org_to_state, ?withArgs([org_name, <<"solr4_migration_dry_run">>, <<"holding">>]), ?andReturn(ok))),
    mock(mover_reindex_dry_run_revert_migration_callback, ?expect(reconfigure_object, ?withArgs([org_name]), ?andReturn(ok))),
    ?assertEqual(ok, mover_reindex_dry_run_prep_migration_callback:migration_action(org_name, account_info)).

migration_action_should_insert_org_return_error_on_other_error() ->
    mock(moser_acct_processor, ?expect(expand_org_info, ?withArgs([?ORG]),
                                       ?andReturn(?EXPANDED_ORG))),
    mock(moser_state_tracker, ?expect(insert_one_org, ?withArgs([?EXPANDED_ORG, <<"solr4_migration_dry_run">>]),
                                                                ?andReturn({error,
                                                                           other_error}))),
    ?assertEqual(other_error, mover_reindex_dry_run_prep_migration_callback:migration_action(org_name, account_info)).

next_object_returns_next_from_migration_queue() ->
    mover_transient_migration_queue:start_link(),
    mover_transient_migration_queue:initialize_queue(mover_reindex_dry_run_prep_migration_callback, [org1, org2]),

    ?assertEqual(org1, mover_reindex_dry_run_prep_migration_callback:next_object()).

supervisor_should_be_transient_worker_sup() ->
    ?assertEqual(mover_transient_worker_sup, mover_reindex_dry_run_prep_migration_callback:supervisor()).

reconfigure_object_is_a_no_op() ->
    ?assertEqual(no_op, mover_reindex_dry_run_prep_migration_callback:reconfigure_object(org, acct)).

migration_complete_is_a_no_op_because_this_is_dry_run() ->
    ?assertEqual(no_op, mover_reindex_dry_run_prep_migration_callback:migration_complete()).
