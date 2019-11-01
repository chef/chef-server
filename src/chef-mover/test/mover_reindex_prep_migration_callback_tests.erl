-module(mover_reindex_prep_migration_callback_tests).

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
    ?assertError({badmatch, true}, mover_reindex_prep_migration_callback:migration_init()).

migration_init_should_initialize_queue_with_orgs() ->
    application:set_env(mover, dry_run, false),
    mock(mover_org_darklaunch, ?expect(disable_org_creation, ?withArgs([]))),
    mock(moser_acct_processor, ?expect(open_account, ?withArgs([]), ?andReturn(dets_handle))),
    mock(moser_acct_processor, ?expect(all_orgs, ?withArgs([dets_handle]),
                                       ?andReturn([
                                                   #org_info{org_name = <<"name1">>},
                                                   #org_info{org_name = <<"name2">>}
                                                  ]))),

    mover_reindex_prep_migration_callback:migration_init(),

    ?assertEqual(2, mover_transient_migration_queue:length(mover_reindex_prep_migration_callback)).

migration_start_worker_args_include_both_org_and_account_info_in_list() ->
    ?assertEqual([org,account_info], mover_reindex_prep_migration_callback:migration_start_worker_args(org, account_info)).

migration_action_should_insert_org_return_ok_on_successful_insert() ->
    mock(moser_acct_processor, ?expect(expand_org_info, ?withArgs([?ORG]),
                                       ?andReturn(?EXPANDED_ORG))),
    mock(mover_org_darklaunch, ?expect(enable_solr1, ?withArgs([org_name]), ?andReturn(ok))),
    mock(moser_state_tracker, ?expect(insert_one_org, ?withArgs([?EXPANDED_ORG, <<"solr4_migration">>]),
                                                                ?andReturn(ok))),
    ?assertEqual(ok, mover_reindex_prep_migration_callback:migration_action(org_name, account_info)).

migration_action_should_insert_org_return_ok_on_duplicate_key() ->
    mock(moser_acct_processor, ?expect(expand_org_info, ?withArgs([?ORG]),
                                       ?andReturn(?EXPANDED_ORG))),
    mock(mover_org_darklaunch, ?expect(enable_solr1, ?withArgs([org_name]), ?andReturn(ok))),
    mock(moser_state_tracker, ?expect(insert_one_org, ?withArgs([?EXPANDED_ORG, <<"solr4_migration">>]),
                                                                ?andReturn({error,
                                                                           {error, error, <<"23505">>, msg, detail}}))),
    ?assertEqual(ok, mover_reindex_prep_migration_callback:migration_action(org_name, account_info)).

migration_action_should_insert_org_return_error_on_other_error() ->
    mock(moser_acct_processor, ?expect(expand_org_info, ?withArgs([?ORG]),
                                       ?andReturn(?EXPANDED_ORG))),
    mock(mover_org_darklaunch, ?expect(enable_solr1, ?withArgs([org_name]), ?andReturn(ok))),
    mock(moser_state_tracker, ?expect(insert_one_org, ?withArgs([?EXPANDED_ORG, <<"solr4_migration">>]),
                                                                ?andReturn({error,
                                                                           other_error}))),
    ?assertEqual(other_error, mover_reindex_prep_migration_callback:migration_action(org_name, account_info)).

next_object_returns_next_from_migration_queue() ->
    mover_transient_migration_queue:start_link(),
    mover_transient_migration_queue:initialize_queue(mover_reindex_prep_migration_callback, [org1, org2]),

    ?assertEqual(org1, mover_reindex_prep_migration_callback:next_object()).

supervisor_should_be_transient_worker_sup() ->
    ?assertEqual(mover_transient_worker_sup, mover_reindex_prep_migration_callback:supervisor()).

reconfigure_object_is_a_no_op() ->
    ?assertEqual(no_op, mover_reindex_prep_migration_callback:reconfigure_object(org, acct)).

migration_complete_should_default_to_solr4_and_enable_org_creation() ->
    mock(mover_org_darklaunch, ?expect(enable_solr4, ?withArgs([]), ?andReturn(ignored))),
    mock(mover_org_darklaunch, ?expect(enable_org_creation, ?withArgs([]), ?andReturn(ignored))),
    mover_reindex_prep_migration_callback:migration_complete().
