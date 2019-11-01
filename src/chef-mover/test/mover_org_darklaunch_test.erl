-module(mover_org_darklaunch_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

?HOAX_FIXTURE.

disable_org_creation_when_dry_run_should_not_send_query() ->
    application:set_env(mover, dry_run, true),
    ?assertEqual(ok, mover_org_darklaunch:disable_org_creation()),
    application:set_env(mover, dry_run, false).

disable_org_creation_when_successful_should_return_query_result() ->
    mock(eredis, ?expect(q, ?withArgs([mover_eredis_client,
                                       ["HMSET", "dl_org__OC_INTERNAL_NO_ORG", "disable_new_orgs", "true"]
                                       ]),
                        ?andReturn({ok, some_result}))),
    ?assertEqual(ok, mover_org_darklaunch:disable_org_creation()).

disable_org_creation_when_error_should_return_query_result() ->
    mock(eredis, ?expect(q, ?withArgs([mover_eredis_client,
                                       ["HMSET", "dl_org__OC_INTERNAL_NO_ORG", "disable_new_orgs", "true"]
                                       ]),
                        ?andReturn({error, some_error}))),
    ?assertEqual({error, some_error}, mover_org_darklaunch:disable_org_creation()).

enable_org_creation_when_dry_run_should_not_send_query() ->
    application:set_env(mover, dry_run, true),
    ?assertEqual(ok, mover_org_darklaunch:enable_org_creation()),
    application:set_env(mover, dry_run, false).

enable_org_creation_when_successful_should_return_query_result() ->
    mock(eredis, ?expect(q, ?withArgs([mover_eredis_client,
                                       ["HDEL", "dl_org__OC_INTERNAL_NO_ORG", "disable_new_orgs"]
                                       ]),
                        ?andReturn({ok, something_else}))),
    ?assertEqual(ok, mover_org_darklaunch:enable_org_creation()).

enable_org_creation_when_error_should_return_query_result() ->
    mock(eredis, ?expect(q, ?withArgs([mover_eredis_client,
                                       ["HDEL", "dl_org__OC_INTERNAL_NO_ORG", "disable_new_orgs"]
                                       ]),
                        ?andReturn({error, some_reason}))),
    ?assertEqual({error, some_reason}, mover_org_darklaunch:enable_org_creation()).

enable_solr4_should_default_solr4() ->
    mock(eredis, ?expect(q, ?withArgs([mover_eredis_client,
                                       ["HSET", "dl_default", "solr4", "true"]
                                       ]),
                        ?andReturn({ok, val}))),
    ?assertEqual(ok, mover_org_darklaunch:enable_solr4()).

enable_solr1_should_set_solr4_false() ->
    mock(eredis, ?expect(q, ?withArgs([mover_eredis_client,
                                       ["HMSET", <<"dl_org_name">>, "solr4", "false"]
                                       ]),
                        ?andReturn({ok, val}))),
    mock(eredis, ?expect(q, ?withArgs([mover_eredis_client,
                                       ["HDEL", <<"dl_org_name">>, "query_aux_solr", "rabbit_aux_vhost"]
                                       ]),
                        ?andReturn({ok, val}))),

    ?assertEqual(ok, mover_org_darklaunch:enable_solr1("name")).
