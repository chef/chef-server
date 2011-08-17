-module(chef_otto_integration).

-include_lib("eunit/include/eunit.hrl").
-include("../src/chef_otto.hrl").
-include("../src/chef_sql.hrl").

-define(DEPS, [sasl, crypto, ibrowse, couchbeam]).
-define(gv(Key, PList), proplists:get_value(Key, PList)).
-define(elem(Elem, List), lists:member(Elem, List)).

-record(couch_mock_info, {valid,
                          success_val}).

set_app_env() ->
    application:set_env(chef_common, couchdb_host, "localhost"),
    application:set_env(chef_common, couchdb_port, 5984).

mock_lookup(#couch_mock_info{valid=Valid,
                             success_val=SuccVal}) ->
    [meck:new(M) || M <- [couchbeam, couchbeam_view]],
    set_app_env(),
    meck:expect(couchbeam, server_connection, fun(_, _, _, _) -> cn end),
    meck:expect(couchbeam, open_db, fun(_, _, _) -> {ok, db} end),
    meck:expect(couchbeam, view, fun(db, {D, S},
                                     [{key, Name}]) ->
                                         case ?elem({D, S, Name}, Valid) of
                                             false ->
                                                 {ok, not_found};
                                             true ->
                                                 {ok, found} end end),
    meck:expect(couchbeam, open_doc, fun(db, <<"found">>) ->
                                             {ok, SuccVal};
                                        (db, V) ->
                                             {error, {unexpected, V}} end),
    meck:expect(couchbeam_view, first, fun(found) ->
                                               {ok, {[{<<"id">>, <<"found">>}]}};
                                          (not_found) ->
                                               {ok, []} end).

mock_bulk_lookup(#couch_mock_info{valid=Valid,
                                  success_val=SuccVal}) ->
    [meck:new(M) || M <- [couchbeam, couchbeam_view]],
    set_app_env(),
    meck:expect(couchbeam, server_connection, fun(_, _, _, _) -> cn end),
    meck:expect(couchbeam, open_db, fun(_, _, _) -> {ok, db} end),
    meck:expect(couchbeam, view, fun(db, {D, S}, [{key, Name}]) ->
                                     case ?elem({D, S, Name}, Valid) of
                                         false ->
                                             {ok, not_found};
                                         true ->
                                             {ok, found}
                                     end;
                                    (db, {D, S}, [{key, Name}, {include_docs, true}]) ->
                                     case ?elem({D, S, Name}, Valid) of
                                         false ->
                                             {ok, not_found};
                                         true ->
                                             {ok, found}
                                     end end),
    meck:expect(couchbeam, open_doc, fun(db, <<"found">>) ->
                                         {ok, SuccVal};
                                        (db, V) ->
                                         {error, {unexpected, V}} end),
    meck:expect(couchbeam_view, first, fun(found) ->
                                           {ok, {[{<<"id">>, <<"found">>}, {<<"_id">>, <<"found">>}]}};
                                          (not_found) ->
                                           {ok, []} end),
    meck:expect(couchbeam_view, fetch, fun(found) ->
                                           {ok, {[{<<"rows">>,
                                                   [{[{<<"doc">>, {[{<<"organization">>, <<"org-xyz">>}, {<<"_id">>, <<"found">>}]}}]}]}]}};
                                          (not_found) ->
                                           {ok, []} end),
    meck:expect(couchbeam, all_docs, fun(db, [{keys, [<<"org-xyz">>]}, {include_docs, true}]) ->
                                         {ok, docs_result};
                                        (db, Else) ->
                                         {unexpected_result, Else}
                                     end),
    meck:expect(couchbeam_view, fold, fun(docs_result, _) ->
                                          [[{<<"name">>, <<"clownco">>}]]
                                      end).

teardown_couch_view_lookup() ->
    meck:unload().

mock_fetch_user() ->
    mock_lookup(#couch_mock_info{valid=[{?mixlib_auth_user_design,
                                         "by_username", <<"clownco-org-admin">>}],
                                 success_val={[{<<"display_name">>,
                                                <<"ClowncoOrgAdmin">>}]}}).

mock_fetch_org() ->
    mock_lookup(#couch_mock_info{valid=[{?mixlib_auth_org_design,
                                         "by_name", <<"clownco">>}],
                                 success_val= {[{<<"clientname">>,
                                                 <<"clownco-validator">>},
                                                {<<"guid">>,
                                                 <<"test-guid">>}]}}).
mock_fetch_client() ->
    mock_lookup(#couch_mock_info{valid=[{?mixlib_auth_org_design,
                                         "by_name", <<"clownco">>},
                                        {?mixlib_auth_client_design,
                                        "by_clientname", <<"clownco-validator">>}],
                                 success_val= {[{<<"clientname">>,
                                                 <<"clownco-validator">>},
                                                {<<"guid">>,
                                                 <<"test-guid">>},
                                                {<<"orgname">>,
                                                 <<"clownco">>}]}}).

mock_fetch_orgs_for_user() ->
    mock_bulk_lookup(#couch_mock_info{valid=[{?mixlib_auth_user_design,
                                              "by_username", <<"clownco-org-admin">>},
                                             {?organization_user_design,
                                              "by_organizations_for_user", <<"1234">>}],
                                      success_val={[<<"clownco">>, {<<"_id">>, <<"1234">>}]}}).

mock_data_bag_exists() ->
    mock_lookup(#couch_mock_info{valid=[{?mixlib_auth_org_design,
                                         "by_name", <<"clownco">>},
                                        {"data_bags", "all_id",
                                         <<"user-bag">>}],
                                 success_val= {[
                                                {<<"guid">>,
                                                 <<"test-guid">>},
                                                {<<"orgname">>,
                                                 <<"clownco">>}]}}).

fetch_user_test_() ->
    {foreach,
     fun() -> mock_fetch_user() end,
     fun(_) -> teardown_couch_view_lookup() end,
     [{"fetch_user found",
       fun() ->
               S = chef_otto:connect(),
               Got = chef_otto:fetch_user(S, "clownco-org-admin"),
               ?assertMatch(<<"ClowncoOrgAdmin">>, ?gv(<<"display_name">>, Got)) end},
      {"fetch_user not found",
       fun() ->
               S = chef_otto:connect(),
               ?assertMatch({user_not_found, not_in_view},
                            chef_otto:fetch_user(S, "fred-is-not-found")) end}]}.

fetch_user_sql_test_() ->
    {foreach,
     fun() ->
             meck:new(dark_launch),
             meck:new(chef_sql)
     end,
     fun(_) -> teardown_couch_view_lookup() end,
     [{"fetch_user SQL found",
       fun() ->
               S = chef_otto:connect(),
               meck:expect(chef_sql, fetch_user,
                           fun("clownco-org-admin") ->
                                   #chef_user{id = <<"123">>,
                                              authz_id = <<"456">>,
                                              username = <<"clownco-org-admin">>,
                                              pubkey_version = 1,
                                              public_key = <<"key data">>}
                           end),
               Got = chef_otto:fetch_user(S, "clownco-org-admin", true),
               ?assertMatch(<<"123">>, ?gv(<<"_id">>, Got))
       end},

      {"fetch_user SQL not found",
       fun() ->
               S = chef_otto:connect(),
               meck:expect(chef_sql, fetch_user,
                           fun("fred-is-not-found") ->
                                   not_found
                           end),
               ?assertMatch({user_not_found, sql},
                            chef_otto:fetch_user(S, "fred-is-not-found", true))
       end}]}.
    


fetch_orgs_for_user_test_() ->
    {foreach,
     fun() ->
             mock_fetch_orgs_for_user(),
             meck:new(dark_launch),
             meck:expect(dark_launch, is_enabled,
                         fun("sql_users") -> false end)
     end,
     fun(_) -> teardown_couch_view_lookup() end,
     [{"fetch_orgs_for_user found",
       fun() ->
               S = chef_otto:connect(),
               Got = chef_otto:fetch_orgs_for_user(S, "clownco-org-admin"),
               ?assertMatch([<<"clownco">>], Got)
       end},
      {"fetch_orgs_for_user not found",
       fun() ->
               S = chef_otto:connect(),
               ?assertMatch({user_not_found, not_in_view},
                            chef_otto:fetch_orgs_for_user(S, "fred-is-not-found"))
       end}]}.

fetch_orgs_for_user_sql_test_() ->
    {foreach,
     fun() ->
             mock_fetch_orgs_for_user(),
             meck:new(dark_launch),
             meck:new(chef_sql),
             meck:expect(dark_launch, is_enabled,
                         fun("sql_users") -> true end)
     end,
     fun(_) -> teardown_couch_view_lookup() end,
     [{"fetch_orgs_for_user SQL found",
       fun() ->
               S = chef_otto:connect(),
               meck:expect(chef_sql, fetch_user,
                           fun("clownco-org-admin") ->
                                   #chef_user{id = <<"1234">>,
                                              authz_id = <<"456">>,
                                              username = <<"clownco-org-admin">>,
                                              pubkey_version = 1,
                                              public_key = <<"key data">>}
                           end),
               Got = chef_otto:fetch_orgs_for_user(S, "clownco-org-admin"),
               ?assertMatch([<<"clownco">>], Got)
       end},

      {"fetch_orgs_for_user SQL not found",
       fun() ->
               S = chef_otto:connect(),
               meck:expect(chef_sql, fetch_user,
                           fun("fred-is-not-found") ->
                                   not_found
                           end),
               ?assertMatch({user_not_found, sql},
                            chef_otto:fetch_orgs_for_user(S, "fred-is-not-found"))
       end}]}.

fetch_org_test_() ->
    {foreach,
     fun() -> mock_fetch_org() end,
     fun(_) -> teardown_couch_view_lookup() end,
     [{"fetch_org",
      fun() ->
              S = chef_otto:connect(),
              Org = chef_otto:fetch_org(S, <<"clownco">>),
              ?assertMatch(<<"clownco-validator">>,
                           ?gv(<<"clientname">>, Org)) end},
      {"fetch_org not found",
       fun() ->
               S = chef_otto:connect(),
               %% FIXME: how can we test the case when org is in view,
               %% but not found in the db.  Need to either manipulate a
               %% test couchdb or introduce some mocks.
               ?assertMatch({org_not_found, not_in_view},
                            chef_otto:fetch_org(S, <<"no-such-org">>)) end}]}.

fetch_client_test_() ->
    {foreach,
     fun() -> mock_fetch_client() end,
     fun(_) -> teardown_couch_view_lookup() end,
     [{"fetch_client",
       fun() ->
               S = chef_otto:connect(),
               OID = ?gv(<<"guid">>, chef_otto:fetch_org(S, <<"clownco">>)),
               Client = chef_otto:fetch_client(S, OID, <<"clownco-validator">>),
               ?assertMatch(<<"clownco">>, ?gv(<<"orgname">>, Client)) end},
      {"fetch_client no such client",
       fun() ->
               S = chef_otto:connect(),
               Org = ?gv(<<"guid">>, chef_otto:fetch_org(S, <<"clownco">>)),
               ?assertEqual({not_found, client},
                            chef_otto:fetch_client(S, Org,
                                                   <<"not-a-known-client">>)) end},
      {"fetch_client with missing org",
        fun() ->
                S = chef_otto:connect(),
                OID = chef_otto:fetch_org_id(S, <<"not-a-known-org">>),
                ?assertEqual({not_found, org},
                             chef_otto:fetch_client(S, OID,
                                                    <<"clownco-validator">>)) end}]}.

data_bag_exists_test_() ->
    {foreach,
     fun() -> mock_data_bag_exists() end,
     fun(_) -> teardown_couch_view_lookup() end,
     [{"data_bag_exists YES",
       fun() ->
               S = chef_otto:connect(),
               OID = ?gv(<<"guid">>, chef_otto:fetch_org(S, <<"clownco">>)),
               ?assertEqual(true, chef_otto:data_bag_exists(S, OID, <<"user-bag">>))
       end},

      {"data_bag_exists NO",
       fun() ->
               S = chef_otto:connect(),
               OID = ?gv(<<"guid">>, chef_otto:fetch_org(S, <<"clownco">>)),
               ?assertEqual(false, chef_otto:data_bag_exists(S, OID, <<"no-such-bag">>))
       end}
     ]}.
