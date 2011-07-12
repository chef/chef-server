-module(chef_otto_integration).

-include_lib("eunit/include/eunit.hrl").
-include("../src/chef_otto.hrl").

-define(DEPS, [sasl, crypto, ibrowse, couchbeam]).
-define(gv(Key, PList), proplists:get_value(Key, PList)).
-define(elem(Elem, List), lists:member(Elem, List)).

-record(couch_mock_info, {valid,
                          success_val}).

mock_lookup(#couch_mock_info{valid=Valid,
                             success_val=SuccVal}) ->
    [meck:new(M) || M <- [couchbeam, couchbeam_view]],
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

teardown_couch_view_lookup() ->
    [meck:unload(M) || M <- [couchbeam, couchbeam_view]].

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
               ?assertEqual(not_found,
                            chef_otto:fetch_client(S, Org,
                                                   <<"not-a-known-client">>)) end},
      {"fetch_client with missing org",
        fun() ->
                S = chef_otto:connect(),
                OID = chef_otto:fetch_org_id(S, <<"not-a-known-org">>),
                ?assertEqual(not_found,
                             chef_otto:fetch_client(S, OID,
                                                    <<"clownco-validator">>)) end}]}.
