-module(chef_sql_clients).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("chef_db.hrl").
-include("chef_types.hrl").

%%%======================================================================
%%% CLIENTS
%%%======================================================================
make_client(Prefix) ->
    AzId = chef_test_suite_helper:make_az_id(Prefix),
    #chef_client{
        server_api_version = ?API_MIN_VER,
	    id = AzId,
	    org_id = chef_test_suite_helper:the_org_id(),
	    name = AzId,
	    authz_id = AzId,
            admin = true,
	    validator = false,
	    public_key =
	    <<"MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAwxOFcrbsV7bEbqzOvW5u"
	      "W5lyB23qsenlUdIGyRttqzGEaki01s7X+PpYy4BLfmVVmA6A6FCbL38CzzTUFX1a"
	      "p6LYQR2Pb1tYjBjZZMUiVnjEgl12Zd1JF8dsPMj2BgPggx5GaGLvCOsajZ0YCDgW"
	      "WkoO/HAEbztFIx2jdSCyD0ZH0ep4fSGDjmkN+5XurS0dBH8J5qPeJjriA/s/RzUb"
	      "ULjr3gvfg49onHxr/kTKbhc78GBOfKSH1ftECCoWnidadW7/lfKbAZ3xiSjLsIxS"
	      "KxavHMeCuSgyReDZpsFOn2Saie26jvLxWrGyn870yIh36wMvCvWKwUQPnluSnstJ"
	      "xwIDAQAB">>,
	    pubkey_version = 1,
	    last_updated_by = chef_test_suite_helper:actor_id(),
	    created_at = {datetime, {{2011,10,1},{16,47,46}}},
	    updated_at = {datetime, {{2011,10,1},{16,47,46}}}
    }.

insert_client_data() ->
    Clients = [ make_client(<<"client01">>), make_client(<<"client02">>) ],
    Expected = lists:duplicate(length(Clients), {ok, 1}),
    Results = [itest_util:create_record(Client) || Client <- Clients ],
    ?assertEqual(Expected, Results).

fetch_client_data() ->
    Expected = make_client(<<"client03">>),

    % Assume an existing client
    ?assertEqual({ok, 1}, itest_util:create_record(Expected)),

    {ok, Got} = itest_util:fetch_record(Expected),
    ?assertEqual(Expected, Got).

bulk_fetch_client_data() ->
  Clients =  [ make_client(<<"client_bulk", Num/binary>>) || Num <- [ <<"0">>, <<"1">>, <<"2">> ] ],
  [ ?assertEqual({ok, 1}, itest_util:create_record(C)) || C <- Clients ],
  Ids = [ C#chef_client.id || C <- Clients ],
  Expected = Clients,

  {ok, Got} = chef_sql:bulk_get_clients(?API_MIN_VER, Ids),
  ?assertEqual(length(Got), 3),
  ?assertEqual(Expected, Got).

delete_client_data() ->
    Existing = make_client(<<"client04">>),

    % Assume an existing client
    ?assertEqual({ok, 1}, itest_util:create_record(Existing)),
    {ok, BeforeDelete} = itest_util:fetch_record(Existing),
    ?assertEqual(Existing, BeforeDelete),

    ?assertEqual({ok, 1}, itest_util:delete_record(Existing)),
    % Is {ok, not_found} correct?
    % This is what chef_sql:fetch_object returns
    ?assertEqual({ok, not_found}, itest_util:fetch_record(Existing)).
