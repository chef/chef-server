-module(chef_sql_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("emysql/include/emysql.hrl").
-include("../src/chef_sql.hrl").

%% fetch_user UserName test
%% - found
%% - not_found
%% - error
fetch_user_test_() ->
    {foreach,
     fun() ->
             meck:new(emysql)
     end,
     fun(_) ->
             meck:unload()
     end,
     [
      {"a user is found",
       fun() ->
               Alice = "alice",
               meck:expect(emysql, prepare,
                           fun(fetch_user_stmt, _) ->
                                   ok
                           end),
               meck:expect(emysql, execute,
                           fun(erchef_pool, fetch_user_stmt, ["alice"]) ->
                                   example_result_packet()
                           end),
               ?assertEqual(#chef_user{id = <<"a1">>,
                                       authz_id = <<"b2">>,
                                       username = <<"alice">>,
                                       pubkey_version = 1,
                                       public_key = <<"key data">>},
                            chef_sql:fetch_user(Alice))       
       end
      }
     ]}.

%% example result_packet record returned by emysql for a successful
%% query.
example_result_packet() ->
    {result_packet,9,
     [{field,2,<<"def">>,<<"opscode_chef">>,<<"users">>,
       <<"users">>,<<"id">>,<<"id">>,254,<<>>,33,96,20483,0},
      {field,3,<<"def">>,<<"opscode_chef">>,<<"users">>,
       <<"users">>,<<"authz_id">>,<<"authz_id">>,254,<<>>,33,96,
       20485,0},
      {field,4,<<"def">>,<<"opscode_chef">>,<<"users">>,
       <<"users">>,<<"username">>,<<"username">>,253,<<>>,33,765,
       20485,0},
      {field,5,<<"def">>,<<"opscode_chef">>,<<"users">>,
       <<"users">>,<<"pubkey_version">>,<<"pubkey_version">>,3,
       <<>>,63,11,4097,0},
      {field,6,<<"def">>,<<"opscode_chef">>,<<"users">>,
       <<"users">>,<<"public_key">>,<<"public_key">>,252,<<>>,33,
       196605,16,0}],
     [[<<"a1">>,
       <<"b2">>,
       <<"alice">>,1,
       <<"key data">>]],
     <<>>}.
