-module(search_integration_test).

-include_lib("eunit/include/eunit.hrl").

%% -export([run/0]).

ensure_ibrowse() ->
    case ibrowse:start() of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        Error ->
            Error
    end.

search_test_() ->
    {foreach,
     % setup
     fun() ->
             ok = ensure_ibrowse(),
             KeyPath = "/tmp/opscode-platform-test/clownco-org-admin.pem",
             {ok, KeyBin} = file:read_file(KeyPath),
             PrivateKey = chef_authn:extract_private_key(KeyBin),
             Client = chef_rest_client:make_chef_rest_client("http://localhost:9898",
                                                             "clownco-org-admin",
                                                             PrivateKey),
             Client
     end,
     fun(_X) ->
             stopping
     end,
     [
       fun(Client) ->
               {"my test",
               fun() ->
                       Got = chef_rest_client:request(Client, "/organizations/clownco/search/nodes?q=*:*&start=0&rows=20&sort=X_CHEF_id_CHEF_X+asc"),
                       ?debugVal(Got),
                       ok
               end}
       end]}.




