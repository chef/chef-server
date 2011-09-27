-module(test_utils).

-export([test_cleanup/1,
         nuke_nodes_from_solr/0]).

-include_lib("emysql/include/emysql.hrl").

%% @doc graceful shutdown of apps started by tests
%% 
%% This seems to get rid of the following error reports otherwise seen at the end of a test run:
%% 
%% =ERROR REPORT==== 21-Sep-2011::09:16:50 ===
%% ** Generic server inet_gethost_native_sup terminating 
%% ** Last message in was {'EXIT',<0.136.0>,killed}
%% ** When Server state == {state,inet_gethost_native,undefined,<0.136.0>,
%%                                {local,inet_gethost_native_sup}}
test_cleanup(_State) ->
    application:stop(emysql),
    application:stop(public_key),
    application:stop(ssl),
    case whereis(inet_gethost_native_sup) of
        P when is_pid(P) ->
            inet_gethost_native:terminate(shutdown, P);
        _ ->
            ok
    end.

nuke_nodes_from_solr() ->
    error_logger:info_msg("nuking nodes from solr~n"),
    Body = <<"<delete><query>X_CHEF_type_CHEF_X:node</query></delete>">>,
    Got = ibrowse:send_req("http://localhost:8983/solr/update",
                           [{"content-type", "application/xml"}],
                           post,
                           Body).
