-module(test_utils).

-export([test_cleanup/1,
         nuke_nodes_from_solr/0,
         force_solr_commit/0]).

-define(SOLR_URL, "http://localhost:8983").

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
    application:stop(ibrowse),
    case whereis(inet_gethost_native_sup) of
        P when is_pid(P) ->
            inet_gethost_native:terminate(shutdown, P);
        _ ->
            ok
    end.

nuke_nodes_from_solr() ->
    error_logger:info_msg("nuking nodes from solr~n"),
    Body = <<"<delete><query>X_CHEF_type_CHEF_X:node</query></delete>">>,
    ibrowse:send_req(?SOLR_URL ++ "/solr/update",
                     [{"content-type", "application/xml"}],
                     post, Body).

force_solr_commit() ->
    %% This is gross, but not sure how we can get around it in the
    %% context of an integration test.  After performing an action
    %% that creates or updates an object, erchef will place the object
    %% on a queue (rabbitmq) for indexing.  We have to account for the
    %% latency of the opscode-expander system to pull the message from
    %% the queue, process it, and send it to solr.  So we sleep.  Then
    %% we trigger a synchronous commit in solr to make sure it is
    %% available.
    timer:sleep(100),
    Url = ?SOLR_URL ++ "/solr/update",
    Body = <<"<commit waitSearcher=\"true\" waitFlush=\"true\" softCommit=\"false\" />">>,
    {ok, "200", _, _} = ibrowse:send_req(Url, [{"content-type", "application/xml"},
                                               {"accept", "application/xml"}],
                                         post, Body).


