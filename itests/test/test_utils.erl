-module(test_utils).

-export([test_cleanup/1]).

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
    application:stop(public_key),
    application:stop(ssl),
    case whereis(inet_gethost_native_sup) of
        P when is_pid(P) ->
            inet_gethost_native:terminate(shutdown, P);
        _ ->
            ok
    end.

