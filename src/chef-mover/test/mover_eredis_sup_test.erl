-module(mover_eredis_sup_test).

-include_lib("hoax/include/hoax.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

?HOAX_FIXTURE.

%% This test is verifying that the eredis_client start_link -> init code path
%% is traversed.  Previously, there was api drift in eredis that prevented
%% the start_link path from being followed.
start_link_should_start_client() ->
    process_flag(trap_exit, true),
    ?assertMatch({error, {connection_error, {connection_error,_}}}, mover_eredis_sup:eredis_start_link("host", 1234, "")).
