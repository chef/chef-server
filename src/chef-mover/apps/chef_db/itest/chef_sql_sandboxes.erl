-module(chef_sql_sandboxes).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("chef_db.hrl").
-include("chef_types.hrl").

%%%======================================================================
%%% SANDBOXES AND CHECKSUMS
%%%======================================================================
make_sandbox(Prefix) ->
    #chef_sandbox{id=itest_util:make_id(Prefix),
                  org_id=itest_util:the_org_id(),
                  created_at = {datetime,{{2011,10,1},{16,47,46}}},
                  checksums=[{<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, false},
                             {<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>, false},
                             {<<"cccccccccccccccccccccccccccccccc">>, false},
                             {<<"dddddddddddddddddddddddddddddddd">>, false}]}.

insert_sandbox() ->
    ?assertEqual({ok, 1},
                 chef_sql:create_sandbox(make_sandbox(<<"abcd">>))).

mark_some_checksums_as_uploaded() ->
    #chef_sandbox{checksums=ChecksumSpec} = make_sandbox(<<"abcd">>),
    %% Hold one aside for checking non-uploaded checksums
    Checksums = [C || {C, false} <- ChecksumSpec, C /= <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>],
    ?assertEqual(ok,
                 chef_sql:mark_checksums_as_uploaded(itest_util:the_org_id(), Checksums)).

check_non_uploaded_checksums() ->
    ?assertEqual([<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>],
                 chef_sql:non_uploaded_checksums(itest_util:make_id(<<"abcd">>), itest_util:the_org_id())).

upload_last_checksum() ->
    ?assertEqual(ok,
                 chef_sql:mark_checksums_as_uploaded(itest_util:the_org_id(), [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>])),
    ?assertEqual([],
                 chef_sql:non_uploaded_checksums(itest_util:make_id(<<"abcd">>), itest_util:the_org_id())).

fetch_sandbox() ->
    ActualValue = chef_sql:fetch(#chef_sandbox{org_id = itest_util:the_org_id(),
                                                     id = itest_util:make_id(<<"abcd">>)}),
    ?assertEqual(#chef_sandbox{id=itest_util:make_id(<<"abcd">>),
                               org_id=itest_util:the_org_id(),
                               created_at={datetime,{{2011,10,1},{16,47,46}}},
                               checksums=[{<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, true},
                                          {<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>, true},
                                          {<<"cccccccccccccccccccccccccccccccc">>, true},
                                          {<<"dddddddddddddddddddddddddddddddd">>, true}]},
                 ActualValue).
%% delete the sandbox
delete_sandbox() ->
    ?assertEqual({ok, 1},
                 chef_sql:delete_sandbox(itest_util:make_id(<<"abcd">>))).
