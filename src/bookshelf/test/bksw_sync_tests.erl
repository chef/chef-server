-module(bksw_sync_tests).

-include_lib("eunit/include/eunit.hrl").

base_dir() ->
    <<"/tmp/fake-test-dir/">>.

remote() ->
    <<"rsync://example.com:111/bookshelf">>.

full_run_args() ->
    ["rsync -r --delete --delete-missing-args -vv /tmp/fake-test-dir/ rsync://example.com:111/bookshelf",
     [stdout,stderr,sync]].

partial_run_args() ->
    ["rsync -r --delete --delete-missing-args -vv --files-from .bookshelf-eunit-test-tmpfile-will-delete /tmp/fake-test-dir/ rsync://example.com:111/bookshelf",
     [stdout,stderr,sync]].

bksw_sync_test_() ->
    {foreach,
     fun() ->
             meck:new([exec, bksw_io_names]),
             file:delete(".bookshelf-eunit-test-tmpfile-will-delete"),
             meck:expect(bksw_io_names, write_path, [{["bookshelf-tmp", "inc-sync"], ".bookshelf-eunit-test-tmpfile-will-delete"}]),
             meck:expect(exec, run, [{full_run_args(), {ok, something}},
                                     {partial_run_args(), {ok, something}}
                                    ]),
             bksw_sync:start_link(base_dir(), remote())
     end,
     fun(_) ->
             meck:unload([exec, bksw_io_names]),
             bksw_sync:stop()
     end,
     [
      {"bksw_sync:status() returns a sync_state record",
       fun() ->
               %% format is always: expected, actual
               ?assertEqual({sync_state, [], base_dir(), remote()}, bksw_sync:status())
       end
      },
      {"bksw_sync:new(Path) adds Path to the unsynced list as a relative path",
       fun() ->
               bksw_sync:new(<<"/tmp/fake-test-dir/foo/bar/added">>),
               {sync_state, [NewPath], _Dir, _Rem} = bksw_sync:status(),
               ?assertEqual(<<"foo/bar/added">>, NewPath)
       end
      },
      {"bksw_sync:delete(Path) adds Path to the unsynced list as a relative path",
       fun() ->
               bksw_sync:delete(<<"/tmp/fake-test-dir/foo/bar/deleted">>),
               {sync_state, [NewPath], _Dir, _Rem} = bksw_sync:status(),
               ?assertEqual(<<"foo/bar/deleted">>, NewPath)
       end
      },
      {"bksw_sync eventually runs a rsync with any partial updates after an addition",
       fun() ->
               bksw_sync:new(<<"/tmp/fake-test-dir/foo/bar/added">>),
               ?assertEqual(ok, meck:wait(exec, run, partial_run_args(), 3000))
       end
      },
      {"bksw_sync eventually runs a rsync with any partial updates after a delete",
       fun() ->
               bksw_sync:delete(<<"/tmp/fake-test-dir/foo/bar/deleted">>),
               ?assertEqual(ok, meck:wait(exec, run, partial_run_args(), 3000))
       end
      },
      {"bksw_sync:sync with no pending updates does not call rsync",
       fun() ->
               meck:reset(exec), % resets the call counts in meck
               bksw_sync:sync(),
               bksw_sync:status(),
               ?assertNot(meck:called(exec, run, partial_run_args()))
       end
      },
      {"bksw_sync runs a full rsync run on startup",
       fun() ->
               ?assertEqual(ok, meck:wait(exec, run, full_run_args(), 3000))
       end
      },
      {"bksw_sync:full_sync triggers a full rsync run",
       fun() ->
               bksw_sync:full_sync(),
               %% The 2 is needed here to account for the full_sync that already ran on startup
               ?assertEqual(ok, meck:wait(2, exec, run, full_run_args(), 1000))
       end
      }
     ]}.
