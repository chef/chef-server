-module(bifrost_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    %% erlang 19.3.x SIGTERM changes caused runit failures.
    %% this reverts to previous working SIGTERM behavior.
    os:set_signal(sigterm, default),

    { ok, AppList } =  application:get_key(bifrost, included_applications),
    [ application:ensure_all_started(App, permanent) || App <- AppList ],
    case os:getenv("DEVVM") of
        "1" ->
            {ok, Dir} = file:get_cwd(),
            SrcDir =  filename:join([Dir, "../../../../../..", "external-deps"]),
            EbinDir = filename:join([Dir, "../../../../../..", "external-deps/ebin"]),
            application:set_env(sync, src_dirs, {add, [{SrcDir,
                                                        [{outdir, EbinDir}]}]}),
            application:set_env(sync, sync_method, scanner),
            application:start(sync);
      _ -> ok
    end,
    bifrost_sup:start_link().

stop(_State) ->
    ok.
