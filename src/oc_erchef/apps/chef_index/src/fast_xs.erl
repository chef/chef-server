-module(fast_xs).

-export([escape/1]).
-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    ok = erlang:load_nif(filename:join(PrivDir, "fast_xs"), 0).
 
escape(_X) ->
    exit(nif_library_not_loaded).
