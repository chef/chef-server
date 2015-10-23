-module(cs_escape).

-export([escape/1,
         escape_safe/1,
         escape_term_safe/1,
         escape_phrase_safe/1
        ]).
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
    ok = erlang:load_nif(filename:join(PrivDir, "cs_escape"), 0).

-spec escape(string()) -> string().
escape(_X) ->
    erlang:nif_error(nif_library_not_loaded).

-spec escape_safe(string()) -> string().
escape_safe(_X) ->
    erlang:nif_error(nif_library_not_loaded).

-spec escape_term_safe(string()) -> string().
escape_term_safe(_X) ->
    erlang:nif_error(nif_library_not_loaded).

-spec escape_phrase_safe(string()) -> string().
escape_phrase_safe(_X) ->
    erlang:nif_error(nif_library_not_loaded).
