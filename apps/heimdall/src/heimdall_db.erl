%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et

-module(heimdall_db).

-export([
         statements/0
        ]).

statements() ->
    Path = filename:join([code:priv_dir(heimdall), "pgsql_statements.config"]),
    {ok, Statements} = file:consult(Path),
    Statements.
