%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright 2011 Opscode, Inc.
-module(mover_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    case mover_sup:start_link() of
        {ok, Pid} ->
            case chef_sql:init() of
                ok ->
                    {ok, Pid};
                Error ->
                    exit(Pid, kill),
                    Error
            end;
        Error ->
            Error
    end.


stop(_State) ->
    ok.

%% %% Internal functions
%% insure_deps() ->
%%     case application:load(mover) of
%%         ok ->
%%             {ok, Deps} = application:get_key(mover, applications),
%%             insure_deps(Deps);
%%         Error ->
%%             Error
%%     end.

%% %% Don't start chef_common to avoid starting node indexer
%% insure_deps([]) ->
%%     ok;
%% insure_deps([H|T]) when H =:= chef_common->
%%     insure_deps(T);
%% insure_deps([H|T]) ->
%%     case application:start(H) of
%%         ok ->
%%             insure_deps(T);
%%         {error, {already_started, H}} ->
%%             insure_deps(T);
%%         Error ->
%%             Error
%%     end.
