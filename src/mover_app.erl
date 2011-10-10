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
