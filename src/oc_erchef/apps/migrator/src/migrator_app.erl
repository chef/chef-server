%%%-------------------------------------------------------------------
%% @doc migrator public API
%% @end
%%%-------------------------------------------------------------------

-module(migrator_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%
% API
%
start(_StartType, _StartArgs) ->
    %% This is lazy, but our supervisor is unlikely to terminate unless we've gone catastrophically
    %% wrong somewhere - so I'm going to create the statement binary cache as a public ets table here
    %% to ensure that it's around for the life of the application.
    %%
    %% We could arrange proper ownership/inheritence if we want to stick with ets for this.
    %% binary_statement_cache = ets:new(binary_statement_cache, [named_table, set, public,
    %%                                                           {write_concurrency, false}]),
    migrator_sup:start_link().

%
% Callbacks
%
stop(_State) ->
    ok.
