%%%-------------------------------------------------------------------
%% @doc chef_license public API
%% @end
%%%-------------------------------------------------------------------

-module(chef_license_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chef_license_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
