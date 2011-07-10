-module(chef_common_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chef_common_sup:start_link().

stop(_State) ->
    ok.
