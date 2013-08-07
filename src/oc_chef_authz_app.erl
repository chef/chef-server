-module(oc_chef_authz_app).


-behaviour(application).
%% API
-export([start/0, stop/0]).
%% Application callbacks
-export([start/2, stop/1]).

start()->
  start(type,args).

stop()->
  stop(state).

start(_StartType, _StartArgs) ->
  ok = oc_chef_authz_http:create_pool(),
  oc_chef_authz_sup:start_link().

stop(_State) ->
  ok = oc_chef_authz_http:delete_pool(),
  ok.
