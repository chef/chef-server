-module(test_util).

-include_lib("eunit/include/eunit.hrl").

-export([setup/1,
         cleanup/1
        ]).

setup(MockedModules) ->
    [meck:new(M) || M <- MockedModules].

cleanup(MockedModules) ->
    [ ?assert(meck:validate(M)) || M <- MockedModules],
    [ meck:unload(M) || M <- MockedModules].

