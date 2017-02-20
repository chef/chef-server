-module(noop_session_cache).

%% This module exists to stop session reuse from happening

-behaviour(ssl_session_cache_api).

-export([init/1, terminate/1, lookup/2, update/3, delete/2, foldl/3,
         select_session/2, size/1]).

%%--------------------------------------------------------------------
%% Description: Return table reference. Called by ssl_manager process.
%%--------------------------------------------------------------------
init(_Options) ->
    ok.

%%--------------------------------------------------------------------
%% Description: Handles cache table at termination of ssl manager.
%%--------------------------------------------------------------------
terminate(_Cache) ->
    ok.

%%--------------------------------------------------------------------
%% Description: Looks up a cach entry. Should be callable from any
%% process.
%%--------------------------------------------------------------------
lookup(_Cache, _Key) ->
    undefined.

%%--------------------------------------------------------------------
%% Description: Caches a new session or updates a already cached one.
%% Will only be called from the ssl_manager process.
%%--------------------------------------------------------------------
update(_Cache, _Key, _Session) ->
    ok.

%%--------------------------------------------------------------------
%% Description: Deletes a cache entry.
%% Will only be called from the ssl_manager process.
%%--------------------------------------------------------------------
delete(_Cache, _Key) ->
    ok.

%%--------------------------------------------------------------------
%% Description: Calls Fun(Elem, AccIn) on successive elements of the
%% cache, starting with AccIn == Acc0. Fun/2 must return a new
%% accumulator which is passed to the next call. The function returns
%% the final value of the accumulator. Acc0 is returned if the cache
%% is empty.Should be callable from any process
%%--------------------------------------------------------------------
foldl(_Fun, _Acc0, _Cache) ->
    [].

%%--------------------------------------------------------------------
%% Description: Selects a session that could be reused. Should be
%% callable from any process.
%%--------------------------------------------------------------------
select_session(_Cache, _PartialKey) ->
    [].

%%--------------------------------------------------------------------
%% Description: Size of the cache.
%% Will only be called from the ssl_manager process.
%%--------------------------------------------------------------------
size(_Cache) ->
    0.
