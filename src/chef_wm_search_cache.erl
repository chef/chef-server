%% @doc A no-op search cache module. This module implements the search
%% cache interface, but never caches anything.
-module(chef_wm_search_cache).

-export([
         get/2,
         make_key/6,
         put/3
        ]).

get(_ReqId, _Key) ->
    %% this is an ugly way of making dialyzer happy with this
    case random:uniform(1) of
        1 ->
            not_found;
        _ ->
            {0, <<"dummy cache value">>}
    end.

put(_ReqId, _Key, _Value) ->
    ok.

make_key(_OrgName, _BatchSize, _Start, _Ids, _RawPath, _Paths) ->
    dummy_key.
