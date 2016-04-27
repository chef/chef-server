%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@chef.io>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%


-module(chef_cache).

-include_lib("stdlib/include/ms_transform.hrl").

-define(MAX_SIZE_OPT, max_size).
-define(TTL_OPT, ttl).
-define(NAME_OPT, name).
-define(OPTIONS, [?MAX_SIZE_OPT, ?TTL_OPT, ?NAME_OPT]).

-define(SIZE_KEY, '$cache_size').
-define(MAX_SIZE_KEY, '$cache_max_size').
-define(TTL_KEY, '$cache_ttl').
-define(NAME_KEY, '$cache_name').
-define(PRUNE_KEY, '$cache_prune').
-define(IF_EXISTS(Name, Expr), case cache_exists(Name) of
                                    true -> Expr;
                                    false -> no_cache
                                end).

-export([init/1,
         close/1,
         clear/1,
         put/3,
         get/2,
         del/2,
         current_size/1,
         max_size/1,
         ttl/1,
         now_ts/0,
         prune_worker/1]).

init(Name) when is_atom(Name) ->
    init([{?NAME_OPT, Name} | envy:get(chef_db, cache_defaults, list)]);

init(Options) when is_list(Options) ->
    {ok, Config} = build_config(Options),
    Name = proplists:get_value(?NAME_KEY, Config),
    ets:new(Name, [named_table, public, {read_concurrency, true},
                   {write_concurrency, true}]),
    write_config(Name, Config),
    ok.

close(Name) ->
    ?IF_EXISTS(Name, ets:delete(Name)).

clear([Name]) when is_list(Name) ->
    clear(list_to_existing_atom(Name));
clear(Name) ->
    ?IF_EXISTS(Name, begin
                         ets:safe_fixtable(Name, true),
                         drop_all(Name, ets:first(Name)),
                         ets:insert(Name, {?SIZE_KEY, 0}),
                         ets:safe_fixtable(Name, false),
                         ok
                     end).

put(Name, Key, Value) when is_binary(Key) orelse is_list(Key) ->
    ?IF_EXISTS(Name, begin
                         prune_entries(Name),
                         true = ets:insert(Name, {Key, Value, now_ts()}),
                         ets:update_counter(Name, ?SIZE_KEY, 1),
                         ok
                     end).

get(Name, Key) when is_binary(Key) orelse is_list(Key) ->
    ?IF_EXISTS(Name, begin
                         prune_entries(Name),
                         Threshold = now_ts() - ttl(Name),
                         case ets:lookup(Name, Key) of
                             [] ->
                                 not_found;
                             [{Key, Value, TS}] ->
                                 if
                                     TS =< Threshold ->
                                         not_found;
                                     true ->
                                         ets:insert(Name, {Key, Value, now_ts()}),
                                         {ok, Value}
                                 end
                         end
                     end).

max_size(Name) ->
    ?IF_EXISTS(Name, begin
                         [{?MAX_SIZE_KEY, Max}] = ets:lookup(Name, ?MAX_SIZE_KEY),
                         Max
                     end).

current_size(Name) ->
    ?IF_EXISTS(Name, begin
                         [{?SIZE_KEY, Size}] = ets:lookup(Name, ?SIZE_KEY),
                         Size
                     end).

ttl(Name) ->
    ?IF_EXISTS(Name, begin
                         [{?TTL_KEY, TTL}] = ets:lookup(Name, ?TTL_KEY),
                         TTL
                     end).

del(Name, Key) when is_binary(Key) orelse is_list(Key) ->
    ?IF_EXISTS(Name, begin
                         true = ets:delete(Name, Key),
                         ets:update_counter(Name, ?SIZE_KEY, -1),
                         ok
                     end).

%% Internal functions
build_config(Options) ->
    build_config(?OPTIONS, Options, []).

build_config([], _Options, Accum) ->
    {ok, [{?SIZE_KEY, 0}|Accum]};
build_config([Opt|T], Options, Accum) ->
    case proplists:get_value(Opt, Options) of
        undefined ->
            error(badarg);
        Value ->
            build_config(T, Options, [{opt_to_key(Opt), Value}|Accum])
    end.

opt_to_key(?MAX_SIZE_OPT) ->
    ?MAX_SIZE_KEY;
opt_to_key(?TTL_OPT) ->
    ?TTL_KEY;
opt_to_key(?NAME_OPT) ->
    ?NAME_KEY.


write_config(Name, Config) ->
    F = fun({Key, Value}) -> ets:insert(Name, {Key, Value}) end,
    [F(Entry) || Entry <- Config].

drop_all(_Name, '$end_of_table') ->
    ok;
drop_all(Name, Key) when is_atom(Key) ->
    drop_all(Name, ets:next(Name, Key));
drop_all(Name, Key) ->
    Next = ets:next(Name, Key),
    ets:delete(Name, Key),
    drop_all(Name, Next).

prune_entries(Name) ->
    case current_size(Name) >= max_size(Name) andalso
        not(is_pruning(Name)) of
        true ->
            spawn(fun() -> prune_monitor(Name) end);
        false ->
            ok
    end.

prune_monitor(Name) ->
    Lock = {Name, self()},
    case global:set_lock(Lock, [node()], 0) of
        true ->
            {Pid, MRef} = spawn_monitor(fun() -> prune_worker(Name) end),
            receive
                {'DOWN', MRef, process, Pid, _Reason} ->
                    mark_prune(Name, false),
                    global:del_lock(Lock)
            end;
        false ->
            ok
    end.

prune_worker(Name) ->
    mark_prune(Name, true),
    MaxSize = max_size(Name),
    TTL = ttl(Name),
    start_pruning(Name, MaxSize, TTL).

start_pruning(Name, MaxSize, TTL) ->
    case current_size(Name) - MaxSize =< 0 of
        true ->
            ok;
        false ->
            Now = now_ts(),
            MS = ets:fun2ms(fun({Key, _, TS}) when Now - TS >= TTL -> Key end),
            do_pruning(Name, MS, TTL, ets:select(Name, MS, 10)),
            timer:sleep(1000),
            start_pruning(Name, MaxSize, TTL)
    end.

do_pruning(_Name, _MS, _TTL, '$end_of_table') ->
    ok;
do_pruning(Name, MS, TTL, {Keys0, _Cont}) ->
    Keys = [Key || Key <- Keys0, not(is_atom(Key))],
    [verify_and_delete(Name, Key, TTL) || Key <- Keys],
    ets:update_counter(Name, ?SIZE_KEY, length(Keys) * -1),
    do_pruning(Name, MS, TTL, ets:select(Name, MS, 10)).

verify_and_delete(Name, Key, TTL) ->
    [{Key, _Value, TS}] = ets:lookup(Name, Key),
    Now = now_ts(),
    case Now - TS >= TTL of
        true ->
            ets:delete(Name, Key);
        false ->
            ok
    end.

now_ts() ->
    Now = os:timestamp(),
    NowSecs = calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(Now)),
    EpochStartSecs = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    NowSecs - EpochStartSecs.

is_pruning(Name) ->
    ets:member(Name, ?PRUNE_KEY).
mark_prune(Name, true) ->
    ets:insert(Name, {?PRUNE_KEY});
mark_prune(Name, false) ->
    ets:delete(Name, ?PRUNE_KEY).

cache_exists(Name) ->
    not(ets:info(Name, name) =:= undefined).
