%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%
%% @doc a gen_fsm that performs a generic migration using the supplied fun.

-module(mover_transient_worker).

-record(state, {
                 object_id :: fun(),
                 processor_fun :: fun(),
                 results :: term()
               }).

%% Comment this out for VIM syntax check/compiles to work.
-compile([{parse_transform, lager_transform}]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% API
-export([start_link/1]).

%% states
-export([migrate/2,
         complete/2 ]).

-include("mover.hrl").

start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

init({ObjectId, ProcessorFun}) ->
    State = #state{object_id = ObjectId, processor_fun = ProcessorFun},
    {ok, migrate, State, 0}.

migrate(timeout, #state{object_id = ObjectId, processor_fun = ProcessorFun} = State) ->
    case ProcessorFun(ObjectId) of
        ok ->
            {next_state, complete, State#state{results = ok}, 0};
        Error ->
            stop_with_failure(State#state{results = Error}, Error, migrate)
    end.

complete(timeout, State) ->
    {stop, normal, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(normal, _StateName, #state{ object_id = ObjectId, results = Results }) ->
    lager:info([], "Terminating after successful migration ~p ~p", [ObjectId, Results]);
terminate(_Other, _StateName, #state{ object_id = ObjectId, results = Results }) ->
    lager:info([], "Unexpected error while to migrate ~p ~p", [ObjectId, Results]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%
%% Internal
%%
stop_with_failure(State, Error, LastState) ->
    lager:error([], "Processing aborted in ~p due to error: ~p", [LastState, Error]),
    {stop, {error, Error, LastState}, State}.
