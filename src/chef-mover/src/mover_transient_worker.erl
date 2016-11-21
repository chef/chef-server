%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
%% @copyright 2013 Opscode, Inc.
%%
%% @doc a gen_fsm that performs a generic migration using the supplied fun.

-module(mover_transient_worker).

-record(state, {
                 object_id :: fun(),
                 processor_args,
                 results :: term(),
                 callback_module
               }).

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

init({CallbackModule, ObjectId, ProcessorArgs}) ->
    State = #state{object_id = ObjectId, processor_args = ProcessorArgs, callback_module = CallbackModule},
    {ok, migrate, State, 0}.

migrate(timeout, #state{processor_args = ProcessorArgs, callback_module = CallbackModule} = State) ->
    try erlang:apply(CallbackModule, migration_action, ProcessorArgs) of
        ok ->
            {next_state, complete, State#state{results = ok}, 0};
        Error ->
            stop_with_failure(State#state{results = Error}, Error, migrate)
    catch
        ErrorType:Reason ->
            HeadlineIoList = io_lib:fwrite("Error in ~p:migration_action", [CallbackModule]),
            Headline = lists:flatten(HeadlineIoList),
            error_logger:error_report({migration_action, Headline}),
            error_logger:error_msg("migration_action args~n~p~n", [ProcessorArgs]),
            error_logger:error_msg("~p~n~p~n~p~n", [ErrorType, Reason, erlang:get_stacktrace()]),
            stop_with_failure(State#state{results = Reason}, Reason, migrate)
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
