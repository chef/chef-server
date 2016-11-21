%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@chef.io>
%% @copyright {{copyright_year}} {{copyright_holder}}

-module(mover_transient_migration_queue).

-behaviour(gen_server).

-export([start_link/0,
         initialize_queue/2,
         length/1,
         next/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {q :: list(term())}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{q = []}}.

%% populate the queue with items contained in  Items
initialize_queue(Key, Items) when is_list(Items) ->
    gen_server:call(?MODULE, {init_queue, Key, Items});
initialize_queue(Key, none) ->
    gen_server:call(?MODULE, {init_queue, Key, []}).

%% How many items in the queue?
length(Key) ->
    gen_server:call(?MODULE, {length, Key}).

%% Get the next item from the head of the queue.
next(Key) ->
    gen_server:call(?MODULE, {next, Key}).

handle_call({init_queue,Key, Items}, _From, #state{q = Q} = State) ->
    {reply, ok, State#state{q = lists:keystore(Key, 1, Q, {Key, Items})}};
handle_call({length,Key}, _From, #state{q = Q} = State) ->
    case lists:keyfind(Key,1, Q) of
        {Key, List} when is_list(List) ->
            {reply, erlang:length(List), State};
        _ ->
            {reply, 0, State}
    end;
handle_call({next,Key}, _From, #state{q = KeyToListPL} = State) ->
    case lists:keyfind(Key,1, KeyToListPL) of
        {Key, [Next | Rest]} ->
            {reply, Next, State#state{q = lists:keyreplace(Key, 1, KeyToListPL, {Key, Rest})}};
        _ ->
            {reply, {ok, no_more}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
