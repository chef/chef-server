%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@opscode.com>
%% @copyright {{copyright_year}} {{copyright_holder}}

-module(mover_transient_migration_queue).

-behaviour(gen_server).

-export([start_link/0,
         initialize_queue/1,
         length/0,
         next/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {q :: term()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{q = queue:new()}}.

%% populate the queue with items contained in  Items
initialize_queue(Items) when is_list(Items) ->
    gen_server:call(?MODULE, {init_queue, Items}).

length() ->
    gen_server:call(?MODULE, length).

next() ->
    gen_server:call(?MODULE, next).

handle_call({init_queue, Items}, _From, State) ->
    {reply, ok, State#state{q = queue:from_list(Items)}};
handle_call(length, _From, #state{q = Q} = State) ->
    {reply, queue:len(Q), State};
handle_call(next, _From, #state{q = Q} = State) ->
    {Result, NewQ} = queue:out(Q),
    {reply, result_to_reply(Result), State#state{q = NewQ}};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

result_to_reply({value, Item}) -> Item;
result_to_reply(empty) -> {ok, no_more}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

