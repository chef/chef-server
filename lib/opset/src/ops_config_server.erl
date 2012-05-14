%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%% @doc
%%% The internal implementation of the config system. There is one
%%% process per named configuration. Configuration names are
%%% registered with gproc and managed by gproc. So when a
%%% configuration is created the no proc is registered with gproc and
%%% when it goes out of existance its unregistered.
%%%
%%% If the a configuration element changes a notification is sent to
%%% all linked processes detailing the nature of the change. Care
%%% should be taken in how configurations are update so as not to
%%% drown the linked processes.
%%% @end
%%%-------------------------------------------------------------------
-module(ops_config_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name, contents=[]}).

%%%===================================================================
%%% Types
%%%===================================================================
-type state() :: record(state).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(opset:name(), opset:config()) ->
                        {ok, pid()} | ignore | {error, Error::term()}.
start_link(Name, Value) ->
    gen_server:start_link(?MODULE, [Name, Value], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(list()) -> {ok, state()} |
                  {ok, state(), Timeout::non_neg_integer()} |
                  ignore |
                  {stop, Reason::term()}.
init([Name, Value]) ->
    true = gproc:reg(opset:form_key(Name)),
    {ok, #state{name=Name, contents=Value}}.

-spec handle_call(Request::term(), From::pid(), state()) ->
                                   {reply, Reply::term(), state()}.
handle_call({get, ItemName}, _From, State=#state{contents=Contents}) ->
    %% Exceptions do no propagate process boundaries so we need to
    %% return a tagged result (we let lists:keysearch do it for us).
    Reply = case lists:keysearch(ItemName, 1, Contents) of
                false ->
                    not_found;
                {value, {ItemName, Value}} ->
                    {ok, Value}
            end,
    {reply, Reply,State};
handle_call({get, ItemName, Default}, _From, State=#state{contents=Contents}) ->
    Reply = case lists:keysearch(ItemName, 1, Contents) of
                false ->
                    Default;
                {value, {ItemName, Value}} ->
                    Value
            end,
    {reply, Reply, State}.

-spec handle_cast(Msg::term(), State::term()) ->
                         {noreply, state()} | {stop, normal, state()}.
handle_cast(delete, State=#state{name=Name,contents=Contents}) ->
    notify(Name, [{delete, Contents}]),
    {stop, normal, State};
handle_cast({delete, ItemName}, State=#state{name=Name,contents=Contents0}) ->
    case lists:keysearch(ItemName, 1, Contents0) of
        false ->
            {noreply, State};
        {value, Item={ItemName, _}} ->
            notify(Name, [{delete, [Item]}]),
            Contents1 = lists:keydelete(ItemName, 1, Contents0),
            {noreply, State#state{contents=Contents1}}
    end;
handle_cast({set, ItemName, ItemValue}, State=#state{name=Name,contents=Contents})->
    NewState =
        State#state{contents=
                        lists:keystore(ItemName, 1, Contents,
                                       {ItemName, ItemValue})},
    notify(Name, [{set, [{ItemName, ItemValue}]}]),
    {noreply, NewState};
handle_cast({set, Items}, State=#state{name=Name,contents=Contents0}) ->
    NewContents = lists:foldl(fun(Item={K,_}, Contents1) ->
                                      lists:keystore(K, 1, Contents1, Item)
                              end, Contents0, Items),
    notify(Name, [{set, Items}]),
    {noreply, State#state{contents=NewContents}};
handle_cast({reset, Config}, State=#state{name=Name,contents=Contents0}) ->
    %% If it is not in the new config then its implicitly deleted. We
    %% grap a list of that before sending the notification.
    Deleted = [Item
               || Item={K,_} <- Contents0, not lists:keymember(K, 1, Config)],
    notify(Name, [{delete, Deleted},
                  {set, Config}]),
    {noreply, State#state{contents=Config}}.

-spec handle_info(Info::term(), State::term()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason::term(), state()) -> ok.
terminate(_Reason, #state{name=Name}) ->
    gproc:unreg(opset:form_key(Name)),
    ok.

-spec code_change(OldVsn::term(), state(), Extra::term()) ->
                         {ok, NewState::term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
notify(Name, ChangedConfigs) ->
    gproc:send({p, l, opset:event_name(Name)}, {self(), Name, ChangedConfigs}).
