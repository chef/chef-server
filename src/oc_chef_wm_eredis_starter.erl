%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @doc Temporary `gen_server' to add eredis clients to the
%% `oc_chef_wm_eredis_sup' supervisor in an asynchronous fashion.
%%
%% The purpose of this server is to allow `oc_chef_wm' to start and
%% serve requests if search caching is enabled, but redis is
%% unavailable. Without this, the failures we will see when attempting
%% to start eredis clients would prevent the entire stack from
%% starting. With this async mechanism, everything starts and eredis
%% clients are started as available. If the eredis client start fails,
%% this server sleeps and tries again. The retries repeat until the
%% desired number of clients is reached at which point this server
%% stops.
%%
%% This server is placed after the `oc_chef_wm_eredis_sup' supervisor
%% in the top-level `oc_chef_wm_sup' supervisor. The purpose of this
%% server is to add children to the eredis sup in an async fasion so
%% that erchef can come up and function if redis is down, but will
%% pickup connections to redis when redis becomes available if search
%% caching is enabled.
%%
-module(oc_chef_wm_eredis_starter).
-behaviour(gen_server).

%% API
-export([
         eredis_start_wrapper/3,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(SLEEP_AFTER_ERROR, 5000).

-record(state, {ok_count = 0,
                want_count = 0}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%
%% callbacks
%%

init([]) ->
    ClientCount = envy:get(oc_chef_wm, eredis_client_pool_size, 0, non_neg_integer),
    ChildCount = child_count(),
    State = #state{want_count = ClientCount, ok_count = ChildCount},
    case ClientCount of
        0 ->
            error_logger:info_msg("search cache DISABLED~n");
        _ ->
            error_logger:info_msg("search cache ENABLED  ~B/~B senders~n",
                                  [ChildCount, ClientCount])
    end,
    {ok, State, 0}.

handle_call(_, _From, State) ->
    {reply, unhandled, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, #state{want_count = 0} = State) ->
    %% we either didn't want any or we've created all that we wanted
    %% and this server is no longer needed.
    {stop, normal, State};
handle_info(timeout, #state{ok_count = OkCount, want_count = WantCount} = State) ->
    %% we could move these into server state, but for now this is fine.
    Host = envy:env(oc_chef_wm, redis_host, string),
    Port = envy:env(oc_chef_wm, redis_port, pos_integer),
    RedisDb = envy:env(oc_chef_wm, redis_db, 0, non_neg_integer),
    StartUp = {?MODULE, eredis_start_wrapper, [Host, Port, RedisDb]},
    %% Query supervisor for current child count so that if we've been
    %% restarted, we give additional children correct Ids.
    Id = child_count(),
    ChildId = make_id(Id),
    ChildSpec = {ChildId, StartUp, permanent,
                 brutal_kill, worker, [eredis]},
    case supervisor:start_child(oc_chef_wm_eredis_sup, ChildSpec) of
        {error, Error} ->
            error_logger:error_report({eredis_start_failed,
                                       {Host, Port, RedisDb}, Error}),
            timer:sleep(?SLEEP_AFTER_ERROR),
            {noreply, State, 0};
        {ok, Pid} ->
            error_logger:info_msg("created eredis client ~p~n", [{ChildId, Pid}]),
            {noreply, State#state{want_count = WantCount - 1, ok_count = OkCount + 1}, 0}
             end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Wrapper around `eredis:start_link/3` that registers the
%% created client with pg2.
eredis_start_wrapper(Host, Port, RedisDb) ->
    case eredis:start_link(Host, Port, RedisDb) of
        {ok, Pid} ->
            ok = pg2:join(redis_search_cache, Pid),
            {ok, Pid};
        Error ->
            Error
    end.

make_id(I) ->
    "eredis_" ++ integer_to_list(I).

child_count() ->
    proplists:get_value(specs, supervisor:count_children(oc_chef_wm_eredis_sup)).
