%% -*- mode: erlang -*-
%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et
-module(bifrost_request_logger).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server API Functions
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% external API Functions
-export([log_access/1]).

-include_lib("webmachine/include/webmachine_logger.hrl").

-compile([{parse_transform, lager_transform}]).

%% We can safely ignore BaseDir because our lager configuration takes
%% care of it for us.
start_link(_BaseDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server API Functions

init([]) ->
    %% We don't actually need a state in this gen_server, so we'll
    %% just use an empty list.
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({log_access, #wm_log_data{response_code=Code}=LogData}, State) ->
    emit_log(Code, generate_msg(LogData)),
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% External API Functions
log_access(#wm_log_data{}=LogData) ->
    gen_server:cast(?MODULE, {log_access, LogData}).

%% Internal Functions
generate_msg(#wm_log_data{response_code = ResponseCode,
                          method = Method,
                          path = Path,
                          notes = Notes}) ->

    ReqId = note(reqid, Notes),
    RequestorId = note(requestor_id, Notes),
    CreatedAuthzId = note(created_authz_id, Notes),
    RawPerfStats = case note(perf_stats, Notes) of
                    undefined -> [];
                    Stats -> Stats
                end,
    PerfStats = filter_perf_stats(ResponseCode, RawPerfStats),

    %% We'll always output information in the log for these fields,
    %% even if their value is 'undefined'.  These are key fields for
    %% monitoring purposes, and always having a value present is
    %% helpful for grouping reports, etc.
    AlwaysLogged = [{status, ResponseCode},
                    {method, Method},
                    {path, Path},
                    {requestor_id, RequestorId} | PerfStats], %% PerfStats is already a list

    %% Other fields, however, can be left out if the value is
    %% undefined.
    %%
    %% create_authz_id will only be present for successful POSTs to
    %% /actors, /groups, /objects, and /containers
    LoggedIfPresent = [{created_authz_id, CreatedAuthzId}],

    FinalFields = lists:append([AlwaysLogged,
                                [{K,V} || {K,V} <- LoggedIfPresent, V /= undefined ]
                               ]),

    log_line(FinalFields).

%% @doc To stay in Splunk's good graces, we'll only add the total
%% request time for 200 responses, and keep all other performance
%% stats for all other responses.
%%
%% (200 is the overwhelming majority of the output of the entire
%% Bifrost API).
filter_perf_stats(200, RawPerfStats) ->
    [ {K,V} || {K,V} <- RawPerfStats, K =:= <<"req_time">> ];
filter_perf_stats(_, RawPerfStats) ->
    RawPerfStats.

emit_log(ResponseCode, Msg) when ResponseCode >= 500 ->
    lager:error(Msg);
emit_log(_ResponseCode, Msg) ->
    lager:info(Msg).

%% @doc Utility method for extracting a value from a Webmachine
%% request's notes... just to cut down on the verbosity a bit.
%%
%% We have to handle the case where Notes is undefined.  Requests that
%% make it through to finish_request/2 in Bifrost will have a notes
%% list (we stuff information we carry around in our #base_state{}
%% record into the notes there so they are available in this logger).
%% However, a request to a path that does not match the dispatch rules
%% (e.g., '/foo/bar/baz') will not flow through finish_request/2,
%% since there is no resource module to handle it. Webmachine does not
%% set a request's notes to the empty list by default, so it will be
%% 'undefined' in this case.  Enough 404s like this in rapid
%% succession would cause the whole system to restart (via a cascade
%% of supervisor restarts), which could be used for a DOS attack.
note(Key, Notes) when is_list(Notes) ->
    proplists:get_value(Key, Notes);
note(_Key, undefined) ->
    undefined.

%% @doc Create an iolist to represent a logging line, given a proplist
%% of key-value pairs.
%%
%% When fully rendered, a typical line might look like this:
%%
%% status=200; method=DELETE; path=/objects/bb93fcce290c975e83732d2b896740d2; module=bifrost_wm_named_resources; reqid=dLQmjjk48CrSTX0kODCr0A==; requestor_id=superuser;
log_line(Pairs) ->
    [[as_io(K), <<"=">>, as_io(V), <<"; ">>] || {K,V} <- Pairs].

%% Cargo-culted from fast_log.erl
%% Convert input to iolist. Handles atoms, lists, binaries, integers, floats, and tuples of
%% the form `{Fmt, Args}' suitable for passing to `io_lib:format/2'. Tuples marked as `{raw,
%% term()}' will be formated using `"~256P"'.
%% This has no dependancies and could be extracted
as_io(X) when is_atom(X) ->
    erlang:atom_to_binary(X, utf8);
as_io(X) when X =:= ""; X =:= <<"">> ->
    <<"empty_string">>;
as_io(X) when is_binary(X); is_list(X) ->
    X;
as_io(X) when is_integer(X) ->
    integer_to_list(X);
as_io(X) when is_float(X) ->
    io_lib:format("~f", [X]);
as_io(X) when is_pid(X) orelse is_reference(X) ->
    io_lib:format("~p", [X]);
as_io({raw, X}) ->
    %% this is last-ditch effort, but may give acceptable results.
    io_lib:format("~256P", [X, 100]);
as_io({Fmt, Args}) ->
    io_lib:format(Fmt, Args).
