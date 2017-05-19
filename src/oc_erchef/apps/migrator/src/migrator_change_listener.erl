%% ex: ts=4 sw=4 et
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-

-module(migrator_change_listener).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([%% API functions
         start_link/0,

        %% gen_server behaviour:
         init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         handle_info/2,
         code_change/3]).




%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, []).

%% gen_server functions
init(_Args) ->
    % Doing a port for now. May be sufficient, but might also wwant to considerj
    % a direct pgsql connection to the DB as an appropriate user.
    {ok, Password} = chef_secrets:get(<<"postgresql">>, <<"db_superuser_password">>),
    PortSettings = [ {line, 1024},
                     {args, ["--start", "-h", "127.0.0.1",
                             "-U", "opscode-pgsql",
                             "-S", "regression_slot", % slot name -
                                                      %  you *did* remember to run
                             %SELECT * FROM pg_create_logical_replication_slot('regression_slot', 'test_decoding');
                             %                        % didn't you? Come back when you're ready.
                             "-f", "-", % out to stdout
                             "-d", "opscode_chef",
                             % Options taken from here:
                             %https://doxygen.postgresql.org/test__decoding_8c.html#ae9ad7b4e35a8ca2df15233e16557cbcf
                             "-o", "skip-empty-xacts",
                             %"-o", "force-binary", % experimenting to see the diff it makes
                             "--no-password"]},
                     {env, [{"PGPASSWORD", Password}]},
                      use_stdio,
                      binary
                      ],
    Port = open_port({spawn_executable,
                      "/opt/opscode/embedded/bin/pg_recvlogical"}, PortSettings),
    {ok, #{port => Port, txid => undefined}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% Lazily not handling neol - looks like nothing approaches the arbitrary 1k I gave it above.
% In the real world, this wouldn't do...
handle_info({_Port, {data, {eol, Message}}}, State) ->
    slurp_data(Message),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
%%
%%

% Formatting notes:
%
% This is what accounts for the basic format of attributes/columns:
% https://doxygen.postgresql.org/test__decoding_8c.html#a3986a57a0308de0150ebd45f7734d464
%
% The rough shape appears to be:
%
% BEGIN $txid
% table $schemaname.$tablename: $fieldname1[$datatype1]:'$value1' $fieldname2[$datatype2]:'$value2' $fieldnamen[$datatypen]:'$valuen'
% COMMIT $txid
%
% Observations:
% * literal 'table' can presumably be other things when appropriate
% * VALUE can span multiple lines when a literal newline is present in the data.
           % it can also presumable include : and ' - so how do we determine
           % when we're at the end of hte field...
% * subsequent fields can be appended to the end of the same line of a multi-line value
%
% So far it does not seem that TX data is intermixed, and reading the decoding code
% seems to agree.
slurp_data(<<"BEGIN ", _TXID/binary>>) ->
    % Ideally we'd track this txid in state and reconcile it
    ok;
slurp_data(<<"COMMIT ", _TXID/binary>>) ->
    % This is where we'd clear it from state...
    ok;
slurp_data(<<"table: ",Rest/binary>>) ->
    % Here is where things get fun. We could have next a single statement on a single line -
    % or it could be the beginning of a statement that spans mutiple lines
    % table $schemaname.$tablename: $fieldname1[$datatype1]:'$value1' $fieldname2[$datatype2]:'$value2' $fieldnamen[$datatypen]:'$valuen'
    lager:info("~p", [Rest]);
slurp_data(All) ->
    % here we'd append data to the last value received - it's a continuation.
    % At least assuming that "table" is the only leader/object type we'll see...
    lager:info(">> ~p", [All]).


%
%
