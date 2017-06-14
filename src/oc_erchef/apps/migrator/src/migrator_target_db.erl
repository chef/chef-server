%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
%% Copyright 2017 Chef Software, Inc. All Rights Reserved.
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

%% This applies SQL updates to the target system directly. It maintains its own connection.
%% We may look at having multiple instances of these (one per DB or more) down the road.
-module(migrator_target_db).
-behaviour(gen_server).


% When you invoke execute/1:
%
% This module maintains a two-level cache:
% 1. statements converted to binary strings with placeholders, suitable for preparing;
%    and
% 2. per-postgres-connection prepared statement references
%    The per-connection cache is specific to this process and it's pg connection.
%
%
%  - check the per-connection cache for the statement. Return that reference if it exists, otherwise:
%  - check the binary cache for a pre-assembled binary to represent this combination
%    of entity, operation, and fields.
%       - if nothing is found, parse the statement into this form and cache it in the binary cache
%    - execute postgres prepare operations for the statement and cache the reference to it.
%  Return the prepared statement reference to the caller.
%
%
%  The binary cache is keyed to phash2(InputTXTerm). This cache is kept in an ets table
%  so that it will survive restarts of the cache process.
%
%  The prepared statement reference cache is keyed to {ConnId, phash2(InputTxTerm)}
%
-define(SERVER, ?MODULE).

-export([
         %% API functions
         start_link/1,
         execute/1,
         %% gen_server behaviour:
         init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         handle_info/2,
         code_change/3]).


%% API
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

execute(TXTerm) ->
    gen_server:call(?SERVER, {execute, TXTerm}).

%% gen_server functions
init(_Config) ->
    {ok, #{cache => dict:new(),
           conn => connect()}}.

handle_call({execute, TXTerm}, _From, State = #{conn := Conn}) ->
    Result = execute_term(TXTerm, Conn),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal
%%
connect() ->
    {ok, Conn} = epgsql:connect("127.0.0.1", "migration_user", "password", [{database, "opscode_chef_target"}]),
    epgsql:equery(Conn, <<"SET session_replication_role=replica">>, []),
    Conn.

execute_term({tx_start, TXID}, _C) ->
    lager:info("Starting replay of: ~p", [TXID]),
    ok;
execute_term({tx_end, TXID}, _C) ->
    lager:info("Finished replay of: ~p", [TXID]),
    ok;
execute_term({_Entity, _Operation, { _Fields, Values }} = Term, Conn) ->
    Query = migrator_encode:encode(Term),
    lager:info("EXECUTE: ~p with data ~p", [Query, Values]),
    Res = epgsql:equery(Conn, Query, Values),
    lager:info("RESULT: ~p", [Res]),
    ok.

    % Steps here:
    % Is phash2 of {Entity, Operation, Fields} in binary_statement_cache ets table?
    % If not, migrator_encoder:encode(Term)
    % If so, get the binary query from there.
    %
    % Is phash2 of BinaryStatement in this processes's cache?
    % if no:
    %   prepare it, bind it, put it in cache
    %
    % Bind the statement to Values, execute.
    % Don't forget to reindex on target server!
