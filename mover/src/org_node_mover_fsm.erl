-module(org_node_mover_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, migrate/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         mark_migration_start/2,
         verify_read_only/2,
         get_node_list/2,
         migrate_nodes/2,
         mark_migration_end/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(state, {org_name,
                org_id,
                batch_size,
                chef_otto,
                node_count = 0,
                node_list = [],
                node_marker}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

migrate(Pid) ->
    gen_fsm:send_event(Pid, go).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------
%% S = node_mover:connect("localhost", 8484).
%% Config = [{org_name, <<"userprimary">>}, {org_id, <<"60d3ed4da757402ea5dd6da9131baeef">>}, {batch_size, 3}, {chef_otto, S}].
%% {ok, Pid} = org_node_mover_fsm:start_link(Config).
%% org_node_mover_fsm:migrate(Pid).
init(Config) ->
    OrgName = proplists:get_value(org_name, Config),
    OrgId = proplists:get_value(org_id, Config),
    BatchSize = proplists:get_value(batch_size, Config),
    Otto = proplists:get_value(chef_otto, Config),
    {ok, mark_migration_start, #state{org_name = OrgName,
                                      org_id = OrgId,
                                      batch_size = BatchSize,
                                      chef_otto = Otto}}.

mark_migration_start(_Event, #state{org_name = Org}=State) ->
    error_logger:info_msg("Starting migration of ~s~n", [Org]),
    {next_state, verify_read_only, State, 0}.

verify_read_only(_Event, #state{org_name = Org}=State) ->
    error_logger:info_msg("Verifying ~s is in READ-ONLY state~n", [Org]),
    %% if in read-only, proceed to next state, otherwise sleep and
    %% next state is self.
    {next_state, get_node_list, State, 0}.

get_node_list(_Event, #state{org_name = Org, org_id = OrgId,
                             batch_size = BatchSize,
                             chef_otto = S}=State) ->
    error_logger:info_msg("Migrating nodes for ~s~n", [Org]),
    %% here, we'll bulk fetch nodes using batch_size, migrate and then
    %% transition to ourselves. Only when we don't find any nodes do
    %% we transition to the wrap up state.

    %% get full node list.  Write this to a nodes to migrate file
    NodeList = chef_otto:fetch_nodes_with_ids(S, OrgId),
    NodesToMigrate = << <<Name/binary, "\n">> || {Name, _} <- NodeList >>,
    file:write_file(<<Org/binary, "_nodes_to_migrate.txt">>, NodesToMigrate),
    
    {next_state, migrate_nodes,
     State#state{node_list = safe_split(BatchSize, NodeList)}, 0}.
    
migrate_nodes(_Event, #state{node_list = {[], []}}=State) ->
    {next_state, mark_migration_end, State, 0};
migrate_nodes(_Event, #state{node_list = {NodeBatch, NodeList},
                             org_name = Org,
                             %% org_id = OrgId,
                             batch_size = BatchSize,
                             chef_otto = _S}=State) ->
    error_logger:info_msg("Migrating nodes for ~s~n", [Org]),
    %% here, we'll bulk fetch nodes using batch_size, migrate and then
    %% transition to ourselves. Only when we don't find any nodes do
    %% we transition to the wrap up state.
    lists:foreach(fun(N) ->
                          io:format("migrate node: ~p~n", [N])
                  end, NodeBatch),
    Migrated = << <<Name/binary, "\n">> || {Name, _} <- NodeBatch >>,
    file:write_file(<<Org/binary, "_nodes_complete.txt">>, Migrated, [append]),
    io:format("****** BATCH COMPLETE\n"),
    {next_state, migrate_nodes,
     State#state{node_list = safe_split(BatchSize, NodeList)}, 0}.

mark_migration_end(_Event, #state{org_name = Org}=State) ->
    error_logger:info_msg("Ending migration of ~s~n", [Org]),
    {stop, normal, State}.

    
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.
