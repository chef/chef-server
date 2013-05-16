%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011-2012 Opscode, Inc.

-module(mover_gen_worker).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         migrate/1,
         init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4,
         behaviour_info/1]).

%% States
-export([preload_objects/2,
         mark_preload_start/2,
         mark_migration_start/2,
         verify_read_only/2,
         get_object_list/2,
         migrate_objects/2,
         mark_migration_end/2]).

-include("mover.hrl").

-define(MAX_INFLIGHT_CHECKS, 20).
-define(INFLIGHT_WAIT, 1000).

-define(NOT_DRY_RUN(X),
        case is_dry_run() of
            true -> dry_run;
            false -> X
        end).

-record(state, {
          %% The callback module that customizes this worker
          module,

          %% Name and id of org this worker is migrating
          org_name,
          org_id,
          %% how many objects to bulk_get at a time
          batch_size,
          %% connection context tuple for chef_otto
          chef_otto,

          %% list of {Name, Id} tuples for all objects in the org.  fetched at start of
          %% migration via chef_otto:fetch_objects_with_ids.  Used at end to verify that all
          %% objects made it to MySQL.
          couch_objects = [],

          %% Used to batch groups of objects for migration and track progress.
          object_list = {[], []},

          %% These objects encounted known skippable errors.  Objects in this list are removed
          %% from the objects in couch_objects for final comparison.
          skip_objects = [],

          %% directory where worker logs get written.  These logs are outside of fast_log
          %% and may not be needed.
          log_dir}).

%% Here is the new-style callback specs, which are commented out to remain compatibility
%% with older Erlang builds.

%% %% The name of the authz type used for fetching authz meta data in preload.
%% -callback authz_type() -> atom().

%% %% Creates a Chef record from CouchDB data. `ObjectData'' is ejson term. Return should be a
%% %% record for the Chef object, like `#chef_node{}''.
%% -callback convert_couch_json_to_object_record(OrgId :: binary(),
%%                                               AuthzId :: binary(),
%%                                               RequestorId :: binary(),
%%                                               ObjectData :: {[{term()}]}) ->
%%     tuple().

%% %% Inserts `Object' into the RDBMS, should be a wrapper for `chef_db:create_*'.
%% -callback create_object(OttoConn :: tuple(), Object :: tuple(), RequestorId :: binary()) ->
%%     ok | {conflict, term()} | {error, term()}.

%% %% Puts object in the queue for delete in Solr
%% -callback delete_object_from_solr(Id :: binary(), OrgId :: binary()) -> ok.

%% %% Fetch a list objects as `{Name, Id}' tuples from couchdb. Use `chef_otto:fetch_*_with_ids'.
%% -callback fetch_objects(OttoConn :: tuple(), OrgId :: binary()) ->
%%     [{Name :: binary(), Id :: binary()}].

%% %% Fetch a list of object names from SQL. This is used at the end of the migration to verify
%% %% that everything we attempted to migrated is there and lines up with what we started with
%% %% in couchdb. Use `chef_sql:fetch_*'.
%% -callback fetch_objects_from_sql(OrgId :: binary()) -> [{Name :: binary()}].

%% %% Accessor for the ID of a Chef object record.
%% -callback object_id(ObjectRec :: tuple()) -> ObjectId :: binary().

%% %% Return the name of the Chef object that the callback module handles as a
%% %% binary. E.g. `<<"role">>'. This is used to improve the readability of logging
%% %% messages.
%% -callback object_name() -> ObjectName :: binary().

%% %% Send the Chef object data to Solr.
%% -callback send_object_to_solr(ObjectRec :: tuple(), ObjectDataEjson :: term()) -> ok.

behaviour_info(callbacks) ->
    [{authz_type, 0},
     {convert_couch_json_to_object_record, 4},
     {create_object, 3},
     {delete_object_from_solr, 2},
     {fetch_objects, 2},
     {fetch_objects_from_sql, 1},
     {object_id, 1},
     {object_name, 0},
     {send_object_to_solr, 2}];
behaviour_info(_) ->
    undefined.

start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

migrate(Pid) ->
    gen_fsm:send_event(Pid, start).

init(Config) ->
    Module = proplists:get_value(module, Config),
    OrgName = proplists:get_value(org_name, Config),
    OrgId = proplists:get_value(org_id, Config),
    BatchSize = proplists:get_value(batch_size, Config),
    Otto = proplists:get_value(chef_otto, Config),
    LogDir = <<"object_migration_log/", OrgName/binary>>,
    ensure_dir(LogDir),
    NextState = case prolists:get_value(action, Config) of
                    preload -> mark_preload_start;
                    migrate -> mark_migration_start
                end,
    {ok, NextState, #state{module = Module,
                           org_name = OrgName,
                           org_id = OrgId,
                           batch_size = BatchSize,
                           chef_otto = Otto,
                           log_dir = LogDir}}.

mark_migration_start(start, #state{org_name = OrgName,
                                   batch_size = BatchSize,
                                   module = Module}=State) ->
    log(info, OrgName, "starting migration (~B ~ss per batch)",
        [BatchSize, Module:object_name()]),
    {next_state, verify_read_only, State, 0}.

mark_preload_start(start, #state{org_name = OrgName}=State) ->
    log(info, OrgName, "starting preload"),
    {next_state, preload_objects, State, 0}.

preload_objects(start, #state{org_name = OrgName,
                              org_id = OrgId,
                              module = Module,
                              chef_otto = S}=State) ->
    log(info, OrgName, "preloading ~s data", [Module:object_name()]),
    ObjectList = Module:fetch_objects(S, OrgId),
    log(info, OrgName, "found ~B ~ss for preloading",
        [length(ObjectList), Module:object_name()]),
    [ store_object(Module, S, OrgName, OrgId, ObjectId, ObjectName)
      || {ObjectName, ObjectId} <- ObjectList ],
    log(info, OrgName, "preloading complete"),
    {stop, normal, State}.

%% FIXME: do we need to make the inflight request check object specific?
verify_read_only(timeout, State) ->
    verify_read_only(0, State);
verify_read_only(N, #state{org_name = OrgName}=State)
  when is_integer(N) andalso N =< ?MAX_INFLIGHT_CHECKS ->
    case mover_redis:inflight_requests_for_org(OrgName) of
        [] ->
            log(info, OrgName, "no in-flight writes, proceeding"),
            mover_redis:delete_tracking(OrgName),
            {next_state, get_object_list, State, 0};
        _Pids ->
            log(info, OrgName, "waiting for in-flight requests to finish"),
            gen_fsm:send_event_after(?INFLIGHT_WAIT, N + 1),
            {next_state, verify_read_only, State}
    end;
verify_read_only(N, #state{org_name = OrgName}=State) when N > ?MAX_INFLIGHT_CHECKS ->
    log(err, OrgName, "FAILED: timeout exceeded waiting for in-flight requests"),
    {stop, normal, State}.


get_object_list(timeout, #state{module = Module,
                                org_name = OrgName, org_id = OrgId,
                                batch_size = BatchSize,
                                chef_otto = S,
                                log_dir = LogDir}=State) ->
    log(info, OrgName, "fetching ~s list", [Module:object_name()]),
    %% here, we'll bulk fetch objects using batch_size, migrate and then
    %% transition to ourselves. Only when we don't find any objects do
    %% we transition to the wrap up state.

    %% get full object list.  Write this to a objects to migrate file
    %% TODO: ObjectMod:fetch_objects(S, OrgId)
    ObjectList = Module:fetch_objects(S, OrgId),
    log(info, OrgName, "found ~s count: ~B", [Module:object_name(), length(ObjectList)]),
    log_object_names(LogDir, ObjectList),
    State1 = State#state{object_list = safe_split(BatchSize, ObjectList),
                         couch_objects = ObjectList},
    {next_state, migrate_objects, State1, 0}.

migrate_objects(timeout, #state{object_list = {[], []}}=State) ->
    {next_state, mark_migration_end, State, 0};
migrate_objects(timeout, #state{module = Module,
                                object_list = {ObjectBatch, ObjectList},
                                org_name = OrgName,
                                org_id = OrgId,
                                batch_size = BatchSize,
                                chef_otto = S,
                                skip_objects = SkipObjects0}=State) ->
    log(info, OrgName, "starting batch ~B objects (~B remaining)",
        [length(ObjectBatch), length(ObjectList)]),

    OrgDb = chef_otto:dbname(OrgId),
    %% Get the object meta data first via read-through cache.  This way, we can filter out
    %% objects that don't have authz data (yes, it happens) and avoid pulling the object data
    %% from couch for objects that were already migrated.
    {ObjectMeta, SkipObjects} = fetch_meta_data_for_objects(Module, S, OrgName, OrgId, ObjectBatch,
                                                            SkipObjects0),
    %% only bulk-get data for objects we have meta data on and that have not already been
    %% migrated.
    ObjectCouchIds = [ Id || #object{id = Id} <- ObjectMeta ],
    ObjectDocs = chef_otto:bulk_get(S, OrgDb, ObjectCouchIds),
    %% bulk_get can return fewer ids then we asked for.  For now, we assert that we have the
    %% expected count to avoid any data/metadata mixups.  If this fails, the worker will
    %% crash and halt the migration for this org.
    case length(ObjectCouchIds) =:= length(ObjectDocs) of
        true -> ok;
        false ->
            log(err, OrgName, "bulk_get returned wrong number of objects"),
            throw(bulk_get_object_count_mismatch)
    end,
    Ctx = chef_db:make_context(<<"mover-worker-req-id">>, S),
    write_objects_to_sql(Module, Ctx, OrgName, lists:zip(ObjectMeta, ObjectDocs)),
    State1 = State#state{object_list = safe_split(BatchSize, ObjectList),
                         skip_objects = SkipObjects},
    {next_state, migrate_objects, State1, 0}.

%% @doc We verify that the object names we fetched from couch at the start of this migration
%% match those we can pull back from MySQL.  If so, then the migration was successful and we
%% issue deletes for all the couch ids to solr.  If the lists are not the same, then one or
%% more objects failed to be migrated.  No solr delete is issued and the worker crashes to
%% indicate that the migration (partially) failed.
mark_migration_end(timeout, #state{module = Module,
                                   org_name = OrgName,
                                   org_id = OrgId,
                                   couch_objects = CouchObjects,
                                   skip_objects = SkipObjects}=State) ->
    {CouchNames0, CouchIds} = lists:unzip(CouchObjects),
    %% remove objects we skipped before comparing to what landed in MySQL
    log(info, OrgName, "unmigrated objects in skip list: ~256P",
        [[ SN || {SN, _} <- SkipObjects ], 20]),
    SkipDict = dict:from_list(SkipObjects),
    CouchNames1 = [ Name || Name <- CouchNames0, dict:is_key(Name, SkipDict) =:= false ],
    CouchNames = lists:sort(CouchNames1),
    SqlNames = lists:sort(Module:fetch_objects_from_sql(OrgId)),
    case CouchNames =:= SqlNames of
        true ->
            log(info, OrgName, "deleting couch ids from solr"),
            [ ?NOT_DRY_RUN(Module:delete_object_from_solr(Id, OrgId)) || Id <- CouchIds ],
            mover_manager:mark_org_time(objects_done, OrgId),
            log(info, OrgName, "migration complete"),
            {stop, normal, State};
        false ->
            {ok, FH} = file:open(<<OrgName/binary, "-object-mismatch.txt">>, [write]),
            io:fwrite(FH, "couch object names~n", []),
            [ io:fwrite(FH, "~s~n", [NN]) || NN <- CouchNames ],
            io:fwrite(FH, "~nmysql object names~n", []),
            [ io:fwrite(FH, "~s~n", [NN]) || NN <- SqlNames ],
            file:close(FH),
            log(err, OrgName, "FAILED MIGRATION: couch: ~B, mysql: ~B",
                [length(CouchNames), length(SqlNames)]),
            throw(object_name_mismatch)
    end.

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

write_objects_to_sql(Module, S, OrgName, [{#object{status = couchdb}=MetaData, ObjectData} | Rest]) ->
    #object{id = OrigId,
            name = Name,
            authz_id = AuthzId,
            org_id = OrgId,
            requestor = RequestorId} = MetaData,
    %% ObjectData comes from chef_otto:bulk_get which for some reason is unwrapping the
    %% top-level tuple of the ejson format.  So we restore it here.
    Object = Module:convert_couch_json_to_object_record(OrgId, AuthzId, RequestorId,
                                                        {ObjectData}),
    %% validate name match
    Name = ej:get({<<"name">>}, ObjectData),
    %% this is where we'd call chef_db:create_object(S, Object) shortcut
    %% will be to pass 'ignore' for first arg since not needed for
    %% emysql backed object creation.
    case Module:create_object(S, Object, RequestorId) of
        ok ->
            log(info, OrgName, "migrated object: ~s ~s (~s => ~s)",
                [OrgId, Name, OrigId, Module:object_id(Object)]),
            ok = ?NOT_DRY_RUN(Module:send_object_to_solr(Object, {ObjectData})),
            mover_object_utils:mark_object(Module:object_name(), complete, OrigId);
            %% %% FIXME: should be using log_dir, but need to refactor.  ETOOMANYARGS alraedy
            %% %% :(
            %% file:write_file(<<"object_migration_log/", OrgName/binary, "/objects_complete.txt">>,
            %%                 iolist_to_binary([Name, <<"\t">>, OrigId, <<"\t">>,
            %%                                   Object#chef_object.id, "\n"]),
            %%                 [append]);
        {conflict, _} ->
            %% In theory, we won't ever get here since we match on object meta data status of
            %% couchdb.
            log(warn, OrgName, "object skipped: ~s (already exists)", [Name]);
        Error ->
            error_logger:error_report({object_create_failed, {Object, Error}}),
            log(err, OrgName, "object create failed"),
            fast_log:err(object_errors, OrgName, "~p", [{Object, Error}]),
            mover_object_utils:mark_object(Module:object_name(), error, OrigId, Error)
    end,
    write_objects_to_sql(Module, S, OrgName, Rest);
write_objects_to_sql(Module, S, OrgName, [{#object{name = Name, status = mysql}, _} | Rest]) ->
    %% Note that this is here for safetly. Based on how this function is called, only good
    %% unmigrated objects will be provided.
    log(warn, OrgName, "object skipped: ~s (already exists)", [Name]),
    write_objects_to_sql(Module, S, OrgName, Rest);
write_objects_to_sql(Module, S, OrgName, [{#object{status={error, _}}, _}|Rest]) ->
    %% FIXME: how should we track errors of this sort if they occur?
    %% This is a case where we found object json, but no matching mixlib
    %% or authz data.
    write_objects_to_sql(Module, S, OrgName, Rest);
write_objects_to_sql(_, _, _, []) ->
    ok.


%% @doc Given a list of all org objects as {Name, Id} pairs, fetch meta data for the object from
%% the cache or read through chef_otto to fetch the meta data.  The returned list may be
%% smaller than the input list of objects as we filter out any objects for which we cannot find
%% meta data as well as any objects that have already been migrated (cache meta data has state
%% other than 'couchdb').
fetch_meta_data_for_objects(Module, S, OrgName, OrgId, ObjectBatch, SkipObjects)
  when is_list(SkipObjects) ->
    fetch_meta_data_for_objects(Module, S, OrgName, OrgId, ObjectBatch, {[], SkipObjects});
fetch_meta_data_for_objects(Module, S, OrgName, OrgId, [{Name, Id}|Rest], {Acc, Skip}) ->
    Acc1 = case dets:lookup(mover_object_utils:table_name(Module:object_name()), Id) of
               [#object{status = couchdb}=Object] ->
                   {[Object|Acc], Skip};
               [#object{status = {error, {missing_authz, _}},
                      id = SkipId, name = SkipName}] ->
                   %% known missing authz data, skip this object
                   {Acc, [{SkipName, SkipId}|Skip]};
               [#object{}] ->
                   %% other error or already migrated, ignore
                   {Acc, Skip};
               [] ->
                   %% object data not found in cache, attempt to look it up
                   %% here.
                   case store_object(Module, S, OrgName, OrgId, Id, Name) of
                       #object{status = couchdb}=Object ->
                           {[Object|Acc], Skip};
                       #object{status = {error, {missing_authz, _}},
                             id = SkipId, name = SkipName} ->
                           %% known missing authz data, skip this object
                           {Acc, [{SkipName, SkipId}|Skip]};
                       #object{status = {error, Why}} ->
                           log(err, OrgName, "unexpected error fetching authz data for ~s ~s ~s",
                               [Module:object_name(), Name, Id]),
                           fast_log:err(object_errors, OrgName, "object authz data not found:~n~p",
                                        [{Name, Id, Why}]),
                           error_logger:error_report({object_not_found,
                                                      OrgName, Name, Id, Why}),
                           {Acc, Skip}
                   end
           end,
    fetch_meta_data_for_objects(Module, S, OrgName, OrgId, Rest, Acc1);
fetch_meta_data_for_objects(_Module, _S, _OrgName, _OrgId, [], Acc) ->
    Acc.

store_object(Module, S, OrgName, OrgId, Id, Name) ->
    Object = mover_object_utils:fetch_object_authz(S, OrgId, Id, Name, Module:authz_type()),
    dets:insert(mover_object_utils:table_name(Module:object_name()), Object),
    ObjectTable = mover_object_utils:error_table_name(Module:object_name()),
    log_object_stored(ObjectTable, OrgName, Object),
    Object.

log_object_stored(_ObjectTable, OrgName, #object{status=couchdb, id=Id, name=Name, org_id=OrgId}) ->
    Self = pid_to_list(self()),
    fast_log:info(object_errors, Self, "authz ok: ~s ~s ~s ~s",
                  [OrgName, Name, OrgId, Id]);
log_object_stored(ObjectTable, OrgName, #object{status={error, {missing_authz, Type}},
                                         id=Id, name=Name, org_id=OrgId}=Node) ->
    dets:insert(ObjectTable, Node),
    Self = pid_to_list(self()),
    fast_log:err(object_errors, Self, "missing authz data (~p): ~s ~s ~s ~s",
                 [Type, OrgName, Name, OrgId, Id]);
log_object_stored(ObjectTable, OrgName, #object{status={error, Why}, id=Id, name=Name, org_id=OrgId}=Node) ->
    dets:insert(ObjectTable, Node),
    Self = pid_to_list(self()),
    fast_log:err(object_errors, Self, "object authz fail: ~s ~s ~s ~s ~p",
                 [OrgName, Name, OrgId, Id, Why]).

safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.

is_dry_run() ->
    {ok, DryRun} = application:get_env(mover, dry_run),
    DryRun.

log(Level, OrgName, Msg) ->
    fast_log:Level(mover_worker_log, OrgName, Msg).

log(Level, OrgName, Fmt, Args) when is_list(Args) ->
    fast_log:Level(mover_worker_log, OrgName, Fmt, Args).

log_object_names(LogDir, ObjectList) ->
    ObjectsToMigrate = << <<Name/binary, "\n">> || {Name, _} <- ObjectList >>,
    file:write_file(<<LogDir/binary, "/objects_to_migrate.txt">>, ObjectsToMigrate).

ensure_dir(Dir) when is_binary(Dir) ->
    filelib:ensure_dir(<<Dir/binary, "/dummy_for_ensure">>).

