-module(chef_telemetry_worker).

-behaviour(gen_server).

-export([
    start_link/0,
    solr_search/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/2,
    terminate/2
]).

-record(state, {
    http_client,
    timer_ref,
    report_time
}).

-record(oc_chef_organization, {
          server_api_version,
          id,
          authz_id,
          name,
          full_name,
          assigned_at,
          last_updated_by,
          created_at,
          updated_at
         }).

-define(DEFAULT_DAYS, 30).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Config) ->
    % RootUrl = proplists:get_value(root_url, Config),
    % Options = proplists:get_value(ibrowse_options, Config, []),
    % HttpClient = oc_httpc_worker:start_link(RootUrl, Options, []),
    State = #state{
        report_time = {12, 00}},
    gen_server:cast(self(), init_timer),
    {ok, State}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(send_data, State) ->
    ReqId = base64:encode(term_to_binary(make_ref())),
    EpochNow = erlang:system_time(seconds), 
    TimeDuration = ?DEFAULT_DAYS * 86400,
    Epoch30DaysOld = EpochNow - TimeDuration,
    QueryString = lists:flatten(io_lib:format("ohai_time:{~p TO ~p}", [Epoch30DaysOld, EpochNow])),
    Query1 = chef_index:query_from_params("node", QueryString, undefined, undefined),
    DbContext = chef_db:make_context("1.0", ReqId, false),
    _Count = 
        case chef_db:count_nodes(DbContext) of
            Count1 when is_integer(Count1) -> Count1;
            Error -> throw({db_error, Error})
        end,
    Orgs = 
        case chef_db:list(#oc_chef_organization{}, DbContext) of
            Orgs1 when is_list(Orgs1) -> Orgs1;
            Error1 -> throw({db_error, Error1})
        end,
    _Stats = [ {Org, get_org_nodes(Org, Query1, ReqId, DbContext)} || Org <- Orgs ],
    % sending data logic hear
    gen_server:cast(self(), init_timer),
    {noreply, State};

handle_cast(init_timer, State) ->
    {_Date, {Hour, Min, _Sec}} = erlang:universaltime(),
    {RHour, RMin} = State#state.report_time,
    CurrentDaySeconds = Hour * 3600 + Min * 60,
    ReportingSeconds = RHour * 3600 + RMin * 60,
    Diff = ReportingSeconds - CurrentDaySeconds,
    if 
        Diff == 0 -> 
            gen_server:cast(self(), send_data);
        Diff > 0 ->
            timer:apply_after(Diff * 1000, gen_server, cast, [self(), send_data]);
        Diff < 0 ->
            timer:apply_after((Diff + 86400) * 1000, gen_server, cast, [self(), send_data])
    end,
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVsn, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

get_org_nodes(OrgName, Query1, ReqId, DbContext) ->
    {Guid1, _AuthzId1} = 
        case chef_db:fetch_org_metadata(DbContext, OrgName) of
            not_found -> throw({org_not_found, OrgName});
            {Guid, AuthzId} -> {Guid, AuthzId}
        end,
    Query = chef_index:add_org_guid_to_query(Query1, Guid1),
    case search(Query, ReqId) of
        {ok, _Start0, _SolrNumFound, Ids} ->
            erlang:length(Ids);
        {error, {solr_400, _}=Why} ->
            io:format("error while getting statestics ~p~n", Why);
        {error, {solr_500, _}=Why} ->
            io:format("error while getting statestics ~p~n", Why)
        end.

search(Query, ReqId) ->
    stats_hero:ctime(ReqId, {chef_solr, search},
        fun() ->
            solr_search(Query)
        end).

solr_search(Query) ->
    try
        chef_index:search(Query)
    catch
        Error:Reason ->
            {Error, Reason}
    end.