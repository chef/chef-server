-module(chef_telemetry_worker).

-behaviour(gen_server).

-include("../../../include/chef_types.hrl").

-export([
    start_link/0
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
    timer_ref,
    report_time,
    reporting_url,
    http_client
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

%% Setting this value for local ip because
%% 1) I don't have the server URL.
%% 2) easy for testing.
%% should be changed to actual server URL ASAP.
-define(DEFAULT_REPORTING_URL, "http://127.0.0.1:9001").
-define(DEFAULT_REPORTING_TIME, {12, 00}).
-define(DEFAULT_IBROWSE_OPTIONS, []).

-define(WINDOW_SECONDS, 300).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Config) ->
    ReportingUrl = envy:get(chef_telemetry, reporting_url, ?DEFAULT_REPORTING_URL, string),
    Fun = fun({Hour, Min}) ->
            Hour >= 0 andalso Hour < 24 andalso Min >= 0 andalso Min < 60
          end, 
    ReportingTime = envy:get(chef_telemetry, reporting_time, ?DEFAULT_REPORTING_TIME, Fun),
    Options = envy:get(chef_telemetry, ibrowse_options, ?DEFAULT_IBROWSE_OPTIONS, list),
    HttpClient = oc_httpc_worker:start_link(ReportingUrl, Options, []),
    State = #state{
        report_time = ReportingTime,
        reporting_url = ReportingUrl,
        http_client = HttpClient},
    gen_server:cast(self(), init_timer),
    {ok, State}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(send_data, State) ->
    % code to get chef_server version.
    [{_Server, ServerVersion, _, _}] = release_handler:which_releases(permanent),
    ReqId = base64:encode(term_to_binary(make_ref())),
    {TotalNodes, _EndTime, _StartTime, ActiveNodes} = get_nodes(ReqId),
    ScanTime = erlang:system_time(seconds),
    CompanyName = get_company_name(),
    Req = generate_request(CompanyName, list_to_binary(ServerVersion), TotalNodes, ActiveNodes, ScanTime),
    send_data(Req, State),
    gen_server:cast(self(), init_timer),
    {noreply, State};

handle_cast(init_timer, State) ->
    {_Date, {Hour, Min, _Sec}} = erlang:universaltime(),
    {RHour, RMin} = State#state.report_time,
    CurrentDaySeconds = Hour * 3600 + Min * 60,
    ReportingSeconds = RHour * 3600 + RMin * 60,
    DelaySeconds = rand:normal() * ?WINDOW_SECONDS,
    Diff = ReportingSeconds - CurrentDaySeconds,
    if 
        Diff == 0 -> 
            timer:apply_after(DelaySeconds * 1000, get_server, cast, [self(), send_data]);
        Diff > 0 ->
            timer:apply_after((Diff + DelaySeconds) * 1000, gen_server, cast, [self(), send_data]);
        Diff < 0 ->
            timer:apply_after((Diff + DelaySeconds + 86400) * 1000, gen_server, cast, [self(), send_data])
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

get_company_name() ->
    case sqerl:adhoc_select([email], users, all, []) of
        {ok, Ids1} ->
            Ids = [Id || [{_, Id}] <- Ids1],
            Fun = 
                fun(Email) -> 
                    case re:run(Email, "^[^@]*@\([^.]*\)\..*$") of
                        {match, [_, {Pos, Len} | _]} ->
                            {true, binary:part(Email, Pos, Len)};
                        _ ->
                            false
                    end
                end,
            CompanyNames = lists:filtermap(Fun, Ids),
            if length(CompanyNames) ->
                throw("no valid Email Ids.");
            true -> 
                get_most_occuring(CompanyNames)
            end;
        Error -> 
            throw(Error)
    end.

get_most_occuring(List) ->
    FirstElement = lists:nth(1, List),
    Fun = fun(Element, Map) ->
        Count = maps:get(Element, Map, 0),
        maps:put(Element, Count + 1, Map)
    end,
    Map1 = lists:foldl(Fun, #{}, List),

    Fun1 = 
        fun(Key1, Count1, {Key2, Count2}) ->
            if 
                Count1 > Count2 ->
                    {Key1, Count1};
                true ->
                    {Key2, Count2}
            end
        end,
    Res1 = maps:fold(Fun1, {FirstElement, 0}, Map1),
    element(1, Res1).

get_nodes(ReqId) ->
    EpochNow = erlang:system_time(seconds), 
    TimeDuration = ?DEFAULT_DAYS * 86400,
    Epoch30DaysOld = EpochNow - TimeDuration,
    QueryString = lists:flatten(io_lib:format("ohai_time:{~p TO ~p}", [Epoch30DaysOld, EpochNow])),
    Query1 = chef_index:query_from_params("node", QueryString, undefined, undefined),
    DbContext = chef_db:make_context("1.0", ReqId, false),
    Count = 
        case chef_db:count_nodes(DbContext) of
            Count1 when is_integer(Count1) -> Count1;
            Error -> throw({db_error, Error})
        end,
    Orgs = 
        case chef_db:list(#oc_chef_organization{}, DbContext) of
            Orgs1 when is_list(Orgs1) -> Orgs1;
            Error1 -> throw({db_error, Error1})
        end,
    Stats = [ {Org, get_org_nodes(Org, Query1, ReqId, DbContext)} || Org <- Orgs ],
    Fun = fun({_Org, Nodes}, Sum) ->
              Sum + Nodes
          end,
    ActiveNodes = lists:foldl(Fun, 0, Stats),
    {Count, calendar:system_time_to_rfc3339(EpochNow,[{offset, "Z"}]), calendar:system_time_to_rfc3339(Epoch30DaysOld,[{offset, "Z"}]), ActiveNodes}.

generate_request(CompanyName, ServerVersion, TotalNodes, ActiveNodes, ScanTime) ->
    jiffy:encode({[
    {<<"licenseId">>, <<"Infra-Server-license-Id">>},
    %%{<<"customerId">>, <<"">>},
    %%{<<"expiration">>, <<"2023-11-30T00:00:00Z">>},
    {<<"customerName">>, to_binary(CompanyName)},
    {<<"periods">>, [
        {[
            {<<"version">>, to_binary(ServerVersion)},
            {<<"date">>, to_binary(epoch_to_string(ScanTime))},
            {<<"period">>, {[
                {<<"start">>, to_binary(epoch_to_string(ScanTime))},
                {<<"end">>, to_binary(epoch_to_string(ScanTime))}
            ]}},
            {<<"summary">>, {[
                {<<"nodes">>, {[
                    {<<"total">>, TotalNodes},
                    {<<"active">>, ActiveNodes}
                ]}},
                {<<"scans">>, {[
                    {<<"total">>, 0},
                    {<<"targets">>, 0}
                 ]}},
                {<<"services">>, {[
                    {<<"targets">>, 0}
                 ]}}
            ]}},
            {<<"evidence">>, {[
                {<<"nodes">>, null},
                {<<"scans">>, null},
                {<<"content">>, null}
             ]}}
        ]}
    ]},
    {<<"metadata">>, {[
        {<<"Infra Server">>, {[
            {<<"deploymentType">>, <<"">>},
            {<<"instanceId">>, <<"">>},
            {<<"fqdn">>, <<"">>},
            {<<"config_location">>, <<"">>},
            {<<"binary_location">>, <<"">>}
        ]}}
    ]}},
    {<<"source">>, <<"Infra Server">>},
    {<<"scannerVersion">>, <<"0.1.0">>},
    {<<"scannedOn">>, to_binary(epoch_to_string(ScanTime))}
            ]}).

to_binary(String) when is_list(String) ->
    list_to_binary(String);

to_binary(Bin) when is_binary(Bin) ->
    Bin;

to_binary(Element) ->
    throw({not_a_binary_or_string, Element}).

epoch_to_string(Epoch) ->
    calendar:system_time_to_rfc3339(Epoch, [{offset, "Z"}]).

send_data(Req, State) ->
    case oc_httpc_worker:request(State#state.http_client,"", [], post, Req, 5000) of
        {ok, _Status, _ResponseHeaders, _ResponseBody} -> ok;
        {error, Reason}                             -> throw({failed_sending_request, Reason})
    end.