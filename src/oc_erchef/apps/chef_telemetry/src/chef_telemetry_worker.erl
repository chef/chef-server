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

-record(current_scan, {
    scan_start_time,
    scan_end_time,
    total_nodes,
    active_nodes,
    company_name
}).

-record(state, {
    timer_ref,
    report_time,
    reporting_url,
    http_client,
    db_context,
    req_id,
    scan_time,
    current_scan,
    running_file,
    ctl_command,
    fqdns
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
    ConfigFile= envy:get(chef_telemetry, config_file, "", string),
    Ctl = envy:get(chef_telemetry, ctl_command, "", string),
    Cmd = "which " ++ Ctl,
    CtlLocation = os:cmd(Cmd),
    {ok, HttpClient} = oc_httpc_worker:start_link(ReportingUrl, Options, []),
    State = #state{
        report_time = ReportingTime,
        reporting_url = ReportingUrl,
        http_client = HttpClient,
        running_file = ConfigFile,
        ctl_command = CtlLocation},
    gen_server:cast(self(), init_timer),
    {ok, State}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(send_data, State) ->
    State1 = init_req(State),
    insert_fqdn(State1),
    case check_send(State1) of
        true -> 
            [{_Server, ServerVersion, _, _}] = release_handler:which_releases(permanent),
            State2 = get_nodes(State1),
            State3 = get_company_name(State2),
            State4 = get_api_fqdn(State3),
            Req = generate_request(list_to_binary(ServerVersion), State4),
            send_data(Req, State3),
            State3;
        _   ->
            State1
    end,
    gen_server:cast(self(), init_timer),
    {noreply, State1};

handle_cast(init_timer, State) ->
    {_Date, {Hour, Min, _Sec}} = erlang:now_to_universal_time(State#state.scan_time),
    {RHour, RMin} = State#state.report_time,
    CurrentDaySeconds = Hour * 3600 + Min * 60,
    ReportingSeconds = RHour * 3600 + RMin * 60,
    DelaySeconds = floor(rand:uniform() * ?WINDOW_SECONDS),
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

init_req(State) ->
    ReqId = base64:encode(term_to_binary(make_ref())),
    DbContext = chef_db:make_context("1.0", ReqId, false),
    CurrentTime = erlang:system_time(seconds),
    StartTime = CurrentTime - (?DEFAULT_DAYS * 86400),
    State#state{
        req_id = ReqId,
        db_context = DbContext,
        scan_time = CurrentTime,
        current_scan = 
            #current_scan{
                scan_start_time = StartTime,
                scan_end_time = CurrentTime}
    }.

get_api_fqdn(State) ->
    case sqerl:execute(<<"select property from telemetry where property like 'FQDN:%'">>) of
        {ok, Rows} when is_list(Rows) ->
            FQDNs = [binary:part(FQDN, 5, size(FQDN) -5) || [{<<"property">>, FQDN}] <- Rows],
            State#state{fqdns = FQDNs};
        _ ->
            State
    end.
            
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

get_company_name(State) ->
    CompanyName = 
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
            if length(CompanyNames) == 0 ->
                throw("no valid Email Ids.");
            true -> 
                get_most_occuring(CompanyNames)
            end;
        Error -> 
            throw(Error)
    end,
    CurrentScan = State#state.current_scan,
    State#state{
        current_scan = CurrentScan#current_scan{company_name = CompanyName}}.

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

get_nodes(#state{req_id = ReqId, db_context = DbContext} = State) ->
    CurrentScan = State#state.current_scan,
    ScanStartTime = CurrentScan#current_scan.scan_start_time,
    ScanEndTime = CurrentScan#current_scan.scan_end_time,
    QueryString = lists:flatten(io_lib:format("ohai_time:{~p TO ~p}", [ScanStartTime, ScanEndTime])),
    Query1 = chef_index:query_from_params("node", QueryString, undefined, undefined),
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
    State#state{
        current_scan = CurrentScan#current_scan{
            total_nodes = Count,
            active_nodes = ActiveNodes}}.

generate_request(ServerVersion, State) ->
    CurrentScan = State#state.current_scan,
    jiffy:encode({[
    {<<"licenseId">>, <<"Infra-Server-license-Id">>},
    %%{<<"customerId">>, <<"">>},
    %%{<<"expiration">>, <<"2023-11-30T00:00:00Z">>},
    {<<"customerName">>, to_binary(State#state.current_scan#current_scan.company_name)},
    {<<"periods">>, [
        {[
            {<<"version">>, to_binary(ServerVersion)},
            {<<"date">>, to_binary(epoch_to_string(CurrentScan#current_scan.scan_end_time))},
            {<<"period">>, {[
                {<<"start">>, to_binary(epoch_to_string(CurrentScan#current_scan.scan_start_time))},
                {<<"end">>, to_binary(epoch_to_string(CurrentScan#current_scan.scan_end_time))}
            ]}},
            {<<"summary">>, {[
                {<<"nodes">>, {[
                    {<<"total">>, CurrentScan#current_scan.total_nodes},
                    {<<"active">>, CurrentScan#current_scan.active_nodes}
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
            {<<"fqdn">>, State#state.fqdns},
            {<<"config_location">>, to_binary(State#state.running_file)},
            {<<"binary_location">>, to_binary(State#state.ctl_command)}
        ]}}
    ]}},
    {<<"source">>, <<"Infra Server">>},
    {<<"scannerVersion">>, <<"0.1.0">>},
    {<<"scannedOn">>, to_binary(epoch_to_string(State#state.scan_time))}
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
    case catch ibrowse:send_req(State#state.reporting_url, [], post, Req, [], 5000) of
        {ok, _Status, _ResponseHeaders, _ResponseBody} -> ok;
        Error                                          -> throw({failed_sending_request, Error})
    end.

check_send(State) ->
    Node = erlang:atom_to_binary(node()),
    Now = calendar:system_time_to_universal_time(State#state.scan_time, second),
    case sqerl:adhoc_select([timestamp1], telemetry, {property, equals, "last_send"}, []) of
        {ok, Rows} when is_list(Rows) ->
            LastSend = to_system_time(Rows),
            case should_send(LastSend, State) of
                true ->
                    sqerl:adhoc_delete(telemetry, {<<"property">>, equals, <<"last_send">>}),
                    sqerl:adhoc_insert(telemetry, [[{<<"property">>, <<"last_send">>}, {<<"valuestring">>, Node}, {<<"timestamp1">>, Now}]]),
                    true;
                false ->
                    false
            end;
        {ok, Rows} when is_list(Rows) andalso length(Rows) == 0 ->
            sqerl:adhoc_insert(telemetry, [[{<<"property">>, <<"last_send">>}, {<<"valuestring">>, Node}, {<<"timestamp1">>, Now}]]),
            true;
        Error -> 
            throw({not_able_to_gead_from_db, Error})
    end.

to_system_time(Rows) ->
    TimeStamps1 = [ proplists:get_value(<<"timestamp1">>, Row) || Row <- Rows, not (proplists:get_value(<<"timestamp1">>, Row) == undefined) ],
    SystemTimes = [calendar:datetime_to_gregorian_seconds({Date, {H, M, floor(S1)}}) - 62167219200 || {Date, {H, M, S1}} <- TimeStamps1],
    MaxFun = 
        fun(Time, Max) ->
            case Time > Max of
                true ->
                    Time;
                _   ->
                    Max
            end
        end,
    lists:foldl( MaxFun, 0, SystemTimes).

should_send(LastSend, State) ->
    LastSend < State#state.scan_time.

insert_fqdn(State) ->
    {ok, HostName} = inet:gethostname(),
    Now = calendar:system_time_to_universal_time(State#state.scan_time, second),
    %%Hostname = os:cmd('hostname -f'),
    HostName1 = "FQDN:" ++ HostName,
    sqerl:adhoc_delete(telemetry, {property, equals, HostName1}),
    sqerl:adhoc_insert(telemetry, [[{<<"property">>, to_binary(HostName1)}, {<<"timestamp1">>, Now}]]).
