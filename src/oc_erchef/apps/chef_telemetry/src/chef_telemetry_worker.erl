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

-ifdef(TEST).
-define(DEFAULT_REPORTING_URL, "http://127.0.0.1:9001/esi/payload:test").
-else.
-define(DEFAULT_REPORTING_URL, "https://services.chef.io/usage/v1/payload").
-endif.

-define(DEFAULT_REPORTING_TIME, {4, 00}).

-define(WINDOW_SECONDS, 300).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Config) ->
    ConfigFile= envy:get(chef_telemetry, running_filepath, "", string),
    Ctl = envy:get(chef_telemetry, ctl_command, "", string),
    Cmd = "which " ++ Ctl,
    CtlLocation = string:trim(os:cmd(Cmd)),
    State = #state{
        report_time = ?DEFAULT_REPORTING_TIME,
        reporting_url = ?DEFAULT_REPORTING_URL,
        running_file = ConfigFile,
        ctl_command = CtlLocation},
    gen_server:cast(self(), init_timer),
    {ok, State}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(send_data, State) ->
    State2 =
        try send_data(State) of
            State1 -> State1
        catch
            _:_ ->
                State
        end,
    gen_server:cast(self(), init_timer),
    {noreply, State2};

handle_cast(init_timer, State) ->
    {_Date, {Hour, Min, _Sec}} = calendar:now_to_universal_time(erlang:timestamp()),
    {RHour, RMin} = State#state.report_time,
    CurrentDaySeconds = Hour * 3600 + Min * 60,
    ReportingSeconds = RHour * 3600 + RMin * 60,
    DelaySeconds = floor(rand:uniform() * ?WINDOW_SECONDS),
    case ReportingSeconds - CurrentDaySeconds of
        Diff when Diff == 0 ->
            timer:apply_after(DelaySeconds * 1000, gen_server, cast, [self(), send_data]);
        Diff when Diff > 0 ->
            timer:apply_after((Diff + DelaySeconds) * 1000, gen_server, cast, [self(), send_data]);
        Diff ->
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

send_data(State) ->
    State6 =
        case chef_telemetry:is_enabled() of
            true ->
                State1 = init_req(State),
                Hostname = get_fqdn(),
                case check_send(Hostname) of
                    true ->
                        [{_Server, ServerVersion, _, _}] = release_handler:which_releases(permanent),
                        State2 = get_nodes(State1),
                        State3 = get_company_name(State2),
                        State4 = get_api_fqdn(State3),
                        Req = generate_request(ServerVersion, State4),
                        send_req(Req, State3),
                        State3;
                     _   ->
                        State1
                end;
            _ ->
                State
        end,
    State6.

get_api_fqdn(State) ->
    sqerl:execute(<<"delete from telemetry where property like 'FQDN:%' and event_timestamp < (current_timestamp - interval '86700')">>),
    case sqerl:execute(<<"select trim(property) as property from telemetry where property like 'FQDN:%'">>) of
        {ok, Rows} when is_list(Rows) ->
            FQDNs = [binary:part(FQDN, 5, size(FQDN) -5) || [{<<"property">>, FQDN}] <- Rows],
            FQDNs1 = mask(FQDNs),
            State#state{fqdns = FQDNs1};
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
        {ok, _Start0, Count, _Ids} ->
            Count;
        {error, {solr_400, _}=Why} ->
            throw({error_getting_nodes, Why});
        {error, {solr_500, _}=Why} ->
            throw({error_getting_nodes, Why})
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

get_license_company_name()->
    {_Lic, _Type, _GracePeriod, _ExpDate, _Msg, CN,_LID}  = gen_server:call(chef_license_worker, get_license),
    CN.

determine_license_id()->
    {_Lic, _Type, _GracePeriod, _ExpDate, _Msg, _CN, LicenseID}  = gen_server:call(chef_license_worker, get_license),
    case LicenseID of
        undefined           ->
            <<"Infra-Server-license-Id">>;
        <<"undefined">>     ->
            <<"Infra-Server-license-Id">>;
        <<>>                ->
            <<"Infra-Server-license-Id">>;
        _                   ->
            LicenseID
    end.


get_company_name(State) ->
    CompanyName =
    case get_license_company_name() of
        CN when CN =:= undefined; CN=:= <<"">>; CN =:= "" ->
            case sqerl:adhoc_select([<<"email">>], <<"users">>, all) of
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
                        case length(CompanyNames) == 0 of
                            true ->
                                throw("no valid Email Ids.");
                            _ ->
                                get_most_occuring(CompanyNames)
                        end;
                    Error ->
                        throw(Error)
            end;
        CN -> CN
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
            case Count1 > Count2 of
                true ->
                    {Key1, Count1};
                _ ->
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
    Query1 = chef_index:query_from_params("node", QueryString, "0", "10000"),
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
    Res = jiffy:encode({[
    {<<"licenseId">>, determine_license_id()},
    {<<"customerName">>, State#state.current_scan#current_scan.company_name},
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
            ]}),
    Res.

to_binary(String) when is_list(String) ->
    list_to_binary(String);

to_binary(Element) ->
    throw({not_a_string, Element}).

epoch_to_string(Epoch) ->
    calendar:system_time_to_rfc3339(Epoch, [{offset, "Z"}]).

send_req(Req, State) ->
    case ibrowse:send_req(State#state.reporting_url, [{"Content-Type", "application/json"}], post, Req, [], 5000) of
        {ok, _Status, _ResponseHeaders, _ResponseBody} -> ok;
        Error                                          -> {failed_sending_request, Error}
    end.

check_send(Hostname) ->
    case sqerl:execute(<<"select telemetry_check_send('", Hostname/binary, "')">>) of
        {ok,[[{_, true}]]} ->
            true;
        {ok,[[{_, false}]]}  ->
            false;
        Error ->
            Error
    end.

get_fqdn() ->
    HostName = binary:bin_to_list(envy:get(oc_chef_wm, actions_fqdn, <<"">>, binary)),
    to_binary("FQDN:" ++ HostName).

mask(FQDNs) ->
    Join = fun(Elements, Separator) ->
        [H | T] = Elements,
        lists:foldl(fun (Value, Acc) -> <<Acc/binary, Separator/binary, Value/binary>> end, H, T)
    end,
    Fun = fun(FQDN) ->
        case re:run(FQDN,
                    <<"(?:(.*?):\/\/?)?\/?(?:[^\/\.]+\.)*?([^\/\.]+)\.?([^\/:]*)(?::([^?\/]*)?)?(.*)?">>,
                    [{capture, all_but_first, binary}]) of
            {match, Parts} ->
                [Protocall, SubDomain, Domain, Rest1, Rest2] = Parts,
                FQDN1 = <<SubDomain/binary, ".", Domain/binary>>,
                case re:run(FQDN1, <<"^[0-9a-fA-F\.:]*$">>) of
                    {match, _} ->
                        Hash = crypto:hash(md5, FQDN1),
                        Domain2 = <<"">>;
                    _ ->
                        FQDN_parts = binary:split(FQDN1, <<"\.">>, [global]),
                        case size(lists:last(FQDN_parts)) =:= 2 of
                            true ->
                                {SubDomain1, Domain1} = lists:split(erlang:length(FQDN_parts) - 3, FQDN_parts),
                                SubDomain2 = Join(SubDomain1, <<".">>),
                                Domain2 = Join(Domain1, <<".">>);
                            _    ->

                                {SubDomain1, Domain1} = lists:split(erlang:length(FQDN_parts) - 2, FQDN_parts),
                                SubDomain2 = Join(SubDomain1, <<".">>),
                                Domain2 = Join(Domain1, <<".">>)
                        end,
                        Hash = crypto:hash(md5, SubDomain2)
                end,

                Hash1 = base64:encode(Hash),
                Len = binary:longest_common_suffix([Hash1, <<"===">>]),
                Hash2 = binary:part(Hash1, {0, size(Hash1) - Len}),
                Res1 =
                    case Protocall /= <<"">> of
                        true ->
                            <<Protocall/binary, "://", Hash2/binary>>;
                        false ->
                            <<Hash2/binary>>
                    end,
                Res2 =
                case Domain2 =:= <<"">> of
                    true ->
                        <<Res1/binary>>;
                    false ->
                        <<Res1/binary, ".", Domain2/binary>>
                end,
                Res3 =
                    case Rest1 /= <<"">> of
                        true ->
                            <<Res2/binary, ":", Rest1/binary, Rest2/binary>>;
                        _ ->
                            <<Res2/binary, Rest2/binary>>
                    end,
                Res3;
            _ ->
                <<"">>
        end
    end,
    lists:map(Fun, FQDNs).
