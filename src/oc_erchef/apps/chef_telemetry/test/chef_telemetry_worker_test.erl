-module(chef_telemetry_worker_test).

-include_lib("eunit/include/eunit.hrl").

-export([
    get_execute/1,
    adhoc_select/3,
    count_nodes/1,
    chef_db_list/2,
    org_metadata/2,
    index_search/1
]).

-define(DEFAULT_CONFIG, []).

-record(state, {
    fqdn_select,
    should_send,
    user_emails = [],
    nodes_count = 0,
    organizations = ["org1", "org2"],
    index_search = [["node1_org1", "node2_org1"], ["node1_org2", "node2_org2"]]
}).

-record(expected, {
    company_name = <<"">>,
    nodes_count = 0,
    active_nodes = 0,
    fqdn = []}).

field_value_test() ->
    State = #state{fqdn_select = {ok, [[{<<"property">>, <<"FQDN:node1.domain1.com">>}],
                                       [{<<"property">>, <<"FQDN:node2.subdomain2.domain2.com">>}],
                                       [{<<"property">>, <<"FQDN:node3.subdomain3.domain3.co.uk">>}]]},
        should_send = true,
        user_emails = [[{<<"email">>, <<"test@testorg.com">>}]],
        nodes_count = 10
    },
    {_Lic, _Type, _GracePeriod, _ExpDate, _Msg, CN,_}  = chef_license:get_license(),
    CN1 = case CN of
        CN when CN == undefined, CN== <<"">>, CN == "" -> <<"testorg">>;
                                                       CN ->CN
    end,

    Expected = #expected{company_name = CN1,
        nodes_count = 10,
        active_nodes = 4,
        fqdn = [<<".*\.domain1.com$">>,
            <<".*\.domain2\.com$">>,
            <<".*\.domain3\.co\.uk$">>]},
    execute(State, Expected, []).

enable_flag_test() ->
    State = #state{fqdn_select = {ok, []},
        should_send = false,
        user_emails = [[{<<"email">>, <<"test@testorg.com">>}]],
        nodes_count = 10
    },
    Expected = #expected{company_name = <<"testorg">>,
        nodes_count = 10,
        active_nodes = 4},
    ?_assertException(error, no_request, execute(State, Expected, [{chef_telemetry, is_enabled, false}])).

execute(State, Expected, Env) ->
    set_env([{chef_telemetry, reporting_url, "http://127.0.0.1:9001/esi/payload:io"}] ++ Env),
    application:start(ibrowse),
    put(state, State),
    setup(),
    chef_telemetry_test_utils:start_server([]),
    register(telemetry_mock_consumer, self()),
    trigger_send_data(),
    Req = get_message(),
    Req1 = jiffy:decode(Req),
    validate(Req1, Expected),
    io:format(user, "json ~p", [jiffy:decode(Req)]).

setup() ->
    meck:new(sqerl, [passthrough]),
    meck:new(chef_db, [passthrough]),
    meck:new(chef_index, [passthrough]),
    meck:new(release_handler, [passthrough]),
    meck:new(stats_hero, [passthrough]),
    meck:expect(sqerl, adhoc_insert, fun(_Table, _Rows) -> ok end),
    meck:expect(sqerl, adhoc_delete, fun(_Table, _Clause) -> ok end),
    meck:expect(sqerl, execute, fun get_execute/1 ),
    meck:expect(sqerl, adhoc_select, fun adhoc_select/3 ),
    meck:expect(chef_db, count_nodes, fun count_nodes/1 ),
    meck:expect(chef_db, list, fun chef_db_list/2 ),
    meck:expect(chef_db, fetch_org_metadata, fun org_metadata/2 ),
    meck:expect(chef_index, search, fun index_search/1),
    meck:expect(release_handler, which_releases, fun(_) -> [{"chef_server", "15.9.38", [], []}] end),
    meck:expect(stats_hero, ctime, fun(_, _, Fun) -> Fun() end).

get_execute(<<"select trim(property) as property from telemetry where property like 'FQDN:%'">>) ->
    State = get(state),
    State#state.fqdn_select;

get_execute(<<"select telemetry_check_send('", _/binary>>) ->
    State = get(state),
    State#state.should_send;

get_execute(_) ->
    ok.

adhoc_select([<<"email">>], <<"users">>, all) ->
    State = get(state),
    {ok, State#state.user_emails}.

count_nodes(_Context) ->
    State = get(state),
    State#state.nodes_count.

chef_db_list(Record, _context) ->
    RecordName = element(1, Record),
    State = get(state),
    case RecordName of
        oc_chef_organization -> State#state.organizations;
        _ -> []
    end.

org_metadata(_context, OrgName) ->
    OrgName1 = list_to_binary(OrgName),
    {OrgName1, OrgName1}.

index_search(_) ->
    State = get(state),
    [Nodes | Rest] = State#state.index_search,
    State1 = State#state{index_search = Rest},
    put(state, State1),
    {ok, 0, length(Nodes), Nodes}.

trigger_send_data() ->
    {ok, State} = chef_telemetry_worker:init([]),
    chef_telemetry_worker:handle_cast(send_data, State).

get_message() ->
    receive
        {http_request, _From, Req} ->
            Req
    after 5000 ->
       throw(no_request)
    end.

determine_license_id()->
    {_Lic, _Type, _GracePeriod, _ExpDate, _Msg, _CN, LicenseID}  = chef_license:get_license(),
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

validate(Req, Expected) ->
    Licence = ej:get({<<"licenseId">>}, Req),
    TotalNodes = ej:get({<<"periods">>, 1, <<"summary">>, <<"nodes">>, <<"total">>}, Req),
    ActiveNodes = ej:get({<<"periods">>, 1, <<"summary">>, <<"nodes">>, <<"active">>}, Req),
    FQDNs = ej:get({<<"metadata">>, <<"Infra Server">>, <<"fqdn">>}, Req),
    ?assertEqual(determine_license_id(), Licence),
    ?assertEqual(Expected#expected.nodes_count, TotalNodes),
    ?assertEqual(Expected#expected.active_nodes, ActiveNodes),
    ?assertEqual(true, check_fqdn(FQDNs, Expected#expected.fqdn)).

set_env(ConfigList) ->
    ConfigList1 = ?DEFAULT_CONFIG ++ ConfigList,
    [ application:set_env(App, Parameter, Value) || {App, Parameter, Value} <- ConfigList1 ].

check_fqdn(ReqFQDNs, Expected) ->
    MatchFun =
        fun(Pattern) ->
            lists:any(
                fun(FQDN) ->
                    match == re:run(FQDN, Pattern, [{capture, none}])
                end, ReqFQDNs)
        end,
    lists:all(MatchFun, Expected).