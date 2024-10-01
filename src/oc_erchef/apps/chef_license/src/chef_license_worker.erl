-module(chef_license_worker).

-behaviour(gen_server).

-include("../../../include/chef_types.hrl").

-export([
    start_link/0,
    get_license/0,
    test_license/0
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
    scanned_time,
    grace_period,
    license_cache,
    message
}).

-define(DEFAULT_LICENSE_SCAN_INTERVAL, 30000). %milli seconds

-define(DEFAULT_FILE_PATH, "/tmp/lic").

-define(LICENSE_SCAN_CMD, "chef-automate license status --result-json ").


%%% ======================================
%%% Exported
%%% ======================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_license()->
    gen_server:call(?MODULE, get_license).

%%% ======================================
%%% Gen Server callbacks
%%% ======================================
init(_Config) ->
    State = check_license(#state{}),
    erlang:send_after(?DEFAULT_LICENSE_SCAN_INTERVAL, self(), check_license),
    {ok, State}.

handle_call(get_license, _From, #state{license_cache = undefined}) ->
    {reply, {valid, false, <<"">>}};
handle_call(get_license, _From, #state{license_cache = Lic, grace_period = GracePeriod, message = Msg} = State) ->
    {reply,{Lic, GracePeriod, Msg}, State};
handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(check_license, State)->
    State1 = check_license(State),
    erlang:send_after(?DEFAULT_LICENSE_SCAN_INTERVAL, self(), check_license),
    {noreply,State1};

handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVsn, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%% =====================
%%% Internal functions
%%% =====================
check_license(State) ->
    JsonStr =
        case catch get_license_info() of
            Result when is_list(Result) -> Result;
            {'EXIT', _} -> <<"">>
        end,
    case process_license(JsonStr) of
        {ok, valid_license} -> State#state{license_cache=valid_license, grace_period=undefined, scanned_time = erlang:timestamp()};
        {ok, commercial_expired, Msg} -> State#state{license_cache=commercial_expired, grace_period=undefined, scanned_time = erlang:timestamp(), message=Msg};
        {ok, commercial_grace_period, Msg} -> State#state{license_cache=commercial_grace_period, grace_period=true, scanned_time = erlang:timestamp(), message=Msg};
        {ok, trial_expired_expired, Msg} -> State#state{license_cache=trial_expired_expired, grace_period=undefined, scanned_time = erlang:timestamp(), message=Msg};
        {error, _} -> State
    end.

get_license_info() ->
    os:cmd(?LICENSE_SCAN_CMD ++ ?DEFAULT_FILE_PATH),
    {ok, Bin} = file:read_file(?DEFAULT_FILE_PATH),
    {JsonStr} = jiffy:decode(Bin),
    JsonStr.

process_license(<<"">>)->
    {error, invalid_json};
process_license(LicJson) ->
    case ej:get({<<"result">>}, LicJson) of
        {LicDetails} ->
            case ej:get({<<"expiration_date">>}, LicDetails) of
                {[{<<"seconds">>,ExpireInSeconds}]} ->
                    case os:system_time(second) < ExpireInSeconds of
                        true -> {ok, valid_license};
                        _ ->
                            case ej:get({<<"license_type">>}, LicDetails) of
                                <<"commercial">> ->
                                    case ej:get({<<"grace_period">>}, LicDetails) of
                                        true ->
                                            %% WARNING
                                            {ok, commercial_grace_period, get_alert_message(commercial_grace_period, 
                                                get_alert_message(commercial_grace_period, ExpireInSeconds))};
                                        _ ->
                                            %% MSG
                                            { ok, commercial_expired, get_alert_message(commercial_expired, ExpireInSeconds)}
                                    end;
                                _ ->
                                    {ok, trial_expired, get_alert_message(trial_expired, ExpireInSeconds)}
                            end
                    end;
                _ ->
                    {error, invalid_response}
            end;
        _ ->
            {error, invalid_response}
    end.

get_alert_message(Type, ExpireInSeconds)->
    case Type of
        trial_expired ->
            <<"Your Progress® Chef® InfraServer™ license has expired or does not exist! You no longer have access to Chef Automate. Please contact the Account Team to upgrade to an Enterprise License.">>;
        commercial_expired ->
            <<"Your Progress® Chef® InfraServer™ license expired on">> ++ sec_to_date(ExpireInSeconds) ++ <<"and you no longer have access to Chef Automate! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com">>;
        commercial_grace_period ->
            <<"Your Progress® Chef® InfraServer™ license expired on">> ++ sec_to_date(ExpireInSeconds) ++ <<"and you are currently on a limited extension period! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com">>
        _ ->
            <<"">>
    end.

sec_to_date(Seconds)->
    BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds       = BaseDate + Seconds,
    { Date,_Time} = calendar:gregorian_seconds_to_datetime(Seconds),
    Date.

%%% =============================
%%% Sample license response
%%% =============================
test_license()->
    {ok,<<"{\"command\":\"chef-automate license status --result-json /tmp/string3\",\"status\":\"OK\",\"error_code\":0,\"error_description\":\"\",\"error_cause\":\"\",\"error_stack_trace\":\"\",\"error_recovery\":\"\",\"error_type\":\"\",\"result\":{\"set\":true,\"license_id\":\"6541d90a-2ed0-4d64-9861-c20fc21a3093\",\"customer_name\":\"janshahid.shaik@progress.com\",\"expiration_date\":{\"seconds\":1735689599},\"deployment_id\":\"0b9907b3-45d2-4faa-b04e-b76e31ba70e5\",\"deployment_type\":\"Standalone\",\"license_type\":\"trial\",\"deployment_at\":{\"seconds\":1727067698}}}">>}.