-module(chef_license_worker).

-behaviour(gen_server).

-include("../../../include/chef_types.hrl").

-export([
    start_link/0,
    get_license/0,
    test_license/0
]).

-ifdef(TEST).
-export([refresh_license/0]).
-endif.

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
        {ok, valid_license} -> State#state{license_cache=valid, grace_period=undefined, scanned_time = erlang:timestamp()};
        {ok, expired} -> State#state{license_cache=expired, grace_period=undefined, scanned_time = erlang:timestamp()};
        {ok, valid_license, grace_period} -> State#state{license_cache=valid, grace_period=true, scanned_time = erlang:timestamp()};
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
                                            {ok, valid_license, grace_period};
                                        _ ->
                                            %% MSG
                                            { ok, expired}
                                    end;
                                _ ->
                                    {ok, expired}
                            end
                    end;
                _ ->
                    {error, invalid_response}
            end;
        _ ->
            {error, invalid_response}
    end.

%%% =============================
%%% Sample license response
%%% =============================
test_license()->
    {ok,<<"{\"command\":\"chef-automate license status --result-json /tmp/string3\",\"status\":\"OK\",\"error_code\":0,\"error_description\":\"\",\"error_cause\":\"\",\"error_stack_trace\":\"\",\"error_recovery\":\"\",\"error_type\":\"\",\"result\":{\"set\":true,\"license_id\":\"6541d90a-2ed0-4d64-9861-c20fc21a3093\",\"customer_name\":\"janshahid.shaik@progress.com\",\"expiration_date\":{\"seconds\":1735689599},\"deployment_id\":\"0b9907b3-45d2-4faa-b04e-b76e31ba70e5\",\"deployment_type\":\"Standalone\",\"license_type\":\"trial\",\"deployment_at\":{\"seconds\":1727067698}}}">>}.