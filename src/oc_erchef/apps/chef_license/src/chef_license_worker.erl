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
    license_type,
    license_cache,
    grace_period,
    expiration_date,
    message,
    customer_name,
    license_id,
    install_time
}).

-define(DEFAULT_LICENSE_SCAN_INTERVAL, 30000). %milli seconds
-define(DEFAULT_FILE_PATH, "/tmp/lic").
-define(LICENSE_SCAN_CMD, "chef-automate license status --result-json ").


%% Valid options: cli, or a path to a file
%% Note that this is defaulted to cli in erl_opts in rebar.config.script but can be overridden
%% with the path at build time by setting OC_LICENSE_PATH=<path> in the environment.
%% (In that case, path should be something like /var/opt/opscode/license.lic)
-ifndef(OC_LICENSE_PATH).
-define(OC_LICENSE_PATH, cli).
-endif.

%%% ======================================
%%% Exported
%%% ======================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_license() ->
    gen_server:call(?MODULE, get_license).

%%% ======================================
%%% Gen Server callbacks
%%% ======================================
init(_Config) ->
    State = check_license(#state{install_time = install_time()}, ?OC_LICENSE_PATH),
    erlang:send_after(?DEFAULT_LICENSE_SCAN_INTERVAL, self(), check_license),
    {ok, State}.

handle_call(get_license, _From, #state{license_cache = undefined, license_type=Type, expiration_date=ExpDate, grace_period = GracePeriod, message = Msg, customer_name=CN, license_id = LicenseId}=State) ->
    {reply, {valid_license, Type, GracePeriod, ExpDate, Msg, CN, LicenseId}, State};
handle_call(get_license, _From, #state{license_cache = Lic, license_type=Type, expiration_date=ExpDate, grace_period = GracePeriod, message = Msg, customer_name=CN, license_id = LicenseId} = State) ->
    {reply, {Lic, Type, GracePeriod, ExpDate, Msg, CN, LicenseId}, State};
handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(check_license, State) ->
    State1 = check_license(State, ?OC_LICENSE_PATH),
    erlang:send_after(?DEFAULT_LICENSE_SCAN_INTERVAL, self(), check_license),
    {noreply, State1};

handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVsn, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%
%%% Internal Implementation
%%%

%% Return an approximate installation time based on the unix timestamp found kvpairs
install_time() ->
  DefaultTime = os:system_time(second),
  case chef_sql:select_rows({value_for_key, [<<"itime">>]}) of
    [[{<<"value">>, Itime}]]->
      try
        binary_to_integer(Itime)
      catch error:badarg ->
        DefaultTime
      end;
    not_found -> {ok, 0};
    {error, Reason} -> {error, Reason}
  end.

% This will continue to be the default behavior - license will only be checked
% as part of an automate install, when the automate CLI is present.
get_license_info(_InstallTime, cli) ->
  os:cmd(?LICENSE_SCAN_CMD ++ ?DEFAULT_FILE_PATH),
  {ok, Bin} = file:read_file(?DEFAULT_FILE_PATH),
  {Json} = jiffy:decode(Bin),
  Json;
% This is for the file-based license mode, where we read the license directly from a file path.
get_license_info(InstallTime, Path) ->
  case load_local_license(InstallTime, Path) of
    {ok, Json} ->
      io:format("Data: ~p~n", Json),
      Json;
    {error, Reason} ->
      io:format("Failed to load local license going with default: ~p~n", [Reason]),
      make_license_payload(InstallTime, #{})
  end.

load_local_license(InstallTime, Loader) when is_function(Loader) ->
  case Loader() of
    {ok, Bin} ->
      %% Input format is the dotted three-part JWT style base64url encoded string, so
      %% we start by splitting on dot and ensuring we get the expected three parts
      case binary:split(Bin, [<<".">>], [trim_all, global]) of
        %% NOTE: This implementation does not verify signature. To implement signature verification,
        %%       we will need to have the license service public key available to the license worker
        %%       at a known path.
        [_Header, Payload, _Signature] ->
          try base64:decode(Payload, #{ padding => false, mode => urlsafe } ) of
            Decoded ->
              try jiffy:decode(Decoded, [return_maps]) of
                J -> {ok, make_license_payload(InstallTime, J)}
              catch _:Reason ->
                { error , {invalid_json, Reason } } % %, Decoded } }
              end
          catch _:Reason ->
            { error, { invalid_base64, Reason } } %, Payload } }
          end;
        _ ->
          { error, { invalid_license_format } }
      end;
    {error, Reason} ->
      { error, { load_failed, Reason } }
  end;
load_local_license(InstallTime, Path) ->
  load_local_license( InstallTime, fun() -> file:read_file(Path) end ).


%% Generates a license payload using a parsed license.  The generated license is 's compatiable with the
%% automate-ctl license status json output object, so that we can use the same processing function for both
%% the CLI and file-based license modes.
%% In some cases, we don't have all the data that the license service would provide, so we'll make substitutes.
make_license_payload(_InstallTime, #{ <<"id">> := LicenseId,
                                      <<"customer">> := CustomerName,
                                      <<"entitlements">> := Entitlements,
                                      <<"type">> := LicenseType
                                    }) ->

  %% The license server itself would give us actual expiration date. We have to
  %% approximate as best we can based the latest end date of available entitilements.
  ExpireInSeconds = case Entitlements of
                      [] ->  0;  % No entitlements, treat as expired or invalid license
                      _ -> lists:max([Ent || #{ <<"end">> := #{<<"seconds">> := Ent}} <- Entitlements])
                    end,

  [ {<<"result">>, {[
                    {<<"license_id">>, LicenseId},
                    {<<"customer_name">>, CustomerName},
                    {<<"expiration_date">>, {[{<<"seconds">>, ExpireInSeconds}]}},
                    {<<"license_type">>, LicenseType},
                    {<<"grace_period">>, false}
                   ]}}
    ];
make_license_payload(InstallTime, _Other) ->
  %% If we don't have the expected fields, we don't have a valid license - so let's make something that looks like one,
  %% but is valid from the install date + 90 days.
  ExpirationTime = InstallTime + (60 * 60 * 24 * 90), % 90 days from install time
  lager:warning("License payload is missing expected fields, treating as valid license with expiration : ~p", [ExpirationTime]),
  [ {<<"result">>, {[
                    {<<"license_id">>, <<>>},
                    {<<"customer_name">>, <<>>},
                    {<<"expiration_date">>, {[{<<"seconds">>, ExpirationTime}]}},
                    {<<"license_type">>, <<"commercial">>},
                    {<<"grace_period">>, false}
                   ]}}
  ].



%%%
%%% =====================
%%% Internal functions
%%% =====================
check_license(#state{install_time = InstallTime} = State,  ModeOrPath) ->
  JsonStr = case catch get_license_info(InstallTime, ModeOrPath) of
              Result when is_list(Result) -> Result;
              {'EXIT', N} ->
                io:format("Oopsie, exited license check ~p~n", [N]),
                <<"">>
            end,
  R2 = process_license(JsonStr),

  io:format("process_license result: ~p ~p~n", [JsonStr, R2]),
  case process_license(JsonStr) of
    {ok, valid_license, ExpDate, CustomerName, LicenseId} ->
      State#state{license_cache=valid_license, grace_period=undefined, scanned_time = erlang:timestamp(), expiration_date=ExpDate, customer_name=CustomerName, license_id = LicenseId};
    {ok, commercial_expired, ExpDate, Msg, CustomerName, LicenseId} ->
      State#state{license_cache=commercial_expired, license_type = <<"commercial">>, grace_period=undefined, scanned_time = erlang:timestamp(), expiration_date=ExpDate, message=Msg, customer_name=CustomerName, license_id = LicenseId};
    {ok, commercial_grace_period, ExpDate, Msg, CustomerName, LicenseId} ->
      State#state{license_cache=commercial_grace_period, grace_period=true, scanned_time = erlang:timestamp(), expiration_date=ExpDate, message=Msg, customer_name=CustomerName, license_id = LicenseId};
    {ok, trial_expired, ExpDate, Msg, CustomerName, LicenseId} ->
      State#state{license_cache=trial_expired_expired, license_type = <<"trial">>, grace_period=undefined, scanned_time = erlang:timestamp(), expiration_date=ExpDate, message=Msg, customer_name=CustomerName, license_id = LicenseId};
    {error, no_license} ->
      State#state{license_cache=trial_expired_expired, license_type = <<"trial">>, grace_period=undefined, scanned_time = erlang:timestamp(), expiration_date="", message=get_alert_message(trial_expired, "")};
    {error, _} -> State
  end.


% missing license: gets treated as a trial that expires at TRACK_INSTALL_DATE + 90 days (configurable at build)
% a present license that is not valid gets treated as an expired license.
% a valid unexpired license gets treated as valid
% a valid expired license gets treated as expired. license gets treated as valid, and we don not
%
process_license(<<"">>) ->
    {error, invalid_json};
process_license(LicJson) ->
    case ej:get({<<"result">>}, LicJson) of
        {LicDetails} ->
            CustomerName = ej:get({<<"customer_name">>}, LicDetails),
            LicenseId = ej:get({<<"license_id">>}, LicDetails),
            case ej:get({<<"expiration_date">>}, LicDetails) of
                {[{<<"seconds">>, ExpireInSeconds}]} ->
                    ExpDate = sec_to_date(ExpireInSeconds),
                    case os:system_time(second) < ExpireInSeconds of
                        true -> {ok, valid_license, ExpDate, CustomerName, LicenseId};
                        _ ->
                            case ej:get({<<"license_type">>}, LicDetails) of
                                <<"commercial">> ->
                                    case ej:get({<<"grace_period">>}, LicDetails) of
                                        true ->
                                            {ok, commercial_grace_period, ExpDate, get_alert_message(commercial_grace_period, ExpDate), CustomerName, LicenseId};
                                        _ ->
                                            {ok, commercial_expired, ExpDate, get_alert_message(commercial_expired, ExpDate), CustomerName, LicenseId}
                                    end;
                                _ ->
                                    {ok, trial_expired, ExpDate, get_alert_message(trial_expired, ExpDate), CustomerName, LicenseId}
                            end
                    end;
                _ ->
                    {error, invalid_response}
            end;
        undefined ->
            case ej:get({<<"status">>}, LicJson) of
                <<"OK">> ->
                    {error, no_license};
                _ ->
                    {error, invalid_response}
            end;
        _ ->
            {error, invalid_response}
    end.


get_alert_message(Type, ExpDate) ->
    case Type of
        trial_expired ->
            "Your Progress Chef InfraServer license has expired or does not exist! You no longer have access to Chef InfraServer. Please contact the Account Team to upgrade to an Enterprise License.";
        commercial_expired ->
           "Your Progress Chef InfraServer license expired on " ++ ExpDate ++ " and you no longer have access to Chef InfraServer! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com";
        commercial_grace_period ->
            "Your Progress Chef InfraServer license expired on " ++ ExpDate ++ " and you are currently on a limited extension period! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com"
    end.

sec_to_date(Seconds) ->
    BaseDate      = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds1      = BaseDate + Seconds,
    { {Year, Month, Day}, _Time} = calendar:gregorian_seconds_to_datetime(Seconds1),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day])).

%%% =============================
%%% Sample license response
%%% =============================
test_license() ->
    {ok, <<"{\"command\":\"chef-automate license status --result-json /tmp/string3\",\"status\":\"OK\",\"error_code\":0,\"error_description\":\"\",\"error_cause\":\"\",\"error_stack_trace\":\"\",\"error_recovery\":\"\",\"error_type\":\"\",\"result\":{\"set\":true,\"license_id\":\"6541d90a-2ed0-4d64-9861-c20fc21a3093\",\"customer_name\":\"janshahid.shaik@progress.com\",\"expiration_date\":{\"seconds\":1735689599},\"deployment_id\":\"0b9907b3-45d2-4faa-b04e-b76e31ba70e5\",\"deployment_type\":\"Standalone\",\"license_type\":\"trial\",\"deployment_at\":{\"seconds\":1727067698}}}">>}.
