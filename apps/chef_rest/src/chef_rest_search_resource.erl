%%%-------------------------------------------------------------------
%%% @author Christopher Brown <cb@opscode.com>
%%% @author Seth Falcon <seth@opscode.com>
%%% @copyright (C) 2011, Opscode, Inc.
%%% @doc
%%% REST resource for submitting searches for Chef resources
%%% @end
%% @author Chris Brown <cb@opscode.com>
%% @author John Keiser <jkeiser@opscode.com>
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright Copyright 2011 Opscode, Inc.
%% @version 0.1
%%%-------------------------------------------------------------------
-module(chef_rest_search_resource).

-export([init/1,
         malformed_request/2,
         is_authorized/2,
         resource_exists/2,
         allowed_methods/2,
         content_types_provided/2,
         finish_request/2,
         to_json/2]).

-record(state, {start_time,
                reqid,
                resource,
                organization_guid,
                organization_name,
                object_type,
                user_name,
                header_fun = undefined,
                couchbeam = undefined,
                solr_query = undefined,
                estatsd_server = undefined,
                estatsd_port = undefined,
                hostname,
                request_type,
                couch_time = 0.0,
                solr_time = 0.0,
                batch_size = 5
}).

-include_lib("webmachine/include/webmachine.hrl").

-define(db_for_guid(X), [<<"chef_">>, X]).
-define(gv(X,L), proplists:get_value(X, L)).
-define(gv(X,L, D), proplists:get_value(X, L, D)).

init(_Any) ->
    %% Initialize random number gen for this process
    {T1, T2, T3} = erlang:now(),
    random:seed(T1, T2, T3),
    % TODO move solr/estatsd config out to chef_rest_sup or chef_rest_app
    {ok, BatchSize} = application:get_env(chef_rest, bulk_fetch_batch_size),
    {ok, EstatsdServer} = application:get_env(chef_rest, estatsd_server),
    % TODO IPv6?
    {ok, EstatsdServerIp} = inet:getaddr(EstatsdServer, inet),
    {ok, EstatsdPort} = application:get_env(chef_rest, estatsd_port),
    State = #state{start_time = now(),
                   resource = atom_to_list(?MODULE),
                   batch_size = BatchSize,
                   estatsd_server = EstatsdServerIp,
                   estatsd_port = EstatsdPort,
                   hostname = hostname(),
                   request_type = "search.get" },
    {ok, State}.

malformed_request(Req, State) ->
    State1 = read_req_id(Req, State),
    % This is the first method we get called on, so this is where we
    % send stats for org name.
    OrgName = wrq:path_info(organization_id, Req),
    State1 = State#state{organization_name = OrgName},
    send_stat(received, Req, State1),
    {GetHeader, State2} = get_header_fun(Req, State1),
    try
        chef_authn:validate_headers(GetHeader, 300),
        % We fill in most stuff here in order to validate the query,
        % but we don't add the organization database until
        % resource_exists() (where we get the org id).
        Query = chef_solr:make_query_from_params(Req),
        {false, Req, State2#state{solr_query = Query}}
    catch
        throw:Why ->
            Msg = malformed_request_message(Why, GetHeader),
            NewReq = wrq:set_resp_body(ejson:encode(Msg), Req),
            {true, NewReq, State2}
    end.

is_authorized(Req, State = #state{organization_name = OrgName}) ->
    UserName = list_to_binary(wrq:get_req_header("x-ops-userid", Req)),
    S = chef_otto:connect(),
    case verify_request_signature(Req, State) of
	{true, Req1, State1} ->
	    case chef_otto:is_user_in_org(S, UserName, OrgName) of
		true -> {true, Req1, State1};
		false ->
		    Msg = error_json(not_member_of_org),
		    Json = ejson:encode(Msg),
                    Req2 = wrq:set_resp_body(Json, Req),
		    {false, Req2, State1}
	    end;
	Other -> Other
    end.

resource_exists(Req, State = #state{solr_query = QueryWithoutGuid}) ->
    try
        OrgGuid = fetch_org_guid(Req, State),
        Query = chef_solr:add_org_guid_to_query(QueryWithoutGuid, OrgGuid),
        {true, Req, State#state{organization_guid = OrgGuid, solr_query = Query}}
    catch
        throw:org_not_found ->
            NoOrg = error_json(org_not_found),
            Req1 = wrq:set_resp_body(ejson:encode(NoOrg), Req),
            {false, Req1, State}
    end.

get_header_fun(Req, State = #state{header_fun = HFun})
  when HFun =:= undefined ->
    GetHeader = fun(H) ->
                        case wrq:get_req_header(H, Req) of
                            B when is_binary(B) -> B;
                            S when is_list(S) -> iolist_to_binary(S);
                            undefined -> undefined
                        end
                end,
    {GetHeader, State#state{header_fun = GetHeader}};
get_header_fun(_Req, State) ->
    {State#state.header_fun, State}.

verify_request_signature(Req, State = #state{organization_name = OrgName}) ->
    UserName = wrq:get_req_header("x-ops-userid", Req),
    % TODO Quit leaking this
    S = chef_otto:connect(),
    case chef_otto:fetch_user_or_client_cert(S, OrgName, UserName) of
        not_found ->
            NoCertMsg = error_json(no_cert),
            {false,
             wrq:set_resp_body(ejson:encode(NoCertMsg), Req), State};
        CertInfo ->
            % TODO this causes us to fetch the organization a second time.  Keep it and pass it in instead.
            Cert = ?gv(cert, CertInfo),
            Body = body_or_default(Req, <<>>),
            HTTPMethod = iolist_to_binary(atom_to_list(wrq:method(Req))),
            Path = iolist_to_binary(wrq:path(Req)),
            {GetHeader, State1} = get_header_fun(Req, State),
            case chef_authn:authenticate_user_request(GetHeader, HTTPMethod,
                                                      Path, Body, Cert, 300) of
                {name, _} ->
                    {true, Req,
                     State1#state{couchbeam = S}};
                {no_authn, Reason} ->
                    Msg = error_json(Reason),
                    Json = ejson:encode(Msg),
                    Req1 = wrq:set_resp_body(Json, Req),
                    % TODO This is a needless mutation
                    {false, Req1, State1#state{couchbeam = S}}
            end
    end.

body_or_default(Req, Default) ->
    case wrq:req_body(Req) of
        undefined -> Default;
        Body -> Body
    end.

error_json(bad_sig) ->
    {[{<<"error">>, [<<"bad signature">>]}]};
error_json(no_cert) ->
    {[{<<"error">>, [<<"user, client, or organization not found">>]}]};
error_json({missing_headers, Missing}) ->
    {[{<<"error">>,
               [<<"missing auth headers">>]},
              {<<"missing_headers">>, Missing}]};
error_json(bad_clock) ->
    {[{<<"error">>, [<<"check clock">>]}]};
error_json(bad_sign_desc) ->
    {[{<<"error">>, [<<"bad signing description">>]}]};
error_json(org_not_found) ->
    {[{<<"error">>, [<<"organization not found">>]}]};
error_json({bad_query, RawQuery}) ->
    {[{<<"error">>, [<<"invalid search query">>]},
              {<<"query">>, RawQuery}]};
error_json(not_member_of_org) ->
    {[{<<"error">>, [<<"Not a member of the organization">>]}]};
error_json(_) ->
    {[{<<"error">>, [<<"problem with headers">>]}]}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

fetch_org_guid(Req, #state{ organization_guid = Id, couchbeam = S}) ->
    case Id of
        Id when is_binary(Id) ->
            Id;
        undefined ->
            OrgName = list_to_binary(wrq:path_info(organization_id, Req)),
            case chef_otto:fetch_org_id(S, OrgName) of
                not_found -> throw(org_not_found);
                Guid -> Guid
            end
    end.

to_json(Req, State = #state{couchbeam = S,
                            solr_query = Query,
                            organization_guid = OrgGuid,
                            batch_size = BatchSize}) ->
    try
        {ok, Start, NumFound, Ids} = chef_solr:search(Query),
        {make_search_results(S, OrgGuid, Ids, BatchSize, Start, NumFound), Req, State}
    catch
        throw:X ->
	    io:format("500! ~s~n", X),
            {{halt, 500}, Req, State}
    end.

finish_request(Req, State) ->
    send_stat(completed, Req, State),
    {true, Req, State}.

make_search_results(S, Db, Ids, BatchSize, Start, NumFound) ->
    Ans0 = search_result_start(Start, NumFound),
    Ans1 = fetch_result_rows(Ids, BatchSize, S, ?db_for_guid(Db), Ans0),
    search_result_finish(Ans1).

% @doc Fetch a list of `Ids' in batches of size `BatchSize'.
%
% Each batch is fetched from connection `S' and database `Db'.
% Results are cons'd onto `Acc' with each batch separated by
% ``<<",">>''.
%
% Each set of results is processed to remove the _rev key and encode
% to JSON using ejson.  The ejson return value is post-processed to
% remove the JSON array markers.  The caller is responsible for adding
% this back to create valid JSON.
%
fetch_result_rows([], _BatchSize, _S, _Db, Acc) ->
    Acc;
fetch_result_rows(Ids, BatchSize, S, Db, Acc) when is_list(Ids) ->
    fetch_result_rows(safe_split(BatchSize, Ids), BatchSize, S, Db, Acc);
fetch_result_rows({Ids, []}, _BatchSize, S, Db, Acc) ->
    Docs = chef_otto:bulk_get(S, Db, Ids),
    [encode_result_rows(Docs) | Acc];
fetch_result_rows({Ids, Rest}, BatchSize, S, Db, Acc) ->
    Next = safe_split(BatchSize, Rest),
    Docs = chef_otto:bulk_get(S, Db, Ids),
    fetch_result_rows(Next, BatchSize, S, Db,
                      [<<",">>, encode_result_rows(Docs) | Acc]).

encode_result_rows(Items) ->
    CleanItems = [{lists:keydelete(<<"_rev">>, 1, Item)} || Item <- Items],
    Bin = ejson:encode(CleanItems),
    %% remove leading '[' and trailing ']' so that we can add to this
    %% result.
    binary:part(Bin, {1, size(Bin) - 2}).

safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.

search_result_start(Start, Total) ->
    % {"total":Total,"start":Start,"rows":[i1, i2]}
    ["\"rows\":[", ",",
     integer_to_list(Start), "\"start\":", ",",
     integer_to_list(Total), "\"total\":", "{"].

search_result_finish(Result) ->
    %% TODO: is iolist_to_binary needed?
    iolist_to_binary(lists:reverse([<<"]}">>|Result])).

malformed_request_message(bad_clock, GetHeader) ->
    User = case GetHeader(<<"X-Ops-UserId">>) of
               undefined ->
                   <<"">>;
               UID ->
                   UID
           end,
    Msg = iolist_to_binary([<<"Failed to authenticate as ">>, User,
                            <<". Synchronize the clock on your host.">>]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({missing_headers, Missing}, _GetHeader) ->
    Msg = iolist_to_binary([
                            <<"missing required authentication header(s) ">>,
                            bin_str_join(Missing, <<", ">>)]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({bad_query, Query}, _GetHeader) ->
    Msg = iolist_to_binary([<<"invalid search query: '">>, Query, <<"'">>]),
    {[{<<"error">>, [Msg]}]}.

bin_str_join(L, Sep) ->
    bin_str_join(L, Sep, []).

bin_str_join([H], _Sep, Acc) ->
    lists:reverse([<<"'">>, H, <<"'">>|Acc]);
bin_str_join([H | T], Sep, Acc) ->
    bin_str_join(T, Sep, [Sep, <<"'">>, H, <<"'">> | Acc]).

hostname() ->
    FullyQualified = net_adm:localhost(),
    Dot = string:chr(FullyQualified, $.),
    case Dot of
        0 -> FullyQualified;
	_Any -> string:substr(FullyQualified, 1, Dot - 1)
    end.

send_stat(received,
          _Any,
          State = #state{
            request_type = RequestType,
            hostname = HostName,
            organization_name = OrgName}) ->
    send_stats(State, [{"erchefAPI.application.allRequests", 1, "m"},
                       {["erchefAPI.application.byRequestType.", RequestType], 1, "m"},
                       {["erchefAPI.", HostName, ".allRequests"], 1, "m"},
                       {["erchefAPI.application.byOrgname.", OrgName], 1, "m"}]);
send_stat(completed,
          Req,
          State = #state{
            request_type = RequestType,
            hostname = HostName,
            organization_name = OrgName,
            start_time = StartTime,
            couch_time = _CouchTime,
            solr_time = _SolrTime}) ->
    % TODO record and report auth, couch and solr time
    RequestTime = timer:now_diff(now(), StartTime) / 1000000.0,
    StatusCode = integer_to_list(wrq:response_code(Req)),
    send_stats(State, [{["erchefAPI.application.byStatusCode.", StatusCode], 1, "m"},
                       {["erchefAPI.", HostName, ".byStatusCode.", StatusCode], 1, "m"},
                       {"erchefAPI.application.allRequests", RequestTime, "h"},
                       {["erchefAPI.application.byRequestType.", RequestType], RequestTime, "h"},
                       {["erchefAPI.application.byOrgname.", OrgName], RequestTime, "h"},
                       {["erchefAPI.", HostName, ".allRequests"], RequestTime, "h"}]).

send_stats(#state{estatsd_server = EstatsdServer, estatsd_port = EstatsdPort}, Stats) ->
    stats_hero:send(EstatsdServer, EstatsdPort, Stats).

read_req_id(Req, State) ->
    {ok, ReqHeaderName} = application:get_env(chef_rest, req_id_header_name),
    ReqId = case wrq:get_req_header(ReqHeaderName, Req) of
                undefined ->
                    [$b,$o,$g,$u,$s|integer_to_list(random:uniform(500000))];
                HV ->
                    HV
            end,
    State#state{reqid=ReqId}.
