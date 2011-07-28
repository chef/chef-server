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
         forbidden/2,
         resource_exists/2,
         allowed_methods/2,
         content_types_provided/2,
         finish_request/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("chef_rest_search_resource.hrl").

-define(db_for_guid(X), [<<"chef_">>, X]).
-define(gv(X,L), proplists:get_value(X, L)).
-define(gv(X,L, D), proplists:get_value(X, L, D)).

init(_Any) ->
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
%% {{trace, "/tmp/traces"}, State}.
%% then in console: wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp/traces").
%% then go to localhost:WXYZ/wmtrace

malformed_request(Req, State) ->
    State1 = read_req_id(Req, State),
    % This is the first method we get called on, so this is where we
    % send stats for org name.
    OrgName = wrq:path_info(organization_id, Req),
    State2 = State1#state{organization_name = OrgName},
    send_stat(received, Req, State2),
    {GetHeader, State3} = get_header_fun(Req, State2),
    try
        chef_authn:validate_headers(GetHeader, 300),
        % We fill in most stuff here in order to validate the query,
        % but we don't add the organization database until
        % resource_exists() (where we get the org id).
        Query = chef_solr:make_query_from_params(Req),
        {false, Req, State3#state{solr_query = Query}}
    catch
        throw:Why ->
            Msg = malformed_request_message(Why, GetHeader),
            NewReq = wrq:set_resp_body(ejson:encode(Msg), Req),
            {true, NewReq, State3}
    end.

is_authorized(Req, State) ->
    case verify_request_signature(Req, State) of
	{true, Req1, State1} ->
            {true, Req1, State1};
	{false, ReqOther, StateOther} ->
            {"X-Ops-Sign version=\"1.0\"", ReqOther, StateOther}
    end.

forbidden(Req, State = #state{organization_name = OrgName}) ->
    UserName = list_to_binary(wrq:get_req_header("x-ops-userid", Req)),
    S = chef_otto:connect(),
    case chef_otto:is_user_in_org(S, UserName, OrgName) of
        true ->
            {false, Req, State};
        false ->
            Msg = is_authorized_message(not_member_of_org, UserName, OrgName),
            {true, wrq:set_resp_body(ejson:encode(Msg), Req), State}
    end.

resource_exists(Req, State = #state{solr_query = QueryWithoutGuid}) ->
    try
        OrgGuid = fetch_org_guid(Req, State),
        Query = chef_solr:add_org_guid_to_query(QueryWithoutGuid, OrgGuid),
        {true, Req, State#state{organization_guid = OrgGuid, solr_query = Query}}
    catch
        throw:org_not_found ->
            %% Not sure we can ever get here; user in org check will
            %% have failed with 403 if no such org.
            NoOrg = resource_exists_message(org_not_found, 
                                            wrq:path_info(organization_id, Req)),
            Req1 = wrq:set_resp_body(ejson:encode(NoOrg), Req),
            {false, Req1, State}
    end.

get_header_fun(Req, State = #state{header_fun = HFun})
  when HFun =:= undefined ->
    GetHeader = fun(H) ->
                        Name = case is_binary(H) of
                                   true -> binary_to_list(H);
                                   false -> H
                               end,
                        case wrq:get_req_header(string:to_lower(Name), Req) of
                            B when is_binary(B) -> B;
                            S when is_list(S) -> iolist_to_binary(S);
                            undefined -> undefined
                        end
                end,
    {GetHeader, State#state{header_fun = GetHeader}};
get_header_fun(_Req, State) ->
    {State#state.header_fun, State}.

-spec verify_request_signature(#wm_reqdata{}, #state{}) ->
                                      {boolean(), #wm_reqdata{}, #state{}}.
%% @doc Perform request signature verification (authenticate)
%% 
%% Fetches user or client certificate and uses it verify the signature
%% on the request.  If the request cannot be verified, then the
%% returned `#wm_reqdata{}' record will have a response body
%% explaining why.
verify_request_signature(Req, State = #state{organization_name = OrgName}) ->
    UserName = wrq:get_req_header("x-ops-userid", Req),
    S = chef_otto:connect(),
    case chef_otto:fetch_user_or_client_cert(S, OrgName, UserName) of
        {not_found, What} ->
            NotFoundMsg = verify_request_message({not_found, What},
                                                 UserName, OrgName),
            {false, wrq:set_resp_body(ejson:encode(NotFoundMsg), Req), State};
        CertInfo ->
            %% TODO this causes us to fetch the organization a second
            %% time.  Keep it and pass it in instead.
            Cert = ?gv(cert, CertInfo),
            Body = body_or_default(Req, <<>>),
            HTTPMethod = iolist_to_binary(atom_to_list(wrq:method(Req))),
            Path = iolist_to_binary(wrq:path(Req)),
            {GetHeader, State1} = get_header_fun(Req, State),
            case chef_authn:authenticate_user_request(GetHeader, HTTPMethod,
                                                      Path, Body, Cert, 300) of
                {name, _} ->
                    {true, Req,
                     State1#state{couchbeam = S,
                                  organization_guid = ?gv(org_guid, CertInfo)}};
                {no_authn, Reason} ->
                    Msg = verify_request_message(Reason, UserName, OrgName),
                    Json = ejson:encode(Msg),
                    Req1 = wrq:set_resp_body(Json, Req),
                    {false, Req1, State1}
            end
    end.

body_or_default(Req, Default) ->
    case wrq:req_body(Req) of
        undefined -> Default;
        Body -> Body
    end.

is_authorized_message(Type, User, Org) when Type =:= not_member_of_org;
                                            Type =:= bad_sig ->
    Msg = iolist_to_binary([<<"'">>, User, <<"' not authorized to search '">>,
                            Org, <<"'.">>]),
    {[{<<"error">>, [Msg]}]};
is_authorized_message(org_not_found, _User, Org) ->
    Msg = iolist_to_binary([<<"organization '">>, Org,
                            <<"' does not exist.">>]),
    {[{<<"error">>, [Msg]}]}.

resource_exists_message(org_not_found, Org) ->
    Msg = iolist_to_binary([<<"organization '">>, Org,
                            <<"' does not exist.">>]),
    {[{<<"error">>, [Msg]}]}.

verify_request_message({not_found, client}, User, _Org) ->
    Msg = iolist_to_binary([<<"Failed to authenticate as '">>, User, <<"'. ">>,
                            <<"Ensure that your node_name and client key ">>,
                            <<"are correct.">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message({not_found, org}, _User, Org) ->
    Msg = iolist_to_binary([<<"organization '">>, Org,
                            <<"' does not exist.">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message(bad_sig, User, Org) ->
    is_authorized_message(bad_sig, User, Org).

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
	    io:format("500! ~p~n", [X]),
            {{halt, 500}, Req, State}
    end.

finish_request(Req, State) ->
    try
        send_stat(completed, Req, State)
    catch
        X:Y ->
            error_logger:error_report({X, Y, erlang:get_stacktrace()})
    end,
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
    encode_results(Docs, Acc);
fetch_result_rows({Ids, Rest}, BatchSize, S, Db, Acc) ->
    Next = safe_split(BatchSize, Rest),
    Docs = chef_otto:bulk_get(S, Db, Ids),
    fetch_result_rows(Next, BatchSize, S, Db,
                      encode_results(Docs, <<",">>, Acc)).

encode_results([], Acc) ->
    Acc;
encode_results(Results, Acc) ->
    [encode_result_rows(Results) | Acc].

encode_results([], _Prefix, Acc) ->
    Acc;
encode_results(Results, Prefix, Acc) ->
    [Prefix, encode_result_rows(Results) | Acc].

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
    {[{<<"error">>, [Msg]}]};
malformed_request_message({bad_param, {Param, Value}}, _GetHeader) ->
    Msg = iolist_to_binary([<<"invalid '">>, Param, <<"' value: '">>, Value, <<"'">>]),
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

send_stat(received,_Any, #state{request_type=RequestType,
                                hostname=HostName,
                                organization_name=OrgName,
                                reqid=ReqId}=State) ->
    send_stats(State, [{"erchefAPI.application.allRequests", 1, "m"},
                       {["erchefAPI.application.byRequestType.", RequestType], 1, "m"},
                       {["erchefAPI.", HostName, ".allRequests"], 1, "m"},
                       {["erchefAPI.application.byOrgname.", OrgName], 1, "m"}]),
    fast_log:info(erchef, ReqId, "request started");
send_stat(completed, Req, #state{request_type=RequestType,
                                 hostname=HostName,
                                 organization_name=OrgName,
                                 start_time=StartTime,
                                 reqid=ReqId}=State) ->
    RequestTime = timer:now_diff(now(), StartTime) div 1000,
    StatusCode = integer_to_list(wrq:response_code(Req)),
    send_stats(State, [{["erchefAPI.application.byStatusCode.", StatusCode], 1, "m"},
                       {["erchefAPI.", HostName, ".byStatusCode.", StatusCode], 1, "m"},
                       {"erchefAPI.application.allRequests", RequestTime, "h"},
                       {["erchefAPI.application.byRequestType.", RequestType], RequestTime, "h"},
                       {["erchefAPI.application.byOrgname.", OrgName], RequestTime, "h"},
                       {["erchefAPI.", HostName, ".allRequests"], RequestTime, "h"}]),
    fast_log:info(erchef, ReqId, "request processing time: ~B", [RequestTime]).

send_stats(#state{estatsd_server = EstatsdServer, estatsd_port = EstatsdPort}, Stats) ->
    stats_hero:send(EstatsdServer, EstatsdPort, Stats).

read_req_id(Req, State) ->
    {ok, ReqHeaderName} = application:get_env(chef_rest, reqid_header_name),
    ReqId = case wrq:get_req_header(ReqHeaderName, Req) of
                undefined ->
                    binary_to_list(base64:encode(term_to_binary(make_ref())));
                HV ->
                    HV
            end,
    State#state{reqid=ReqId}.
