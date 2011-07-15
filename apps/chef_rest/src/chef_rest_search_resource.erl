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
         to_json/2]).

-record(state, {start_time,
                resource,
                organization_guid,
                organization_name,
                object_type,
                user_name,
                header_fun = undefined,
                couchbeam = undefined,
                solr_url = undefined,
                solr_query = undefined,
		org_guid = undefined,
                end_time,
                batch_size = 5
}).

-include_lib("webmachine/include/webmachine.hrl").

-define(db_for_guid(X), [<<"chef_">>, X]).
-define(gv(X,L), proplists:get_value(X, L)).
-define(gv(X,L, D), proplists:get_value(X, L, D)).

init(_Any) ->
    {ok, BatchSize} = application:get_env(chef_rest, bulk_fetch_batch_size),
    {ok, SolrUrl} = application:get_env(chef_rest, solr_url),
    State = #state{start_time = chef_rest_util:iso_8601_utc(),
                   resource = atom_to_list(?MODULE),
                   batch_size = BatchSize,
                   solr_url = SolrUrl },
    {ok, State}.

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

resource_exists(Req, State = #state{solr_query = QueryWithoutGuid}) ->
    try
        OrgGuid = fetch_org_guid(Req, State),
        Query = chef_solr:add_org_guid_to_query(QueryWithoutGuid, OrgGuid),
        {true, Req, State#state{org_guid = OrgGuid, solr_query = Query}}
    catch
        throw:org_not_found ->
            NoOrg = bad_auth_message(org_not_found),
            Req1 = wrq:set_resp_body(ejson:encode(NoOrg), Req),
            {false, Req1, State}
    end.

verify_request_signature(Req, State) ->
    OrgName = wrq:path_info(organization_id, Req),
    UserName = wrq:get_req_header("x-ops-userid", Req),
    % TODO Quit leaking this
    S = chef_otto:connect(),
    case chef_otto:fetch_user_or_client_cert(S, OrgName, UserName) of
        not_found ->
            NoCertMsg = bad_auth_message(no_cert),
            {false,
             wrq:set_resp_body(ejson:encode(NoCertMsg), Req), State};
        CertInfo ->
            Cert = ?gv(cert, CertInfo),
            OrgId = ?gv(org_guid, CertInfo, State#state.organization_guid),
            Body = body_or_default(Req, <<>>),
            HTTPMethod = iolist_to_binary(atom_to_list(wrq:method(Req))),
            Path = iolist_to_binary(wrq:path(Req)),
            {GetHeader, State1} = get_header_fun(Req, State),
            case chef_authn:authenticate_user_request(GetHeader, HTTPMethod,
                                                      Path, Body, Cert, 300) of
                {name, _} ->
                    {true, Req,
                     State1#state{organization_guid = OrgId, couchbeam = S}};
                {no_authn, Reason} ->
                    Msg = bad_auth_message(Reason),
                    Json = ejson:encode(Msg),
                    Req1 = wrq:set_resp_body(Json, Req),
                    % TODO This is a needless mutation
                    {false, Req1, State1#state{couchbeam = S}}
            end
    end.

is_authorized(Req, State) ->
    OrgName = list_to_binary(wrq:path_info(organization_id, Req)),
    UserName = list_to_binary(wrq:get_req_header("x-ops-userid", Req)),
    S = chef_otto:connect(),
    case verify_request_signature(Req, State) of
	{true, Req1, State1} ->
	    case chef_otto:is_user_in_org(S, UserName, OrgName) of
		true -> {true, Req1, State1};
		false -> 
		    Msg = bad_auth_message(not_member_of_org),
		    Json = ejson:encode(Msg),
                    Req2 = wrq:set_resp_body(Json, Req),
		    {false, Req2, State1}
	    end;
	Other -> Other
    end.	  

malformed_request(Req, State) ->
    {GetHeader, State1} = get_header_fun(Req, State),
    {Malformed, Req1, State2} =
        try
            chef_authn:validate_headers(GetHeader, 300),
            % We fill in most stuff here in order to validate the query, but we don't add
            % the organization database until resource_exists() (where we get the org id).
            Query = chef_solr:make_query_from_params(Req),
            {false, Req, State1#state{solr_query = Query}}
        catch
            throw:Why ->
                Msg = bad_auth_message(Why),
                NewReq = wrq:set_resp_body(ejson:encode(Msg), Req),
                {true, NewReq, State1}
        end,
    {Malformed, Req1, State2}.

body_or_default(Req, Default) ->
    case wrq:req_body(Req) of
        undefined -> Default;
        Body -> Body
    end.

% FIXME: perhaps rename as these aren't just auth related messages,
% but the organization seems useful.
bad_auth_message(bad_sig) ->
    {struct, [{<<"error">>, [<<"bag signature">>]}]};
bad_auth_message(no_cert) ->
    {struct, [{<<"error">>, [<<"user, client, or organization not found">>]}]};
bad_auth_message({missing_headers, Missing}) ->
    {struct, [{<<"error">>,
               [<<"missing auth headers">>]},
              {<<"missing_headers">>, Missing}]};
bad_auth_message(bad_clock) ->
    {struct, [{<<"error">>, [<<"check clock">>]}]};
bad_auth_message(bad_sign_desc) ->
    {struct, [{<<"error">>, [<<"bad signing description">>]}]};
bad_auth_message(org_not_found) ->
    {struct, [{<<"error">>, [<<"organization not found">>]}]};
bad_auth_message({bad_query, RawQuery}) ->
    {struct, [{<<"error">>, [<<"invalid search query">>]},
              {<<"query">>, RawQuery}]};
bad_auth_message(not_member_of_org) -> 
    {struct, [{<<"error">>, [<<"Not a member of the organization">>]}]};
bad_auth_message(_) ->
    {struct, [{<<"error">>, [<<"problem with headers">>]}]}.



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
                            solr_url = SolrUrl,
                            solr_query = Query,
                            org_guid = OrgGuid,
                            batch_size = BatchSize}) ->
    try
        {ok, Start, NumFound, Ids} = chef_solr:search(SolrUrl, Query),
        {make_search_results(S, OrgGuid, Ids, BatchSize, Start, NumFound), Req, State}
    catch
        throw:X ->
	    io:format("500! ~s~n", X),
            {{halt, 500}, Req, State}
    end.

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
