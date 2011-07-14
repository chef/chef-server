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
                solr_query = undefined,
                end_time
}).

-include_lib("webmachine/include/webmachine.hrl").

-define(db_for_guid(X), [<<"chef_">>, X]).
-define(gv(X,L), proplists:get_value(X, L)).
-define(gv(X,L, D), proplists:get_value(X, L, D)).

init(_Any) ->
    State = #state{start_time = chef_rest_util:iso_8601_utc(),
                   resource = atom_to_list(?MODULE)},
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

resource_exists(Req, State) ->
    {true, Req, State}.

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
            %% transform query first to avoid possible db fetch for org
            %% guid for bad queries
            Query = transform_query(http_uri:decode(wrq:get_qs_value("q", Req))),
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

solr_query_url(ObjType, Db, Query) ->
    Fq = "%2BX_CHEF_type_CHEF_X%3A~s+%2BX_CHEF_database_CHEF_X%3Achef_~s",
    Url = "/solr/select?fq=" ++ Fq ++ "&indent=off&q=~s"
        "&start=0"
        "&rows=500"
        "&wt=json"
        "&sort=",
    io_lib:format(Url, [ObjType, Db, Query]).

% /solr/select?
    % fq=%2BX_CHEF_type_CHEF_X%3Anode+%2BX_CHEF_database_CHEF_X%3Achef_288da1c090ff45c987346d2829257256
    % &indent=off
    % &q=content%3Aattr1__%3D__v%2A

% solr:
solr_query(Path) ->
    % FIXME: error handling
    Url = "http://localhost:8983" ++ Path,
    {ok, _Code, _Head, Body} = ibrowse:send_req(Url, [], get),
    Body.

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

to_json(Req, State = #state{couchbeam = S, solr_query = Query}) ->
    % FIXME: probably want to have a request parsing method and
    % implement the valid_request callback.  In that case, we'll put
    % the parsed/validated OrgName and ObjType into fields in the
    % resource state record and they will be available here.
    ObjType = wrq:path_info(object_type, Req),
    try
        Db = fetch_org_guid(Req, State),
        {execute_solr_query(Query, ObjType, Db, S), Req, State}
    catch
        throw:org_not_found ->
            NoOrg = bad_auth_message(org_not_found),
            Req1 = wrq:set_resp_body(ejson:encode(NoOrg), Req),
            {{halt, 404}, Req1, State};
        throw:_X ->
            {{halt, 500}, Req, State}
    end.

transform_query(RawQuery) when is_binary(RawQuery) ->
    case chef_lucene:parse(RawQuery) of
        Query when is_binary(Query) ->
            ibrowse_lib:url_encode(binary_to_list(Query));
        _ ->
            throw({bad_query, RawQuery})
    end;
transform_query(RawQuery) when is_list(RawQuery) ->
    transform_query(list_to_binary(RawQuery)).

execute_solr_query(Query, ObjType, Db, S) ->
    Url = solr_query_url(ObjType, Db, Query),
    SolrDataRaw = solr_query(Url),
    % FIXME: Probably want a solr module that knows how to query solr
    % and parse its responses.
    SolrData = ejson:decode(SolrDataRaw),
    DocList = ej:get({<<"response">>, <<"docs">>}, SolrData),
    Ids = [ ej:get({<<"X_CHEF_id_CHEF_X">>}, Doc) || Doc <- DocList ],
    Ans0 = search_result_start(0, length(Ids)),
    Ans1 = fetch_result_rows(Ids, 5, S, ?db_for_guid(Db), Ans0),
    search_result_finish(Ans1).

    % Docs = chef_otto:bulk_get(S, ?db_for_guid(Db), Ids),
    % %% remove couchdb revision ID from results, mark as objects for JSON
    % %% encoding.
    % JSONDocs = [{lists:keydelete(<<"_rev">>, 1, Doc)} || Doc <- Docs],
    % Ans = {[
    %         {<<"rows">>, JSONDocs},
    %         {<<"start">>, 0},
    %         {<<"total">>, length(JSONDocs)}
    %       ]},
    % ejson:encode(Ans).

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
