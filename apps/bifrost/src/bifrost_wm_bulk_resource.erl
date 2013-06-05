-module(bifrost_wm_bulk_resource).

-include("bifrost_wm_rest_endpoint.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-mixin([{bifrost_wm_base, [create_path/2]}]).

-export([process_post/2]).

init(Config) ->
    bifrost_wm_base:init(?MODULE, Config).

post_is_create(Req, State) ->
    % We're not creating anything (for a change), just processing information, so
    % we want to return 200's instead of 201's here; also, instead of from_json, we'll
    % be using process_post below
    {false, Req, State}.

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

validate_request(Req, State) ->
    % We really don't care if there's a requestor or not, because (1) we're not
    % limiting access permissions on this endpoint to any particular requestors, and
    % (2) the requesting actor in the header and the requestor for which we are
    % requesting information need not be the same.
    {false, Req, State}.

auth_info(_Method) ->
    ignore.

process_post(Req, State) ->
    try
        Body = wrq:req_body(Req),
        {ReqId, Permission, Type, Collection} = parse_body(Body),
        case ?SH_TIME(ReqId, bifrost_db, bulk_check, (ReqId, Permission, Type)) of
            List when is_list(List) ->
                Excluded = lists:filter(fun(X) -> not lists:member(X, List) end,
                                        Collection),
                case Excluded of
                    [] ->
                        {true, wrq:set_resp_body(<<"{}">>, Req), State};
                    Unauthorized ->
                        Req0 = bifrost_wm_util:set_json_body(Req, {[{<<"unauthorized">>,
                                                                     Unauthorized}]}),
                        {true, Req0, State}
                end;
            {error, ErrorType} ->
                bifrost_wm_error:set_db_exception(Req, State, ErrorType)
        end
    catch
        throw:{error, invalid_json} ->
            {_Return, ErrReq, _State} =
                bifrost_wm_error:set_malformed_request(Req, State, invalid_json),
            {{halt, 400}, ErrReq, State};
        throw:{db_error, Error} ->
            bifrost_wm_error:set_db_exception(Req, State, Error)
    end.

parse_body(Body) ->
    try
        Ejson = bifrost_wm_util:decode(Body),
        ReqId = ej:get({<<"requestor_id">>}, Ejson),
        Permission = binary_to_atom(ej:get({<<"permission">>}, Ejson), utf8),
        AuthType = binary_to_atom(ej:get({<<"type">>}, Ejson), utf8),
        Collection = ej:get({<<"collection">>}, Ejson),
        case {ReqId, Collection} of
            {Id, Targets} when is_binary(Id) andalso is_list(Targets) ->
                case Permission of
                    Perm when Perm =:= create orelse Perm =:= read orelse
                              Perm =:= update orelse Perm =:= delete orelse
                              Perm =:= grant ->
                        case AuthType of
                            Type when Type =:= actor orelse Type =:= container orelse
                                      Type =:= group orelse Type =:= object ->
                                {ReqId, Permission, AuthType, Collection};
                            _ ->
                                throw({error, invalid_json})
                        end;
                    _ ->
                        throw({error, invalid_json})
                end;
            {_, _} ->
                throw({error, invalid_json})
        end
    catch
        error:badarg ->
            throw({error, invalid_json});
        throw:{error, {_, invalid_json}} ->
            throw({error, invalid_json});
        throw:{error, {_, truncated_json}} ->
            throw({error, invalid_json})
    end.
