-module(bifrost_wm_bulk_delete_resource).

-include("bifrost_wm_rest_endpoint.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-mixin([{bifrost_wm_base, [create_path/2]}]).

-export([
         process_post/2
        ]).

init(Config) ->
    bifrost_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

validate_request(Req, State) ->
    bifrost_wm_base:validate_requestor(Req, State).

auth_info(_Method) ->
    ignore.

process_post(Req, State) ->
    try
        Body = wrq:req_body(Req),
        {Type, Collection} = parse_body(Body),
        case ?SH_TIME(<<"superuser">>, bifrost_db, bulk_delete,
                      (Collection, Type)) of
            CountDeleted when is_integer(CountDeleted) ->
                Req0 = bifrost_wm_util:set_json_body(Req, {[{<<"count">>,
                                                             CountDeleted}]}),
                {true, Req0, State};
            {error, Error} ->
                bifrost_wm_error:set_db_exception(Req, State, Error)
        end
    catch
        throw:{error, ErrorType} ->
            {_Return, ErrReq, _State} =
                bifrost_wm_error:set_malformed_request(Req, State, ErrorType),
            {{halt, 400}, ErrReq, State};
        throw:{db_error, ErrorType} ->
            bifrost_wm_error:set_db_exception(Req, State, ErrorType)
    end.

valid_type(<<"actor">>) -> ok;
valid_type(<<"group">>) -> ok.

normalize_type(<<"actor">>) -> actor;
normalize_type(<<"group">>) -> group.


regex_for(authz_id) ->
    {ok, Regex} = re:compile("^[a-fA-F0-9]{32}$"),
    {Regex, <<"invalid authz ID, must be 32-digit hex string">>}.

bulk_spec() ->
    {[
      {<<"type">>, {fun_match, {fun valid_type/1, string,
                                <<"invalid authz type, must be 'actor', 'container',"
                                  " 'group', or 'object'">>}}},
      {<<"collection">>, {array_map, {string_match, regex_for(authz_id)}}}
     ]}.

parse_body(Body) ->
    try
        Ejson = bifrost_wm_util:decode(Body),
        case ej:valid(bulk_spec(), Ejson) of
            ok ->
                AuthType = normalize_type(ej:get({<<"type">>}, Ejson)),
                Collection = ej:get({<<"collection">>}, Ejson),
                {AuthType, Collection};
            BadSpec ->
                throw(BadSpec)
        end
    catch
        throw:{error, {_, invalid_json}} ->
            throw({error, invalid_json});
        throw:{error, {_, truncated_json}} ->
            throw({error, invalid_json});
        throw:{ej_invalid, string_match, _, _, _, _, Message} ->
            throw({error, {ej_invalid, Message}});
        throw:{ej_invalid, fun_match, _, _, _, _, Message} ->
            throw({error, {ej_invalid, Message}});
        throw:{ej_invalid, array_elt, _, _, _, _, Message} ->
            throw({error, {ej_invalid, Message}});
        throw:{ej_invalid, missing, Type, _, _, _, _} ->
            throw({error, {missing, Type}});
        throw:{ej_invalid, json_type, Type, _, _, _, _} ->
            throw({error, {json_type, Type}})
    end.
