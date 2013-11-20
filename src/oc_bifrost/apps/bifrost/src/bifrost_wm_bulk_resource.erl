-module(bifrost_wm_bulk_resource).

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
        case ?SH_TIME(ReqId, bifrost_db, bulk_permission,
                      (ReqId, Collection, Permission, Type)) of
            Authorized when is_list(Authorized) ->
                %% The query above returns which of the supplied targets in the
                %% parsed collection have permission, but we need to return the
                %% opposite, so we do this:
                Unauthorized = sets:to_list(sets:subtract(sets:from_list(Collection),
                                                          sets:from_list(Authorized))),
                case Unauthorized of
                    [] ->
                        % If we don't set a body here (or anywhere else, of course,
                        % but that shouldn't happen anyway), web machine should
                        % return 204, which is exactly the behavior we want.
                        {true, Req, State};
                    Unauthorized ->
                        Req0 = bifrost_wm_util:set_json_body(Req, {[{<<"unauthorized">>,
                                                                     Unauthorized}]}),
                        {true, Req0, State}
                end;
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

valid_perm(<<"create">>) -> ok;
valid_perm(<<"read">>) -> ok;
valid_perm(<<"update">>) -> ok;
valid_perm(<<"delete">>) -> ok;
valid_perm(<<"grant">>) -> ok;
valid_perm(_) -> error.

valid_type(<<"actor">>) -> ok;
valid_type(<<"container">>) -> ok;
valid_type(<<"group">>) -> ok;
valid_type(<<"object">>) -> ok;
valid_type(_) -> error.

regex_for(authz_id) ->
    {ok, Regex} = re:compile("^[a-fA-F0-9]{32}$"),
    {Regex, <<"invalid authz ID, must be 32-digit hex string">>}.

bulk_spec() ->
    {[
      {<<"requestor_id">>, {string_match, regex_for(authz_id)}},
      {<<"permission">>, {fun_match, {fun valid_perm/1, string,
                                      <<"invalid permission type, must be 'create',"
                                        " 'read', 'update', 'delete', or 'grant'">>}}},
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
                ReqId = ej:get({<<"requestor_id">>}, Ejson),
                Permission = binary_to_atom(ej:get({<<"permission">>}, Ejson), utf8),
                AuthType = binary_to_atom(ej:get({<<"type">>}, Ejson), utf8),
                Collection = ej:get({<<"collection">>}, Ejson),
                {ReqId, Permission, AuthType, Collection};
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
