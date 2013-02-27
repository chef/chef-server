-module(heimdall_wm_named_resources).

-include("heimdall_wm_rest_endpoint.hrl").

-export([delete_resource/2,
         to_json/2]).

init(Config) ->
    heimdall_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['GET', 'DELETE'], Req, State}.

validate_request(Req, State) ->
    heimdall_wm_base:validate_requestor(Req, State).

auth_info('GET') ->
    read;
auth_info('DELETE') ->
    delete.

% maybe move this to heimdall_wm_util?  At the moment, however, this is the
% only place it's used; there's no other way to directly get group membership
get_members(Type, AuthzId) ->
    case heimdall_db:group_membership(Type, AuthzId) of
        {error, Error} ->
            throw({db_error, Error});
        List ->
            List
    end.

to_json(Req, #base_state{authz_id = AuthzId, request_type = RequestType} = State) ->
    case RequestType of
        group ->
            try
                Actors = get_members(actor, AuthzId),
                Groups = get_members(group, AuthzId),
                {heimdall_wm_util:encode({[{<<"actors">>, Actors},
                                           {<<"groups">>, Groups}]}), Req, State}
            catch
                throw:{db_error, Error} ->
                    heimdall_wm_error:set_db_exception(Req, State, Error)
            end;
        _ ->
            {<<"{}">>, Req, State}
    end.

delete_resource(Req, #base_state{authz_id = AuthzId,
                                 request_type = Type} = State) ->
    case heimdall_db:delete(Type, AuthzId) of
        ok ->
            {true, Req, State};
        {error, Error} ->
            heimdall_wm_error:set_db_exception(Req, State, Error)
    end.
