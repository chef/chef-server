-module(bifrost_wm_transitive_member_resource).

-include("bifrost_wm_rest_endpoint.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-export([to_json/2]).

-spec init([any()]) -> {ok, base_state()}.
init(Config) ->
    bifrost_wm_base:init(?MODULE, Config).

-spec allowed_methods(wm_req(), base_state()) ->  {['GET'], wm_req(), base_state()}.
allowed_methods(Req, State) ->
    {['GET'], Req, State}.

-spec auth_info('GET') -> read.
auth_info('GET') ->
    read.

-spec validate_request(wm_req(), base_state()) ->
			      {{halt, 401 | 403 | 404}, wm_req(), base_state()} |
			      {boolean(), wm_req(), base_state()}.
validate_request(Req, State) ->
    bifrost_wm_base:validate_requestor(Req, State).

%% TODO currently only supports actor since db query depends on db function
%% groups_for_actor and a similar db function for groups does not exist, but
%% will leave code in for groups in case we ever want to implement.
-spec to_json(wm_req(), base_state()) ->
		     {{halt, 404 | 400}, wm_req(), base_state()} |
		     {binary(), wm_req(), base_state()}.
to_json(Req, #base_state{reqid = ReqId,
			 authz_id = GroupAuthzId,
			 member_id = MemberAuthzId,
			 member_type = MemberType} = State) ->
    try
	%% check if member_id exists since it doesn't get checked like base_state(authz_id)
	case ?SH_TIME(ReqId, bifrost_db, exists, (MemberType, MemberAuthzId)) of
	    false ->
		{{halt, 404}, Req, State};
	    true ->
		IsMember = is_member(ReqId, MemberType, MemberAuthzId, GroupAuthzId),
		{bifrost_wm_util:encode({[{<<"is_member">>, IsMember}]}), Req, State}
	end
    catch
	throw:{db_error, Error} ->
            bifrost_wm_error:set_db_exception(Req, State, Error)
    end.

%% @private
-spec is_member(request_id(), auth_type(), auth_id(), auth_id()) -> boolean().
is_member(ReqId, MemberType, MemberAuthzId, GroupAuthzId) ->
    %% Recursively get all groups that contain ActorAuthzId and see if GroupAuthzId is in the list.
    case ?SH_TIME(ReqId, bifrost_db, is_recursive_member_of_group, (MemberType, MemberAuthzId, GroupAuthzId)) of
	{error, Error} ->
	    throw({db_error, Error});
	Result ->
	    Result
    end.
