-module(bifrost_wm_transitive_member_resource).

-include("bifrost_wm_rest_endpoint.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-export([to_json/2]).

init(Config) ->
    bifrost_wm_base:init(?MODULE, Config).

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

auth_info('GET') ->
    read.

validate_request(Req, State) ->
    bifrost_wm_base:validate_requestor(Req, State).

to_json(Req, #base_state{reqid = ReqId, 
			 authz_id = GroupAuthzId, 
			 member_id = MemberAuthzId,
			 member_type = MemberType} = State) ->
    try
	%% check if member_id exists and get it's id (authz_id already validated)
	case check_exists_and_get_id(ReqId, MemberType, MemberAuthzId) of
	    not_found -> {{halt, 404}, Req, State};
	    MemberId ->  
		lager:error("this: ~p", [is_member(ReqId, MemberId, GroupAuthzId)]),
		{is_member(ReqId, MemberId, GroupAuthzId), Req, State}
	end
    catch
	throw:{db_error, Error} ->
            bifrost_wm_error:set_db_exception(Req, State, Error)
    end.

check_exists_and_get_id(ReqId, Type, MemberAuthzId) ->
    ?SH_TIME(ReqId, bifrost_db, get_id, (Type, MemberAuthzId)).

is_member(ReqId, ActorId, GroupAuthzId) ->
    %% Recursively get all groups that contain ActorAuthzId and see if GroupAuthzId is in the list.
    lager:error("final id ~p", [ActorId]),
    IsMember = case ?SH_TIME(ReqId, bifrost_db, get_all_parent_groups_for_actor, (ActorId)) of
		   {error, Error} ->
		       throw({db_error, Error});
		   List ->
		       lager:error("listss: ~p ~p", [GroupAuthzId, List]),
		       lists:member(list_to_binary(GroupAuthzId), List)
	       end,
    bifrost_wm_util:encode({[{<<"is_member">>, IsMember}]}).
