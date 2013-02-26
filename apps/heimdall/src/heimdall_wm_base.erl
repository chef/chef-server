-module(heimdall_wm_base).

-export([content_types_accepted/2,
         content_types_provided/2,
         finish_request/2,
         forbidden/2,
         init/2,
         malformed_request/2,
         ping/2,
         post_is_create/2,
         service_available/2,
         validate_requestor/2]).

-include("heimdall_wm.hrl").

init(Resource, Config) ->
    State = #base_state{module = Resource},
    State0 = case Config of
                 [Type, MemberType] ->
                     State#base_state{request_type = Type,
                                      member_type = MemberType};
                 [Type] ->
                     State#base_state{request_type = Type};
                 [] ->
                     State
    end,
    {ok, State0}.

ping(Req, State) ->
    {pong, Req, State}.

service_available(Req, State) ->
    {true, Req, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

malformed_request(Req, #base_state{module = Module} = State) ->
    % These following three may well come back as 'undefined' depending on the
    % requested endpoint, but that's fine, in those cases we don't care anyway since
    % we won't be using them in the first place:
    Id = wrq:path_info(id, Req),
    Action = case wrq:path_info(action, Req) of
                 undefined ->
                     undefined;
                 Permission ->
                     list_to_existing_atom(Permission)
             end,
    MemberId = wrq:path_info(member_id, Req),
    Module:validate_request(Req, State#base_state{authz_id = Id, action = Action,
                                                  member_id = MemberId}).

validate_requestor(Req, State) ->
    State0 = heimdall_wm_util:get_requestor(Req, State),
    case State0#base_state.requestor_id of
        undefined ->
            heimdall_wm_error:set_malformed_request(Req, State, missing_requestor);
        _ ->
            {false, Req, State0}
    end.

forbidden(Req, #base_state{module = Module} = State) ->
    case Module:auth_info(wrq:method(Req)) of
        ignore ->
            {false, Req, State};
        Permission ->
            % TODO: Check that authz_id exists for type
            % TODO: GET ACL for entity, store in #base_state.acl
            case Permission of
                any ->
                    % TODO: check all instead
                    {false, Req, State};
                _Other ->
                    % TODO: check that type instead
                    {false, Req, State}
            end
    end.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

finish_request(Req, State) ->
    {true, Req, State}.
