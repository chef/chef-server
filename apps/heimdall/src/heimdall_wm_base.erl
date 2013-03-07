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
    State = #base_state{module = Resource, reqid = new_request_id()},
    State0 = case Config of
                 [Superuser, Type, MemberType] ->
                     State#base_state{superuser_id = Superuser, request_type = Type,
                                      member_type = MemberType};
                 [Superuser, Type] ->
                     State#base_state{superuser_id = Superuser, request_type = Type};
                 [Superuser] ->
                     State#base_state{superuser_id = Superuser}
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
    try
        State0 = heimdall_wm_util:get_requestor(Req, State),
        case State0#base_state.requestor_id of
            undefined ->
                heimdall_wm_error:set_malformed_request(Req, State, missing_requestor);
            _ ->
                {false, Req, State0}
        end
    catch
        throw:{bad_requestor, Id} ->
            heimdall_wm_error:set_malformed_request(Req, State, {bad_requestor, Id})
    end.

forbidden(Req, #base_state{module = Module, authz_id = Id, request_type = Type,
                           requestor_id = RequestorId} = State) ->
    case Module:auth_info(wrq:method(Req)) of
        ignore ->
            {false, Req, State};
        Permission ->
            case heimdall_db:exists(Type, Id) of
                false ->
                    {{halt, 404}, Req, State};
                true ->
                    Result = case Permission of
                                 any ->
                                     heimdall_acl:check_any_access(Type, Id, RequestorId);
                                 Other ->
                                     heimdall_acl:check_access(Type, Id, RequestorId, Other)
                             end,
                    case Result of
                        true ->
                            {false, Req, State};
                        false ->
                            heimdall_wm_error:set_access_exception(Req, State, Permission)
                    end
            end
    end.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

finish_request(Req, State) ->
    {true, Req, State}.

%% @doc Generates a new, unique request ID that we can use to attach
%% metrics to.  Taken from chef_wm, with the exception that we do not
%% currently offer the option to take a pre-generated request ID from
%% a header value.
%%
%% This generated ID is unique to Heimdall, and is distinct from the
%% requests to Erchef, Reporting, Pushy, etc. that ultimately trigger
%% this request.
-spec new_request_id() -> request_id().
new_request_id() ->
    base64:encode(crypto:md5(term_to_binary(make_ref()))).
