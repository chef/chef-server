-module(bifrost_wm_error).

-include("bifrost_wm.hrl").

-export([set_db_exception/3,
         set_access_exception/3,
         set_malformed_request/3]).

%% Handy helper function for error handling
error_ejson(Message) when is_binary(Message) ->
    {[{<<"error">>, Message}]};
error_ejson(Message) when is_list(Message) ->
    error_ejson(iolist_to_binary(Message)).

halt(Code, Req, State, Msg) when is_integer(Code) ->
    {{halt, Code}, bifrost_wm_util:set_json_body(Req, Msg), State};
halt(Value, Req, State, Msg) when is_boolean(Value) ->
    {Value, bifrost_wm_util:set_json_body(Req, Msg), State}.

%% Sets the error message in the body and returns the return tuple to malformed
%% request (which could contain true or {halt, XXX} if some other return code is
%% appropriate.
-spec set_malformed_request(wm_req(), base_state(), atom() | tuple()) ->
                                {{halt, non_neg_integer()}, wm_req(), base_state()} |
                                {true, wm_req(), base_state()}.
set_malformed_request(Req, State, Error) ->
    {Code, Msg} = malformed_request_message(Error),
    halt(Code, Req, State, Msg).

% TODO: change this to 401 in the future?  Right now the old server returns 403,
% so keeping it for compatibility although it's arguably wrong
malformed_request_message(missing_requestor) ->
    {403, error_ejson(<<"must specify a requesting actor id">>)};
malformed_request_message(invalid_json) ->
    {true, error_ejson(<<"invalid JSON in request body">>)};
malformed_request_message({bad_requestor, Id}) ->
    {401, error_ejson([<<"requesting actor id of '">>, Id, <<"' does not exist">>])};
malformed_request_message({ej_invalid, Message}) ->
    {true, error_ejson([Message])};
malformed_request_message({missing, Type}) ->
    {true, error_ejson([<<"required request key '">>, Type, <<"' is missing">>])};
malformed_request_message({json_type, Type}) ->
    {true, error_ejson([<<"request key '">>, Type, <<"' is incorrect JSON type">>])};
malformed_request_message(Why) ->
    error({unexpected_error_message, Why}).

%% Sets the error message in the body and returns the return tuple to forbidden
-spec set_access_exception(wm_req(), base_state(), permission()) ->
                                {true, wm_req(), base_state()}.
set_access_exception(Req, State, Permission) ->
    Msg = access_exception_message(Permission),
    halt(true, Req, State, Msg).

access_exception_message(any) ->
    error_ejson(<<"must be in one of the create, read, update, delete, grant "
                  "access control entries to perform this action">>);
access_exception_message(Other) ->
    error_ejson([<<"must be in the ">>, atom_to_list(Other),
                 <<" access control entry to perform this action">>]).

%% Sets the error message in the body and returns the return tuple to malformed
%% request (which should contain {halt, XXX} with whatever return code is
%% appropriate.
-spec set_db_exception(wm_req(), base_state(), tuple()) ->
                                {{halt, 400}, wm_req(), base_state()}.
set_db_exception(Req, State, Error) ->
    Msg = db_exception_message(Error),
    halt(400, Req, State, Msg).

find_non_existent(_, []) ->
    none;
find_non_existent(Type, [Head|Tail]) ->
    case bifrost_db:exists(Type, Head) of
        true ->
            find_non_existent(Type, Tail);
        false ->
            {Head, Type}
    end.

find_non_existent_member(Actors, Groups) ->
    case find_non_existent(actor, Actors) of
        none ->
            find_non_existent(group, Groups);
        Other ->
            Other
    end.

db_exception_message({non_existent_authorizee_for_acl, Type, Id}) ->
    error_ejson([<<"attempt to add non-existent ">>, atom_to_list(Type), <<" '">>, Id,
                 <<"' to ACL">>]);
db_exception_message({non_existent_member_for_acl, Actors, Groups}) ->
    % In this error, all we know is that we triggered an error with an invalid actor
    % or group somewhere, so we have to go find it:
    {Id, Type} = find_non_existent_member(Actors, Groups),
    error_ejson([<<"attempt to add non-existent ">>, atom_to_list(Type), <<" '">>, Id,
                 <<"' to ACL">>]);
db_exception_message({non_existent_member_for_group, Type, Id}) ->
    error_ejson([<<"attempt to add non-existent ">>, atom_to_list(Type), <<" '">>, Id,
                 <<"' to group">>]);
db_exception_message({group_cycle, Id}) ->
    error_ejson([<<"attempt to add group '">>, Id,
                 <<"' would create a cycle, which is not allowed">>]);
db_exception_message({not_found_in_group, Id}) ->
    error_ejson([<<"supplied ID '">>, Id, <<"' not in group, cannot be deleted">>]);
db_exception_message({invalid_actor, Id}) ->
    error_ejson([<<"supplied ID '">>, Id, <<"' does not exist">>]);
db_exception_message(Why) ->
    error({unexpected_error_message, Why}).
