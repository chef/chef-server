-module(heimdall_wm_error).

-include("heimdall_wm.hrl").

-export([set_db_exception/3,
         set_malformed_request/3]).

%% Sets the error message in the body and returns the return tuple to malformed
%% request (which could contain true or {halt, XXX} if some other return code is
%% appropriate.
set_malformed_request(Req, State, Error) ->
    case malformed_request_message(Error, Req, State) of
        {Code, Msg} when is_integer(Code) ->
            {{halt, Code}, heimdall_wm_util:set_json_body(Req, Msg), State};
        Msg ->
            {true, heimdall_wm_util:set_json_body(Req, Msg), State}
    end.

malformed_request_message(missing_requestor, _Req, _State) ->
    {403, {[{<<"error">>, <<"must specify a requesting actor id">>}]}};
malformed_request_message(invalid_json, _Req, _State) ->
    {[{<<"error">>, <<"invalid JSON in request body">>}]};
malformed_request_message({bad_requestor, Id}, _Req, _State) ->
    {401, {[{<<"error">>, iolist_to_binary([<<"requesting actor id of '">>, Id,
                                            <<"' does not exist">>])}]}};
malformed_request_message(Why, _Req, _State) ->
    error({unexpected_error_message, Why}).

%% Sets the error message in the body and returns the return tuple to malformed
%% request (which should contain {halt, XXX} with whatever return code is
%% appropriate.
set_db_exception(Req, State, Error) ->
    Msg = db_exception_message(Error, Req, State),
    {{halt, 400}, heimdall_wm_util:set_json_body(Req, Msg), State}.

db_exception_message({non_existent_authorizee_for_acl, Type, Id}, _Req, _State) ->
    {[{<<"error">>, iolist_to_binary([<<"attempt to add non-existent ">>,
                                      atom_to_list(Type), <<" '">>, Id,
                                      <<"' to ACL">>])}]};
db_exception_message({non_existent_member_for_group, Type, Id}, _Req, _State) ->
    {[{<<"error">>, iolist_to_binary([<<"attempt to add non-existent ">>,
                                      atom_to_list(Type), <<" '">>, Id,
                                      <<"' to group">>])}]};
db_exception_message({group_cycle, Id}, _Req, _State) ->
    {[{<<"error">>, iolist_to_binary([<<"attempt to add group '">>, Id,
                                      <<"' to itself, which is not allowed">>])}]};
db_exception_message({not_found_in_group, Id}, _Req, _State) ->
    {[{<<"error">>, iolist_to_binary([<<"supplied ID '">>, Id,
                                      <<"' not in group, cannot be deleted">>])}]};
db_exception_message(Why, _Req, _State) ->
    error({unexpected_error_message, Why}).
