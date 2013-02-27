-module(heimdall_wm_error).

-include("heimdall_wm.hrl").

-export([set_db_exception/3,
         set_malformed_request/3]).

%% Sets the error message in the body and returns the return tuple to malformed
%% request (which could contain true or {halt, XXX} if some other return code is
%% appropriate.
set_malformed_request(Req, State, Error) ->
    Msg = malformed_request_message(Error, Req, State),
    {true, heimdall_wm_util:set_json_body(Req, Msg), State}.

malformed_request_message(missing_requestor, _Req, _State) ->
    {[{<<"error">>, <<"must specify a requesting actor id">>}]};
malformed_request_message(invalid_json, _Req, _State) ->
    {[{<<"error">>, <<"invalid JSON in request body">>}]};
malformed_request_message({bad_requestor, Id}, _Req, _State) ->
    {[{<<"error">>, iolist_to_binary([<<"requesting actor id of '">>, Id,
                                      <<"' does not exist">>])}]};
malformed_request_message(Why, _Req, _State) ->
    error({unexpected_error_message, Why}).

%% Sets the error message in the body and returns the return tuple to malformed
%% request (which should contain {halt, XXX} with whatever return code is
%% appropriate.

%% So far, all errors are unexpected, we bail on everything:
set_db_exception(Req, State, Error) ->
    Msg = db_exception_message(Error, Req, State),
    {{halt, 400}, heimdall_wm_util:set_json_body(Req, Msg), State}.

db_exception_message({non_existent_authorizee_for_acl, Type, Id}, _Req, _State) ->
    {[{<<"error">>, iolist_to_binary([<<"attempt to add non-existent ">>,
                                      atom_to_list(Type), <<" '">>, Id,
                                      <<"' to ACL">>])}]};
db_exception_message({non_existent_actor_for_group, Id}, _Req, _State) ->
    {[{<<"error">>, iolist_to_binary([<<"attempt to add non-existent actor '">>, Id,
                                      <<"' to group">>])}]};
db_exception_message({non_existent_group_for_group, Id}, _Req, _State) ->
    {[{<<"error">>, iolist_to_binary([<<"attempt to add non-existent group '">>, Id,
                                      <<"' to group">>])}]};
db_exception_message(Why, _Req, _State) ->
    error({unexpected_error_message, Why}).
